(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WebAssembly code generation from AffineScript AST.

    This module translates type-checked and borrow-checked AffineScript
    programs into WebAssembly modules.
*)

open Ast
open Wasm

(** Ownership kind for typed-wasm schema annotations.
    Maps AffineScript ownership qualifiers to typed-wasm Level 7/10 verification. *)
type ownership_kind =
  | Unrestricted  (** Plain value, no ownership constraint (Wasm i32/f64 etc.) *)
  | Linear        (** TyOwn / own — consumed exactly once (typed-wasm Level 10 linearity) *)
  | SharedBorrow  (** TyRef / ref — read-only aliasing safety (typed-wasm Level 7) *)
  | ExclBorrow    (** TyMut / mut — exclusive mutable aliasing safety (typed-wasm Level 7) *)

(** Code generation context *)
type context = {
  types : func_type list;            (** type section *)
  funcs : func list;                 (** function definitions *)
  exports : export list;             (** exports *)
  imports : import list;             (** imports *)
  globals : global list;             (** global variables *)
  locals : (string * int) list;      (** local variable name to index map *)
  next_local : int;                  (** next available local index *)
  loop_depth : int;                  (** current loop nesting depth *)
  func_indices : (string * int) list;
  (** Top-level name environment shared by functions and constants.
      - [k >= 0]: Wasm function index (imports + defined functions).
                  Populated by both [TopFn] (defined function) and
                  [TopFn _ with fd_body = FnExtern] (host-supplied import).
      - [k < 0]:  Constant (global): actual global index is [-(k+1)].
                  Populated by [TopConst].
      Entries are inserted in source declaration order by [gen_decl]. *)
  lambda_funcs : func list;          (** lifted lambda functions *)
  next_lambda_id : int;              (** next lambda function ID *)
  heap_ptr : int option;             (** global index for heap pointer, if initialized *)
  field_layouts : (string * (string * int) list) list;  (** variable name -> [(field, offset)] *)
  struct_layouts : (string * (string * int) list) list;
  (** Struct type name -> [(field, offset)]. Registered from TopType(TyStruct)
      at decl time so function-parameter and call-result field accesses can
      recover the field layout by type, not by let-binding shape. *)
  fn_ret_structs : (string * string) list;
  (** Function name -> struct type name it returns (if any). Lets a
      `let s = make()` call site register s's field layout in field_layouts
      when the callee's return type is a known struct. *)
  variant_tags : (string * int) list;  (** constructor name -> tag (int) *)
  string_data : (string * int) list; (** string content -> memory offset *)
  next_string_offset : int;          (** next available offset for string data *)
  datas : data list;                 (** data segments *)
  ownership_annots : (int * ownership_kind list * ownership_kind) list;
  (** Collected ownership annotations: (func_index, param_kinds, return_kind).
      Emitted as the [affinescript.ownership] Wasm custom section for typed-wasm
      Level 7/10 verification. Kind encoding: 0=Unrestricted, 1=Linear, 2=SharedBorrow, 3=ExclBorrow. *)
  wasi_func_indices : (string * int) list;
  (** ADR-015 S4 (#180): WASI preview1 import name → wasm func index.
      Populated at module-assembly time from the optional-imports
      pre-scan. `fd_write` is always present at 0; other entries
      (`clock_time_get`, `environ_sizes_get`, `args_sizes_get`, …) are
      added on-demand in a canonical order, with indices computed by
      position. WASI builtin special-cases look up their own import
      index by name from this map. *)
}

(** Code generation error *)
type codegen_error =
  | UnsupportedFeature of string
  | UnboundVariable of string
  | TypeMismatch of string
[@@deriving show]

type 'a result = ('a, codegen_error) Result.t

(** Result bind operator *)
let ( let* ) = Result.bind

(** Count imported functions (for index offsets) *)
let import_func_count (ctx : context) : int =
  List.fold_left (fun acc imp ->
    match imp.i_desc with
    | ImportFunc _ -> acc + 1
    | _ -> acc
  ) 0 ctx.imports

(** Create initial context *)
let create_context () : context = {
  types = [];
  funcs = [];
  exports = [];
  imports = [];
  globals = [];
  locals = [];
  next_local = 0;
  loop_depth = 0;
  func_indices = [];
  lambda_funcs = [];
  next_lambda_id = 0;
  heap_ptr = None;
  field_layouts = [];
  struct_layouts = [];
  fn_ret_structs = [];
  variant_tags = [];
  string_data = [];
  next_string_offset = 2048;  (* Start strings after heap at offset 2048 *)
  datas = [];
  ownership_annots = [];
  wasi_func_indices = [];
}

(** Extract ownership kind from a parameter declaration.
    Checks p_ownership first; falls back to the shape of p_ty. *)
let ownership_kind_of_param (p : param) : ownership_kind =
  match p.p_ownership with
  | Some Own -> Linear
  | Some Ref -> SharedBorrow
  | Some Mut -> ExclBorrow
  | None ->
    match p.p_ty with
    | TyOwn _ -> Linear
    | TyRef _ -> SharedBorrow
    | TyMut _ -> ExclBorrow
    | _ -> Unrestricted

(** If [ty] names a known struct (through any number of own/ref/mut wrappers),
    return that struct's name. Lets us recover a struct's field layout from
    parameter and return-type annotations so `.field_N` reads use the correct
    offset instead of defaulting to 0. *)
let rec struct_name_of_ty (ty : type_expr) : string option =
  match ty with
  | TyCon id -> Some id.name
  | TyApp (id, _) -> Some id.name
  | TyOwn inner | TyRef inner | TyMut inner -> struct_name_of_ty inner
  | _ -> None

(** Extract ownership kind from an optional return type expression *)
let ownership_kind_of_ret (ret : type_expr option) : ownership_kind =
  match ret with
  | Some (TyOwn _) -> Linear
  | Some (TyRef _) -> SharedBorrow
  | Some (TyMut _) -> ExclBorrow
  | _ -> Unrestricted

(** Encode an ownership_kind as a single byte (0–3) *)
let ownership_kind_byte = function
  | Unrestricted -> 0 | Linear -> 1 | SharedBorrow -> 2 | ExclBorrow -> 3

(** Build the payload for the [affinescript.ownership] Wasm custom section.
    Encoding (all little-endian):
      u32  entry_count
      per entry:
        u32  func_index
        u8   param_count
        u8*  param_kind  (one per param, see kind encoding above)
        u8   return_kind *)
let build_ownership_section (annots : (int * ownership_kind list * ownership_kind) list) : bytes =
  if annots = [] then Bytes.empty
  else
    let buf = Buffer.create 64 in
    let write_u32_le n =
      Buffer.add_char buf (Char.chr  (n         land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr  8) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
    in
    let write_u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
    write_u32_le (List.length annots);
    List.iter (fun (func_idx, param_kinds, ret_kind) ->
      write_u32_le func_idx;
      write_u8 (List.length param_kinds);
      List.iter (fun k -> write_u8 (ownership_kind_byte k)) param_kinds;
      write_u8 (ownership_kind_byte ret_kind)
    ) annots;
    Buffer.to_bytes buf

(** Map AffineScript type to WASM value type *)
let type_to_wasm (ty : type_expr) : value_type result =
  match ty with
  | TyCon id when id.name = "Float" -> Ok F64
  | TyCon id when id.name = "Bool" -> Ok I32
  | TyCon id when id.name = "Int" -> Ok I32
  | TyCon id when id.name = "Char" -> Ok I32
  | TyCon id when id.name = "String" -> Ok I32  (* pointer to heap *)
  | TyCon id when id.name = "Nat" -> Ok I32
  | TyCon _ -> Ok I32  (* default for user types — heap pointer *)
  | TyApp _ | TyTuple _ | TyRecord _ | TyArrow _ -> Ok I32
  | _ -> Ok I32  (* conservative default *)

(** Allocate a new local variable *)
let alloc_local (ctx : context) (name : string) : (context * int) =
  let idx = ctx.next_local in
  let locals' = (name, idx) :: ctx.locals in
  ({ ctx with locals = locals'; next_local = idx + 1 }, idx)

(** Look up local variable index *)
let lookup_local (ctx : context) (name : string) : int result =
  match List.assoc_opt name ctx.locals with
  | Some idx -> Ok idx
  | None -> Error (UnboundVariable name)

(** Generate code to bind a pattern to the value on the WASM stack.
    Assumes the RHS value is already on the stack.
    Returns instructions that consume the stack value and bind locals. *)
let rec gen_pattern_bind (ctx : context) (pat : pattern) : (context * instr list) result =
  match pat with
  | PatVar id ->
    let (ctx', idx) = alloc_local ctx id.name in
    Ok (ctx', [LocalSet idx])
  | PatWildcard _ ->
    (* Discard the value *)
    Ok (ctx, [Drop])
  | PatTuple pats ->
    (* Value is a heap pointer to the tuple. Store it in a temp, then
       load each element at its offset and bind the sub-pattern. *)
    let (ctx', tmp_idx) = alloc_local ctx "__tuple_tmp" in
    let n = List.length pats in
    let* (ctx_final, elem_codes) = List.fold_left (fun acc (i, sub_pat) ->
      let* (c, codes) = acc in
      (* Load tuple element: memory[tmp + i*4] *)
      let load_code = [
        LocalGet tmp_idx;
        I32Const (Int32.of_int (i * 4));
        I32Add;
        I32Load (2, 0);
      ] in
      let* (c', bind_code) = gen_pattern_bind c sub_pat in
      Ok (c', codes @ load_code @ bind_code)
    ) (Ok (ctx', [])) (List.mapi (fun i p -> (i, p)) pats) in
    let _ = n in
    Ok (ctx_final, [LocalSet tmp_idx] @ elem_codes)
  | PatAs (id, sub_pat) ->
    (* Bind the whole value to id, then also match sub-pattern *)
    let (ctx', idx) = alloc_local ctx id.name in
    (* Duplicate value: store to local, get it back for sub-pattern *)
    let* (ctx'', sub_code) = gen_pattern_bind ctx' sub_pat in
    Ok (ctx'', [LocalTee idx] @ sub_code)
  | _ ->
    (* Other patterns (literals, constructors, records, or) need runtime
       checking which is complex in WASM. For now, treat as a variable
       bind of the whole value with a generated name. *)
    let (ctx', idx) = alloc_local ctx "__pat_bind" in
    Ok (ctx', [LocalSet idx])

(** Ensure heap pointer global is initialized.
    Returns (context, heap_global_idx). *)
let ensure_heap_ptr (ctx : context) : (context * int) =
  match ctx.heap_ptr with
  | Some idx -> (ctx, idx)
  | None ->
    (* Create heap pointer global initialized to 1024 (1KB) *)
    let idx = List.length ctx.globals in
    let heap_global = {
      g_type = I32;
      g_mutable = true;
      g_init = [I32Const 1024l];  (* Start heap at 1KB *)
    } in
    ({ ctx with
       globals = ctx.globals @ [heap_global];
       heap_ptr = Some idx }, idx)

(** Generate code to allocate memory on the heap.
    Returns instructions that leave the allocated address on the stack.
    size_in_bytes: number of bytes to allocate *)
let gen_heap_alloc (ctx : context) (size_in_bytes : int) : (context * instr list) =
  let (ctx', heap_idx) = ensure_heap_ptr ctx in
  (* Get current heap pointer, then increment it *)
  let alloc_code = [
    GlobalGet heap_idx;           (* Get current heap address *)
    GlobalGet heap_idx;           (* Get it again *)
    I32Const (Int32.of_int size_in_bytes);  (* Size to allocate *)
    I32Add;                       (* Calculate new heap pointer *)
    GlobalSet heap_idx;           (* Update heap pointer *)
    (* Stack now has the allocated address *)
  ] in
  (ctx', alloc_code)

(** Find free variables in an expression.
    Returns list of variable names that are used but not bound within the expression.
    bound_vars: variables already bound in enclosing scope (parameters, let bindings) *)
let rec find_free_vars (bound_vars : string list) (expr : expr) : string list =
  match expr with
  | ExprLit _ -> []
  | ExprVar id ->
    if List.mem id.name bound_vars then [] else [id.name]
  | ExprBinary (e1, _, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprUnary (_, e) ->
    find_free_vars bound_vars e
  | ExprIf ei ->
    find_free_vars bound_vars ei.ei_cond @
    find_free_vars bound_vars ei.ei_then @
    (match ei.ei_else with
     | Some e -> find_free_vars bound_vars e
     | None -> [])
  | ExprLet lb ->
    let rhs_free = find_free_vars bound_vars lb.el_value in
    (* Add bound variable to scope for body *)
    let new_bound = match lb.el_pat with
      | PatVar id -> id.name :: bound_vars
      | _ -> bound_vars
    in
    let body_free = match lb.el_body with
      | Some e -> find_free_vars new_bound e
      | None -> []
    in
    rhs_free @ body_free
  | ExprLambda lam ->
    (* Parameters are bound within lambda *)
    let param_names = List.map (fun p -> p.p_name.name) lam.elam_params in
    find_free_vars (param_names @ bound_vars) lam.elam_body
  | ExprApp (f, args) ->
    find_free_vars bound_vars f @
    List.concat (List.map (find_free_vars bound_vars) args)
  | ExprBlock blk ->
    (* Statements may introduce bindings *)
    let (bound_after, free) = List.fold_left (fun (bound, acc_free) stmt ->
      match stmt with
      | StmtLet sl ->
        let rhs_free = find_free_vars bound sl.sl_value in
        let new_bound = match sl.sl_pat with
          | PatVar id -> id.name :: bound
          | _ -> bound
        in
        (new_bound, acc_free @ rhs_free)
      | StmtExpr e ->
        (bound, acc_free @ find_free_vars bound e)
      | _ -> (bound, acc_free)
    ) (bound_vars, []) blk.blk_stmts in
    (* The tail expression is in scope of the block's own `let`
       bindings, so its free vars must exclude them — use the
       threaded [bound_after], not the original [bound_vars]. (Prior
       code used [bound_vars], spuriously reporting block-local
       binders as free; surfaced by #225 PR3c chained continuations.) *)
    let expr_free = match blk.blk_expr with
      | Some e -> find_free_vars bound_after e
      | None -> []
    in
    free @ expr_free
  | ExprMatch m ->
    find_free_vars bound_vars m.em_scrutinee @
    List.concat (List.map (fun arm -> find_free_vars bound_vars arm.ma_body) m.em_arms)
  | ExprReturn e_opt ->
    (match e_opt with Some e -> find_free_vars bound_vars e | None -> [])
  | ExprTuple exprs | ExprArray exprs ->
    List.concat (List.map (find_free_vars bound_vars) exprs)
  | ExprRecord r ->
    List.concat (List.map (fun (_, e_opt) ->
      match e_opt with
      | Some e -> find_free_vars bound_vars e
      | None -> []
    ) r.er_fields)
  | ExprField (e, _) -> find_free_vars bound_vars e
  | ExprTupleIndex (e, _) -> find_free_vars bound_vars e
  | ExprIndex (e1, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprVariant _ -> []
  | ExprSpan (e, _) -> find_free_vars bound_vars e
  | _ -> []  (* Other expressions *)

(** Remove duplicates from list *)
let dedup (lst : string list) : string list =
  List.fold_left (fun acc x ->
    if List.mem x acc then acc else x :: acc
  ) [] lst |> List.rev

(** Generate code for a literal *)
let gen_literal (ctx : context) (lit : literal) : (context * instr) result =
  match lit with
  | LitUnit _ -> Ok (ctx, I32Const 0l)  (* Unit represented as 0 *)
  | LitBool (b, _) -> Ok (ctx, I32Const (if b then 1l else 0l))
  | LitInt (n, _) -> Ok (ctx, I32Const (Int32.of_int n))
  | LitFloat (f, _) -> Ok (ctx, F64Const f)
  | LitChar (c, _) -> Ok (ctx, I32Const (Int32.of_int (Char.code c)))
  | LitString (s, _) ->
    (* Check if string already exists *)
    begin match List.assoc_opt s ctx.string_data with
      | Some offset ->
        (* String already in memory, return pointer *)
        Ok (ctx, I32Const (Int32.of_int offset))
      | None ->
        (* Add new string to data section *)
        let offset = ctx.next_string_offset in
        let str_bytes = Bytes.of_string s in
        let str_len = Bytes.length str_bytes in
        (* String layout: [length: i32][...utf8 bytes...] *)
        let len_bytes = Bytes.create 4 in
        Bytes.set_int32_le len_bytes 0 (Int32.of_int str_len);
        let full_data = Bytes.cat len_bytes str_bytes in
        let data_segment = { d_data = full_data; d_offset = offset } in
        let ctx' = {
          ctx with
          string_data = (s, offset) :: ctx.string_data;
          next_string_offset = offset + 4 + str_len;
          datas = data_segment :: ctx.datas;
        } in
        Ok (ctx', I32Const (Int32.of_int offset))
    end

(** Generate code for binary operation *)
let gen_binop (op : binary_op) : instr =
  match op with
  | OpAdd -> I32Add
  | OpSub -> I32Sub
  | OpMul -> I32Mul
  | OpDiv -> I32DivS
  | OpMod -> I32RemS
  | OpEq -> I32Eq
  | OpNe -> I32Ne
  | OpLt -> I32LtS
  | OpLe -> I32LeS
  | OpGt -> I32GtS
  | OpGe -> I32GeS
  | OpAnd -> I32And
  | OpOr -> I32Or
  | OpBitAnd -> I32And
  | OpBitOr -> I32Or
  | OpBitXor -> I32Xor
  | OpShl -> I32Shl
  | OpShr -> I32ShrS
  | OpConcat -> I32Add (* Placeholder *)

(** Generate code for unary operation *)
let gen_unop (op : unary_op) : instr result =
  match op with
  | OpNeg -> Ok I32Sub  (* 0 - x *)
  | OpNot -> Ok I32Eqz  (* x == 0 *)
  | OpBitNot -> Ok I32Xor (* -1 ^ x *)
  | OpRef | OpMutRef -> Error (UnsupportedFeature "OpRef/OpMutRef handled in ExprUnary")
  | OpDeref -> Error (UnsupportedFeature "OpDeref handled in ExprUnary")

(** ADR-013 #225 PR3c — recursive CPS hook. The async-boundary transform
    ([detect_async_base_case] + [gen_async_base_case]) is defined below
    [gen_expr] but must be reachable from *inside* the continuation
    lambda's body generation so that a continuation which is itself an
    async boundary is transformed too (Async→Async chaining). A forward
    reference, populated once at module init, breaks the definition-order
    cycle without relocating the whole transform into the rec group.
    Returns [Some result] when [expr] matched the async shape (and
    `thenableThen` is importable), else [None] ⇒ caller lowers normally.
    Recursion terminates: each application peels exactly one async
    boundary off a finite, strictly-smaller continuation. *)
let async_transform_hook
  : (context -> expr -> (context * instr list) result option) ref
  = ref (fun _ _ -> None)

(** Generate code for an expression, returning instructions and updated context *)
let rec gen_expr (ctx : context) (expr : expr) : (context * instr list) result =
  match expr with
  | ExprLit lit ->
    let* (ctx', instr) = gen_literal ctx lit in
    Ok (ctx', [instr])

  | ExprVar id ->
    begin match lookup_local ctx id.name with
      | Ok idx -> Ok (ctx, [LocalGet idx])
      | Error _ ->
        (* Fallback: bare identifier that names a zero-arity enum variant.
           Matches the ExprCall resolution at line 658 so that both
           `Initialised` and `Initialised()` work as expressions when the
           name is known as a variant constructor. Without this, a match
           arm body of the form `Uninitialised => Initialised` fails with
           UnboundVariable even though the parser accepts it. *)
        begin match List.assoc_opt id.name ctx.variant_tags with
          | Some tag -> Ok (ctx, [I32Const (Int32.of_int tag)])
          | None ->
            (* Top-level const bindings are stored in func_indices with a
               negative sentinel: actual global index = -(k+1). *)
            begin match List.assoc_opt id.name ctx.func_indices with
              | Some k when k < 0 -> Ok (ctx, [GlobalGet (-(k + 1))])
              | _ -> Error (UnboundVariable id.name)
            end
        end
    end

  | ExprBinary (left, OpConcat, right) ->
    (* List concatenation `a ++ b`. `OpConcat` was a placeholder `I32Add`
       (it just summed the two list pointers), so every `++` produced a
       garbage/zero-length list. Real implementation, in the canonical
       list layout fixed by #255: `[len@+0][elem i @ +4 + i*4]`.
       Allocate len(a)+len(b), copy a's then b's elements. *)
    let* (ctx1, left_code)  = gen_expr ctx  left  in
    let* (ctx2, right_code) = gen_expr ctx1 right in
    let (ctx3, heap_idx) = ensure_heap_ptr ctx2 in
    let (ctx4, a)   = alloc_local ctx3 "__cat_a"   in
    let (ctx5, b)   = alloc_local ctx4 "__cat_b"   in
    let (ctx6, la)  = alloc_local ctx5 "__cat_la"  in
    let (ctx7, lb)  = alloc_local ctx6 "__cat_lb"  in
    let (ctx8, dst) = alloc_local ctx7 "__cat_dst" in
    let (ctx9, k)   = alloc_local ctx8 "__cat_k"   in
    let copy_loop src_ptr count dst_base_off =
      (* for k in 0..count: dst[dst_base_off + k] = src[k]
         element addr = ptr + idx*4, value/store via static +4 offset
         (skips the length word) — exactly the #255 convention. *)
      [ I32Const 0l; LocalSet k;
        Block (BtEmpty, [ Loop (BtEmpty, [
          LocalGet k; LocalGet count; I32GeS; BrIf 1;
          (* dst slot: dst + (dst_base_off + k)*4 *)
          LocalGet dst;
          LocalGet dst_base_off; LocalGet k; I32Add;
          I32Const 4l; I32Mul; I32Add;
          (* value: src[k] = *(src + k*4 + 4) *)
          LocalGet src_ptr; LocalGet k; I32Const 4l; I32Mul; I32Add;
          I32Load (2, 4);
          I32Store (2, 4);
          LocalGet k; I32Const 1l; I32Add; LocalSet k;
          Br 0 ]) ]) ]
    in
    let (ctxA, zero) = alloc_local ctx9 "__cat_zero" in
    let code =
      left_code @ [LocalSet a] @ right_code @ [LocalSet b] @
      [ LocalGet a; I32Load (2, 0); LocalSet la;
        LocalGet b; I32Load (2, 0); LocalSet lb;
        I32Const 0l; LocalSet zero;
        (* dst = heap; heap += 4 + (la+lb)*4 *)
        GlobalGet heap_idx; LocalSet dst;
        GlobalGet heap_idx;
        I32Const 4l;
        LocalGet la; LocalGet lb; I32Add; I32Const 4l; I32Mul;
        I32Add; I32Add;
        GlobalSet heap_idx;
        (* dst length = la + lb *)
        LocalGet dst; LocalGet la; LocalGet lb; I32Add; I32Store (2, 0) ]
      @ copy_loop a la zero   (* dst[0..la)  := a *)
      @ copy_loop b lb la     (* dst[la..)   := b *)
      @ [ LocalGet dst ]
    in
    Ok (ctxA, code)

  | ExprBinary (left, op, right) ->
    let* (ctx', left_code) = gen_expr ctx left in
    let* (ctx'', right_code) = gen_expr ctx' right in
    let op_instr = gen_binop op in
    Ok (ctx'', left_code @ right_code @ [op_instr])

  | ExprUnary (op, operand) ->
    begin match op with
      | OpRef | OpMutRef ->
        (* Take reference: &expr / &mut expr — same pointer representation;
           exclusivity is a static borrow property (CORE-01 pt2 / #177). *)
        (* Allocate heap memory, store the value, return pointer *)
        let* (ctx', operand_code) = gen_expr ctx operand in
        let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx' 4 in
        let (ctx_with_ptr, ptr_idx) = alloc_local ctx_with_heap "__ref_ptr" in
        let (ctx_with_val, val_idx) = alloc_local ctx_with_ptr "__ref_val" in

        (* Strategy: alloc, save ptr, eval operand, save val, store val at ptr, return ptr *)
        let ref_code = alloc_code @ [
          LocalSet ptr_idx;     (* Save allocated pointer *)
        ] @ operand_code @ [    (* Evaluate operand (value on stack) *)
          LocalSet val_idx;     (* Save value *)
          LocalGet ptr_idx;     (* Load pointer *)
          LocalGet val_idx;     (* Load value *)
          I32Store (2, 0);      (* Store: mem[ptr+0] = value *)
          LocalGet ptr_idx;     (* Return pointer *)
        ] in
        Ok (ctx_with_val, ref_code)

      | OpDeref ->
        (* Dereference: *ptr *)
        (* Load value from pointer *)
        let* (ctx', ptr_code) = gen_expr ctx operand in
        let deref_code = [
          I32Load (2, 0);  (* Load i32 from pointer *)
        ] in
        Ok (ctx', ptr_code @ deref_code)

      | _ ->
        (* Other unary ops *)
        let* (ctx', operand_code) = gen_expr ctx operand in
        let* op_instr = gen_unop op in
        let prefix = match op with
          | OpNeg -> [I32Const 0l]  (* 0 - operand *)
          | _ -> []
        in
        Ok (ctx', prefix @ operand_code @ [op_instr])
    end

  | ExprIf ei ->
    let* (ctx', cond_code) = gen_expr ctx ei.ei_cond in
    let* (ctx'', then_code) = gen_expr ctx' ei.ei_then in
    let else_result = match ei.ei_else with
      | Some e -> gen_expr ctx'' e
      | None -> Ok (ctx'', [I32Const 0l])  (* Default to 0 if no else *)
    in
    let* (ctx_final, else_code) = else_result in
    Ok (ctx_final, cond_code @ [If (BtType I32, then_code, else_code)])

  | ExprBlock blk ->
    gen_block ctx blk

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e ->
        let* (ctx', code) = gen_expr ctx e in
        Ok (ctx', code @ [Return])
      | None ->
        Ok (ctx, [Return])
    end

  | ExprLet lb ->
    let* (ctx', rhs_code) = gen_expr ctx lb.el_value in
    let* (ctx'', pat_code) = gen_pattern_bind ctx' lb.el_pat in
    begin match lb.el_body with
      | Some body ->
        let* (ctx_final, body_code) = gen_expr ctx'' body in
        Ok (ctx_final, rhs_code @ pat_code @ body_code)
      | None ->
        Ok (ctx'', rhs_code @ pat_code @ [I32Const 0l])
    end

  | ExprLambda lam ->
    (* Detect free variables (captured from enclosing scope) *)
    let param_names = List.map (fun p -> p.p_name.name) lam.elam_params in
    let all_free = find_free_vars param_names lam.elam_body in
    (* Filter to only variables currently in scope *)
    let captured_vars = List.filter (fun name ->
      List.mem_assoc name ctx.locals
    ) (dedup all_free) in

    let lambda_id = ctx.next_lambda_id in

    (* If there are captured variables, create closure environment *)
    let (ctx_after_env, env_code) = if List.length captured_vars > 0 then
      (* Create environment tuple with captured values *)
      let num_captured = List.length captured_vars in
      let env_size = num_captured * 4 in
      let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx env_size in
      let (ctx_with_temp, env_idx) = alloc_local ctx_with_heap "__closure_env" in

      let save_code = [LocalTee env_idx] in

      (* Store each captured variable in environment *)
      (* Note: Each store consumes env_ptr and value, but we push env_ptr before each store,
         so after all stores, one env_ptr remains on stack *)
      let store_code = List.mapi (fun i var_name ->
        let var_idx = List.assoc var_name ctx.locals in
        [
          LocalGet env_idx;
          LocalGet var_idx;
          I32Store (2, i * 4);
        ]
      ) captured_vars |> List.concat in

      (* Don't push env_idx again - one is already on stack after stores *)
      (ctx_with_temp, alloc_code @ save_code @ store_code)
    else
      (* No captures - environment is null (0) *)
      (ctx, [I32Const 0l])
    in

    (* Create fresh context for lambda function. [next_lambda_id] is
       advanced *before* body generation so a nested lambda created
       while lowering this body (e.g. a chained CPS continuation, #225
       PR3c) gets a distinct id rather than re-using [lambda_id]. *)
    let lambda_ctx =
      { ctx_after_env with
        locals = []; next_local = 0; loop_depth = 0;
        next_lambda_id = lambda_id + 1 } in

    (* Environment is always first parameter (even if unused) for uniform calling convention *)
    let (ctx_with_env, _) = alloc_local lambda_ctx "__env" in
    let env_param_offset = 1 in

    (* Regular parameters come after environment *)
    let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
      alloc_local c param.p_name.name
    ) (ctx_with_env, 0) lam.elam_params in

    (* Add captured variables to local scope (load from environment) *)
    let (ctx_with_captured, load_captured_code) = if List.length captured_vars > 0 then
      let (c, code) = List.fold_left (fun (c_acc, code_acc) (i, var_name) ->
        let (c', var_idx) = alloc_local c_acc var_name in
        let load_code = [
          LocalGet 0;  (* Environment pointer *)
          I32Load (2, i * 4);
          LocalSet var_idx;
        ] in
        (c', code_acc @ load_code)
      ) (ctx_with_params, []) (List.mapi (fun i v -> (i, v)) captured_vars) in
      (c, code)
    else
      (ctx_with_params, [])
    in

    let param_count = env_param_offset + List.length lam.elam_params in

    (* Generate lambda body *)
    (* #225 PR3c: if the lambda body is itself an async boundary (a
       continuation that chains another async call), transform it so
       Thenables compose up the chain; otherwise lower normally. *)
    let* (ctx_final, body_code) =
      match !async_transform_hook ctx_with_captured lam.elam_body with
      | Some r -> r
      | None -> gen_expr ctx_with_captured lam.elam_body
    in

    (* Compute additional locals (beyond parameters and captured vars) *)
    let local_count = ctx_final.next_local - param_count in
    let locals = if local_count > 0 then
      [{ l_count = local_count; l_type = I32 }]
    else
      []
    in

    (* Create function type for lambda (env param always included + regular params) *)
    let param_types = I32 :: List.map (fun _ -> I32) lam.elam_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Thread the POST-body module-level state forward while keeping the
       enclosing scope's local state. The body may have mutated module
       accumulators (a nested lambda + its types/globals/datas, #225
       PR3c chaining); rebuilding from [ctx_after_env] would silently
       drop them. Enclosing locals/next_local/loop_depth/field_layouts
       stay from [ctx_after_env] (the lambda's inner locals must not
       leak out). For a non-nested body these module fields equal
       [ctx_after_env]'s, so behaviour is unchanged. *)
    let ctx_post = { ctx_after_env with
      types = ctx_final.types;
      funcs = ctx_final.funcs;
      exports = ctx_final.exports;
      imports = ctx_final.imports;
      globals = ctx_final.globals;
      func_indices = ctx_final.func_indices;
      lambda_funcs = ctx_final.lambda_funcs;
      next_lambda_id = ctx_final.next_lambda_id;
      heap_ptr = ctx_final.heap_ptr;
      struct_layouts = ctx_final.struct_layouts;
      fn_ret_structs = ctx_final.fn_ret_structs;
      variant_tags = ctx_final.variant_tags;
      string_data = ctx_final.string_data;
      next_string_offset = ctx_final.next_string_offset;
      datas = ctx_final.datas;
      ownership_annots = ctx_final.ownership_annots;
    } in

    (* Add type to types list *)
    let type_idx = List.length ctx_post.types in
    let ctx_with_type = { ctx_post with types = ctx_post.types @ [func_type] } in

    (* Create lambda function *)
    let lambda_func = {
      f_type = type_idx;
      f_locals = locals;
      f_body = load_captured_code @ body_code;
    } in

    (* The closure's stored function id MUST be this lambda's index in
       the final [lambda_funcs] list, because the element segment maps
       table slot i -> the i-th lambda's wasm func (see gen_module), and
       wrapHandler dispatches via `table.get(fnId)`. The pre-reserved
       [lambda_id] equals the list position ONLY for non-nested lambdas
       (id order == append order). A nested lambda (e.g. a chained CPS
       continuation, #225 PR3c) is appended *before* its enclosing
       lambda yet has a *higher* id, so id ≠ position there. Use the
       append position (= current list length) instead. *)
    let lambda_slot = List.length ctx_with_type.lambda_funcs in
    let ctx_with_lambda = {
      ctx_with_type with
      lambda_funcs = ctx_with_type.lambda_funcs @ [lambda_func];
    } in

    (* Return a closure: (function_id, env_pointer) as a 2-element tuple *)
    let closure_size = 8 in  (* 2 * 4 bytes *)
    let (ctx_final2, closure_alloc) = gen_heap_alloc ctx_with_lambda closure_size in
    let (ctx_final3, closure_idx) = alloc_local ctx_final2 "__closure" in

    (* `LocalSet`, not `LocalTee`: the alloc'd pointer must be consumed
       into the local, NOT left on the stack — every subsequent use
       re-fetches it via `LocalGet closure_idx`, and the comments below
       ("Stack is now [closure_idx, env_ptr]") assume an empty stack
       here. With `LocalTee` the dangling pointer made `closure_code`
       leave TWO values; this never failed before only because the #199
       closure path had never been validated by a real wasm engine
       end-to-end (static-only until #225 PR2). *)
    let closure_code = closure_alloc @ [LocalSet closure_idx] @ [
      (* Store function ID at offset 0 *)
      LocalGet closure_idx;
      I32Const (Int32.of_int lambda_slot);
      I32Store (2, 0);
    ] @ [
      (* Store environment pointer at offset 4 *)
      LocalGet closure_idx;
    ] @ env_code @ [
      (* env_code left env_ptr on stack, closure_idx is below it *)
      (* Stack is now [closure_idx, env_ptr] with env_ptr on top *)
      I32Store (2, 4);
      (* Return closure pointer *)
      LocalGet closure_idx;
    ] in

    Ok (ctx_final3, closure_code)

  | ExprApp (func_expr, args) ->
    (* Check for built-in WASI functions first *)
    begin match func_expr with
      | ExprVar id when id.name = "print" && List.length args = 1 ->
        (* print(x) - print integer without newline *)
        let* (ctx_with_arg, arg_code) = gen_expr ctx (List.hd args) in

        (* Allocate temp local to hold the value *)
        let (ctx_with_temp, value_temp) = alloc_local ctx_with_arg "__print_value" in

        (* Ensure heap pointer is initialized *)
        let (ctx_with_heap, heap_idx) = ensure_heap_ptr ctx_with_temp in

        (* Get or create fd_write import - assume it's at index 0 for now *)
        let fd_write_idx = 0 in

        (* Generate WASI print code *)
        let print_code = arg_code @ [LocalSet value_temp] @
          Wasi_runtime.gen_print_int heap_idx value_temp fd_write_idx in

        Ok (ctx_with_heap, print_code)

      | ExprVar id when id.name = "println" && List.length args = 0 ->
        (* println() - print newline *)
        let (ctx_with_temp, temp_local) = alloc_local ctx "__println_temp" in
        (* Ensure heap pointer is initialized *)
        let (ctx_with_heap, heap_idx) = ensure_heap_ptr ctx_with_temp in
        let fd_write_idx = 0 in
        let println_code = Wasi_runtime.gen_println heap_idx fd_write_idx temp_local in
        Ok (ctx_with_heap, println_code)

      | ExprVar id when id.name = "println" && List.length args = 1 ->
        (* println(s) - print string and newline *)
        let* (ctx_with_arg, arg_code) = gen_expr ctx (List.hd args) in
        let (ctx_with_ptr, str_ptr) = alloc_local ctx_with_arg "__println_str_ptr" in
        let (ctx_with_temp, temp_local) = alloc_local ctx_with_ptr "__println_temp" in
        let (ctx_with_heap, heap_idx) = ensure_heap_ptr ctx_with_temp in
        let fd_write_idx = 0 in
        let print_code =
          arg_code @
          [LocalSet str_ptr] @
          Wasi_runtime.gen_print_str heap_idx str_ptr fd_write_idx temp_local @
          [Drop] @
          Wasi_runtime.gen_println heap_idx fd_write_idx temp_local
        in
        Ok (ctx_with_heap, print_code)

      | ExprVar id when id.name = "clock_now_ms" && List.length args = 1 ->
        (* ADR-015 S4a (#180): clock_now_ms(clock_id) -> Int ms. Lowers
           to a `wasi_snapshot_preview1.clock_time_get` call; the import
           is added on-demand at module assembly (S4b refactor), and the
           index lives in [ctx.wasi_func_indices] (no hardcoded idx).
           Under the S3 component path the reactor adapter bridges this
           to wasi:clocks. Effect row is `Time` (tracking-only). *)
        let* (ctx_with_arg, arg_code) = gen_expr ctx (List.hd args) in
        let (ctx_with_arg2, clock_arg_local) =
          alloc_local ctx_with_arg "__clock_id" in
        let (ctx_with_scratch, scratch_local) =
          alloc_local ctx_with_arg2 "__clock_scratch" in
        let (ctx_with_heap, heap_idx) = ensure_heap_ptr ctx_with_scratch in
        let clock_func_idx =
          try List.assoc "clock_time_get" ctx.wasi_func_indices
          with Not_found -> 1 (* defensive; pre-scan guarantees presence *)
        in
        let code =
          arg_code @ [LocalSet clock_arg_local] @
          Wasi_runtime.gen_clock_now_ms
            heap_idx clock_arg_local scratch_local clock_func_idx
        in
        Ok (ctx_with_heap, code)

      | ExprVar id when id.name = "net_shutdown" && List.length args = 2 ->
        (* ADR-015 S6b (#180): net_shutdown(fd, how) -> Int errno.
           Lowers to a `wasi_snapshot_preview1.sock_shutdown` import
           (on-demand, via the same Effect_sites pre-scan as the S4
           builtins). The command adapter bridges to `wasi:sockets/tcp`
           at runtime. Pure pass-through: push both args, call. *)
        let* (ctx_fd,  fd_code)  = gen_expr ctx     (List.nth args 0) in
        let* (ctx_how, how_code) = gen_expr ctx_fd  (List.nth args 1) in
        let sock_func_idx =
          try List.assoc "sock_shutdown" ctx.wasi_func_indices
          with Not_found -> 1
        in
        let code = fd_code @ how_code @ [Call sock_func_idx] in
        Ok (ctx_how, code)

      | ExprVar id when (id.name = "env_count" || id.name = "arg_count")
                        && List.length args = 1 ->
        (* ADR-015 S4b (#180): env_count(u: Unit) / arg_count(u: Unit)
           — i32 count returns. Lower to the matching
           `wasi_snapshot_preview1.{environ,args}_sizes_get` import
           (added on-demand at module assembly; idx looked up in
           [ctx.wasi_func_indices]). The Unit arg satisfies the
           zero-param-fn collapse wart; it is evaluated but its value
           is unused. String accessors (env_at/arg_at) need byte-level
           wasm IR ops (currently absent) and are a tracked follow-up. *)
        let wasi_name =
          if id.name = "env_count" then "environ_sizes_get"
          else "args_sizes_get"
        in
        let sizes_func_idx =
          try List.assoc wasi_name ctx.wasi_func_indices
          with Not_found -> 1
        in
        let* (ctx_with_arg, arg_code) = gen_expr ctx (List.hd args) in
        let (ctx_with_scratch, scratch_local) =
          alloc_local ctx_with_arg ("__" ^ id.name ^ "_scratch") in
        let (ctx_with_heap, heap_idx) = ensure_heap_ptr ctx_with_scratch in
        let code =
          arg_code @ [Drop] @
          Wasi_runtime.gen_count_via_sizes_get
            heap_idx scratch_local sizes_func_idx
        in
        Ok (ctx_with_heap, code)

      | ExprVar id when List.mem_assoc id.name ctx.variant_tags ->
        (* Enum constructor called as a function: Circle(5), Rect({x:1,y:2}), etc.
           Layout: [tag: i32][field1: i32][field2: i32]...
           Reuses the same heap-boxing approach as ExprVariant with args. *)
        let tag = List.assoc id.name ctx.variant_tags in
        let num_fields = List.length args in
        let size_in_bytes = 4 + (num_fields * 4) in

        let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
        let (ctx_with_temp, variant_ptr) = alloc_local ctx_with_heap "__variant_ptr" in

        let store_tag = [
          LocalTee variant_ptr;
          I32Const (Int32.of_int tag);
          I32Store (2, 0);
        ] in

        let rec store_args ctx' offset arg_list =
          match arg_list with
          | [] -> Ok (ctx', [])
          | arg :: rest ->
            let* (ctx'', arg_code) = gen_expr ctx' arg in
            let store_code = [LocalGet variant_ptr] @ arg_code @ [I32Store (2, offset)] in
            let* (ctx''', rest_code) = store_args ctx'' (offset + 4) rest in
            Ok (ctx''', store_code @ rest_code)
        in

        let* (ctx_final, args_store_code) = store_args ctx_with_temp 4 args in
        Ok (ctx_final, alloc_code @ store_tag @ args_store_code @ [LocalGet variant_ptr])

      | _ ->
        (* Not a built-in, proceed with normal function call *)
        (* Generate code for arguments (left to right) *)
        let* (ctx_after_args, all_arg_code) = List.fold_left (fun acc arg ->
          let* (ctx', accumulated_code) = acc in
          let* (ctx'', arg_code) = gen_expr ctx' arg in
          Ok (ctx'', accumulated_code @ arg_code)
        ) (Ok (ctx, [])) args in

        (* Generate code for function expression *)
        begin match func_expr with
          | ExprVar id ->
            (* Check if it's a named function or a variable holding a lambda *)
            begin match List.assoc_opt id.name ctx_after_args.func_indices with
          | Some func_idx ->
            (* Direct function call *)
            let call_instr = Call func_idx in
            Ok (ctx_after_args, all_arg_code @ [call_instr])
          | None ->
            (* Check if it's a local variable (could be a closure) *)
            begin match lookup_local ctx_after_args id.name with
              | Ok local_idx ->
                (* Closure is a tuple: (func_id, env_ptr) *)
                (* Load function ID and environment, then call indirect *)

                (* Allocate temp locals for closure components *)
                let (ctx_temp1, func_id_idx) = alloc_local ctx_after_args "__func_id" in
                let (ctx_temp2, env_ptr_idx) = alloc_local ctx_temp1 "__env_ptr" in

                (* Extract closure components *)
                let extract_closure = [
                  (* Load function ID from offset 0 *)
                  LocalGet local_idx;
                  I32Load (2, 0);
                  LocalSet func_id_idx;

                  (* Load environment pointer from offset 4 *)
                  LocalGet local_idx;
                  I32Load (2, 4);
                  LocalSet env_ptr_idx;
                ] in

                (* Create type signature: env + user args *)
                let param_types = I32 :: List.map (fun _ -> I32) args in
                let result_type = [I32] in
                let call_type = { ft_params = param_types; ft_results = result_type } in

                (* Find or add this type *)
                let type_idx =
                  (* OCaml 4.14 compat: List.find_index is 5.1+. Inline equivalent. *)
                  let rec find_idx i = function
                    | [] -> List.length ctx_temp2.types
                    | t :: _ when t = call_type -> i
                    | _ :: rest -> find_idx (i + 1) rest
                  in
                  find_idx 0 ctx_temp2.types
                in

                (* Call: push env, push user args, push func_id, call indirect *)
                let call_instrs = [
                  LocalGet env_ptr_idx;    (* Environment as first arg *)
                ] @ all_arg_code @ [       (* User arguments *)
                  LocalGet func_id_idx;    (* Function ID for indirect call *)
                  CallIndirect type_idx
                ] in

                Ok (ctx_temp2, extract_closure @ call_instrs)
              | Error _ ->
                Error (UnboundVariable ("Function or variable not found: " ^ id.name))
            end
        end
      | ExprLambda _ ->
        (* Lambda expression as function - generate lambda and call it *)
        let* (ctx_with_lambda, lambda_code) = gen_expr ctx_after_args func_expr in

        (* Create type signature for the call *)
        let param_types = List.map (fun _ -> I32) args in
        let result_type = [I32] in
        let call_type = { ft_params = param_types; ft_results = result_type } in

        (* Find matching type index *)
        let type_idx =
          (* OCaml 4.14 compat: List.find_index is 5.1+. Inline equivalent. *)
          let rec find_idx i = function
            | [] -> List.length ctx_with_lambda.types
            | t :: _ when t = call_type -> i
            | _ :: rest -> find_idx (i + 1) rest
          in
          find_idx 0 ctx_with_lambda.types
        in

        Ok (ctx_with_lambda, all_arg_code @ lambda_code @ [CallIndirect type_idx])

      | ExprVariant (_type_name, variant_name) ->
        (* Variant constructor with arguments: Type::Variant(arg1, arg2, ...) *)
        (* Layout: [tag: i32][field1: i32][field2: i32]... *)
        let num_fields = List.length args in
        let size_in_bytes = 4 + (num_fields * 4) in  (* tag + fields *)

        (* Allocate heap memory *)
        let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx_after_args size_in_bytes in
        let (ctx_with_temp, variant_ptr) = alloc_local ctx_with_heap "__variant_ptr" in

        (* Get or assign tag for this variant *)
        let (ctx_with_tag, tag) = match List.assoc_opt variant_name.name ctx_with_temp.variant_tags with
          | Some t -> (ctx_with_temp, t)
          | None ->
            let new_tag = List.length ctx_with_temp.variant_tags in
            ({ ctx_with_temp with variant_tags = (variant_name.name, new_tag) :: ctx_with_temp.variant_tags }, new_tag)
        in

        (* Store tag at offset 0 *)
        let store_tag = [
          LocalTee variant_ptr;  (* Save pointer *)
          I32Const (Int32.of_int tag);
          I32Store (2, 0);
        ] in

        (* Generate code to evaluate each arg and store it *)
        let rec store_args ctx offset arg_list =
          match arg_list with
          | [] -> Ok (ctx, [])
          | arg :: rest ->
            let* (ctx', arg_code) = gen_expr ctx arg in
            let store_code = [
              LocalGet variant_ptr;
            ] @ arg_code @ [
              I32Store (2, offset);
            ] in
            let* (ctx'', rest_code) = store_args ctx' (offset + 4) rest in
            Ok (ctx'', store_code @ rest_code)
        in

        let* (ctx_final, args_store_code) = store_args ctx_with_tag 4 args in

        (* Return the variant pointer *)
        let return_code = [LocalGet variant_ptr] in

        Ok (ctx_final, alloc_code @ store_tag @ args_store_code @ return_code)

      | _ ->
        (* Other expressions that evaluate to functions - treat as indirect call *)
        let* (ctx_final, func_code) = gen_expr ctx_after_args func_expr in

        (* Create type signature for the call *)
        let param_types = List.map (fun _ -> I32) args in
        let result_type = [I32] in
        let call_type = { ft_params = param_types; ft_results = result_type } in

        (* Find matching type index *)
        let type_idx =
          (* OCaml 4.14 compat: List.find_index is 5.1+. Inline equivalent. *)
          let rec find_idx i = function
            | [] -> List.length ctx_final.types
            | t :: _ when t = call_type -> i
            | _ :: rest -> find_idx (i + 1) rest
          in
          find_idx 0 ctx_final.types
        in

        Ok (ctx_final, all_arg_code @ func_code @ [CallIndirect type_idx])
        end
    end

  | ExprMatch match_expr ->
    (* Evaluate scrutinee and store in a temporary local *)
    let* (ctx_after_scrutinee, scrutinee_code) = gen_expr ctx match_expr.em_scrutinee in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_after_scrutinee "__match_tmp" in

    (* Generate code for each match arm *)
    let rec gen_arms ctx arms =
      match arms with
      | [] ->
        (* No arms matched - this shouldn't happen with exhaustive patterns *)
        (* Return 0 as a fallback *)
        Ok (ctx, [I32Const 0l])
      | arm :: rest ->
        (* Generate pattern matching code *)
        let* (ctx_after_pat, pattern_test, _bindings) = gen_pattern ctx temp_idx arm.ma_pat in

        (* Generate body code with bindings *)
        let* (ctx_after_body, body_code) = gen_expr ctx_after_pat arm.ma_body in

        (* If there are more arms, generate else branch *)
        if List.length rest > 0 then
          let* (ctx_final, else_code) = gen_arms ctx_after_body rest in
          (* pattern_test leaves boolean on stack, then If uses it *)
          Ok (ctx_final,
              pattern_test @
              [If (BtType I32,
                   body_code,    (* Then: pattern matched, execute body *)
                   else_code)])  (* Else: try next arm *)
        else
          (* Last arm - just execute body if pattern matches *)
          (* If pattern doesn't match, return 0 (shouldn't happen) *)
          Ok (ctx_after_body,
              pattern_test @
              [If (BtType I32,
                   body_code,        (* Then: pattern matched, execute body *)
                   [I32Const 0l])])  (* Else: fallback (shouldn't reach) *)
    in

    let* (ctx_final, arms_code) = gen_arms ctx_with_temp match_expr.em_arms in

    (* Complete code: eval scrutinee, store in temp, then try arms *)
    Ok (ctx_final, scrutinee_code @ [LocalSet temp_idx] @ arms_code)

  | ExprTuple elements ->
    (* Tuple layout in memory: [elem0: I32][elem1: I32][elem2: I32]... *)
    (* No length field - tuple size is fixed at creation *)
    let num_elements = List.length elements in
    let size_in_bytes = num_elements * 4 in  (* 4 bytes per element *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__tup_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalSet temp_idx] in

    (* Generate code to store each element *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, elem_expr) ->
      let* (ctx_acc, code_acc) = acc in
      (* Generate code for element value *)
      let* (ctx', elem_code) = gen_expr ctx_acc elem_expr in

      (* Store at offset (idx * 4) *)
      let offset = idx * 4 in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ elem_code @ [
        I32Store (2, offset);  (* Store element at offset *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i e -> (i, e)) elements) in

    (* Complete code: allocate, save to temp, store elements, return pointer *)
    Ok (ctx_final, alloc_code @ save_code @ store_code @ [LocalGet temp_idx])

  | ExprArray elements ->
    (* Array layout in memory: [length: I32][elem0: I32][elem1: I32]... *)
    let num_elements = List.length elements in
    let size_in_bytes = 4 + (num_elements * 4) in  (* 4 for length + 4 per element *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__arr_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalSet temp_idx] in

    (* Store length at offset 0 *)
    let length_code = [
      LocalGet temp_idx;
      I32Const (Int32.of_int num_elements);
      I32Store (2, 0);  (* Store length at offset 0 *)
    ] in

    (* Generate code to store each element *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, elem_expr) ->
      let* (ctx_acc, code_acc) = acc in
      (* Generate code for element value *)
      let* (ctx', elem_code) = gen_expr ctx_acc elem_expr in

      (* Store at offset 4 + (idx * 4) *)
      let offset = 4 + (idx * 4) in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ elem_code @ [
        I32Store (2, offset);  (* Store element at offset *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i e -> (i, e)) elements) in

    (* Complete code: allocate, save to temp, store length, store elements, return pointer *)
    Ok (ctx_final, alloc_code @ save_code @ length_code @ store_code @ [LocalGet temp_idx])

  | ExprRecord rec_expr ->
    (* Allocate memory for record fields *)
    let num_fields = List.length rec_expr.er_fields in
    let size_in_bytes = num_fields * 4 in  (* Each field is 4 bytes (I32) *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__rec_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalSet temp_idx] in

    (* Generate code to store each field *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, (field, expr_opt)) ->
      let* (ctx_acc, code_acc) = acc in
      (* Get field value expression *)
      let field_expr = match expr_opt with
        | Some e -> e
        | None -> ExprVar field  (* Field punning: {x} means {x: x} *)
      in
      (* Generate code for field value *)
      let* (ctx', field_code) = gen_expr ctx_acc field_expr in

      (* Store at offset (field_index * 4) from base address *)
      let offset = idx * 4 in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ field_code @ [
        I32Store (2, offset);  (* Store at offset with alignment 2 (4-byte) *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i x -> (i, x)) rec_expr.er_fields) in

    (* Complete code: allocate, save to temp (leaving on stack), store fields, leave address *)
    (* But we already consumed the address from stack when storing, so push it again *)
    Ok (ctx_final, alloc_code @ save_code @ store_code @ [LocalGet temp_idx])

  | ExprField (record_expr, field) ->
    (* Generate code for record expression (gets pointer) *)
    let* (ctx', record_code) = gen_expr ctx record_expr in

    (* Look up field offset from field_layouts *)
    (* For now, only supports field access on variables assigned record literals *)
    let field_offset = match record_expr with
      | ExprVar var_name ->
        (* Look up variable's field layout *)
        begin match List.assoc_opt var_name.name ctx.field_layouts with
          | Some layout ->
            (* Find field offset in layout *)
            begin match List.assoc_opt field.name layout with
              | Some offset -> offset
              | None -> 0  (* Field not found, default to 0 *)
            end
          | None -> 0  (* Variable layout not tracked, default to 0 *)
        end
      | _ -> 0  (* Complex expression, default to 0 *)
    in

    (* Load from memory at field offset *)
    let load_code = [
      I32Load (2, field_offset)  (* Load with alignment 2 (4-byte) and offset *)
    ] in

    Ok (ctx', record_code @ load_code)

  | ExprTupleIndex (tuple_expr, index) ->
    (* Generate code for tuple expression (gets pointer) *)
    let* (ctx', tuple_code) = gen_expr ctx tuple_expr in

    (* Calculate offset: index * 4 (no length field in tuples) *)
    let offset = index * 4 in

    (* Load from memory at offset *)
    let load_code = [
      I32Load (2, offset)  (* Load with alignment 2 (4-byte) and offset *)
    ] in

    Ok (ctx', tuple_code @ load_code)

  | ExprIndex (array_expr, index_expr) ->
    (* Generate code for array (gets pointer) *)
    let* (ctx_after_arr, array_code) = gen_expr ctx array_expr in

    (* Generate code for index *)
    let* (ctx_after_idx, index_code) = gen_expr ctx_after_arr index_expr in

    (* Calculate offset: 4 + (index * 4) *)
    (* Stack after array_code @ index_code: [array_ptr, index] *)
    let offset_calc = [
      I32Const 4l;        (* Constant 4 for element size *)
      I32Mul;             (* index * 4 *)
      I32Const 4l;        (* Add 4 to skip length field *)
      I32Add;             (* offset = 4 + (index * 4) *)
    ] in

    (* Add base pointer to offset and load *)
    let load_code = [
      I32Add;             (* base_ptr + offset *)
      I32Load (2, 0);     (* Load from calculated address *)
    ] in

    (* Complete code: array_ptr, index, calculate offset, add to base, load *)
    Ok (ctx_after_idx, array_code @ index_code @ offset_calc @ load_code)

  | ExprVariant (_type_name, variant_name) ->
    (* Look up variant tag *)
    (* For now, use variant name directly to find tag *)
    begin match List.assoc_opt variant_name.name ctx.variant_tags with
      | Some tag ->
        (* Zero-argument variant: just return the tag as an integer *)
        Ok (ctx, [I32Const (Int32.of_int tag)])
      | None ->
        (* Tag not found - assign a new sequential tag based on name *)
        (* This is a fallback for when type declarations aren't processed *)
        let tag = List.length ctx.variant_tags in
        let ctx' = { ctx with variant_tags = (variant_name.name, tag) :: ctx.variant_tags } in
        Ok (ctx', [I32Const (Int32.of_int tag)])
    end

  | ExprRowRestrict (base, _field) ->
    (* Row restriction at runtime just evaluates the base record.
       The field removal is a type-level operation — at the WASM level
       the record pointer is unchanged (fields are still in memory). *)
    gen_expr ctx base

  | ExprHandle eh ->
    (* Effect handlers in WASM: compile the body expression.
       Effect handling requires continuation support which WASM doesn't
       natively have. We compile as a simple wrapper that evaluates the
       body — unhandled effects will trap at runtime. *)
    gen_expr ctx eh.eh_body

  | ExprResume arg_opt ->
    (* Resume passes through the argument value *)
    begin match arg_opt with
      | Some e -> gen_expr ctx e
      | None -> Ok (ctx, [I32Const 0l])  (* unit *)
    end

  | ExprTry et ->
    (* WASM 1.0 has no exception-handling proposal.
       - catch arms: cannot be lowered — UnsupportedFeature.
       - body + optional finally: compile sequentially; a local temp
         preserves the body result across the finally block, matching
         the language semantics (finally result is always discarded). *)
    begin match et.et_catch with
    | Some _ ->
        Error (UnsupportedFeature
          "try/catch in WASM 1.0 backend — \
           requires the WASM exception-handling proposal; \
           use the Julia backend (-julia) or the interpreter (-i)")
    | None ->
        let* (ctx', body_code) = gen_block ctx et.et_body in
        begin match et.et_finally with
        | None -> Ok (ctx', body_code)
        | Some blk ->
            (* Store body result in a temp local, run finally, restore. *)
            let (ctx'', tmp_idx) = alloc_local ctx' "__try_result" in
            let* (ctx''', fin_code) = gen_block ctx'' blk in
            Ok (ctx''',
              body_code
              @ [LocalSet tmp_idx]   (* stash body result      *)
              @ fin_code
              @ [Drop]               (* discard finally result  *)
              @ [LocalGet tmp_idx])  (* restore body result     *)
        end
    end

  | ExprUnsafe ops ->
    (* Compile unsafe operations — evaluate contained expressions *)
    begin match ops with
      | [] -> Ok (ctx, [I32Const 0l])
      | [UnsafeRead e] -> gen_expr ctx e
      | [UnsafeWrite (_, value)] -> gen_expr ctx value
      | [UnsafeOffset (base, _)] -> gen_expr ctx base
      | [UnsafeTransmute (_, _, e)] -> gen_expr ctx e
      | [UnsafeForget e] ->
        let* (ctx', code) = gen_expr ctx e in
        Ok (ctx', code @ [Drop; I32Const 0l])
      | _ -> Error (UnsupportedFeature "Multiple unsafe operations in codegen")
    end

  | ExprSpan (e, _) ->
    gen_expr ctx e

(** Generate code for a block *)
and gen_block (ctx : context) (blk : block) : (context * instr list) result =
  let* (ctx', stmt_codes) = List.fold_left (fun acc stmt ->
    let* (c, codes) = acc in
    let* (c', code) = gen_stmt c stmt in
    Ok (c', codes @ code)
  ) (Ok (ctx, [])) blk.blk_stmts in
  match blk.blk_expr with
  | Some e ->
    let* (ctx_final, expr_code) = gen_expr ctx' e in
    Ok (ctx_final, stmt_codes @ expr_code)
  | None ->
    Ok (ctx', stmt_codes @ [I32Const 0l])

(** Generate pattern matching test code.
    Returns (context, test_code, bindings).
    test_code should leave a boolean (i32) on the stack indicating if pattern matches.
    bindings is a list of (name, temp_idx) for variables bound by the pattern. *)
and gen_pattern (ctx : context) (scrutinee_local : int) (pat : pattern)
  : (context * instr list * (string * int) list) result =
  match pat with
  | PatWildcard _ ->
    (* Wildcard always matches, no bindings *)
    Ok (ctx, [I32Const 1l], [])

  | PatVar id ->
    (* Variable pattern always matches and binds the scrutinee to the variable *)
    let (ctx', var_idx) = alloc_local ctx id.name in
    let bind_code = [
      LocalGet scrutinee_local;  (* Get scrutinee value *)
      LocalSet var_idx;          (* Bind to pattern variable *)
      I32Const 1l                (* Pattern matches *)
    ] in
    Ok (ctx', bind_code, [(id.name, var_idx)])

  | PatLit lit ->
    (* Literal pattern matches if scrutinee equals the literal *)
    let* (ctx', lit_instr) = gen_literal ctx lit in
    let test_code = [
      LocalGet scrutinee_local;  (* Get scrutinee value *)
      lit_instr;                 (* Get literal value *)
      I32Eq                      (* Compare *)
    ] in
    Ok (ctx', test_code, [])

  | PatCon (con, sub_patterns) ->
    (* Constructor pattern - match against tag *)
    if List.length sub_patterns = 0 then
      (* Zero-argument constructor: compare scrutinee to tag *)
      begin match List.assoc_opt con.name ctx.variant_tags with
        | Some tag ->
          let test_code = [
            LocalGet scrutinee_local;  (* Get scrutinee (should be tag) *)
            I32Const (Int32.of_int tag);  (* Expected tag *)
            I32Eq  (* Compare *)
          ] in
          Ok (ctx, test_code, [])
        | None ->
          (* Tag not found - auto-assign and match *)
          let tag = List.length ctx.variant_tags in
          let ctx' = { ctx with variant_tags = (con.name, tag) :: ctx.variant_tags } in
          let test_code = [
            LocalGet scrutinee_local;
            I32Const (Int32.of_int tag);
            I32Eq
          ] in
          Ok (ctx', test_code, [])
      end
    else
      (* Constructor with arguments: Some(x), Ok(a, b), etc. *)
      (* scrutinee is a pointer to [tag: i32][field1: i32][field2: i32]... *)

      (* Get or assign tag for this variant *)
      let (ctx_with_tag, tag) = match List.assoc_opt con.name ctx.variant_tags with
        | Some t -> (ctx, t)
        | None ->
          let new_tag = List.length ctx.variant_tags in
          ({ ctx with variant_tags = (con.name, new_tag) :: ctx.variant_tags }, new_tag)
      in

      (* Test: load tag from scrutinee and compare. Leaves the boolean
         on the stack. Binding code below is stack-neutral (see net-zero
         analysis in bind_fields), so the boolean survives through to the
         end of full_code. Prior implementation saved the bool via
         LocalTee and re-pushed via LocalGet at the end, which left TWO
         booleans on the stack and broke WASM validation in any match arm
         whose body produced an i32 result — e.g. enum-in-match returning
         distinct zero-arity or arg constructors across arms. *)
      let tag_test = [
        LocalGet scrutinee_local;  (* variant pointer *)
        I32Load (2, 0);            (* load tag from offset 0 *)
        I32Const (Int32.of_int tag);
        I32Eq;                     (* bool on stack — one value *)
      ] in

      (* Extract fields and bind variables. Each bind is net-zero on the
         stack (LocalGet +1, I32Load 0, LocalSet -1), so the tag-test
         boolean above remains on top of the stack as the pattern result. *)
      let rec bind_fields ctx_acc bindings_acc offset patterns =
        match patterns with
        | [] -> Ok (ctx_acc, bindings_acc)
        | pat :: rest ->
          begin match pat with
            | PatVar id ->
              (* Allocate local for this field *)
              let (ctx', field_idx) = alloc_local ctx_acc id.name in
              (* Code to load field: scrutinee[offset] *)
              let load_code = [
                LocalGet scrutinee_local;
                I32Load (2, offset);
                LocalSet field_idx;
              ] in
              bind_fields ctx' (bindings_acc @ load_code) (offset + 4) rest
            | PatWildcard _ ->
              (* Wildcard - skip this field *)
              bind_fields ctx_acc bindings_acc (offset + 4) rest
            | _ ->
              Error (UnsupportedFeature "Only variable and wildcard patterns supported in variant constructor arguments")
          end
      in

      let* (ctx_final, binding_code) = bind_fields ctx_with_tag [] 4 sub_patterns in

      let full_code = tag_test @ binding_code in

      Ok (ctx_final, full_code, [])

  | PatTuple sub_patterns ->
    (* Tuple pattern: (a, b, c) *)
    (* scrutinee is a pointer to [elem0: i32][elem1: i32][elem2: i32]... *)

    (* Bind each element to sub-pattern *)
    let rec bind_elements ctx_acc offset patterns =
      match patterns with
      | [] -> Ok (ctx_acc, [])
      | pat :: rest ->
        begin match pat with
          | PatVar id ->
            (* Allocate local for this element *)
            let (ctx', elem_idx) = alloc_local ctx_acc id.name in
            (* Load element from tuple *)
            let load_code = [
              LocalGet scrutinee_local;
              I32Load (2, offset);
              LocalSet elem_idx;
            ] in
            let* (ctx_final, rest_code) = bind_elements ctx' (offset + 4) rest in
            Ok (ctx_final, load_code @ rest_code)
          | PatWildcard _ ->
            (* Skip this element *)
            bind_elements ctx_acc (offset + 4) rest
          | _ ->
            Error (UnsupportedFeature "Only variable and wildcard patterns supported in tuple patterns")
        end
    in

    let* (ctx_final, binding_code) = bind_elements ctx 0 sub_patterns in
    (* Tuple patterns always match (no tag to check) *)
    let match_code = binding_code @ [I32Const 1l] in
    Ok (ctx_final, match_code, [])

  | PatRecord (field_pats, _has_wildcard) ->
    (* Record pattern: {x: a, y: b} *)
    (* scrutinee is a pointer to record with sequential field layout *)
    (* Limitation: assumes fields in pattern order match memory layout *)

    let rec bind_fields ctx_acc offset field_patterns =
      match field_patterns with
      | [] -> Ok (ctx_acc, [])
      | (field_name, pat_opt) :: rest ->
        (* Default pattern is PatVar with same name as field *)
        let pat = match pat_opt with
          | Some p -> p
          | None -> PatVar field_name
        in

        begin match pat with
          | PatVar id ->
            (* Allocate local for this field *)
            let (ctx', field_idx) = alloc_local ctx_acc id.name in
            (* Load field from record at sequential offset *)
            let load_code = [
              LocalGet scrutinee_local;
              I32Load (2, offset);
              LocalSet field_idx;
            ] in
            let* (ctx_final, rest_code) = bind_fields ctx' (offset + 4) rest in
            Ok (ctx_final, load_code @ rest_code)
          | PatWildcard _ ->
            (* Skip this field, but advance offset *)
            bind_fields ctx_acc (offset + 4) rest
          | _ ->
            Error (UnsupportedFeature "Only variable and wildcard patterns supported in record field patterns")
        end
    in

    let* (ctx_final, binding_code) = bind_fields ctx 0 field_pats in
    (* Record patterns always match (no tag check) *)
    let match_code = binding_code @ [I32Const 1l] in
    Ok (ctx_final, match_code, [])

  | PatOr (pat1, pat2) ->
    (* Or pattern: p1 | p2 *)
    (* Try pat1, if it fails try pat2 *)
    (* Limitation: both patterns should bind the same variables *)

    let* (ctx_after_p1, test1, _bindings1) = gen_pattern ctx scrutinee_local pat1 in
    let* (ctx_after_p2, test2, _bindings2) = gen_pattern ctx_after_p1 scrutinee_local pat2 in

    (* Generate: test1 or test2 *)
    (* If test1 succeeds (1), return 1 *)
    (* If test1 fails (0), try test2 *)
    let or_code = test1 @ [
      If (BtType I32,
          [I32Const 1l],  (* Then: first pattern matched *)
          test2)          (* Else: try second pattern *)
    ] in

    Ok (ctx_after_p2, or_code, [])

  | PatAs (bind_id, sub_pat) ->
    (* As pattern: x @ sub_pattern *)
    (* Bind scrutinee to variable and match sub-pattern *)

    (* Allocate local for the binding *)
    let (ctx_with_bind, bind_idx) = alloc_local ctx bind_id.name in

    (* Bind the scrutinee *)
    let bind_code = [
      LocalGet scrutinee_local;
      LocalSet bind_idx;
    ] in

    (* Match the sub-pattern *)
    let* (ctx_final, sub_test, _sub_bindings) = gen_pattern ctx_with_bind scrutinee_local sub_pat in

    (* Combine: bind then test sub-pattern *)
    Ok (ctx_final, bind_code @ sub_test, [])

(** Generate code for a statement *)
and gen_stmt (ctx : context) (stmt : stmt) : (context * instr list) result =
  match stmt with
  | StmtLet sl ->
    let* (ctx', rhs_code) = gen_expr ctx sl.sl_value in
    begin match sl.sl_pat with
      | PatVar id ->
        (* Track the bound variable's field layout so subsequent `.field_N`
           reads pick the right offset. Three sources, in order:
           1. Explicit `let s: State = ...` annotation → look up struct_layouts.
           2. RHS is a record literal → layout from literal's field order.
           3. RHS is `f(...)` where f's declared return type is a struct
              → look up fn_ret_structs then struct_layouts.
           4. RHS is another bound variable `let t = s` where s has a
              tracked layout → copy it.
           Any source misses fall back to no tracking (existing behaviour). *)
        let layout_from_ty_annot () =
          match sl.sl_ty with
          | Some ty ->
            begin match struct_name_of_ty ty with
              | Some sname -> List.assoc_opt sname ctx'.struct_layouts
              | None -> None
            end
          | None -> None
        in
        let layout_from_rhs () =
          match sl.sl_value with
          | ExprRecord rec_expr ->
            Some (List.mapi (fun i (fn, _) -> (fn.name, i * 4)) rec_expr.er_fields)
          | ExprApp (ExprVar fn_id, _) ->
            begin match List.assoc_opt fn_id.name ctx'.fn_ret_structs with
              | Some sname -> List.assoc_opt sname ctx'.struct_layouts
              | None -> None
            end
          | ExprVar src_id -> List.assoc_opt src_id.name ctx'.field_layouts
          | _ -> None
        in
        let layout_opt =
          match layout_from_ty_annot () with
          | Some _ as s -> s
          | None -> layout_from_rhs ()
        in
        let (ctx'', idx) = alloc_local ctx' id.name in
        let ctx_with_layout = match layout_opt with
          | Some layout ->
            { ctx'' with field_layouts = (id.name, layout) :: ctx''.field_layouts }
          | None -> ctx''
        in
        Ok (ctx_with_layout, rhs_code @ [LocalSet idx])
      | _ ->
        (* Complex patterns — use gen_pattern_bind for tuples, wildcards, etc. *)
        let* (ctx'', pat_code) = gen_pattern_bind ctx' sl.sl_pat in
        Ok (ctx'', rhs_code @ pat_code)
    end

  | StmtExpr e ->
    let* (ctx', code) = gen_expr ctx e in
    Ok (ctx', code @ [Drop])  (* Discard expression result *)

  | StmtAssign (lhs, op, rhs) ->
    begin match lhs with
      | ExprVar id ->
        (* Variable assignment: x = expr or x += expr *)
        let* idx = lookup_local ctx id.name in
        let* (ctx', rhs_code) = gen_expr ctx rhs in
        begin match op with
          | AssignEq ->
            (* Simple assignment *)
            Ok (ctx', rhs_code @ [LocalSet idx])
          | AssignAdd | AssignSub | AssignMul | AssignDiv ->
            (* Compound assignment: x op= expr  =>  x = x op expr *)
            let binop = match op with
              | AssignAdd -> OpAdd
              | AssignSub -> OpSub
              | AssignMul -> OpMul
              | AssignDiv -> OpDiv
              | _ -> failwith "unreachable"
            in
            let op_instr = gen_binop binop in
            Ok (ctx', [
              LocalGet idx;     (* Load current value *)
            ] @ rhs_code @ [    (* Evaluate RHS *)
              op_instr;         (* Perform operation *)
              LocalSet idx      (* Store result *)
            ])
        end
      | ExprUnary (OpDeref, ptr_expr) ->
        (* Pointer dereference assignment: *ptr = expr *)
        let* (ctx', ptr_code) = gen_expr ctx ptr_expr in
        let* (ctx'', rhs_code) = gen_expr ctx' rhs in
        begin match op with
          | AssignEq ->
            (* Simple dereference assignment *)
            Ok (ctx'', ptr_code @ rhs_code @ [I32Store (2, 0)])
          | AssignAdd | AssignSub | AssignMul | AssignDiv ->
            (* Compound dereference assignment: *ptr op= expr  =>  *ptr = *ptr op expr *)
            let binop = match op with
              | AssignAdd -> OpAdd
              | AssignSub -> OpSub
              | AssignMul -> OpMul
              | AssignDiv -> OpDiv
              | _ -> failwith "unreachable"
            in
            let op_instr = gen_binop binop in
            (* Need temp locals for pointer and result *)
            let (ctx_with_ptr, temp_ptr) = alloc_local ctx'' "__assign_ptr" in
            let (ctx_with_val, temp_val) = alloc_local ctx_with_ptr "__assign_val" in
            Ok (ctx_with_val, ptr_code @ [
              LocalTee temp_ptr;   (* Save pointer *)
              I32Load (2, 0);      (* Load current value *)
            ] @ rhs_code @ [        (* Evaluate RHS *)
              op_instr;            (* Perform operation *)
              LocalSet temp_val;   (* Save result *)
              LocalGet temp_ptr;   (* Load pointer *)
              LocalGet temp_val;   (* Load result *)
              I32Store (2, 0)      (* Store: mem[ptr] = result *)
            ])
        end
      | ExprIndex (arr_expr, idx_expr) ->
        (* Array index assignment: arr[i] = expr *)
        let* (ctx', arr_code) = gen_expr ctx arr_expr in
        let* (ctx'', idx_code) = gen_expr ctx' idx_expr in
        let* (ctx''', rhs_code) = gen_expr ctx'' rhs in
        (* Array is a pointer, need to compute: arr + (idx * 4) *)
        let (ctx_with_temp, temp_ptr) = alloc_local ctx''' "__arr_ptr" in
        begin match op with
          | AssignEq ->
            Ok (ctx_with_temp, arr_code @ idx_code @ [
              I32Const 4l;
              I32Mul;              (* idx * 4 *)
              I32Add;              (* arr + (idx * 4) *)
            ] @ rhs_code @ [
              I32Store (2, 0)      (* Store value *)
            ])
          | AssignAdd | AssignSub | AssignMul | AssignDiv ->
            let binop = match op with
              | AssignAdd -> OpAdd
              | AssignSub -> OpSub
              | AssignMul -> OpMul
              | AssignDiv -> OpDiv
              | _ -> failwith "unreachable"
            in
            let op_instr = gen_binop binop in
            let (ctx_with_val, temp_val) = alloc_local ctx_with_temp "__assign_val" in
            Ok (ctx_with_val, arr_code @ idx_code @ [
              I32Const 4l;
              I32Mul;              (* idx * 4 *)
              I32Add;              (* arr + (idx * 4) *)
              LocalTee temp_ptr;   (* Save computed address *)
              I32Load (2, 0);      (* Load current value *)
            ] @ rhs_code @ [
              op_instr;            (* Perform operation *)
              LocalSet temp_val;   (* Save result *)
              LocalGet temp_ptr;   (* Load address *)
              LocalGet temp_val;   (* Load result *)
              I32Store (2, 0)      (* Store result *)
            ])
        end
      | _ ->
        Error (UnsupportedFeature "Assignment to this expression type not yet supported")
    end

  | StmtWhile (cond, body) ->
    let* (ctx', cond_code) = gen_expr ctx cond in
    let* (ctx'', body_code) = gen_block ctx' body in
    (* Loop with conditional exit *)
    Ok (ctx'', [Block (BtEmpty, [
      Loop (BtEmpty,
        cond_code @ [I32Eqz; BrIf 1] @  (* If condition false, exit *)
        body_code @ [Br 0]  (* Continue loop *)
      )
    ])])

  | StmtFor (pat, iter_expr, body) ->
    (* For loop: for pat in iter { body }
       Iterates over an array or range.
       For now, handle arrays: iterate from index 0 to length-1 *)
    let* (ctx', iter_code) = gen_expr ctx iter_expr in

    (* Create temp locals for array pointer, length, and index *)
    let (ctx_with_arr, arr_ptr) = alloc_local ctx' "__for_arr" in
    let (ctx_with_len, len_var) = alloc_local ctx_with_arr "__for_len" in
    let (ctx_with_idx, idx_var) = alloc_local ctx_with_len "__for_idx" in

    (* For pattern, currently only support simple variable binding *)
    begin match pat with
      | PatVar id ->
        let (ctx_with_item, item_var) = alloc_local ctx_with_idx id.name in

        (* Generate body code in context with item variable *)
        let* (ctx_final, body_code) = gen_block ctx_with_item body in

        (* For loop structure:
           1. Evaluate iterator (array pointer)
           2. Load length from array[-4] (stored before first element)
           3. Initialize index to 0
           4. Loop:
              - Check if index < length, exit if not
              - Load array[index] into item variable
              - Execute body
              - Increment index
              - Continue loop *)
        Ok (ctx_final, iter_code @ [
          LocalSet arr_ptr;           (* Save array pointer *)
          LocalGet arr_ptr;
          I32Load (2, 0);             (* Load length (canonical layout:
                                         length @ arr+0, elems @ arr+4+i*4 —
                                         matches ExprArray / ExprIndex) *)
          LocalSet len_var;           (* Save length *)
          I32Const 0l;
          LocalSet idx_var;           (* index = 0 *)
          Block (BtEmpty, [
            Loop (BtEmpty, [
              (* Check if index < length *)
              LocalGet idx_var;
              LocalGet len_var;
              I32GeS;                 (* index >= length? *)
              BrIf 1;                 (* Exit loop if true *)

              (* Load array[index] into item variable.
                 elem i @ arr + 4 + i*4 (offset 4 skips the length word). *)
              LocalGet arr_ptr;
              LocalGet idx_var;
              I32Const 4l;
              I32Mul;                 (* index * 4 *)
              I32Add;                 (* arr + index*4 *)
              I32Load (2, 4);         (* Load array[index] (+4 = skip len) *)
              LocalSet item_var;      (* item = array[index] *)
            ] @ body_code @ [
              (* Increment index *)
              LocalGet idx_var;
              I32Const 1l;
              I32Add;
              LocalSet idx_var;       (* index++ *)
              Br 0                    (* Continue loop *)
            ])
          ])
        ])
      | PatWildcard _ ->
        (* Iterate but don't bind *)
        let* (ctx_final, body_code) = gen_block ctx_with_idx body in
        Ok (ctx_final, iter_code @ [
          LocalSet arr_ptr;
          LocalGet arr_ptr;
          I32Load (2, 0); LocalSet len_var;  (* length @ arr+0 (canonical) *)
          I32Const 0l; LocalSet idx_var;
          Block (BtEmpty, [
            Loop (BtEmpty, [
              LocalGet idx_var; LocalGet len_var; I32GeS; BrIf 1;
            ] @ body_code @ [
              LocalGet idx_var; I32Const 1l; I32Add; LocalSet idx_var;
              Br 0
            ])
          ])
        ])
      | PatTuple pats ->
        (* Tuple destructuring in for loop — load element, then extract fields *)
        let (ctx_with_elem, elem_var) = alloc_local ctx_with_idx "__for_elem" in
        (* Bind each tuple field from memory *)
        let* (ctx_with_binds, bind_codes) = List.fold_left (fun acc (i, sub_pat) ->
          let* (c, codes) = acc in
          match sub_pat with
          | PatVar id ->
            let (c', local_idx) = alloc_local c id.name in
            Ok (c', codes @ [
              LocalGet elem_var;
              I32Const (Int32.of_int (i * 4));
              I32Add;
              I32Load (2, 0);
              LocalSet local_idx;
            ])
          | PatWildcard _ -> Ok (c, codes)
          | _ -> Ok (c, codes)  (* Skip complex sub-patterns *)
        ) (Ok (ctx_with_elem, [])) (List.mapi (fun i p -> (i, p)) pats) in
        let* (ctx_final, body_code) = gen_block ctx_with_binds body in
        Ok (ctx_final, iter_code @ [
          LocalSet arr_ptr;
          LocalGet arr_ptr;
          I32Load (2, 0); LocalSet len_var;  (* length @ arr+0 (canonical) *)
          I32Const 0l; LocalSet idx_var;
          Block (BtEmpty, [
            Loop (BtEmpty, [
              LocalGet idx_var; LocalGet len_var; I32GeS; BrIf 1;
              LocalGet arr_ptr; LocalGet idx_var;
              I32Const 4l; I32Mul; I32Add;
              I32Load (2, 4); LocalSet elem_var;  (* +4 skips length word *)
            ] @ bind_codes @ body_code @ [
              LocalGet idx_var; I32Const 1l; I32Add; LocalSet idx_var;
              Br 0
            ])
          ])
        ])
      | _ ->
        (* Fallback: bind whole element as a temp *)
        let (ctx_with_tmp, tmp_var) = alloc_local ctx_with_idx "__for_tmp" in
        let* (ctx_final, body_code) = gen_block ctx_with_tmp body in
        Ok (ctx_final, iter_code @ [
          LocalSet arr_ptr;
          LocalGet arr_ptr;
          I32Load (2, 0); LocalSet len_var;  (* length @ arr+0 (canonical) *)
          I32Const 0l; LocalSet idx_var;
          Block (BtEmpty, [
            Loop (BtEmpty, [
              LocalGet idx_var; LocalGet len_var; I32GeS; BrIf 1;
              LocalGet arr_ptr; LocalGet idx_var;
              I32Const 4l; I32Mul; I32Add;
              I32Load (2, 4); LocalSet tmp_var;  (* +4 skips length word *)
            ] @ body_code @ [
              LocalGet idx_var; I32Const 1l; I32Add; LocalSet idx_var;
              Br 0
            ])
          ])
        ])
    end

(** Generate code for a function *)
(* ── #225 PR2: selective CPS transform for the WasmGC Async backend ──
   ADR-013 (docs/specs/async-on-wasm-cps.adoc). PR2 scope = the base
   case only: an `Async` function whose body is `let r = <async-call>;
   <cont>` with NO live-local capture across the split. Live-local
   capture + Async→Async chaining + the typed Response reader are PR3.
   Detection is deliberately conservative: any shape it does not
   recognise falls back to the pre-existing synchronous lowering, so
   PR2 is strictly additive (no behaviour change for unrecognised
   Async fns — same as today, where codegen ignores fd_eff entirely). *)

(** ADR-013 obligation 2 (effect-row fidelity): the transform triggers
    iff `Async ∈ fd_eff`. Pure recursive walk of the effect row. *)
let rec eff_expr_has_async (e : effect_expr) : bool =
  match e with
  (* A bare effect name (`Async`, `Net`, …) parses as [EffVar] — only the
     parametric form (`Throws[E]`) is [EffCon] (parser.mly effect_term).
     Both spellings must be checked. *)
  | EffVar id -> id.name = "Async"
  | EffCon (id, _) -> id.name = "Async"
  | EffUnion (a, b) -> eff_expr_has_async a || eff_expr_has_async b

let fn_is_async (fd : fn_decl) : bool =
  match fd.fd_eff with
  | None -> false
  | Some e -> eff_expr_has_async e

(** Single binder name of a trivial `let` pattern, if any. The PR2 base
    case only recognises `let <var> = <async-call>; <cont>`. *)
let simple_pat_name (p : pattern) : string option =
  match p with
  | PatVar id -> Some id.name
  | _ -> None

(** Recognised async-primitive calls (ADR-013 #225 PR3a, owner-chosen
    structural-conservative async-boundary identification — see
    affinescript#234 for the effect-threaded generalisation). The async
    boundary is a `let` whose RHS is a call to one of these names. Extend
    this list as wasm-path async stdlib primitives are added. *)
(* ADR-016 / #234 S4: the hardcoded `async_primitives` name set is
   RETIRED. The async boundary is exactly "a call whose effect row ⊇
   Async", decided from the typecheck side-table keyed by the shared
   [Effect_sites] ordinal ([Effect_sites.is_async_call]). The ADR's
   "table-miss fallback" is the oracle being empty / count-mismatched
   (⇒ [is_async_call] is always false ⇒ no transform = exact pre-#234
   behaviour), NOT a name list — so no structural name disjunct
   remains. `http_request_thenable` is still detected because
   stdlib/Http.affine declares it `/ { Net, Async }`. *)
let is_async_prim_call (e : expr) : bool =
  Effect_sites.is_async_call e

(** Does [e] contain (at any depth) a call whose effect row ⊇ Async?
    Enforces the PR3a single-boundary rule (a pre-value must not hide
    an async call; Async→Async chaining is PR3c). Now effect-driven via
    the shared [Effect_sites] traversal — same call-site set the
    numbering uses, so it can never miss a shape the detector counts. *)
let mentions_async_prim (e : expr) : bool =
  Effect_sites.exists_call Effect_sites.is_async_call e

(** Async-fn body recogniser (ADR-013 #225). Returns
    [Some (pre, binder, async_call, cont)] iff the body is, after
    trivial-block unwrapping, a sequence of zero or more simple
    `let`-bindings ([pre]) followed by `let binder = <async-prim call>`
    then a continuation [cont] (the remaining stmts + tail expr), where:

    - PR2: [pre] = [] (zero live-local capture).
    - PR3a: [pre] may bind live locals; [cont] may reference them — they
      are captured into the continuation env by the proven #199
      ExprLambda path (which already marshals N captures). [pre] is
      restricted to simple `let`s so the captured set is well-defined.
    - PR3c: [cont] may itself be an async boundary. It is NOT rejected
      here; the recursive [async_transform_hook] re-applies this
      recogniser to the continuation lambda's body, so a chain
      `let a = async(); let b = async(); …` lowers to nested
      continuations whose Thenables compose up the call chain.

    [extra] is the set of live-local names available for #199 capture at
    the call site (the enclosing context's locals — including, for a
    recursively-transformed continuation, the *outer* binder and outer
    captured locals). Capture soundness: every free name in [cont] must
    be the binder, a param, a top-level func/const/global, a [pre]-bound
    local, or one of [extra] — anything else ⇒ [None] (fall back to the
    unchanged synchronous lowering, zero regression). A recognised async
    primitive nested inside a [pre] value (i.e. not in `let`-binding
    head position) is an unsupported shape ⇒ [None]. The affine/linear
    single-use obligation (ADR-013 obl. 1) is discharged by composition
    (PR3b): borrow-check runs on this straight-line AST before the
    transform, and the once-resumption trap guarantees each continuation
    executes exactly once — no new static machinery here. *)
let detect_async_base_case ~(globals : string list) ~(params : string list)
    ~(extra : string list) (body : expr)
    : (stmt list * string * expr * expr) option =
  let rec unwrap = function
    | ExprBlock { blk_stmts = []; blk_expr = Some e } -> unwrap e
    | e -> e
  in
  let pre_bound_name = function
    | StmtLet sl -> simple_pat_name sl.sl_pat
    | _ -> None
  in
  let accept pre binder (call : expr) (cont : expr) =
    if not (is_async_prim_call call) then None
    else begin
      (* [pre] must be only simple `let`s (well-defined capture set). *)
      let pre_names = List.map pre_bound_name pre in
      if List.exists (fun n -> n = None) pre_names then None
      else
        let pre_bound = List.filter_map (fun x -> x) pre_names in
        (* A nested async primitive in a pre value is unsupported (the
           boundary must be the `let`-binding head); cont MAY chain. *)
        let pre_vals = List.filter_map
          (function StmtLet sl -> Some sl.sl_value | _ -> None) pre in
        if List.exists mentions_async_prim pre_vals then None
        else
          let allowed =
            binder :: params @ globals @ pre_bound @ extra in
          let escaping =
            List.filter (fun v -> not (List.mem v allowed))
              (dedup (find_free_vars [] cont))
          in
          if escaping = [] then Some (pre, binder, call, cont) else None
    end
  in
  (* Split a stmt list at the first `let b = <async-prim call>`. *)
  let rec split_at_boundary acc = function
    | StmtLet sl :: rest when is_async_prim_call sl.sl_value ->
      begin match simple_pat_name sl.sl_pat with
        | Some binder -> Some (List.rev acc, binder, sl.sl_value, rest)
        | None -> None
      end
    | s :: rest -> split_at_boundary (s :: acc) rest
    | [] -> None
  in
  match unwrap body with
  (* Expression-form let: `let r = <async-prim call>; cont` (no pre). *)
  | ExprLet lb ->
    begin match simple_pat_name lb.el_pat, lb.el_body with
      | Some binder, Some cont -> accept [] binder lb.el_value cont
      | _ -> None
    end
  (* Block form: optional simple `let`s, then the async boundary, then
     the continuation (remaining stmts + tail expr). *)
  | ExprBlock { blk_stmts; blk_expr } ->
    begin match split_at_boundary [] blk_stmts with
      | Some (pre, binder, call, post) ->
        let cont = ExprBlock { blk_stmts = post; blk_expr } in
        accept pre binder call cont
      | None -> None
    end
  | _ -> None

(** ADR-013 #225 transform. Lowers a recognised `Async` fn body
    `<pre simple-lets>; let binder = <async-call>; <cont>` to:
      1. [pre] generated synchronously (its locals become live locals,
         in scope for capture);
      2. the async call (yields a `Thenable` handle), bound to [binder];
      3. [cont] reified as a zero-arg continuation via the EXISTING #199
         ExprLambda path — [binder] and every [pre]-bound local that
         [cont] references are auto-captured into the `[fnId@0,envPtr@4]`
         env (the #199 path already marshals N captures; no new closure
         code); a once-resumption trap is prepended to its body;
      4. `thenableThen(handle, <closure>)`; the fn returns the result.
    Pure/non-recognised fns never reach here (caller gates on
    [fn_is_async] + [detect_async_base_case]); behaviour is unchanged
    for them, exactly as before. *)
let gen_async_base_case (ctx : context) (pre : stmt list) (binder : string)
    (async_call : expr) (cont : expr) (thenable_then_idx : int)
    : (context * instr list) result =
  (* [pre] runs synchronously before the async call; folding [gen_stmt]
     allocates its locals into the context so the continuation's #199
     capture pass (find_free_vars ∩ ctx.locals) picks them up. Simple
     `let`s are stack-neutral, so [pre_code] leaves the stack empty. *)
  let* (ctx_pre, pre_code) =
    List.fold_left (fun acc st ->
      let* (c, code) = acc in
      let* (c', s) = gen_stmt c st in
      Ok (c', code @ s)) (Ok (ctx, [])) pre in
  let* (ctx1, call_code) = gen_expr ctx_pre async_call in
  let (ctx2, binder_idx) = alloc_local ctx1 binder in
  (* Once-resumption guard global (ADR-013 obligation 1): a second
     continuation entry traps. Defence-in-depth over the host's
     single-fire (`thenableThen` settles a Promise exactly once). *)
  let fired_gidx = List.length ctx2.globals in
  let fired_global =
    { g_type = I32; g_mutable = true; g_init = [I32Const 0l] } in
  let ctx3 = { ctx2 with globals = ctx2.globals @ [fired_global] } in
  let cont_lambda =
    ExprLambda { elam_params = []; elam_ret_ty = None; elam_body = cont } in
  let* (ctx4, closure_code) = gen_expr ctx3 cont_lambda in
  let guard =
    [ GlobalGet fired_gidx;
      If (BtEmpty, [Unreachable], []);
      I32Const 1l; GlobalSet fired_gidx ] in
  let ctx5 =
    match List.rev ctx4.lambda_funcs with
    | last :: rest ->
      let patched = { last with f_body = guard @ last.f_body } in
      { ctx4 with lambda_funcs = List.rev (patched :: rest) }
    | [] -> ctx4   (* unreachable: ExprLambda always lifts exactly one *)
  in
  let body =
    pre_code                    (* synchronous prelude; stack-neutral *)
    @ call_code
    @ [ LocalSet binder_idx ]
    @ [ LocalGet binder_idx ]   (* arg 1: the Thenable handle *)
    @ closure_code              (* arg 2: [fnId,envPtr] continuation *)
    @ [ Call thenable_then_idx ]
  in
  Ok (ctx5, body)

(* #225 PR3c: populate the forward reference (declared before [gen_expr])
   so the continuation-body site in the [ExprLambda] lowering re-applies
   the transform — giving Async→Async chaining where Thenables compose
   up the call chain. A continuation has no params (everything it needs
   is captured), hence [~params:[]]; [~extra] is the live-local set the
   #199 path can capture, which for a recursively-transformed inner
   continuation includes the *outer* binder and outer captures. Requires
   `thenableThen` to be importable; otherwise [None] ⇒ the ExprLambda
   site lowers the body normally (no behaviour change). *)
let () =
  async_transform_hook :=
    (fun ctx body ->
       match List.assoc_opt "thenableThen" ctx.func_indices with
       | None -> None
       | Some tt ->
         begin match detect_async_base_case
                       ~globals:(List.map fst ctx.func_indices)
                       ~params:[]
                       ~extra:(List.map fst ctx.locals)
                       body with
         | Some (pre, binder, call, cont) ->
           Some (gen_async_base_case ctx pre binder call cont tt)
         | None -> None
         end)

let gen_function (ctx : context) (fd : fn_decl) : (context * func) result =
  (* Create fresh context for function scope, but preserve lambda_funcs and next_lambda_id *)
  let fn_ctx = { ctx with locals = []; next_local = 0; loop_depth = 0 } in

  (* Parameters become locals 0..n-1. When a parameter's declared type
     names a known struct, register its field layout under the parameter
     name so body-side `.field_N` reads resolve to the correct offset
     rather than defaulting to 0. *)
  let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
    let (c', idx) = alloc_local c param.p_name.name in
    let c'' =
      match struct_name_of_ty param.p_ty with
      | Some sname ->
        begin match List.assoc_opt sname c'.struct_layouts with
          | Some layout ->
            { c' with field_layouts = (param.p_name.name, layout) :: c'.field_layouts }
          | None -> c'
        end
      | None -> c'
    in
    (c'', idx)
  ) (fn_ctx, 0) fd.fd_params in

  let param_count = List.length fd.fd_params in

  (* Generate function body *)
  let body_expr = match fd.fd_body with
    | FnBlock blk -> ExprBlock blk
    | FnExpr e -> e
  in
  (* ADR-013 PR2: an `Async` fn whose body is the recognised base-case
     shape is lowered via the CPS transform; everything else keeps the
     existing synchronous lowering verbatim (no behaviour change). The
     transform also requires `thenableThen` to be resolvable as an
     import; if it is not in scope we fall back rather than fail. *)
  let async_base =
    if fn_is_async fd then
      match detect_async_base_case
              ~globals:(List.map fst ctx_with_params.func_indices)
              ~params:(List.map (fun p -> p.p_name.name) fd.fd_params)
              ~extra:(List.map fst ctx_with_params.locals)
              body_expr with
      | Some (pre, binder, call, cont) ->
        begin match List.assoc_opt "thenableThen"
                      ctx_with_params.func_indices with
          | Some tt -> Some (pre, binder, call, cont, tt)
          | None -> None
        end
      | None -> None
    else None
  in
  let* (ctx_final, body_code) =
    match async_base with
    | Some (pre, binder, call, cont, tt_idx) ->
      gen_async_base_case ctx_with_params pre binder call cont tt_idx
    | None -> gen_expr ctx_with_params body_expr
  in

  (* Compute additional locals (beyond parameters) *)
  let local_count = ctx_final.next_local - param_count in
  let locals = if local_count > 0 then
    [{ l_count = local_count; l_type = I32 }]
  else
    []
  in

  (* Create function (type index will be set by gen_decl) *)
  let func = {
    f_type = 0;  (* Will be overridden *)
    f_locals = locals;
    f_body = body_code;
  } in

  (* Return updated context with any lambda functions that were created *)
  Ok (ctx_final, func)

(** Generate code for a top-level declaration *)
(** Find the index of [ft] in [types], or append it. Returns (idx, new_types). *)
let intern_func_type (types : func_type list) (ft : func_type) : int * func_type list =
  let rec find_idx i = function
    | [] -> (List.length types, types @ [ft])
    | t :: _ when t = ft -> (i, types)
    | _ :: rest -> find_idx (i + 1) rest
  in
  find_idx 0 types

(** Build a WASM-side [func_type] for a top-level function declaration, mirroring
    the convention used by [gen_decl TopFn]: every param is i32, the result is
    [i32]. Used both for local fn types and for imported fn types so that calls
    through either path agree on signature. *)
let func_type_of_fn_decl (fd : fn_decl) : func_type =
  let params = List.map (fun _ -> I32) fd.fd_params in
  let results = [I32] in
  { ft_params = params; ft_results = results }

let gen_decl (ctx : context) (decl : top_level) : context result =
  match decl with
  | TopFn fd when fd.fd_body = FnExtern ->
    (* `extern fn name(params) -> Ret;` — host-supplied implementation.
       Emit a Wasm import entry under the conventional "env" namespace
       (matches what the Node-CJS shim's import map populates) and register
       the local alias in func_indices so call sites resolve normally.
       Mirrors gen_imports's lowering for cross-module imports. *)
    let ft = func_type_of_fn_decl fd in
    let (type_idx, types_after) = intern_func_type ctx.types ft in
    let import_func_idx = import_func_count ctx in
    let import = {
      i_module = "env";
      i_name = fd.fd_name.name;
      i_desc = ImportFunc type_idx;
    } in
    Ok { ctx with
         types = types_after;
         imports = ctx.imports @ [import];
         func_indices = (fd.fd_name.name, import_func_idx) :: ctx.func_indices;
       }

  | TopType td when td.td_body = TyExtern ->
    (* Opaque host-supplied type — no Wasm artifact. *)
    Ok ctx

  | TopFn fd ->
    (* Create function type *)
    let param_types = List.map (fun _ -> I32) fd.fd_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Add type to types list *)
    let type_idx = List.length ctx.types in
    let ctx_with_type = { ctx with types = ctx.types @ [func_type] } in

    (* Determine function index before generating *)
    let func_idx = import_func_count ctx_with_type + List.length ctx_with_type.funcs in

    (* Stage 2: Extract ownership annotations for typed-wasm [affinescript.ownership] section *)
    let param_kinds = List.map ownership_kind_of_param fd.fd_params in
    let ret_kind = ownership_kind_of_ret fd.fd_ret_ty in

    (* Register function name to index mapping and record ownership annotations.
       Also record the function's return struct name (if any) so call sites
       `let s = f(...)` can register s's field layout. *)
    let fn_ret_structs' = match fd.fd_ret_ty with
      | Some ty ->
        begin match struct_name_of_ty ty with
          | Some sname -> (fd.fd_name.name, sname) :: ctx_with_type.fn_ret_structs
          | None -> ctx_with_type.fn_ret_structs
        end
      | None -> ctx_with_type.fn_ret_structs
    in
    let ctx_with_func_idx = { ctx_with_type with
      func_indices = ctx_with_type.func_indices @ [(fd.fd_name.name, func_idx)];
      ownership_annots = ctx_with_type.ownership_annots @ [(func_idx, param_kinds, ret_kind)];
      fn_ret_structs = fn_ret_structs';
    } in

    (* Generate function with correct type index *)
    let* (ctx_after_gen, func) = gen_function ctx_with_func_idx fd in
    let func_with_type = { func with f_type = type_idx } in

    (* Export function if marked `pub` OR if its name matches a reserved
       game-loop hook (preserved for backward compatibility with the
       IDApTIK CharacterSelect bridge and the pre-`pub` compiler behaviour). *)
    let game_hook_names = ["main"; "init_state"; "step_state"; "get_state"; "mission_active"] in
    let is_pub = (fd.fd_vis = Ast.Public) in
    let is_game_hook = List.mem fd.fd_name.name game_hook_names in
    let export = if is_pub || is_game_hook then
      [{ e_name = fd.fd_name.name; e_desc = ExportFunc func_idx }]
    else
      []
    in
    (* Use ctx_after_gen to preserve any lambda_funcs created during generation *)
    Ok { ctx_after_gen with
         funcs = ctx_after_gen.funcs @ [func_with_type];
         exports = ctx_after_gen.exports @ export
       }

  | TopConst tc ->
    (* Constants are compiled as WASM globals.
       The initial value must be a constant expression (I32Const etc).
       For complex initialisers, fall back to a local. *)
    let* (ctx', init_code) = gen_expr ctx tc.tc_value in
    let global_idx = List.length ctx'.globals in
    let global = {
      g_type = I32;
      g_mutable = false;
      g_init = init_code;
    } in
    let ctx'' = { ctx' with
      globals = ctx'.globals @ [global];
      func_indices = (tc.tc_name.name, -(global_idx + 1)) :: ctx'.func_indices;
    } in
    Ok ctx''

  | TopType td ->
    begin match td.td_body with
      | TyEnum variants ->
        (* Assign sequential tags to each variant constructor *)
        let ctx_with_tags = List.fold_left (fun c_acc (idx, vd) ->
          (* Register: constructor_name -> tag *)
          { c_acc with variant_tags = (vd.vd_name.name, idx) :: c_acc.variant_tags }
        ) ctx (List.mapi (fun i v -> (i, v)) variants) in
        Ok ctx_with_tags
      | TyStruct fields ->
        (* Build the struct's field layout so function parameters and call
           results of this type can resolve `.field_N` to the correct offset.
           Layout: fields packed sequentially at 4-byte offsets, matching the
           ExprRecord store path which writes fields in declaration order. *)
        let layout = List.mapi (fun i sf -> (sf.sf_name.name, i * 4)) fields in
        Ok { ctx with struct_layouts = (td.td_name.name, layout) :: ctx.struct_layouts }
      | TyAlias _ ->
        Ok ctx
    end

  | TopEffect _ | TopTrait _ | TopImpl _ ->
    (* These declarations don't generate code *)
    Ok ctx

  | TopExternType _ ->
    (* Opaque host type — no code generated; type is available to the type-checker *)
    Ok ctx

  | TopExternFn ef ->
    (* Add a WebAssembly import for the extern fn declaration.
       Module name defaults to "env" (conventional host environment namespace). *)
    let param_types = List.map (fun _ -> I32) ef.ef_params in
    let result_types = match ef.ef_ret_ty with
      | None -> []
      | Some _ -> [I32]
    in
    let func_type = { ft_params = param_types; ft_results = result_types } in
    let type_idx = List.length ctx.types in
    let ctx_with_type = { ctx with types = ctx.types @ [func_type] } in
    let func_idx = import_func_count ctx_with_type in
    let import_entry = { i_module = "env"; i_name = ef.ef_name.name;
                         i_desc = ImportFunc type_idx } in
    Ok { ctx_with_type with
         imports = ctx_with_type.imports @ [import_entry];
         func_indices = (ef.ef_name.name, func_idx) :: ctx_with_type.func_indices }

(** Cross-module imports: walk [prog.prog_imports], load each referenced module
    via [loader], and for every imported top-level binding register the
    appropriate WASM artefact + entry in [func_indices].

    - Imported [TopFn]: emit a WASM [(import "<mod>" "<fn>" (func ...))]
      entry and bind the local alias to its positive function index.
    - Imported [TopConst]: compile its initialiser inline against the
      importer's context and append a fresh [global] entry; bind the
      local alias under the negative-sentinel convention from §3 of
      `docs/specs/codegen-environment.adoc`. (WASM module-linking for
      globals isn't standard yet, so each importer keeps its own copy
      of the constant — fine for the v0.1 surface.)

    Silent on missing modules / unresolved items / loader errors: the
    resolver runs before codegen and would have already errored. *)
let gen_imports (loader : Module_loader.t) (imports : import_decl list) (ctx : context)
    : context result =
  let process_one ctx (mod_path, orig_name, alias_opt) =
    match Module_loader.load_module loader mod_path with
    | Error _ -> Ok ctx
    | Ok loaded ->
      let local_name = Option.value alias_opt ~default:orig_name in
      (* Look up the imported binding — function or constant — in declaration
         order. The first match wins; same-name fn+const in the same module
         would be a resolver-level error and never reach codegen.
         Inline-record fields (TopConst) are destructured at the match site
         so the constructor's anonymous record type does not escape. *)
      let item = List.find_map (function
        | TopFn fd when fd.fd_name.name = orig_name -> Some (`Fn fd)
        | TopConst { tc_name; tc_value; _ } when tc_name.name = orig_name ->
          Some (`Const tc_value)
        | _ -> None
      ) loaded.mod_program.prog_decls in
      match item with
      | None -> Ok ctx
      | Some (`Fn fd) ->
        let ft = func_type_of_fn_decl fd in
        let (type_idx, types_after) = intern_func_type ctx.types ft in
        let import_func_idx = import_func_count ctx in
        let import = {
          i_module = String.concat "." mod_path;
          i_name = orig_name;
          i_desc = ImportFunc type_idx;
        } in
        Ok { ctx with
             types = types_after;
             imports = ctx.imports @ [import];
             func_indices = (local_name, import_func_idx) :: ctx.func_indices;
           }
      | Some (`Const tc_value) ->
        let* (ctx', init_code) = gen_expr ctx tc_value in
        let global_idx = List.length ctx'.globals in
        let global = {
          g_type = I32;
          g_mutable = false;
          g_init = init_code;
        } in
        Ok { ctx' with
             globals = ctx'.globals @ [global];
             func_indices = (local_name, -(global_idx + 1)) :: ctx'.func_indices;
           }
  in
  let expand_import imp : (string list * string * string option) list =
    let path_strs path = List.map (fun (id : ident) -> id.name) path in
    match imp with
    | ImportSimple _ -> []
    | ImportList (path, items) ->
      let p = path_strs path in
      List.map (fun item ->
        (p, item.ii_name.name, Option.map (fun (id : ident) -> id.name) item.ii_alias)
      ) items
    | ImportGlob path ->
      let p = path_strs path in
      (match Module_loader.load_module loader p with
       | Error _ -> []
       | Ok lm ->
         List.filter_map (function
           | TopFn fd when fd.fd_vis = Public || fd.fd_vis = PubCrate ->
             Some (p, fd.fd_name.name, None)
           | TopConst { tc_vis; tc_name; _ }
             when tc_vis = Public || tc_vis = PubCrate ->
             Some (p, tc_name.name, None)
           | _ -> None
         ) lm.mod_program.prog_decls)
  in
  let entries = List.concat_map expand_import imports in
  List.fold_left (fun acc e ->
    let* ctx = acc in
    process_one ctx e
  ) (Ok ctx) entries

(** Generate WASM module from AffineScript program.

    [?loader] supplies the module loader used to resolve cross-module imports.
    Defaults to a fresh loader with [Module_loader.default_config ()] so that
    existing call sites keep working without modification. *)
let generate_module ?loader (prog : program) : wasm_module result =
  (* ADR-016 / #234 S3: bind the effect-side-table oracle to THIS
     (post-optimizer) program's nodes. Ordinals are stable across the
     constant-folding optimizer (it never adds/removes/reorders calls),
     so the producer's ordinal→has-Async map keys correctly here. A
     count-mismatch makes [bind_consumer] a no-op ⇒ structural
     fallback. Safe if the producer never ran (empty ⇒ no-op). *)
  Effect_sites.bind_consumer prog;
  let loader = match loader with
    | Some l -> l
    | None -> Module_loader.create (Module_loader.default_config ())
  in
  let ctx = create_context () in

  (* Add WASI fd_write import at index 0 *)
  let (fd_write_import, fd_write_type) = Wasi_runtime.create_fd_write_import () in
  let fd_write_type_idx = 0 in  (* Will be first type *)
  let fd_write_import_fixed = { fd_write_import with i_desc = ImportFunc fd_write_type_idx } in

  (* ADR-015 S4 (#180): register WASI preview1 imports ON DEMAND so
     non-using units stay byte-identical to pre-S4 (the "import what
     you use" principle — adding unconditionally would force every
     host stub them and break every test harness). Pre-scan with the
     shared Effect_sites traversal: each builtin name maps 1:1 to its
     WASI import; appended after fd_write (idx 0) in a CANONICAL ORDER
     so the index assignment is deterministic across compilations.
     Each builtin's gen_expr case looks up its index by name in
     `ctx.wasi_func_indices` (no hardcoded indices — clean
     multi-import indexing). *)
  let uses (builtin : string) : bool =
    Effect_sites.fold_calls
      (fun acc _ord call ->
        acc ||
        match call with
        | ExprApp (ExprVar id, _) -> id.name = builtin
        | _ -> false)
      false prog
  in
  let optional_wasi =
    (* (guest_builtin_name, wasi_import_name, factory) — canonical order. *)
    [ ("clock_now_ms", "clock_time_get",     Wasi_runtime.create_clock_time_get_import);
      ("env_count",    "environ_sizes_get",  Wasi_runtime.create_environ_sizes_get_import);
      ("arg_count",    "args_sizes_get",     Wasi_runtime.create_args_sizes_get_import);
      ("net_shutdown", "sock_shutdown",      Wasi_runtime.create_sock_shutdown_import);
    ]
    |> List.filter_map
         (fun (b, w, f) -> if uses b then Some (w, f ()) else None)
    |> List.mapi (fun i (w, (imp, ty)) -> (i + 1, w, imp, ty))
  in
  let opt_types = List.map (fun (_, _, _, ty) -> ty) optional_wasi in
  let opt_imports =
    List.map (fun (idx, _, imp, _) -> { imp with i_desc = ImportFunc idx })
      optional_wasi
  in
  let wasi_indices =
    ("fd_write", fd_write_type_idx) ::
    List.map (fun (idx, name, _, _) -> (name, idx)) optional_wasi
  in
  let ctx_with_wasi =
      {
        ctx with
        types = fd_write_type :: opt_types @ ctx.types;
        imports = fd_write_import_fixed :: opt_imports @ ctx.imports;
        wasi_func_indices = wasi_indices;
      }
  in

  let* ctx_with_imports = gen_imports loader prog.prog_imports ctx_with_wasi in

  let* ctx' = List.fold_left (fun acc decl ->
    let* c = acc in
    gen_decl c decl
  ) (Ok ctx_with_imports) prog.prog_decls in

  (* Merge regular functions and lambda functions *)
  let num_regular_funcs = List.length ctx'.funcs in
  let import_offset = import_func_count ctx' in
  let all_funcs = ctx'.funcs @ ctx'.lambda_funcs in

  (* Create function table if there are lambdas *)
  let (tables, elems) = if List.length ctx'.lambda_funcs > 0 then
    (* Table size = number of lambda functions *)
    let table_size = List.length ctx'.lambda_funcs in
    let table = [{ tab_type = { lim_min = table_size; lim_max = Some table_size } }] in

    (* Create element segment to initialize table with lambda function indices *)
    (* Lambda functions start at index num_regular_funcs *)
    let lambda_func_indices = List.mapi (fun i _ -> import_offset + num_regular_funcs + i) ctx'.lambda_funcs in
    let elem_seg = [{
      e_table = 0;
      e_offset = 0;
      e_funcs = lambda_func_indices;
    }] in
    (table, elem_seg)
  else
    ([], [])
  in

  (* Add memory export, and — when the unit lifts any closure — the
     function table under the name the #199 host ABI dispatches through
     (`inst.exports.__indirect_function_table`, see wrapHandler in
     packages/affine-vscode/mod.js). Before #225 PR2 no end-to-end
     closure dispatch was ever exercised in wasm (PR1's skeleton was
     pure pass-through), so this export was missing though the #199
     marshalling code assumed it; the CPS continuation makes it load-
     bearing. Guarded on a non-empty table so closure-free modules are
     byte-for-byte unchanged. *)
  let table_export =
    if tables <> [] then
      [{ e_name = "__indirect_function_table"; e_desc = ExportTable 0 }]
    else []
  in
  let exports_with_mem =
    ({ e_name = "memory"; e_desc = ExportMemory 0 } :: ctx'.exports)
    @ table_export
  in

  (* ADR-015 S6 — WIT export lifting. When the unit exports a parameter-
     less `main`, also emit a `_start : () -> ()` shim that calls it and
     drops the i32 result. With `_start` present, tools/componentize.sh
     --command can wrap the core module with the WASI preview1→preview2
     *command* adapter, producing a real `wasi:cli/run`-exporting
     component that any WASI 0.2 host (`wasmtime run`, jco) can invoke.
     Purely additive: every existing consumer (reactor componentize,
     game-loop hosts that call `main` directly) sees the same exports
     plus an extra `_start`; main-with-params and units that already
     export `_start` are skipped. *)
  let final_types, final_funcs, final_exports =
    let has_user_start =
      List.exists (fun e -> e.e_name = "_start") exports_with_mem
    in
    let main_export =
      List.find_opt (fun e -> e.e_name = "main") exports_with_mem
    in
    match main_export, has_user_start with
    | Some { e_desc = ExportFunc main_idx; _ }, false ->
      let main_local_idx = main_idx - import_offset in
      if main_local_idx < 0 || main_local_idx >= List.length all_funcs then
        (ctx'.types, all_funcs, exports_with_mem)
      else
        let main_func = List.nth all_funcs main_local_idx in
        let main_type = List.nth ctx'.types main_func.f_type in
        if main_type.ft_params <> [] then
          (ctx'.types, all_funcs, exports_with_mem)
        else
          let void_ft = { ft_params = []; ft_results = [] } in
          let (void_type_idx, types_after) =
            intern_func_type ctx'.types void_ft
          in
          let drop_instrs =
            List.map (fun _ -> Drop) main_type.ft_results
          in
          let start_func = {
            f_type = void_type_idx;
            f_locals = [];
            f_body = Call main_idx :: drop_instrs;
          } in
          let funcs_after = all_funcs @ [start_func] in
          let start_idx = import_offset + List.length all_funcs in
          let exports_after =
            exports_with_mem
            @ [{ e_name = "_start"; e_desc = ExportFunc start_idx }]
          in
          (types_after, funcs_after, exports_after)
    | _ -> (ctx'.types, all_funcs, exports_with_mem)
  in

  (* Stage 2: Build [affinescript.ownership] custom section from collected annotations *)
  let ownership_payload = build_ownership_section ctx'.ownership_annots in
  let custom_sections = if Bytes.length ownership_payload > 0 then
    [("affinescript.ownership", ownership_payload)]
  else
    []
  in

  Ok {
    types = final_types;
    funcs = final_funcs;
    tables = tables;
    mems = [{ mem_type = { lim_min = 1; lim_max = None } }];  (* 1 page default *)
    globals = ctx'.globals;
    exports = final_exports;
    imports = ctx'.imports;
    elems = elems;
    datas = List.rev ctx'.datas;  (* Reverse to get original order *)
    start = None;
    custom_sections;
  }
