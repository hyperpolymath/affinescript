(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** WebAssembly GC code generation from AffineScript AST.

    Translates type-checked AffineScript programs to {!Wasm_gc.gc_module} values,
    which are then serialised by {!Wasm_gc_encode.write_gc_module_to_file}.

    {2 Type mapping}

    AffineScript types map to WasmGC types as follows:

    | AffineScript         | WasmGC                          |
    |----------------------|---------------------------------|
    | Int / Bool / Char    | GcPrim I32  (no boxing)         |
    | Float                | GcPrim F64  (no boxing)         |
    | String               | GcRef (HtConcrete string_array) |
    | Array[T]             | GcRef (HtConcrete array_type)   |
    | Record / Struct      | GcRef (HtConcrete struct_type)  |
    | Tuple                | GcRef (HtConcrete anon_struct)  |
    | Variant (0-arg)      | GcPrim I32  (integer tag)       |
    | Unknown / param      | GcAnyref                        |

    {2 Affine ownership}

    Because AffineScript has affine (linear) types, exclusively-owned GC
    objects are encoded as non-null refs — [GcRef (HtConcrete typeidx)] —
    rather than nullable [GcAnyref].  This aligns affine-type semantics with
    the WasmGC reference hierarchy without needing runtime null checks.

    {2 First-pass limitations}

    Function parameters and return values default to [GcAnyref] when no type
    annotation is present.  Field access on function-parameter records requires
    a [ref.cast] before [struct.get].  Future passes will propagate concrete
    types through the function signature.
*)

open Ast
open Wasm_gc

(** {1 Error type} *)

type codegen_error =
  | UnsupportedFeature of string
  | UnboundVariable of string
  | UnboundType of string
  (** Raised when [ExprApp] names a function that has no entry in [func_indices].
      This is always a compiler bug — every defined function must be registered
      before codegen reaches a call-site. *)
  | UnboundFunction of string
[@@deriving show]

type 'a cg_result = ('a, codegen_error) Result.t

let ( let* ) = Result.bind

(** {1 Codegen context} *)

(** GC code generation context.

    Module-level fields accumulate across top-level declarations.
    Function-level fields are reset at the start of each function. *)
type gc_ctx = {

  (* ── Module-level accumulated state ──────────────────────────────── *)

  (** GC type definitions in registration order.
      A type's index in the type section equals its position here. *)
  gc_type_defs    : gc_type_def list;
  next_type_idx   : int;

  (** Accumulated function definitions. *)
  gc_funcs_acc    : gc_func list;

  (** Accumulated named exports. *)
  gc_exports_acc  : gc_export list;

  (** Function name → absolute function index (import_count + position). *)
  func_indices    : (string * int) list;

  (** Number of imported functions (offsets defined-function indices). *)
  import_count    : int;

  (* ── GC type registry ────────────────────────────────────────────── *)

  (** Named struct registry: AffineScript type name → GC type index.
      Populated from [TopType (TyStruct _)] declarations. *)
  struct_reg      : (string * int) list;

  (** Field-name map: type name → [(field_name, field_index_in_struct)].
      Needed to emit [StructGet] with the correct field index. *)
  field_reg       : (string * (string * int) list) list;

  (** Anonymous struct registry: comma-joined field names → GC type index.
      Used for structurally-typed record literals not backed by a named type.
      Example key: "x,y" for a record with fields x and y in that order. *)
  anon_struct_reg : (string * int) list;

  (** Array type registry: element-kind key → GC type index.
      Keys: "i32" | "f64" | "anyref" *)
  array_reg       : (string * int) list;

  (** Variant constructor → integer tag (same convention as {!Codegen}). *)
  variant_tags    : (string * int) list;

  (* ── Function-level state (reset per function) ───────────────────── *)

  (** Variable name → local index within the current function. *)
  locals          : (string * int) list;

  (** Local index → GC value type (for building [gf_locals] vector).
      Covers only indices ≥ param_count (extra locals beyond params). *)
  local_kinds     : (int * gc_valtype) list;

  next_local      : int;

  (** Number of function parameters = number of pre-declared locals. *)
  param_count     : int;

  loop_depth      : int;

  (** Variable name → GC struct type index.
      Set when a let-binding is assigned from [ExprRecord] or [ExprTuple],
      enabling direct [StructGet] without a preceding [ref.cast]. *)
  var_gc_type     : (string * int) list;
}

(** {1 Context helpers} *)

let create_gc_ctx () : gc_ctx = {
  gc_type_defs    = [];
  next_type_idx   = 0;
  gc_funcs_acc    = [];
  gc_exports_acc  = [];
  func_indices    = [];
  import_count    = 0;
  struct_reg      = [];
  field_reg       = [];
  anon_struct_reg = [];
  array_reg       = [];
  variant_tags    = [];
  locals          = [];
  local_kinds     = [];
  next_local      = 0;
  param_count     = 0;
  loop_depth      = 0;
  var_gc_type     = [];
}

(** Register a GC type definition, returning the updated context and type index. *)
let register_gc_type (ctx : gc_ctx) (typedef : gc_type_def) : gc_ctx * int =
  let idx = ctx.next_type_idx in
  ({ ctx with
     gc_type_defs  = ctx.gc_type_defs @ [typedef];
     next_type_idx = idx + 1
   }, idx)

(** Allocate a new named local with a given GC value type.
    Returns the updated context and the local's index. *)
let alloc_local (ctx : gc_ctx) (name : string) (vt : gc_valtype) : gc_ctx * int =
  let idx = ctx.next_local in
  let kinds' =
    if idx >= ctx.param_count
    then (idx, vt) :: ctx.local_kinds
    else ctx.local_kinds
  in
  ({ ctx with
     locals      = (name, idx) :: ctx.locals;
     local_kinds = kinds';
     next_local  = idx + 1
   }, idx)

(** Look up the local index for a named variable. *)
let lookup_local (ctx : gc_ctx) (name : string) : int cg_result =
  match List.assoc_opt name ctx.locals with
  | Some idx -> Ok idx
  | None     -> Error (UnboundVariable name)

(** {1 Type conversion helpers} *)

(** Map an AffineScript type annotation to a GC value type.

    Primitives (Int, Bool, Char, Nat) become [GcPrim I32]; Float becomes
    [GcPrim F64]; all other types become [GcAnyref] in this first pass.
    A future pass will propagate concrete struct/array ref types through
    the type registry. *)
let rec as_type_to_gc_valtype (ty : type_expr) : gc_valtype =
  match ty with
  | TyCon id when id.name = "Int"  || id.name = "Bool"
               || id.name = "Char" || id.name = "Nat"  -> GcPrim I32
  | TyCon id when id.name = "Float"                    -> GcPrim F64
  | TyCon _                                             -> GcAnyref
  | TyOwn inner | TyRef inner                           -> as_type_to_gc_valtype inner
  | _                                                   -> GcAnyref

(** Map an AffineScript type annotation to a struct field type.
    Affine records use non-mutable anyref fields by default. *)
and as_type_to_field_type (ty : type_expr) : field_type =
  match as_type_to_gc_valtype ty with
  | GcPrim I32 -> field_i32
  | GcPrim F64 -> field_f64
  | _          -> field_anyref

(** Infer the GC value type of an expression (heuristic for local allocation).
    Used when no type annotation is available. *)
let gc_valtype_of_expr (expr : expr) : gc_valtype =
  match expr with
  | ExprLit (LitInt _ | LitBool _ | LitChar _ | LitUnit _) -> GcPrim I32
  | ExprLit (LitFloat _) -> GcPrim F64
  | ExprBinary _ | ExprUnary _ -> GcPrim I32
  | ExprRecord _ | ExprTuple _ | ExprArray _ -> GcAnyref
  | _ -> GcAnyref  (* conservative *)

(** {1 Registry helpers} *)

(** Find or register an anonymous struct type keyed by ordered field names.
    Returns (ctx', type_idx, field_name_to_index_map). *)
let find_or_register_anon_struct
    (ctx : gc_ctx)
    (field_names : string list)
    (field_types : field_type list)
    : gc_ctx * int * (string * int) list =
  let key = String.concat "," field_names in
  match List.assoc_opt key ctx.anon_struct_reg with
  | Some idx ->
    let field_map = List.mapi (fun i n -> (n, i)) field_names in
    (ctx, idx, field_map)
  | None ->
    let (ctx', idx) = register_gc_type ctx (GcStructType field_types) in
    let field_map = List.mapi (fun i n -> (n, i)) field_names in
    let ctx'' = { ctx' with anon_struct_reg = (key, idx) :: ctx'.anon_struct_reg } in
    (ctx'', idx, field_map)

(** Find or register a GC array type for a given element kind.
    elem_key: "i32" | "f64" | "anyref"
    Returns (ctx', type_idx). *)
let find_or_register_array (ctx : gc_ctx) (elem_key : string) (elem_ft : field_type)
    : gc_ctx * int =
  match List.assoc_opt elem_key ctx.array_reg with
  | Some idx -> (ctx, idx)
  | None ->
    let (ctx', idx) = register_gc_type ctx (GcArrayType elem_ft) in
    ({ ctx' with array_reg = (elem_key, idx) :: ctx'.array_reg }, idx)

(** Determine the array element kind and field type from the first element expression.
    Used as a heuristic when element type annotations are unavailable. *)
let array_elem_info_from_expr (elem_opt : expr option) : string * field_type =
  match elem_opt with
  | Some (ExprLit (LitInt _ | LitBool _ | LitChar _)) ->
    ("i32", { ft_storage = StVal (GcPrim I32); ft_mutable = true })
  | Some (ExprLit (LitFloat _)) ->
    ("f64", { ft_storage = StVal (GcPrim F64); ft_mutable = true })
  | _ ->
    ("anyref", { ft_storage = StVal GcAnyref; ft_mutable = true })

(** Get the GC struct type index for a record-valued expression.
    Returns [None] when the concrete type cannot be determined statically. *)
let gc_struct_type_of_expr (ctx : gc_ctx) (expr : expr) : int option =
  match expr with
  | ExprVar id -> List.assoc_opt id.name ctx.var_gc_type
  | _ -> None

(** {1 Instruction helpers} *)

(** Wrap a standard WASM 1.0 instruction. *)
let std (i : Wasm.instr) : gc_instr = Std i

(** Push an i32 constant. *)
let push_i32 (n : int) : gc_instr = Std (Wasm.I32Const (Int32.of_int n))

(** {1 Binary operator mapping} *)

let gen_binop (op : binary_op) : Wasm.instr =
  match op with
  | OpAdd    -> Wasm.I32Add   | OpSub    -> Wasm.I32Sub
  | OpMul    -> Wasm.I32Mul   | OpDiv    -> Wasm.I32DivS
  | OpMod    -> Wasm.I32RemS  | OpEq     -> Wasm.I32Eq
  | OpNe     -> Wasm.I32Ne    | OpLt     -> Wasm.I32LtS
  | OpLe     -> Wasm.I32LeS   | OpGt     -> Wasm.I32GtS
  | OpGe     -> Wasm.I32GeS   | OpAnd    -> Wasm.I32And
  | OpOr     -> Wasm.I32Or    | OpBitAnd -> Wasm.I32And
  | OpBitOr  -> Wasm.I32Or    | OpBitXor -> Wasm.I32Xor
  | OpShl    -> Wasm.I32Shl   | OpShr    -> Wasm.I32ShrS
  | OpConcat -> Wasm.I32Add (* Placeholder *)

(** {1 Expression codegen} *)

(** Generate GC instructions for an expression.

    Returns [(ctx', instrs)] where [instrs] leaves exactly one value on the
    WASM value stack.  [ctx'] carries any newly registered GC types or locals.

    Key GC operations:
    - [ExprRecord] → allocate via [struct.new]; non-null affine ownership
    - [ExprField]  → [ref.cast] then [struct.get] (or direct if type is known)
    - [ExprArray]  → allocate via [array.new_fixed]; non-null affine ownership
    - [ExprIndex]  → [ref.cast] then [array.get]
    - [ExprTuple]  → anonymous struct via [struct.new]
*)
let rec gen_gc_expr (ctx : gc_ctx) (expr : expr) : (gc_ctx * gc_instr list) cg_result =
  match expr with

  (* ── Literals ──────────────────────────────────────────────────── *)

  | ExprLit (LitUnit _)        -> Ok (ctx, [push_i32 0])
  | ExprLit (LitBool (b, _))   -> Ok (ctx, [push_i32 (if b then 1 else 0)])
  | ExprLit (LitInt (n, _))    -> Ok (ctx, [Std (Wasm.I32Const (Int32.of_int n))])
  | ExprLit (LitFloat (f, _))  -> Ok (ctx, [Std (Wasm.F64Const f)])
  | ExprLit (LitChar (c, _))   -> Ok (ctx, [push_i32 (Char.code c)])
  | ExprLit (LitString (_, _)) ->
    (* Strings in GC mode would be (array (mut i8)) refs.
       First pass: emit a null anyref as a safe placeholder. *)
    Ok (ctx, [RefNull HtAny])

  (* ── Variable access ───────────────────────────────────────────── *)

  | ExprVar id ->
    let* idx = lookup_local ctx id.name in
    Ok (ctx, [std (Wasm.LocalGet idx)])

  (* ── Arithmetic / logical / comparison ─────────────────────────── *)

  | ExprBinary (lhs, op, rhs) ->
    let* (ctx1, lhs_code) = gen_gc_expr ctx  lhs in
    let* (ctx2, rhs_code) = gen_gc_expr ctx1 rhs in
    Ok (ctx2, lhs_code @ rhs_code @ [std (gen_binop op)])

  | ExprUnary (OpNeg, operand) ->
    let* (ctx', code) = gen_gc_expr ctx operand in
    Ok (ctx', [push_i32 0] @ code @ [std Wasm.I32Sub])

  | ExprUnary (OpNot, operand) ->
    let* (ctx', code) = gen_gc_expr ctx operand in
    Ok (ctx', code @ [std Wasm.I32Eqz])

  | ExprUnary (_, operand) ->
    gen_gc_expr ctx operand

  (* ── Function calls ────────────────────────────────────────────── *)

  | ExprApp (ExprVar id, args) ->
    let* (ctx_after_args, arg_codes_rev) =
      List.fold_left (fun acc arg ->
        let* (c, rev_codes) = acc in
        let* (c', code) = gen_gc_expr c arg in
        Ok (c', code :: rev_codes)
      ) (Ok (ctx, [])) args
    in
    let arg_codes = List.concat (List.rev arg_codes_rev) in

    begin match id.name with
      | "int" ->
        Ok (ctx_after_args, arg_codes @ [Std (Wasm.I32TruncF64S)])
      | "float" ->
        Ok (ctx_after_args, arg_codes @ [Std (Wasm.F64ConvertI32S)])
      | _ ->
        match List.assoc_opt id.name ctx_after_args.func_indices with
        | Some func_idx ->
          Ok (ctx_after_args, arg_codes @ [Std (Wasm.Call func_idx)])
        | None ->
          (* BUG-005: every reachable function must be registered in func_indices
             before codegen visits any call-site.  Silently emitting drop+null here
             would produce well-typed but semantically wrong WASM that is impossible
             to debug at runtime.  Fail loudly instead. *)
          Error (UnboundFunction id.name)
    end

  | ExprApp (callee, args) ->
    (* BUG-005: indirect / higher-order calls (callee is not a plain ExprVar) are
       not yet lowered to call_ref in the WasmGC backend.  The old behaviour —
       evaluate callee+args, drop everything, push null — was silently wrong and
       would crash at the call-site with an opaque type error.  Reject explicitly
       until call_ref support is added. *)
    let _ = (callee, args) in
    Error (UnsupportedFeature "indirect / higher-order call in WasmGC backend (call_ref not yet implemented)")

  (* ── Record allocation (core GC operation) ─────────────────────── *)

  | ExprRecord rec_expr ->
    let field_names = List.map (fun (id, _) -> id.name) rec_expr.er_fields in
    let n = List.length field_names in

    (* Generate code for each field value in declaration order.
       Stack at struct.new call: field[0], field[1], ..., field[n-1]. *)
    let* (ctx_after, field_codes_rev) =
      List.fold_left (fun acc (_, expr_opt) ->
        let* (c, rev_codes) = acc in
        let fexpr = match expr_opt with
          | Some e -> e
          | None   ->
            let i = List.length rev_codes in
            ExprVar { name = List.nth field_names i; span = Span.dummy }
        in
        let* (c', code) = gen_gc_expr c fexpr in
        Ok (c', code :: rev_codes)
      ) (Ok (ctx, [])) rec_expr.er_fields
    in
    let field_codes = List.concat (List.rev field_codes_rev) in

    (* Register anonymous struct type for this field signature.
       All fields default to immutable anyref — field types are refined
       when a named type annotation is available (see gen_gc_decl/TopType). *)
    let field_types = List.init n (fun _ -> field_anyref) in
    let (ctx', type_idx, _field_map) =
      find_or_register_anon_struct ctx_after field_names field_types
    in

    (* struct.new is non-null — this is an affinely-owned GC object. *)
    Ok (ctx', field_codes @ [StructNew type_idx])

  (* ── Record field access ────────────────────────────────────────── *)

  | ExprField (base_expr, field_id) ->
    let* (ctx', base_code) = gen_gc_expr ctx base_expr in

    (* Attempt to identify the concrete struct type statically:
       (a) The base is a named variable with a tracked GC type index.
       (b) The base is a named struct from struct_reg. *)
    let find_type_and_field () : (int * int) option =
      (* Try var_gc_type lookup first *)
      let type_idx_opt = gc_struct_type_of_expr ctx' base_expr in
      match type_idx_opt with
      | Some type_idx ->
        (* Reconstruct the field map for this type index *)
        let reverse_anon = List.map (fun (k, v) -> (v, k)) ctx'.anon_struct_reg in
        let reverse_named = List.map (fun (k, v) -> (v, k)) ctx'.struct_reg in
        let type_name_opt =
          match List.assoc_opt type_idx reverse_named with
          | Some n -> Some n
          | None ->
            (* Anonymous struct: rebuild field names from key *)
            Option.map (fun key ->
              (* Use key directly as type name for field_reg lookup *)
              let names = String.split_on_char ',' key in
              String.concat "," names
            ) (List.assoc_opt type_idx reverse_anon)
        in
        begin match type_name_opt with
          | Some type_name ->
            begin match List.assoc_opt type_name ctx'.field_reg with
              | Some fields ->
                begin match List.assoc_opt field_id.name fields with
                  | Some field_idx -> Some (type_idx, field_idx)
                  | None -> None
                end
              | None ->
                (* Anonymous struct: field index = position in field names *)
                let anon_key = match List.assoc_opt type_idx reverse_anon with
                  | Some k -> k | None -> "" in
                let names = String.split_on_char ',' anon_key in
                let rec find_pos i = function
                  | [] -> None
                  | n :: _ when n = field_id.name -> Some (type_idx, i)
                  | _ :: rest -> find_pos (i + 1) rest
                in
                find_pos 0 names
            end
          | None -> None
        end
      | None -> None
    in

    begin match find_type_and_field () with
      | Some (type_idx, field_idx) ->
        (* Concrete type known: ref.cast (non-null, affine) then struct.get *)
        Ok (ctx', base_code @
          [RefCast (HtConcrete type_idx);
           StructGet (type_idx, field_idx)])
      | None ->
        (* Unknown type: cannot emit struct.get without type index.
           Emit drop + null as a safe placeholder — field access on unknown
           struct types requires propagating type info through the pipeline. *)
        Ok (ctx', base_code @ [std Wasm.Drop; RefNull HtAny])
    end

  (* ── Tuple allocation ───────────────────────────────────────────── *)

  | ExprTuple elems ->
    let n = List.length elems in
    let* (ctx_after, elem_codes_rev) =
      List.fold_left (fun acc elem ->
        let* (c, rev_codes) = acc in
        let* (c', code) = gen_gc_expr c elem in
        Ok (c', code :: rev_codes)
      ) (Ok (ctx, [])) elems
    in
    let elem_codes = List.concat (List.rev elem_codes_rev) in

    (* Tuple fields are positional: _0, _1, _2, ... *)
    let field_names = List.init n (fun i -> Printf.sprintf "_%d" i) in
    let field_types = List.init n (fun _ -> field_anyref) in
    let (ctx', type_idx, _) =
      find_or_register_anon_struct ctx_after field_names field_types
    in
    Ok (ctx', elem_codes @ [StructNew type_idx])

  (* ── Tuple element access ───────────────────────────────────────── *)

  | ExprTupleIndex (tuple_expr, index) ->
    let* (ctx', base_code) = gen_gc_expr ctx tuple_expr in
    let type_idx_opt = gc_struct_type_of_expr ctx' tuple_expr in
    begin match type_idx_opt with
      | Some type_idx ->
        Ok (ctx', base_code @
          [RefCast (HtConcrete type_idx);
           StructGet (type_idx, index)])
      | None ->
        (* Unknown tuple type: cannot emit struct.get safely *)
        Ok (ctx', base_code @ [std Wasm.Drop; RefNull HtAny])
    end

  (* ── Array allocation ───────────────────────────────────────────── *)

  | ExprArray elems ->
    let n = List.length elems in
    let first_elem = match elems with e :: _ -> Some e | [] -> None in
    let (elem_key, elem_ft) = array_elem_info_from_expr first_elem in
    let (ctx', array_type_idx) = find_or_register_array ctx elem_key elem_ft in

    if n = 0 then
      (* array.new_default with length 0 — valid empty GC array *)
      Ok (ctx', [push_i32 0; ArrayNewDefault array_type_idx])
    else begin
      (* Push all elements onto the stack, then array.new_fixed.
         Stack: elem[0], elem[1], ..., elem[n-1] *)
      let* (ctx'', elem_codes_rev) =
        List.fold_left (fun acc elem ->
          let* (c, rev_codes) = acc in
          let* (c', code) = gen_gc_expr c elem in
          Ok (c', code :: rev_codes)
        ) (Ok (ctx', [])) elems
      in
      let elem_codes = List.concat (List.rev elem_codes_rev) in
      Ok (ctx'', elem_codes @ [ArrayNewFixed (array_type_idx, n)])
    end

  (* ── Array element access ───────────────────────────────────────── *)

  | ExprIndex (arr_expr, idx_expr) ->
    let* (ctx1, arr_code) = gen_gc_expr ctx  arr_expr in
    let* (ctx2, idx_code) = gen_gc_expr ctx1 idx_expr in

    (* Look up array GC type index from var_gc_type if possible,
       otherwise fall back to the most recently registered array type. *)
    let array_type_idx =
      match gc_struct_type_of_expr ctx2 arr_expr with
      | Some tidx -> tidx
      | None ->
        match ctx2.array_reg with
        | (_, idx) :: _ -> idx
        | [] -> 0
    in

    (* array.get: ref.cast the array ref, then array.get *)
    Ok (ctx2,
      arr_code @
      [RefCast (HtConcrete array_type_idx)] @
      idx_code @
      [ArrayGet array_type_idx])

  (* ── Variant (zero-argument) ────────────────────────────────────── *)

  | ExprVariant (_, variant_name) ->
    (* Zero-argument variants are represented as i32 tags — identical to
       the WASM 1.0 backend.  Non-zero-arg variants would need a GC struct
       with a tag field, which is added in a future pass. *)
    let (tag, ctx') =
      match List.assoc_opt variant_name.name ctx.variant_tags with
      | Some t -> (t, ctx)
      | None   ->
        let t = List.length ctx.variant_tags in
        (t, { ctx with variant_tags = (variant_name.name, t) :: ctx.variant_tags })
    in
    Ok (ctx', [push_i32 tag])

  (* ── If / else ──────────────────────────────────────────────────── *)

  | ExprIf ei ->
    let* (ctx1, cond_code)  = gen_gc_expr ctx  ei.ei_cond in
    let* (ctx2, then_code)  = gen_gc_expr ctx1 ei.ei_then in
    begin match ei.ei_else with
      | None ->
        (* No else branch: if runs body for effect, result is unit (0) *)
        Ok (ctx2, cond_code @
          [GcIf (GcBtPrim I32,
             then_code @ [push_i32 0],  (* then: body result discarded, push 0 *)
             [push_i32 0])])            (* else: unit *)
      | Some else_expr ->
        let* (ctx3, else_code) = gen_gc_expr ctx2 else_expr in
        (* Use I32 result type — if branches return numeric results.
           For GC-ref-typed results, GcBtRef HtAny would be appropriate. *)
        Ok (ctx3, cond_code @
          [GcIf (GcBtPrim I32, then_code, else_code)])
    end

  (* ── Block ──────────────────────────────────────────────────────── *)

  | ExprBlock blk ->
    gen_gc_block ctx blk

  (* ── Inline let (expr form: let x = rhs in body) ────────────────── *)

  | ExprLet lb ->
    let* (ctx1, rhs_code) = gen_gc_expr ctx lb.el_value in
    let vt = gc_valtype_of_expr lb.el_value in
    begin match lb.el_pat with
      | PatVar id ->
        let (ctx2, local_idx) = alloc_local ctx1 id.name vt in

        (* Track GC struct type when RHS is a structural value *)
        let ctx3 = match lb.el_value with
          | ExprRecord _ | ExprTuple _ ->
            begin match ctx2.anon_struct_reg with
              | (_, type_idx) :: _ ->
                { ctx2 with var_gc_type = (id.name, type_idx) :: ctx2.var_gc_type }
              | [] -> ctx2
            end
          | _ -> ctx2
        in

        (* Generate body or return unit if this is a standalone binding *)
        begin match lb.el_body with
          | Some body_expr ->
            let* (ctx_final, body_code) = gen_gc_expr ctx3 body_expr in
            Ok (ctx_final,
              rhs_code @
              [std (Wasm.LocalSet local_idx)] @
              body_code)
          | None ->
            Ok (ctx3, rhs_code @ [std (Wasm.LocalSet local_idx); push_i32 0])
        end

      | PatWildcard _ ->
        let body_code_or_unit = match lb.el_body with
          | Some body ->
            begin match gen_gc_expr ctx1 body with
            | Ok (_, code) -> code
            | Error _ -> [push_i32 0]
            end
          | None -> [push_i32 0]
        in
        let* ctx_final = match lb.el_body with
          | Some body -> let* (c, _) = gen_gc_expr ctx1 body in Ok c
          | None -> Ok ctx1
        in
        Ok (ctx_final, rhs_code @ [std Wasm.Drop] @ body_code_or_unit)

      | _ ->
        (* Complex pattern — bind to anonymous temp, continue with body *)
        let (ctx2, _tmp) = alloc_local ctx1 "__let_tmp" GcAnyref in
        begin match lb.el_body with
          | Some body_expr ->
            let* (ctx_final, body_code) = gen_gc_expr ctx2 body_expr in
            Ok (ctx_final, rhs_code @ [std Wasm.Drop] @ body_code)
          | None ->
            Ok (ctx2, rhs_code @ [std Wasm.Drop; push_i32 0])
        end
    end

  (* ── Return ─────────────────────────────────────────────────────── *)

  | ExprReturn expr_opt ->
    begin match expr_opt with
      | Some e ->
        let* (ctx', code) = gen_gc_expr ctx e in
        Ok (ctx', code @ [std Wasm.Return])
      | None ->
        Ok (ctx, [push_i32 0; std Wasm.Return])
    end

  (* ── Pattern match ──────────────────────────────────────────────── *)

  | ExprMatch m ->
    let* (ctx1, scrutinee_code) = gen_gc_expr ctx m.em_scrutinee in

    (* Store scrutinee in a temporary local (anyref covers all cases) *)
    let (ctx2, scrutinee_local) = alloc_local ctx1 "__scrutinee" GcAnyref in
    let save_code = [std (Wasm.LocalSet scrutinee_local)] in

    (* Build right-to-left if/else chain: last arm is the default *)
    let* (ctx_final, match_code) =
      List.fold_right
        (fun arm acc ->
          let* (c_acc, default_code) = acc in
          begin match arm.ma_pat with

            | PatWildcard _ ->
              let* (c', body_code) = gen_gc_expr c_acc arm.ma_body in
              Ok (c', body_code)  (* Wildcard: ignore default, use body *)

            | PatVar id ->
              let (c_with_var, var_idx) = alloc_local c_acc id.name GcAnyref in
              let* (c', body_code) = gen_gc_expr c_with_var arm.ma_body in
              Ok (c',
                [std (Wasm.LocalGet scrutinee_local);
                 std (Wasm.LocalSet var_idx)] @
                body_code)

            | PatLit lit ->
              let lit_instr = match lit with
                | LitBool (b, _) -> push_i32 (if b then 1 else 0)
                | LitInt (n, _)  -> push_i32 n
                | LitChar (c, _) -> push_i32 (Char.code c)
                | _              -> push_i32 0
              in
              let* (c', body_code) = gen_gc_expr c_acc arm.ma_body in
              let test =
                [std (Wasm.LocalGet scrutinee_local);
                 lit_instr; std Wasm.I32Eq]
              in
              Ok (c', test @
                [GcIf (GcBtPrim I32, body_code, default_code)])

            | PatCon (con, _sub_pats) ->
              let (tag, c_acc') =
                match List.assoc_opt con.name c_acc.variant_tags with
                | Some t -> (t, c_acc)
                | None   ->
                  let t = List.length c_acc.variant_tags in
                  (t, { c_acc with variant_tags =
                    (con.name, t) :: c_acc.variant_tags })
              in
              let* (c', body_code) = gen_gc_expr c_acc' arm.ma_body in
              let test =
                [std (Wasm.LocalGet scrutinee_local);
                 push_i32 tag; std Wasm.I32Eq]
              in
              Ok (c', test @
                [GcIf (GcBtPrim I32, body_code, default_code)])

            | _ ->
              (* Unsupported pattern: skip arm, fall to default *)
              Ok (c_acc, default_code)
          end)
        m.em_arms
        (Ok (ctx2, [push_i32 0]))  (* terminal default: return 0 *)
    in
    Ok (ctx_final, scrutinee_code @ save_code @ match_code)

  (* ── Effect / error-handling passthrough ────────────────────────── *)

  | ExprHandle _eh ->
    (* Effect handler dispatch is not implementable in this WasmGC backend
       without either:
         - The WASM exception-handling proposal (EH) to propagate PerformEffect
           across stack frames and capture the perform-site continuation, OR
         - A whole-program CPS transform before codegen.

       Silently compiling just the body (the previous behaviour) was wrong:
       any handler arms are dropped, so effects are never caught, and the
       first `perform` would trap at the op stub rather than dispatch to the
       correct handler arm.  Fail loudly instead.

       To use algebraic effects, compile with the interpreter backend (-i). *)
    Error (UnsupportedFeature
      "effect handler (handle { ... }) in WasmGC backend — \
       requires WASM EH proposal or CPS transform; use `--interp` / `-i`")

  | ExprTry et ->
    (* WasmGC 1.0 does not support the exception-handling proposal.
       - catch arms: UnsupportedFeature (cannot trap-and-resume in GC mode).
       - body + optional finally: compile sequentially with a GcAnyref temp
         to preserve the body result across the finally block. *)
    begin match et.et_catch with
    | Some _ ->
        Error (UnsupportedFeature
          "try/catch in WasmGC backend — \
           requires the WASM exception-handling proposal; \
           use the Julia backend (-julia) or the interpreter (-i)")
    | None ->
        let* (ctx', body_code) = gen_gc_block ctx et.et_body in
        begin match et.et_finally with
        | None -> Ok (ctx', body_code)
        | Some blk ->
            let (ctx'', tmp_idx) = alloc_local ctx' "__try_result" GcAnyref in
            let* (ctx''', fin_code) = gen_gc_block ctx'' blk in
            Ok (ctx''',
              body_code
              @ [std (Wasm.LocalSet tmp_idx)]   (* stash body result      *)
              @ fin_code
              @ [std Wasm.Drop]                 (* discard finally result  *)
              @ [std (Wasm.LocalGet tmp_idx)]   (* restore body result     *))
        end
    end

  | ExprResume _arg_opt ->
    (* `resume` is only meaningful inside an effect handler arm.  The WasmGC
       backend has no handler dispatch (see ExprHandle above), so emitting
       the argument value here would be silently wrong — the enclosing handle
       expression already fails with UnsupportedFeature before we ever reach
       a resume.  Fail consistently. *)
    Error (UnsupportedFeature
      "`resume` expression in WasmGC backend — \
       only valid inside a `handle` block; use `--interp` / `-i`")

  | ExprRowRestrict (base, _) ->
    (* Row restriction is type-level; GC pointer is unchanged *)
    gen_gc_expr ctx base

  | ExprSpan (e, _) ->
    gen_gc_expr ctx e

  (* ── Fallback ───────────────────────────────────────────────────── *)

  | _ ->
    (* Unsupported expression: null anyref is a safe non-crashing placeholder *)
    Ok (ctx, [RefNull HtAny])

(** {1 Block codegen} *)

(** Generate GC instructions for a block.

    Statements execute for effects (results discarded).  The trailing
    expression, if present, leaves its value on the stack.  A block
    with no trailing expression pushes unit (0). *)
and gen_gc_block (ctx : gc_ctx) (blk : block) : (gc_ctx * gc_instr list) cg_result =
  let* (ctx', stmt_codes) =
    List.fold_left (fun acc stmt ->
      let* (c, codes) = acc in
      let* (c', code) = gen_gc_stmt c stmt in
      Ok (c', codes @ code)
    ) (Ok (ctx, [])) blk.blk_stmts
  in
  match blk.blk_expr with
  | Some e ->
    let* (ctx_final, expr_code) = gen_gc_expr ctx' e in
    Ok (ctx_final, stmt_codes @ expr_code)
  | None ->
    Ok (ctx', stmt_codes @ [push_i32 0])

(** {1 Statement codegen} *)

and gen_gc_stmt (ctx : gc_ctx) (stmt : stmt) : (gc_ctx * gc_instr list) cg_result =
  match stmt with

  | StmtLet sl ->
    let* (ctx1, rhs_code) = gen_gc_expr ctx sl.sl_value in
    let vt = gc_valtype_of_expr sl.sl_value in
    begin match sl.sl_pat with

      | PatVar id ->
        let (ctx2, local_idx) = alloc_local ctx1 id.name vt in

        (* Track GC struct type index when RHS is a structural allocation *)
        let ctx3 = match sl.sl_value with
          | ExprRecord _ | ExprTuple _ ->
            begin match ctx2.anon_struct_reg with
              | (_, type_idx) :: _ ->
                { ctx2 with var_gc_type = (id.name, type_idx) :: ctx2.var_gc_type }
              | [] -> ctx2
            end
          | _ -> ctx2
        in
        Ok (ctx3, rhs_code @ [std (Wasm.LocalSet local_idx)])

      | PatTuple sub_pats ->
        (* Destructure: save tuple ref, then extract each element via StructGet *)
        let (ctx2, tmp_idx) = alloc_local ctx1 "__tup_tmp" GcAnyref in

        (* Determine the tuple's struct type index from the most recent anon entry *)
        let type_idx_opt = match ctx2.anon_struct_reg with
          | (_, idx) :: _ -> Some idx
          | [] -> None
        in

        let* (ctx_final, bind_codes) =
          List.fold_left (fun acc (i, sub_pat) ->
            let* (c, codes) = acc in
            begin match sub_pat with
              | PatVar id ->
                let (c', var_idx) = alloc_local c id.name GcAnyref in
                let load = match type_idx_opt with
                  | Some tidx ->
                    [std (Wasm.LocalGet tmp_idx);
                     RefCast (HtConcrete tidx);
                     StructGet (tidx, i);
                     std (Wasm.LocalSet var_idx)]
                  | None ->
                    (* Type unknown: bind 0 as placeholder *)
                    [push_i32 0; std (Wasm.LocalSet var_idx)]
                in
                Ok (c', codes @ load)
              | PatWildcard _ -> Ok (c, codes)
              | _ -> Ok (c, codes)
            end
          ) (Ok (ctx2, [])) (List.mapi (fun i p -> (i, p)) sub_pats)
        in
        Ok (ctx_final,
          rhs_code @ [std (Wasm.LocalSet tmp_idx)] @ bind_codes)

      | PatWildcard _ ->
        Ok (ctx1, rhs_code @ [std Wasm.Drop])

      | _ ->
        (* Other patterns: evaluate RHS for effects, discard *)
        Ok (ctx1, rhs_code @ [std Wasm.Drop])
    end

  | StmtExpr e ->
    let* (ctx', code) = gen_gc_expr ctx e in
    Ok (ctx', code @ [std Wasm.Drop])

  | StmtAssign (lhs, _op, rhs) ->
    let* (ctx', rhs_code) = gen_gc_expr ctx rhs in
    begin match lhs with
      | ExprVar id ->
        let* local_idx = lookup_local ctx' id.name in
        Ok (ctx', rhs_code @ [std (Wasm.LocalSet local_idx)])
      | _ ->
        (* Complex assignment targets (array index, field) deferred *)
        Ok (ctx', rhs_code @ [std Wasm.Drop])
    end

  | StmtWhile (cond, body) ->
    let* (ctx1, cond_code) = gen_gc_expr ctx  cond in
    let* (ctx2, body_code) = gen_gc_block ctx1 body in
    (* while (cond) { body }  →  block { loop { cond; i32.eqz; br_if 1; body; br 0 } } *)
    Ok (ctx2, [GcBlock (GcBtEmpty,
      [GcLoop (GcBtEmpty,
        cond_code @
        [std Wasm.I32Eqz; std (Wasm.BrIf 1)] @
        body_code @
        [std Wasm.Drop; std (Wasm.Br 0)]
      )]
    )])

  | StmtFor (pat, iter_expr, body) ->
    (* For loop over a GC array: iter_expr produces an array ref.
       Iterates from index 0 to array.len (exclusive). *)
    let* (ctx1, iter_code) = gen_gc_expr ctx iter_expr in

    let (ctx2, arr_local)  = alloc_local ctx1 "__for_arr" GcAnyref in
    let (ctx3, len_local)  = alloc_local ctx2 "__for_len" (GcPrim I32) in
    let (ctx4, idx_local)  = alloc_local ctx3 "__for_idx" (GcPrim I32) in

    (* Use the most recently registered array type for array.len and array.get *)
    let array_type_idx = match ctx4.array_reg with
      | (_, idx) :: _ -> idx
      | [] -> 0
    in

    let init_code =
      iter_code @
      [std (Wasm.LocalSet arr_local);
       std (Wasm.LocalGet arr_local);
       RefCast (HtConcrete array_type_idx);
       ArrayLen;
       std (Wasm.LocalSet len_local);
       push_i32 0;
       std (Wasm.LocalSet idx_local)]
    in

    begin match pat with
      | PatVar item_id ->
        let (ctx5, item_local) = alloc_local ctx4 item_id.name GcAnyref in
        let* (ctx_final, body_code) = gen_gc_block ctx5 body in
        Ok (ctx_final, init_code @ [GcBlock (GcBtEmpty,
          [GcLoop (GcBtEmpty,
            (* Check: idx >= len → exit *)
            [std (Wasm.LocalGet idx_local);
             std (Wasm.LocalGet len_local);
             std Wasm.I32GeS; std (Wasm.BrIf 1);
             (* Load arr[idx] → item_local *)
             std (Wasm.LocalGet arr_local);
             RefCast (HtConcrete array_type_idx);
             std (Wasm.LocalGet idx_local);
             ArrayGet array_type_idx;
             std (Wasm.LocalSet item_local);
            ] @
            body_code @
            [std Wasm.Drop;         (* discard body result *)
             std (Wasm.LocalGet idx_local);
             push_i32 1;
             std Wasm.I32Add;
             std (Wasm.LocalSet idx_local);
             std (Wasm.Br 0)]       (* next iteration *)
          )]
        )])

      | PatWildcard _ ->
        let* (ctx_final, body_code) = gen_gc_block ctx4 body in
        Ok (ctx_final, init_code @ [GcBlock (GcBtEmpty,
          [GcLoop (GcBtEmpty,
            [std (Wasm.LocalGet idx_local);
             std (Wasm.LocalGet len_local);
             std Wasm.I32GeS; std (Wasm.BrIf 1);
            ] @
            body_code @
            [std Wasm.Drop;
             std (Wasm.LocalGet idx_local);
             push_i32 1; std Wasm.I32Add;
             std (Wasm.LocalSet idx_local);
             std (Wasm.Br 0)]
          )]
        )])

      | _ ->
        (* Other patterns: iterate without binding *)
        let* (ctx_final, body_code) = gen_gc_block ctx4 body in
        Ok (ctx_final, init_code @ [GcBlock (GcBtEmpty,
          [GcLoop (GcBtEmpty,
            [std (Wasm.LocalGet idx_local);
             std (Wasm.LocalGet len_local);
             std Wasm.I32GeS; std (Wasm.BrIf 1);
            ] @
            body_code @
            [std Wasm.Drop;
             std (Wasm.LocalGet idx_local);
             push_i32 1; std Wasm.I32Add;
             std (Wasm.LocalSet idx_local);
             std (Wasm.Br 0)]
          )]
        )])
    end

(** {1 Function codegen} *)

(** Generate a GC function from an AffineScript function declaration.

    Parameter types are inferred from type annotations; parameters without
    annotations default to [GcAnyref].  The return type is similarly
    inferred from [fd_ret_ty]. *)
let gen_gc_function (ctx : gc_ctx) (fd : fn_decl) : (gc_ctx * gc_func) cg_result =
  (* Reset function-level state, preserving all module-level state *)
  let fn_ctx = { ctx with
    locals      = [];
    local_kinds = [];
    next_local  = 0;
    param_count = 0;
    loop_depth  = 0;
    var_gc_type = [];
  } in

  (* Allocate parameters as locals 0..n-1 *)
  let (ctx_with_params, param_gc_types) =
    List.fold_left (fun (c, pts) param ->
      let vt = as_type_to_gc_valtype param.p_ty in
      let (c', _) = alloc_local c param.p_name.name vt in
      (c', pts @ [vt])
    ) (fn_ctx, []) fd.fd_params
  in
  let param_count = List.length fd.fd_params in
  let ctx_params = { ctx_with_params with param_count } in

  (* Infer return type from annotation, defaulting to GcAnyref *)
  let result_vt = match fd.fd_ret_ty with
    | Some ty -> as_type_to_gc_valtype ty
    | None    -> GcAnyref
  in

  (* Register the function's GC func type in the type section *)
  let func_type_def = GcFuncType {
    gft_params  = param_gc_types;
    gft_results = [result_vt];
  } in
  let (ctx_with_ftype, type_idx) = register_gc_type ctx_params func_type_def in

  (* Generate function body *)
  let body_expr = match fd.fd_body with
    | FnBlock blk -> ExprBlock blk
    | FnExpr e    -> e
  in
  let* (ctx_after, body_code) = gen_gc_expr ctx_with_ftype body_expr in

  (* Collect extra locals (those declared beyond the parameters) *)
  let extra_locals =
    List.filter_map (fun (idx, vt) ->
      if idx >= param_count then Some (1, vt) else None
    ) (List.rev ctx_after.local_kinds)
    (* rev: local_kinds is prepend-ordered; we want ascending index order *)
  in

  let func = {
    gf_type   = type_idx;
    gf_locals = extra_locals;
    gf_body   = body_code;
  } in

  (* Restore module-level state from ctx_after; drop function-level state *)
  let ctx_restored = { ctx_after with
    locals      = ctx.locals;
    local_kinds = ctx.local_kinds;
    next_local  = ctx.next_local;
    param_count = ctx.param_count;
    loop_depth  = ctx.loop_depth;
    var_gc_type = ctx.var_gc_type;
  } in

  Ok (ctx_restored, func)

(** {1 Declaration codegen} *)

(** Generate GC module entries from a top-level AffineScript declaration.

    - [TopFn]: generates a function, registers its type, adds export if applicable
    - [TopType (TyStruct)]: registers a named GC struct type with field map
    - [TopType (TyEnum)]: registers variant tags (as in the WASM 1.0 backend)
    - Other declarations: no GC output *)
let gen_gc_decl (ctx : gc_ctx) (decl : top_level) : gc_ctx cg_result =
  match decl with

  | TopFn fd ->
    let func_idx = ctx.import_count + List.length ctx.gc_funcs_acc in
    let ctx_with_idx = {
      ctx with func_indices = (fd.fd_name.name, func_idx) :: ctx.func_indices
    } in
    let* (ctx', func) = gen_gc_function ctx_with_idx fd in

    let export_names =
      ["main"; "init_state"; "step_state"; "get_state"; "mission_active"]
    in
    let new_exports =
      if List.mem fd.fd_name.name export_names then
        [{ ge_name = fd.fd_name.name; ge_desc = GcExportFunc func_idx }]
      else []
    in
    Ok { ctx' with
      gc_funcs_acc   = ctx'.gc_funcs_acc @ [func];
      gc_exports_acc = ctx'.gc_exports_acc @ new_exports;
    }

  | TopType td ->
    begin match td.td_body with
      | TyStruct sf_list ->
        (* Named struct: register with precise field types from annotations *)
        let field_types = List.map (fun sf -> as_type_to_field_type sf.sf_ty) sf_list in
        let (ctx', type_idx) = register_gc_type ctx (GcStructType field_types) in
        let field_map = List.mapi (fun i sf -> (sf.sf_name.name, i)) sf_list in
        Ok { ctx' with
          struct_reg = (td.td_name.name, type_idx) :: ctx'.struct_reg;
          field_reg  = (td.td_name.name, field_map) :: ctx'.field_reg;
        }

      | TyEnum variants ->
        (* Variant tags — same sequential assignment as the WASM 1.0 backend *)
        let ctx' =
          List.fold_left (fun c (idx, vd) ->
            { c with variant_tags = (vd.vd_name.name, idx) :: c.variant_tags }
          ) ctx (List.mapi (fun i v -> (i, v)) variants)
        in
        Ok ctx'

      | TyAlias _ ->
        (* Type aliases are purely type-level; no GC output *)
        Ok ctx
    end

  | TopConst _ | TopTrait _ | TopImpl _ ->
    Ok ctx

  | TopEffect ed ->
    (* Register each effect operation as an unreachable stub function.
       This gives each op a valid func_indices entry (so direct calls at
       least produce a trap rather than a link error), and correctly
       offsets function indices for all subsequent definitions.

       Full effect handler dispatch requires either:
         - The WASM exception-handling proposal (EH, standardised 2023) to
           propagate perform-site continuations across stack frames, OR
         - A whole-program CPS transform before codegen.
       Neither is implemented in this backend yet.  Use the interpreter
       (`-i`) for programs that perform effects.

       All parameters are typed as GcAnyref (conservative): concrete types
       are not yet propagated from the type-inference pass into codegen. *)
    let ctx' = List.fold_left (fun ctx (op : effect_op_decl) ->
      let n_params = List.length op.eod_params in
      let func_type = GcFuncType {
        gft_params  = List.init n_params (fun _ -> GcAnyref);
        gft_results = [GcAnyref];
      } in
      let (ctx1, type_idx) = register_gc_type ctx func_type in
      let func_idx = ctx1.import_count + List.length ctx1.gc_funcs_acc in
      let stub : gc_func = {
        gf_type   = type_idx;
        gf_locals = [];
        (* Trap immediately: calling an effect op in WasmGC without a
           handler dispatch mechanism is always a programming error. *)
        gf_body   = [Std Wasm.Unreachable];
      } in
      { ctx1 with
        func_indices = (op.eod_name.name, func_idx) :: ctx1.func_indices;
        gc_funcs_acc = ctx1.gc_funcs_acc @ [stub];
      }
    ) ctx ed.ed_ops in
    Ok ctx'

(** {1 Module-level entry point} *)

(** Generate a {!Wasm_gc.gc_module} from an AffineScript program.

    This is called by the compiler CLI when [--wasm-gc] is present on the
    [compile] subcommand.

    The generated module:
    - Declares GC struct/array types for all records, tuples, and arrays
    - Uses [struct.new] and [array.new_fixed] for allocation (no bump allocator)
    - Has no linear memory section
    - Exports the same function names as the WASM 1.0 backend
    - Is compatible with V8 ≥ Chrome 119, SpiderMonkey ≥ Firefox 120,
      and Wasmtime with [--wasm-features gc] *)
let generate_gc_module (prog : program) : (gc_module, codegen_error) Result.t =
  let ctx = create_gc_ctx () in

  let* ctx' =
    List.fold_left (fun acc decl ->
      let* c = acc in
      gen_gc_decl c decl
    ) (Ok ctx) prog.prog_decls
  in

  Ok {
    gc_types   = ctx'.gc_type_defs;
    gc_imports = [];  (* GC mode: no WASI; GC objects are not in linear memory *)
    gc_funcs   = ctx'.gc_funcs_acc;
    gc_exports = ctx'.gc_exports_acc;
  }

(** Show a codegen error for CLI output. *)
let format_codegen_error (e : codegen_error) : string =
  match e with
  | UnsupportedFeature msg -> Printf.sprintf "GC codegen: unsupported feature: %s" msg
  | UnboundVariable name   -> Printf.sprintf "GC codegen: unbound variable: %s" name
  | UnboundType name       -> Printf.sprintf "GC codegen: unbound type: %s" name
  | UnboundFunction name   -> Printf.sprintf "GC codegen: function '%s' has no func_indices entry (compiler bug — register before codegen)" name
