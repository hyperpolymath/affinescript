(** WebAssembly code generation for AffineScript *)

open Ast

(** WASM value types *)
type wasm_type =
  | I32
  | I64
  | F32
  | F64
  | Funcref
  | Externref
[@@deriving show]

(** WASM instructions *)
type wasm_instr =
  (* Constants *)
  | I32Const of int
  | I64Const of int64
  | F32Const of float
  | F64Const of float
  (* Local/Global variables *)
  | LocalGet of int
  | LocalSet of int
  | LocalTee of int
  | GlobalGet of int
  | GlobalSet of int
  (* Memory *)
  | I32Load of int * int  (** align, offset *)
  | I32Store of int * int
  | I64Load of int * int
  | I64Store of int * int
  | F32Load of int * int
  | F32Store of int * int
  | F64Load of int * int
  | F64Store of int * int
  | MemorySize
  | MemoryGrow
  (* Arithmetic (i32) *)
  | I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU
  | I32And | I32Or | I32Xor | I32Shl | I32ShrS | I32ShrU
  | I32Eqz | I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU
  | I32LeS | I32LeU | I32GeS | I32GeU
  (* Arithmetic (i64) *)
  | I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU
  | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU
  | I64Eqz | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU
  | I64LeS | I64LeU | I64GeS | I64GeU
  (* Arithmetic (f32) *)
  | F32Add | F32Sub | F32Mul | F32Div
  | F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge
  | F32Neg | F32Abs | F32Sqrt | F32Ceil | F32Floor | F32Trunc
  (* Arithmetic (f64) *)
  | F64Add | F64Sub | F64Mul | F64Div
  | F64Eq | F64Ne | F64Lt | F64Gt | F64Le | F64Ge
  | F64Neg | F64Abs | F64Sqrt | F64Ceil | F64Floor | F64Trunc
  (* Conversions *)
  | I32WrapI64
  | I64ExtendI32S | I64ExtendI32U
  | F32ConvertI32S | F32ConvertI32U | F32ConvertI64S | F32ConvertI64U
  | F64ConvertI32S | F64ConvertI32U | F64ConvertI64S | F64ConvertI64U
  | I32TruncF32S | I32TruncF32U | I32TruncF64S | I32TruncF64U
  | I64TruncF32S | I64TruncF32U | I64TruncF64S | I64TruncF64U
  | F32DemoteF64 | F64PromoteF32
  | I32ReinterpretF32 | I64ReinterpretF64
  | F32ReinterpretI32 | F64ReinterpretI64
  (* Control flow *)
  | Unreachable
  | Nop
  | Block of wasm_type option * wasm_instr list
  | Loop of wasm_type option * wasm_instr list
  | If of wasm_type option * wasm_instr list * wasm_instr list option
  | Br of int           (** Branch to label *)
  | BrIf of int         (** Conditional branch *)
  | BrTable of int list * int  (** Branch table *)
  | Return
  | Call of int         (** Call function by index *)
  | CallIndirect of int (** Call function by table index *)
  (* Parametric *)
  | Drop
  | Select
[@@deriving show]

(** WASM function type *)
type wasm_func_type = {
  ft_params: wasm_type list;
  ft_results: wasm_type list;
}
[@@deriving show]

(** WASM function *)
type wasm_func = {
  fn_name: string;
  fn_type: wasm_func_type;
  fn_locals: wasm_type list;
  fn_body: wasm_instr list;
  fn_export: bool;
}
[@@deriving show]

(** WASM global *)
type wasm_global = {
  gl_name: string;
  gl_type: wasm_type;
  gl_mutable: bool;
  gl_init: wasm_instr list;
  gl_export: bool;
}
[@@deriving show]

(** WASM import *)
type wasm_import =
  | ImportFunc of string * string * wasm_func_type
  | ImportGlobal of string * string * wasm_type * bool
  | ImportMemory of string * string * int * int option
  | ImportTable of string * string * int * int option
[@@deriving show]

(** WASM export *)
type wasm_export =
  | ExportFunc of string * int
  | ExportGlobal of string * int
  | ExportMemory of string
  | ExportTable of string
[@@deriving show]

(** WASM module *)
type wasm_module = {
  mod_types: wasm_func_type list;
  mod_imports: wasm_import list;
  mod_funcs: wasm_func list;
  mod_globals: wasm_global list;
  mod_exports: wasm_export list;
  mod_memory: (int * int option) option;  (** min, max pages *)
  mod_data: (int * string) list;  (** offset, data *)
}

(** Code generation context *)
type context = {
  ctx_funcs: (string, int) Hashtbl.t;  (** function name -> index *)
  ctx_globals: (string, int) Hashtbl.t;  (** global name -> index *)
  ctx_locals: (string, int) Hashtbl.t;  (** local name -> index *)
  mutable ctx_local_count: int;
  mutable ctx_func_count: int;
  mutable ctx_global_count: int;
  mutable ctx_label_depth: int;
  ctx_types: wasm_func_type list ref;
  ctx_module: wasm_module ref;
}

let create_context () = {
  ctx_funcs = Hashtbl.create 64;
  ctx_globals = Hashtbl.create 32;
  ctx_locals = Hashtbl.create 32;
  ctx_local_count = 0;
  ctx_func_count = 0;
  ctx_global_count = 0;
  ctx_label_depth = 0;
  ctx_types = ref [];
  ctx_module = ref {
    mod_types = [];
    mod_imports = [];
    mod_funcs = [];
    mod_globals = [];
    mod_exports = [];
    mod_memory = Some (1, Some 16);  (* 1 page min, 16 pages max *)
    mod_data = [];
  };
}

(** Add a local variable, returning its index *)
let add_local ctx name ty =
  let idx = ctx.ctx_local_count in
  Hashtbl.replace ctx.ctx_locals name idx;
  ctx.ctx_local_count <- idx + 1;
  idx

(** Look up local variable index *)
let get_local ctx name =
  Hashtbl.find_opt ctx.ctx_locals name

(** Add a function, returning its index *)
let add_func ctx name =
  let idx = ctx.ctx_func_count in
  Hashtbl.replace ctx.ctx_funcs name idx;
  ctx.ctx_func_count <- idx + 1;
  idx

(** Look up function index *)
let get_func ctx name =
  Hashtbl.find_opt ctx.ctx_funcs name

(** Convert AffineScript type to WASM type *)
let rec type_to_wasm ty =
  match ty with
  | Types.TInt | Types.TNat -> I32
  | Types.TFloat -> F64
  | Types.TBool -> I32
  | Types.TChar -> I32
  | Types.TUnit -> I32  (* Unit = 0 *)
  | Types.TString -> I32  (* Pointer to string data *)
  | Types.TRef _ | Types.TMut _ | Types.TOwn _ -> I32  (* Pointers *)
  | Types.TTuple _ -> I32  (* Pointer to tuple *)
  | Types.TRecord _ -> I32  (* Pointer to record *)
  | Types.TApp ("Array", _) -> I32  (* Pointer to array *)
  | Types.TArrow _ -> I32  (* Function reference index *)
  | _ -> I32  (* Default to i32 *)

(** Compile literal to WASM *)
let compile_literal = function
  | LitInt (n, _) -> [I32Const n]
  | LitFloat (f, _) -> [F64Const f]
  | LitBool (true, _) -> [I32Const 1]
  | LitBool (false, _) -> [I32Const 0]
  | LitChar (c, _) -> [I32Const (Char.code c)]
  | LitString (_, _) -> [I32Const 0]  (* TODO: string allocation *)
  | LitUnit _ -> [I32Const 0]

(** Compile binary operator *)
let compile_binop op =
  match op with
  | OpAdd -> [I32Add]
  | OpSub -> [I32Sub]
  | OpMul -> [I32Mul]
  | OpDiv -> [I32DivS]
  | OpMod -> [I32RemS]
  | OpEq -> [I32Eq]
  | OpNe -> [I32Ne]
  | OpLt -> [I32LtS]
  | OpLe -> [I32LeS]
  | OpGt -> [I32GtS]
  | OpGe -> [I32GeS]
  | OpAnd -> [I32And]
  | OpOr -> [I32Or]
  | OpBitAnd -> [I32And]
  | OpBitOr -> [I32Or]
  | OpBitXor -> [I32Xor]
  | OpShl -> [I32Shl]
  | OpShr -> [I32ShrS]

(** Compile unary operator *)
let compile_unop op =
  match op with
  | OpNeg -> [I32Const (-1); I32Mul]
  | OpNot -> [I32Eqz]
  | OpBitNot -> [I32Const (-1); I32Xor]
  | OpRef -> []  (* Handled specially *)
  | OpDeref -> [I32Load (2, 0)]  (* Load from pointer *)

(** Compile expression to WASM instructions *)
let rec compile_expr ctx expr =
  match expr with
  | ExprSpan (e, _) -> compile_expr ctx e

  | ExprLit lit -> compile_literal lit

  | ExprVar id ->
      (match get_local ctx id.name with
       | Some idx -> [LocalGet idx]
       | None ->
           match get_func ctx id.name with
           | Some idx -> [I32Const idx]  (* Function reference *)
           | None -> [I32Const 0])  (* Unknown - return 0 *)

  | ExprLet { el_pat; el_value; el_body; _ } ->
      let value_instrs = compile_expr ctx el_value in
      let bind_instrs = compile_pattern_bind ctx el_pat in
      let body_instrs = match el_body with
        | Some body -> compile_expr ctx body
        | None -> [I32Const 0]
      in
      value_instrs @ bind_instrs @ body_instrs

  | ExprIf { ei_cond; ei_then; ei_else } ->
      let cond_instrs = compile_expr ctx ei_cond in
      let then_instrs = compile_expr ctx ei_then in
      let else_instrs = match ei_else with
        | Some e -> Some (compile_expr ctx e)
        | None -> Some [I32Const 0]
      in
      cond_instrs @ [If (Some I32, then_instrs, else_instrs)]

  | ExprMatch { em_scrutinee; em_arms } ->
      (* Simplified: compile as nested if-else chain *)
      let scrutinee_instrs = compile_expr ctx em_scrutinee in
      let idx = add_local ctx "_match" I32 in
      let set_instr = [LocalSet idx] in
      let arms_instrs = compile_match_arms ctx idx em_arms in
      scrutinee_instrs @ set_instr @ arms_instrs

  | ExprLambda _ ->
      (* Lambdas compile to function indices *)
      [I32Const 0]  (* TODO: closure support *)

  | ExprApp (func, args) ->
      let func_instrs = compile_expr ctx func in
      let args_instrs = List.concat_map (compile_expr ctx) args in
      (* For now, assume direct call via function index *)
      args_instrs @ func_instrs @ [CallIndirect 0]  (* TODO: proper call *)

  | ExprField (e, _field) ->
      (* Record field access - compute offset and load *)
      let record_instrs = compile_expr ctx e in
      record_instrs @ [I32Load (2, 0)]  (* TODO: field offset *)

  | ExprTupleIndex (e, idx) ->
      let tuple_instrs = compile_expr ctx e in
      tuple_instrs @ [I32Const (idx * 4); I32Add; I32Load (2, 0)]

  | ExprIndex (arr, idx) ->
      let arr_instrs = compile_expr ctx arr in
      let idx_instrs = compile_expr ctx idx in
      arr_instrs @ idx_instrs @ [I32Const 4; I32Mul; I32Add; I32Load (2, 0)]

  | ExprTuple exprs ->
      (* Allocate tuple on heap and store elements *)
      let size = List.length exprs * 4 in
      let alloc = [I32Const size; Call 0]  (* Call malloc *)
      in
      let stores = List.mapi (fun i e ->
        let elem_instrs = compile_expr ctx e in
        [LocalGet 0]  (* Base pointer *)
        @ [I32Const (i * 4); I32Add]
        @ elem_instrs
        @ [I32Store (2, 0)]
      ) exprs |> List.concat in
      alloc @ stores @ [LocalGet 0]

  | ExprArray exprs ->
      (* Similar to tuple but with length prefix *)
      let len = List.length exprs in
      let size = (len + 1) * 4 in
      let alloc = [I32Const size; Call 0] in
      let store_len = [
        LocalGet 0;
        I32Const len;
        I32Store (2, 0)
      ] in
      let stores = List.mapi (fun i e ->
        let elem_instrs = compile_expr ctx e in
        [LocalGet 0; I32Const ((i + 1) * 4); I32Add]
        @ elem_instrs
        @ [I32Store (2, 0)]
      ) exprs |> List.concat in
      alloc @ store_len @ stores @ [LocalGet 0]

  | ExprRecord { er_fields; _ } ->
      (* Similar to tuple *)
      let size = List.length er_fields * 4 in
      let alloc = [I32Const size; Call 0] in
      let stores = List.mapi (fun i (_, expr_opt) ->
        let elem_instrs = match expr_opt with
          | Some e -> compile_expr ctx e
          | None -> [I32Const 0]
        in
        [LocalGet 0; I32Const (i * 4); I32Add]
        @ elem_instrs
        @ [I32Store (2, 0)]
      ) er_fields |> List.concat in
      alloc @ stores @ [LocalGet 0]

  | ExprRowRestrict (e, _) ->
      compile_expr ctx e  (* Simplified *)

  | ExprBinary (e1, op, e2) ->
      let e1_instrs = compile_expr ctx e1 in
      let e2_instrs = compile_expr ctx e2 in
      let op_instrs = compile_binop op in
      e1_instrs @ e2_instrs @ op_instrs

  | ExprUnary (op, e) ->
      let e_instrs = compile_expr ctx e in
      let op_instrs = compile_unop op in
      e_instrs @ op_instrs

  | ExprBlock blk ->
      compile_block ctx blk

  | ExprReturn e_opt ->
      (match e_opt with
       | Some e -> compile_expr ctx e @ [Return]
       | None -> [I32Const 0; Return])

  | ExprVariant (_, _) ->
      [I32Const 0]  (* TODO: variant encoding *)

  | ExprTry _ | ExprHandle _ | ExprResume _ | ExprUnsafe _ ->
      [I32Const 0]  (* TODO *)

and compile_pattern_bind ctx pat =
  match pat with
  | PatWildcard _ -> [Drop]
  | PatVar id ->
      let idx = add_local ctx id.name I32 in
      [LocalSet idx]
  | PatLit _ -> [Drop]
  | PatTuple pats ->
      (* Value is pointer to tuple on stack *)
      let base_idx = add_local ctx "_tuple_base" I32 in
      [LocalSet base_idx] @
      List.concat (List.mapi (fun i p ->
        [LocalGet base_idx; I32Const (i * 4); I32Add; I32Load (2, 0)]
        @ compile_pattern_bind ctx p
      ) pats)
  | PatRecord _ | PatCon _ | PatOr _ | PatAs _ ->
      [Drop]  (* Simplified *)

and compile_match_arms ctx scrutinee_idx = function
  | [] -> [I32Const 0]
  | [arm] ->
      (* Last arm - just execute *)
      compile_expr ctx arm.ma_body
  | arm :: rest ->
      (* Check pattern, if match execute body, else try next *)
      let check_instrs = compile_pattern_check ctx scrutinee_idx arm.ma_pat in
      let body_instrs = compile_expr ctx arm.ma_body in
      let else_instrs = compile_match_arms ctx scrutinee_idx rest in
      check_instrs @ [If (Some I32, body_instrs, Some else_instrs)]

and compile_pattern_check _ctx scrutinee_idx = function
  | PatWildcard _ -> [I32Const 1]  (* Always matches *)
  | PatVar _ -> [I32Const 1]  (* Always matches *)
  | PatLit lit ->
      [LocalGet scrutinee_idx] @
      compile_literal lit @
      [I32Eq]
  | _ -> [I32Const 1]  (* Simplified *)

and compile_block ctx { blk_stmts; blk_expr } =
  let stmt_instrs = List.concat_map (compile_stmt ctx) blk_stmts in
  let expr_instrs = match blk_expr with
    | Some e -> compile_expr ctx e
    | None -> [I32Const 0]
  in
  stmt_instrs @ expr_instrs

and compile_stmt ctx = function
  | StmtLet { sl_pat; sl_value; _ } ->
      compile_expr ctx sl_value @ compile_pattern_bind ctx sl_pat

  | StmtExpr e ->
      compile_expr ctx e @ [Drop]

  | StmtAssign (target, _, value) ->
      (match target with
       | ExprVar id ->
           (match get_local ctx id.name with
            | Some idx -> compile_expr ctx value @ [LocalSet idx]
            | None -> [])
       | ExprIndex (arr, idx) ->
           compile_expr ctx arr @
           compile_expr ctx idx @
           [I32Const 4; I32Mul; I32Add] @
           compile_expr ctx value @
           [I32Store (2, 0)]
       | _ -> [])

  | StmtWhile (cond, body) ->
      ctx.ctx_label_depth <- ctx.ctx_label_depth + 1;
      let cond_instrs = compile_expr ctx cond in
      let body_instrs = compile_block ctx body in
      ctx.ctx_label_depth <- ctx.ctx_label_depth - 1;
      [Block (None, [
        Loop (None,
          cond_instrs @
          [I32Eqz; BrIf 1] @  (* Exit if condition false *)
          body_instrs @
          [Drop] @  (* Discard block result *)
          [Br 0]    (* Continue loop *)
        )
      ])]

  | StmtFor (pat, iter, body) ->
      (* Desugar to while loop *)
      let iter_instrs = compile_expr ctx iter in
      let idx_local = add_local ctx "_for_idx" I32 in
      let arr_local = add_local ctx "_for_arr" I32 in
      let len_local = add_local ctx "_for_len" I32 in
      iter_instrs @
      [LocalSet arr_local] @
      [LocalGet arr_local; I32Load (2, 0); LocalSet len_local] @  (* Get length *)
      [I32Const 0; LocalSet idx_local] @
      [Block (None, [
        Loop (None,
          (* Check idx < len *)
          [LocalGet idx_local; LocalGet len_local; I32GeS; BrIf 1] @
          (* Get element *)
          [LocalGet arr_local;
           LocalGet idx_local; I32Const 4; I32Mul;
           I32Const 4; I32Add;  (* Skip length field *)
           I32Add; I32Load (2, 0)] @
          compile_pattern_bind ctx pat @
          compile_block ctx body @
          [Drop] @
          (* Increment idx *)
          [LocalGet idx_local; I32Const 1; I32Add; LocalSet idx_local] @
          [Br 0]
        )
      ])]

(** Compile a function declaration *)
let compile_fn_decl ctx (decl: fn_decl) =
  (* Reset locals *)
  Hashtbl.clear ctx.ctx_locals;
  ctx.ctx_local_count <- 0;
  (* Add parameters as locals *)
  let param_types = List.map (fun p ->
    let _ = add_local ctx p.p_name.name I32 in
    I32  (* Simplified: all params are i32 *)
  ) decl.fd_params in
  (* Compile body *)
  let body_instrs = match decl.fd_body with
    | FnBlock blk -> compile_block ctx blk
    | FnExpr e -> compile_expr ctx e
  in
  let extra_locals = ctx.ctx_local_count - List.length decl.fd_params in
  let local_types = List.init extra_locals (fun _ -> I32) in
  {
    fn_name = decl.fd_name.name;
    fn_type = {
      ft_params = param_types;
      ft_results = [I32];  (* Simplified: all functions return i32 *)
    };
    fn_locals = local_types;
    fn_body = body_instrs;
    fn_export = decl.fd_vis = Public;
  }

(** Compile a program to WASM module *)
let compile_program (prog: program) =
  let ctx = create_context () in
  (* First pass: register all functions *)
  List.iter (fun decl ->
    match decl with
    | TopFn fd -> ignore (add_func ctx fd.fd_name.name)
    | _ -> ()
  ) prog.prog_decls;
  (* Second pass: compile functions *)
  let funcs = List.filter_map (fun decl ->
    match decl with
    | TopFn fd -> Some (compile_fn_decl ctx fd)
    | _ -> None
  ) prog.prog_decls in
  (* Build exports *)
  let exports = List.filter_map (fun fn ->
    if fn.fn_export then
      match get_func ctx fn.fn_name with
      | Some idx -> Some (ExportFunc (fn.fn_name, idx))
      | None -> None
    else None
  ) funcs in
  {
    mod_types = List.map (fun fn -> fn.fn_type) funcs;
    mod_imports = [
      (* Import memory allocator *)
      ImportFunc ("env", "malloc", { ft_params = [I32]; ft_results = [I32] });
      ImportFunc ("env", "print", { ft_params = [I32]; ft_results = [] });
    ];
    mod_funcs = funcs;
    mod_globals = [];
    mod_exports = exports @ [ExportMemory "memory"];
    mod_memory = Some (1, Some 256);
    mod_data = [];
  }

(** Emit WASM binary *)
let emit_binary _module =
  (* TODO: Implement actual WASM binary encoding *)
  (* For now, return a placeholder *)
  Bytes.empty

(** Emit WASM text format (WAT) *)
let emit_wat module_ =
  let buf = Buffer.create 4096 in
  let add s = Buffer.add_string buf s in
  let addln s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  addln "(module";

  (* Types *)
  List.iteri (fun i ft ->
    add (Printf.sprintf "  (type $t%d (func" i);
    if ft.ft_params <> [] then begin
      add " (param";
      List.iter (fun t ->
        add (match t with I32 -> " i32" | I64 -> " i64" | F32 -> " f32" | F64 -> " f64" | _ -> " i32")
      ) ft.ft_params;
      add ")"
    end;
    if ft.ft_results <> [] then begin
      add " (result";
      List.iter (fun t ->
        add (match t with I32 -> " i32" | I64 -> " i64" | F32 -> " f32" | F64 -> " f64" | _ -> " i32")
      ) ft.ft_results;
      add ")"
    end;
    addln "))"
  ) module_.mod_types;

  (* Imports *)
  List.iter (fun imp ->
    match imp with
    | ImportFunc (mod_name, func_name, ft) ->
        addln (Printf.sprintf "  (import \"%s\" \"%s\" (func $%s_%s (param i32) (result i32)))"
                 mod_name func_name mod_name func_name)
    | ImportMemory (mod_name, mem_name, min, max) ->
        let max_str = match max with Some m -> Printf.sprintf " %d" m | None -> "" in
        addln (Printf.sprintf "  (import \"%s\" \"%s\" (memory %d%s))"
                 mod_name mem_name min max_str)
    | _ -> ()
  ) module_.mod_imports;

  (* Memory *)
  (match module_.mod_memory with
   | Some (min, max) ->
       let max_str = match max with Some m -> Printf.sprintf " %d" m | None -> "" in
       addln (Printf.sprintf "  (memory (export \"memory\") %d%s)" min max_str)
   | None -> ());

  (* Functions *)
  List.iteri (fun i fn ->
    add (Printf.sprintf "  (func $%s (type $t%d)" fn.fn_name i);
    List.iter (fun _ -> add " (local i32)") fn.fn_locals;
    addln "";
    (* Emit body - simplified *)
    List.iter (fun instr ->
      add "    ";
      (match instr with
       | I32Const n -> addln (Printf.sprintf "i32.const %d" n)
       | LocalGet n -> addln (Printf.sprintf "local.get %d" n)
       | LocalSet n -> addln (Printf.sprintf "local.set %d" n)
       | I32Add -> addln "i32.add"
       | I32Sub -> addln "i32.sub"
       | I32Mul -> addln "i32.mul"
       | I32DivS -> addln "i32.div_s"
       | I32Eq -> addln "i32.eq"
       | I32Ne -> addln "i32.ne"
       | I32LtS -> addln "i32.lt_s"
       | I32GtS -> addln "i32.gt_s"
       | I32LeS -> addln "i32.le_s"
       | I32GeS -> addln "i32.ge_s"
       | Return -> addln "return"
       | Drop -> addln "drop"
       | _ -> addln "nop")
    ) fn.fn_body;
    addln "  )"
  ) module_.mod_funcs;

  (* Exports *)
  List.iter (fun exp ->
    match exp with
    | ExportFunc (name, idx) ->
        addln (Printf.sprintf "  (export \"%s\" (func %d))" name idx)
    | _ -> ()
  ) module_.mod_exports;

  addln ")";
  Buffer.contents buf
