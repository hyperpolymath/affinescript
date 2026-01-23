(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WebAssembly code generation from AffineScript AST.

    This module translates type-checked and borrow-checked AffineScript
    programs into WebAssembly modules.
*)

open Ast
open Wasm

(** Code generation context *)
type context = {
  types : func_type list;            (** type section *)
  funcs : func list;                 (** function definitions *)
  exports : export list;             (** exports *)
  imports : import list;             (** imports *)
  locals : (string * int) list;      (** local variable name to index map *)
  next_local : int;                  (** next available local index *)
  loop_depth : int;                  (** current loop nesting depth *)
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

(** Create initial context *)
let create_context () : context = {
  types = [];
  funcs = [];
  exports = [];
  imports = [];
  locals = [];
  next_local = 0;
  loop_depth = 0;
}

(** Map AffineScript type to WASM value type *)
let type_to_wasm (_ty : type_expr) : value_type result =
  (* For now, use I32 as default *)
  (* TODO: Proper type mapping based on type_expr *)
  Ok I32

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

(** Generate code for a literal *)
let gen_literal (lit : literal) : instr result =
  match lit with
  | LitUnit _ -> Ok (I32Const 0l)  (* Unit represented as 0 *)
  | LitBool (b, _) -> Ok (I32Const (if b then 1l else 0l))
  | LitInt (n, _) -> Ok (I32Const (Int32.of_int n))
  | LitFloat (f, _) -> Ok (F64Const f)
  | LitChar (c, _) -> Ok (I32Const (Int32.of_int (Char.code c)))
  | LitString _ -> Error (UnsupportedFeature "String literals not yet supported in codegen")

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

(** Generate code for unary operation *)
let gen_unop (op : unary_op) : instr result =
  match op with
  | OpNeg -> Ok I32Sub  (* 0 - x *)
  | OpNot -> Ok I32Eqz  (* x == 0 *)
  | OpBitNot -> Error (UnsupportedFeature "Bitwise NOT")
  | OpRef -> Error (UnsupportedFeature "References not yet supported in codegen")
  | OpDeref -> Error (UnsupportedFeature "Dereference not yet supported in codegen")

(** Generate code for an expression *)
let rec gen_expr (ctx : context) (expr : expr) : (instr list) result =
  match expr with
  | ExprLit lit ->
    let* instr = gen_literal lit in
    Ok [instr]

  | ExprVar id ->
    let* idx = lookup_local ctx id.name in
    Ok [LocalGet idx]

  | ExprBinary (left, op, right) ->
    let* left_code = gen_expr ctx left in
    let* right_code = gen_expr ctx right in
    let op_instr = gen_binop op in
    Ok (left_code @ right_code @ [op_instr])

  | ExprUnary (op, operand) ->
    let* operand_code = gen_expr ctx operand in
    let* op_instr = gen_unop op in
    let prefix = match op with
      | OpNeg -> [I32Const 0l]  (* 0 - operand *)
      | _ -> []
    in
    Ok (prefix @ operand_code @ [op_instr])

  | ExprIf ei ->
    let* cond_code = gen_expr ctx ei.ei_cond in
    let* then_code = gen_expr ctx ei.ei_then in
    let else_code = match ei.ei_else with
      | Some e -> gen_expr ctx e
      | None -> Ok [I32Const 0l]  (* Default to 0 if no else *)
    in
    let* else_code = else_code in
    Ok (cond_code @ [If (BtType I32, then_code, else_code)])

  | ExprBlock blk ->
    gen_block ctx blk

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e ->
        let* code = gen_expr ctx e in
        Ok (code @ [Return])
      | None ->
        Ok [Return]
    end

  | ExprLet lb ->
    let* rhs_code = gen_expr ctx lb.el_value in
    (* For now, only handle simple variable patterns *)
    begin match lb.el_pat with
      | PatVar id ->
        let (ctx', idx) = alloc_local ctx id.name in
        let set_code = [LocalSet idx] in
        begin match lb.el_body with
          | Some body ->
            let* body_code = gen_expr ctx' body in
            Ok (rhs_code @ set_code @ body_code)
          | None ->
            Ok (rhs_code @ set_code @ [I32Const 0l])
        end
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | ExprLambda _ ->
    Error (UnsupportedFeature "Lambda expressions not yet supported in codegen")

  | ExprApp _ ->
    Error (UnsupportedFeature "Function application not yet supported in codegen")

  | ExprMatch _ ->
    Error (UnsupportedFeature "Match expressions not yet supported in codegen")

  | ExprTuple _ ->
    Error (UnsupportedFeature "Tuples not yet supported in codegen")

  | ExprArray _ ->
    Error (UnsupportedFeature "Arrays not yet supported in codegen")

  | ExprRecord _ ->
    Error (UnsupportedFeature "Records not yet supported in codegen")

  | ExprField _ ->
    Error (UnsupportedFeature "Field access not yet supported in codegen")

  | ExprTupleIndex _ ->
    Error (UnsupportedFeature "Tuple indexing not yet supported in codegen")

  | ExprIndex _ ->
    Error (UnsupportedFeature "Array indexing not yet supported in codegen")

  | ExprVariant _ ->
    Error (UnsupportedFeature "Variants not yet supported in codegen")

  | ExprRowRestrict _ ->
    Error (UnsupportedFeature "Row restriction not supported in codegen")

  | ExprHandle _ ->
    Error (UnsupportedFeature "Effect handlers not yet supported in codegen")

  | ExprResume _ ->
    Error (UnsupportedFeature "Resume not yet supported in codegen")

  | ExprTry _ ->
    Error (UnsupportedFeature "Try/catch not yet supported in codegen")

  | ExprUnsafe _ ->
    Error (UnsupportedFeature "Unsafe operations not yet supported in codegen")

  | ExprSpan (e, _) ->
    gen_expr ctx e

(** Generate code for a block *)
and gen_block (ctx : context) (blk : block) : (instr list) result =
  let* stmt_codes = List.fold_left (fun acc stmt ->
    let* codes = acc in
    let* code = gen_stmt ctx stmt in
    Ok (codes @ code)
  ) (Ok []) blk.blk_stmts in
  match blk.blk_expr with
  | Some e ->
    let* expr_code = gen_expr ctx e in
    Ok (stmt_codes @ expr_code)
  | None ->
    Ok (stmt_codes @ [I32Const 0l])

(** Generate code for a statement *)
and gen_stmt (ctx : context) (stmt : stmt) : (instr list) result =
  match stmt with
  | StmtLet sl ->
    let* rhs_code = gen_expr ctx sl.sl_value in
    (* For now, only handle simple variable patterns *)
    begin match sl.sl_pat with
      | PatVar id ->
        let (_, idx) = alloc_local ctx id.name in
        Ok (rhs_code @ [LocalSet idx])
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | StmtExpr e ->
    let* code = gen_expr ctx e in
    Ok (code @ [Drop])  (* Discard expression result *)

  | StmtAssign _ ->
    Error (UnsupportedFeature "Assignment not yet supported in codegen")

  | StmtWhile (cond, body) ->
    let* cond_code = gen_expr ctx cond in
    let* body_code = gen_block ctx body in
    (* Loop with conditional exit *)
    Ok [Block (BtEmpty, [
      Loop (BtEmpty,
        cond_code @ [I32Eqz; BrIf 1] @  (* If condition false, exit *)
        body_code @ [Br 0]  (* Continue loop *)
      )
    ])]

  | StmtFor _ ->
    Error (UnsupportedFeature "For loops not yet supported in codegen")

(** Generate code for a function *)
let gen_function (ctx : context) (fd : fn_decl) : func result =
  (* Create context with parameters as locals *)
  let (ctx', _) = List.fold_left (fun (c, _) param ->
    alloc_local c param.p_name.name
  ) (ctx, 0) fd.fd_params in

  (* Generate function body *)
  let body_expr = match fd.fd_body with
    | FnBlock blk -> ExprBlock blk
    | FnExpr e -> e
  in
  let* body_code = gen_expr ctx' body_expr in

  (* Create function type *)
  let param_types = List.map (fun _ -> I32) fd.fd_params in
  let result_type = [I32] in  (* TODO: Get from function signature *)
  let _func_type = { ft_params = param_types; ft_results = result_type } in

  (* Add type to context (simplified - assumes no duplicate types) *)
  let type_idx = List.length ctx.types in
  (* TODO: Actually add func_type to ctx.types *)

  (* Create function *)
  let func = {
    f_type = type_idx;
    f_locals = [];  (* Locals computed from ctx'.next_local *)
    f_body = body_code;
  } in

  Ok func

(** Generate code for a top-level declaration *)
let gen_decl (ctx : context) (decl : top_level) : context result =
  match decl with
  | TopFn fd ->
    let* func = gen_function ctx fd in
    let func_idx = List.length ctx.funcs in
    (* Add export for main function *)
    let export = if fd.fd_name.name = "main" then
      [{ e_name = "main"; e_desc = ExportFunc func_idx }]
    else
      []
    in
    Ok { ctx with
         funcs = ctx.funcs @ [func];
         exports = ctx.exports @ export
       }

  | TopConst _ ->
    Error (UnsupportedFeature "Constants not yet supported in codegen")

  | TopType _ | TopEffect _ | TopTrait _ | TopImpl _ ->
    (* Type declarations don't generate code *)
    Ok ctx

(** Generate WASM module from AffineScript program *)
let generate_module (prog : program) : wasm_module result =
  let ctx = create_context () in
  let* ctx' = List.fold_left (fun acc decl ->
    let* c = acc in
    gen_decl c decl
  ) (Ok ctx) prog.prog_decls in

  Ok {
    types = ctx'.types;
    funcs = ctx'.funcs;
    tables = [];
    mems = [{ mem_type = { lim_min = 1; lim_max = None } }];  (* 1 page default *)
    globals = [];
    exports = ctx'.exports;
    imports = ctx'.imports;
    start = None;
  }
