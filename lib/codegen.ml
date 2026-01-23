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
  func_indices : (string * int) list;  (** function name to index map *)
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
  func_indices = [];
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

(** Generate code for an expression, returning instructions and updated context *)
let rec gen_expr (ctx : context) (expr : expr) : (context * instr list) result =
  match expr with
  | ExprLit lit ->
    let* instr = gen_literal lit in
    Ok (ctx, [instr])

  | ExprVar id ->
    let* idx = lookup_local ctx id.name in
    Ok (ctx, [LocalGet idx])

  | ExprBinary (left, op, right) ->
    let* (ctx', left_code) = gen_expr ctx left in
    let* (ctx'', right_code) = gen_expr ctx' right in
    let op_instr = gen_binop op in
    Ok (ctx'', left_code @ right_code @ [op_instr])

  | ExprUnary (op, operand) ->
    let* (ctx', operand_code) = gen_expr ctx operand in
    let* op_instr = gen_unop op in
    let prefix = match op with
      | OpNeg -> [I32Const 0l]  (* 0 - operand *)
      | _ -> []
    in
    Ok (ctx', prefix @ operand_code @ [op_instr])

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
    (* For now, only handle simple variable patterns *)
    begin match lb.el_pat with
      | PatVar id ->
        let (ctx'', idx) = alloc_local ctx' id.name in
        let set_code = [LocalSet idx] in
        begin match lb.el_body with
          | Some body ->
            let* (ctx_final, body_code) = gen_expr ctx'' body in
            Ok (ctx_final, rhs_code @ set_code @ body_code)
          | None ->
            Ok (ctx'', rhs_code @ set_code @ [I32Const 0l])
        end
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | ExprLambda _ ->
    Error (UnsupportedFeature "Lambda expressions not yet supported in codegen")

  | ExprApp (func_expr, args) ->
    (* Generate code for arguments (left to right) *)
    let* (ctx_final, all_arg_code) = List.fold_left (fun acc arg ->
      let* (ctx', accumulated_code) = acc in
      let* (ctx'', arg_code) = gen_expr ctx' arg in
      Ok (ctx'', accumulated_code @ arg_code)
    ) (Ok (ctx, [])) args in

    (* Get function name and index *)
    begin match func_expr with
      | ExprVar id ->
        (* Direct function call *)
        begin match List.assoc_opt id.name ctx_final.func_indices with
          | Some func_idx ->
            let call_instr = Call func_idx in
            Ok (ctx_final, all_arg_code @ [call_instr])
          | None ->
            Error (UnboundVariable ("Function not found: " ^ id.name))
        end
      | _ ->
        (* Indirect calls (function pointers) not yet supported *)
        Error (UnsupportedFeature "Indirect function calls not yet supported")
    end

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

(** Generate code for a statement *)
and gen_stmt (ctx : context) (stmt : stmt) : (context * instr list) result =
  match stmt with
  | StmtLet sl ->
    let* (ctx', rhs_code) = gen_expr ctx sl.sl_value in
    (* For now, only handle simple variable patterns *)
    begin match sl.sl_pat with
      | PatVar id ->
        let (ctx'', idx) = alloc_local ctx' id.name in
        Ok (ctx'', rhs_code @ [LocalSet idx])
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | StmtExpr e ->
    let* (ctx', code) = gen_expr ctx e in
    Ok (ctx', code @ [Drop])  (* Discard expression result *)

  | StmtAssign _ ->
    Error (UnsupportedFeature "Assignment not yet supported in codegen")

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

  | StmtFor _ ->
    Error (UnsupportedFeature "For loops not yet supported in codegen")

(** Generate code for a function *)
let gen_function (ctx : context) (fd : fn_decl) : func result =
  (* Create fresh context for function scope *)
  let fn_ctx = { ctx with locals = []; next_local = 0; loop_depth = 0 } in

  (* Parameters become locals 0..n-1 *)
  let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
    alloc_local c param.p_name.name
  ) (fn_ctx, 0) fd.fd_params in

  let param_count = List.length fd.fd_params in

  (* Generate function body *)
  let body_expr = match fd.fd_body with
    | FnBlock blk -> ExprBlock blk
    | FnExpr e -> e
  in
  let* (ctx_final, body_code) = gen_expr ctx_with_params body_expr in

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

  Ok func

(** Generate code for a top-level declaration *)
let gen_decl (ctx : context) (decl : top_level) : context result =
  match decl with
  | TopFn fd ->
    (* Create function type *)
    let param_types = List.map (fun _ -> I32) fd.fd_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Add type to types list *)
    let type_idx = List.length ctx.types in
    let ctx_with_type = { ctx with types = ctx.types @ [func_type] } in

    (* Determine function index before generating *)
    let func_idx = List.length ctx_with_type.funcs in

    (* Register function name to index mapping *)
    let ctx_with_func_idx = { ctx_with_type with
      func_indices = ctx_with_type.func_indices @ [(fd.fd_name.name, func_idx)]
    } in

    (* Generate function with correct type index *)
    let* func = gen_function ctx_with_func_idx fd in
    let func_with_type = { func with f_type = type_idx } in

    (* Add export for main function *)
    let export = if fd.fd_name.name = "main" then
      [{ e_name = "main"; e_desc = ExportFunc func_idx }]
    else
      []
    in
    Ok { ctx_with_func_idx with
         funcs = ctx_with_func_idx.funcs @ [func_with_type];
         exports = ctx_with_func_idx.exports @ export
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
