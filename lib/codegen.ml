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
  lambda_funcs : func list;          (** lifted lambda functions *)
  next_lambda_id : int;              (** next lambda function ID *)
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
  lambda_funcs = [];
  next_lambda_id = 0;
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

  | ExprLambda lam ->
    (* Lift lambda to a top-level function *)
    let lambda_id = ctx.next_lambda_id in

    (* Create fresh context for lambda function *)
    let lambda_ctx = { ctx with locals = []; next_local = 0; loop_depth = 0 } in

    (* Parameters become locals 0..n-1 *)
    let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
      alloc_local c param.p_name.name
    ) (lambda_ctx, 0) lam.elam_params in

    let param_count = List.length lam.elam_params in

    (* Generate lambda body *)
    let* (ctx_final, body_code) = gen_expr ctx_with_params lam.elam_body in

    (* Compute additional locals (beyond parameters) *)
    let local_count = ctx_final.next_local - param_count in
    let locals = if local_count > 0 then
      [{ l_count = local_count; l_type = I32 }]
    else
      []
    in

    (* Create function type for lambda *)
    let param_types = List.map (fun _ -> I32) lam.elam_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Add type to types list *)
    let type_idx = List.length ctx.types in
    let ctx_with_type = { ctx with types = ctx.types @ [func_type] } in

    (* Create lambda function *)
    let lambda_func = {
      f_type = type_idx;
      f_locals = locals;
      f_body = body_code;
    } in

    (* Add lambda function to lifted functions *)
    let ctx_with_lambda = {
      ctx_with_type with
      lambda_funcs = ctx_with_type.lambda_funcs @ [lambda_func];
      next_lambda_id = lambda_id + 1;
    } in

    (* The lambda evaluates to its function index in the table *)
    (* Function table index = base_func_count + lambda_id *)
    (* For now, just return the lambda_id - will be adjusted when creating table *)
    Ok (ctx_with_lambda, [I32Const (Int32.of_int lambda_id)])

  | ExprApp (func_expr, args) ->
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
            (* Check if it's a local variable (could be a lambda) *)
            begin match lookup_local ctx_after_args id.name with
              | Ok local_idx ->
                (* Indirect call through function table *)
                (* Create type signature for the call based on number of args *)
                let param_types = List.map (fun _ -> I32) args in
                let result_type = [I32] in
                let call_type = { ft_params = param_types; ft_results = result_type } in

                (* Find or add this type to the types list *)
                let type_idx =
                  match List.find_index (fun t -> t = call_type) ctx_after_args.types with
                  | Some idx -> idx
                  | None ->
                    (* Type not found - this shouldn't happen if lambda was created properly *)
                    (* but let's add it to be safe *)
                    List.length ctx_after_args.types
                in

                let call_instrs = [
                  LocalGet local_idx;  (* Get function table index from variable *)
                  CallIndirect type_idx  (* Indirect call with matching type *)
                ] in
                Ok (ctx_after_args, all_arg_code @ call_instrs)
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
          match List.find_index (fun t -> t = call_type) ctx_with_lambda.types with
          | Some idx -> idx
          | None -> List.length ctx_with_lambda.types
        in

        Ok (ctx_with_lambda, all_arg_code @ lambda_code @ [CallIndirect type_idx])
      | _ ->
        (* Other expressions that evaluate to functions - treat as indirect call *)
        let* (ctx_final, func_code) = gen_expr ctx_after_args func_expr in

        (* Create type signature for the call *)
        let param_types = List.map (fun _ -> I32) args in
        let result_type = [I32] in
        let call_type = { ft_params = param_types; ft_results = result_type } in

        (* Find matching type index *)
        let type_idx =
          match List.find_index (fun t -> t = call_type) ctx_final.types with
          | Some idx -> idx
          | None -> List.length ctx_final.types
        in

        Ok (ctx_final, all_arg_code @ func_code @ [CallIndirect type_idx])
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
    let* lit_instr = gen_literal lit in
    let test_code = [
      LocalGet scrutinee_local;  (* Get scrutinee value *)
      lit_instr;                 (* Get literal value *)
      I32Eq                      (* Compare *)
    ] in
    Ok (ctx, test_code, [])

  | PatCon (_con, sub_patterns) ->
    (* Constructor pattern - for now, just match wildcards in sub-patterns *)
    (* Full implementation would need to decode the constructor tag and fields *)
    if List.length sub_patterns = 0 then
      (* Simple constructor with no arguments - just match *)
      Ok (ctx, [I32Const 1l], [])
    else
      (* Constructor with arguments - not yet supported *)
      Error (UnsupportedFeature "Constructor patterns with arguments not yet supported in codegen")

  | PatTuple _ ->
    Error (UnsupportedFeature "Tuple patterns not yet supported in codegen")

  | PatRecord _ ->
    Error (UnsupportedFeature "Record patterns not yet supported in codegen")

  | PatOr _ ->
    Error (UnsupportedFeature "Or patterns not yet supported in codegen")

  | PatAs _ ->
    Error (UnsupportedFeature "As patterns not yet supported in codegen")

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
let gen_function (ctx : context) (fd : fn_decl) : (context * func) result =
  (* Create fresh context for function scope, but preserve lambda_funcs and next_lambda_id *)
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

  (* Return updated context with any lambda functions that were created *)
  Ok (ctx_final, func)

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
    let* (ctx_after_gen, func) = gen_function ctx_with_func_idx fd in
    let func_with_type = { func with f_type = type_idx } in

    (* Add export for main function *)
    let export = if fd.fd_name.name = "main" then
      [{ e_name = "main"; e_desc = ExportFunc func_idx }]
    else
      []
    in
    (* Use ctx_after_gen to preserve any lambda_funcs created during generation *)
    Ok { ctx_after_gen with
         funcs = ctx_after_gen.funcs @ [func_with_type];
         exports = ctx_after_gen.exports @ export
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

  (* Merge regular functions and lambda functions *)
  let num_regular_funcs = List.length ctx'.funcs in
  let all_funcs = ctx'.funcs @ ctx'.lambda_funcs in

  (* Create function table if there are lambdas *)
  let (tables, elems) = if List.length ctx'.lambda_funcs > 0 then
    (* Table size = number of lambda functions *)
    let table_size = List.length ctx'.lambda_funcs in
    let table = [{ tab_type = { lim_min = table_size; lim_max = Some table_size } }] in

    (* Create element segment to initialize table with lambda function indices *)
    (* Lambda functions start at index num_regular_funcs *)
    let lambda_func_indices = List.mapi (fun i _ -> num_regular_funcs + i) ctx'.lambda_funcs in
    let elem_seg = [{
      e_table = 0;
      e_offset = 0;
      e_funcs = lambda_func_indices;
    }] in
    (table, elem_seg)
  else
    ([], [])
  in

  Ok {
    types = ctx'.types;
    funcs = all_funcs;
    tables = tables;
    mems = [{ mem_type = { lim_min = 1; lim_max = None } }];  (* 1 page default *)
    globals = [];
    exports = ctx'.exports;
    imports = ctx'.imports;
    elems = elems;
    start = None;
  }
