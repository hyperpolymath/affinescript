(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Tree-walking interpreter for AffineScript.

    This module implements a big-step operational semantics
    interpreter with affine type checking at runtime.
*)

open Ast
open Value

(** Result bind operator *)
let ( let* ) = Result.bind

(** Evaluate a literal *)
let eval_literal (lit : literal) : value =
  match lit with
  | LitUnit _ -> VUnit
  | LitBool (b, _) -> VBool b
  | LitInt (n, _) -> VInt n
  | LitFloat (f, _) -> VFloat f
  | LitChar (c, _) -> VChar c
  | LitString (s, _) -> VString s

(** Match a pattern against a value, returning bindings *)
let rec match_pattern (pat : pattern) (v : value) : (string * value) list result =
  match pat with
  | PatWildcard _ -> Ok []
  | PatVar id -> Ok [(id.name, v)]
  | PatLit lit ->
    let expected = eval_literal lit in
    if value_eq v expected then Ok []
    else Error PatternMatchFailure

  | PatTuple pats ->
    begin match v with
      | VTuple vs when List.length pats = List.length vs ->
        List.fold_left2 (fun acc pat v ->
          let* bindings = acc in
          let* new_bindings = match_pattern pat v in
          Ok (new_bindings @ bindings)
        ) (Ok []) pats vs
      | _ -> Error PatternMatchFailure
    end

  | PatRecord (fields, _has_rest) ->
    begin match v with
      | VRecord record_fields ->
        List.fold_left (fun acc (field_id, pat_opt) ->
          let* bindings = acc in
          match get_field field_id.name record_fields with
          | Ok field_val ->
            begin match pat_opt with
              | Some p ->
                let* new_bindings = match_pattern p field_val in
                Ok (new_bindings @ bindings)
              | None ->
                Ok ((field_id.name, field_val) :: bindings)
            end
          | Error _ -> Error PatternMatchFailure
        ) (Ok []) fields
      | _ -> Error PatternMatchFailure
    end

  | PatCon (con, pats) ->
    begin match v with
      | VVariant (tag, val_opt) when tag = con.name ->
        begin match (pats, val_opt) with
          | ([], None) -> Ok []
          | ([pat], Some val_) -> match_pattern pat val_
          | (pats, Some (VTuple vs)) when List.length pats = List.length vs ->
            List.fold_left2 (fun acc pat v ->
              let* bindings = acc in
              let* new_bindings = match_pattern pat v in
              Ok (new_bindings @ bindings)
            ) (Ok []) pats vs
          | _ -> Error PatternMatchFailure
        end
      | _ -> Error PatternMatchFailure
    end

  | PatOr (p1, p2) ->
    begin match match_pattern p1 v with
      | Ok bindings -> Ok bindings
      | Error _ -> match_pattern p2 v
    end

  | PatAs (id, pat) ->
    let* bindings = match_pattern pat v in
    Ok ((id.name, v) :: bindings)

(** Evaluate an expression *)
let rec eval (env : env) (expr : expr) : value result =
  match expr with
  | ExprLit lit -> Ok (eval_literal lit)

  | ExprVar id -> lookup_env id.name env

  | ExprLet lb ->
    let* rhs_val = eval env lb.el_value in
    let* bindings = match_pattern lb.el_pat rhs_val in
    let env' = extend_env_list bindings env in
    begin match lb.el_body with
      | Some body -> eval env' body
      | None -> Ok VUnit
    end

  | ExprIf ei ->
    let* cond_val = eval env ei.ei_cond in
    if is_truthy cond_val then
      eval env ei.ei_then
    else begin
      match ei.ei_else with
      | Some else_expr -> eval env else_expr
      | None -> Ok VUnit
    end

  | ExprMatch em ->
    let* scrut_val = eval env em.em_scrutinee in
    eval_match_arms env scrut_val em.em_arms

  | ExprLambda lam ->
    Ok (VClosure {
      cl_params = lam.elam_params;
      cl_body = lam.elam_body;
      cl_env = env;
    })

  | ExprApp (func, args) ->
    let* func_val = eval env func in
    let* arg_vals = eval_list env args in
    apply_function func_val arg_vals

  | ExprBinary (left, op, right) ->
    let* left_val = eval env left in
    let* right_val = eval env right in
    eval_binop op left_val right_val

  | ExprUnary (op, operand) ->
    let* operand_val = eval env operand in
    unary_op op operand_val

  | ExprTuple exprs ->
    let* vals = eval_list env exprs in
    Ok (VTuple vals)

  | ExprArray exprs ->
    let* vals = eval_list env exprs in
    Ok (VArray (Array.of_list vals))

  | ExprRecord er ->
    let* field_vals = List.fold_right (fun (id, expr_opt) acc ->
      let* fields = acc in
      match expr_opt with
      | Some e ->
        let* v = eval env e in
        Ok ((id.name, v) :: fields)
      | None ->
        (* Punning: {x} is short for {x: x} *)
        let* v = lookup_env id.name env in
        Ok ((id.name, v) :: fields)
    ) er.er_fields (Ok []) in
    Ok (VRecord field_vals)

  | ExprField (base, field) ->
    let* base_val = eval env base in
    begin match base_val with
      | VRecord fields -> get_field field.name fields
      | _ -> Error (TypeMismatch "Expected record")
    end

  | ExprTupleIndex (base, idx) ->
    let* base_val = eval env base in
    begin match base_val with
      | VTuple elems -> get_tuple_elem idx elems
      | _ -> Error (TypeMismatch "Expected tuple")
    end

  | ExprIndex (arr, idx_expr) ->
    let* arr_val = eval env arr in
    let* idx_val = eval env idx_expr in
    begin match (arr_val, idx_val) with
      | (VArray arr, VInt idx) -> get_array_elem idx arr
      | (VArray _, _) -> Error (TypeMismatch "Array index must be integer")
      | _ -> Error (TypeMismatch "Expected array")
    end

  | ExprBlock blk ->
    eval_block env blk

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e -> eval env e
      | None -> Ok VUnit
    end

  | ExprVariant (type_id, variant_id) ->
    (* Type::Variant syntax - just the variant constructor without payload *)
    let _ = type_id in  (* Ignore type part for now *)
    Ok (VVariant (variant_id.name, None))

  | ExprRowRestrict _ ->
    Error (RuntimeError "Row restriction not supported at runtime")

  | ExprHandle _ ->
    Error (RuntimeError "Effect handlers not yet implemented")

  | ExprResume _ ->
    Error (RuntimeError "Resume not yet implemented")

  | ExprTry _ ->
    Error (RuntimeError "Try/catch not yet implemented")

  | ExprUnsafe ops ->
    (* Evaluate unsafe operations - for now, just evaluate contained expressions *)
    begin match ops with
      | [] -> Ok VUnit
      | [UnsafeRead e] -> eval env e
      | [UnsafeWrite (ptr, value)] ->
        let* _ptr_val = eval env ptr in
        let* _val = eval env value in
        Ok VUnit
      | [UnsafeOffset (base, offset)] ->
        let* _base = eval env base in
        let* _offset = eval env offset in
        Ok VUnit
      | [UnsafeTransmute (_, _, e)] -> eval env e
      | [UnsafeForget e] ->
        let* _ = eval env e in
        Ok VUnit
      | [UnsafeAssume _] -> Ok VUnit
      | _ -> Error (RuntimeError "Multiple unsafe operations not yet supported")
    end

  | ExprSpan (e, _) ->
    eval env e

(** Evaluate a list of expressions *)
and eval_list (env : env) (exprs : expr list) : value list result =
  List.fold_right (fun expr acc ->
    let* vals = acc in
    let* v = eval env expr in
    Ok (v :: vals)
  ) exprs (Ok [])

(** Evaluate match arms *)
and eval_match_arms (env : env) (scrut_val : value) (arms : match_arm list) : value result =
  match arms with
  | [] -> Error PatternMatchFailure
  | arm :: rest ->
    begin match match_pattern arm.ma_pat scrut_val with
      | Ok bindings ->
        let env' = extend_env_list bindings env in
        (* Check guard if present *)
        begin match arm.ma_guard with
          | Some guard ->
            let* guard_val = eval env' guard in
            if is_truthy guard_val then
              eval env' arm.ma_body
            else
              eval_match_arms env scrut_val rest
          | None ->
            eval env' arm.ma_body
        end
      | Error _ ->
        eval_match_arms env scrut_val rest
    end

(** Evaluate a block *)
and eval_block (env : env) (blk : block) : value result =
  let* env' = eval_stmts env blk.blk_stmts in
  match blk.blk_expr with
  | Some e -> eval env' e
  | None -> Ok VUnit

(** Evaluate statements, returning updated environment *)
and eval_stmts (env : env) (stmts : stmt list) : env result =
  List.fold_left (fun acc stmt ->
    let* env = acc in
    eval_stmt env stmt
  ) (Ok env) stmts

(** Evaluate a statement *)
and eval_stmt (env : env) (stmt : stmt) : env result =
  match stmt with
  | StmtLet sl ->
    let* rhs_val = eval env sl.sl_value in
    let* bindings = match_pattern sl.sl_pat rhs_val in
    Ok (extend_env_list bindings env)

  | StmtExpr e ->
    let* _ = eval env e in
    Ok env

  | StmtAssign (lhs, _op, rhs) ->
    let* lhs_val = eval env lhs in
    let* rhs_val = eval env rhs in
    let* () = assign lhs_val rhs_val in
    Ok env

  | StmtWhile (cond, body) ->
    eval_while env cond body

  | StmtFor (pat, iter, body) ->
    let* iter_val = eval env iter in
    eval_for env pat iter_val body

(** Evaluate while loop *)
and eval_while (env : env) (cond : expr) (body : block) : env result =
  let* cond_val = eval env cond in
  if is_truthy cond_val then
    let* _ = eval_block env body in
    eval_while env cond body
  else
    Ok env

(** Evaluate for loop *)
and eval_for (env : env) (pat : pattern) (iter : value) (body : block) : env result =
  match iter with
  | VArray arr ->
    Array.fold_left (fun acc elem ->
      let* env = acc in
      let* bindings = match_pattern pat elem in
      let env' = extend_env_list bindings env in
      let* _ = eval_block env' body in
      Ok env
    ) (Ok env) arr
  | _ -> Error (TypeMismatch "Expected iterable")

(** Evaluate binary operation *)
and eval_binop (op : binary_op) (left : value) (right : value) : value result =
  match (left, right) with
  | (VInt a, VInt b) -> binop_int op a b
  | (VFloat a, VFloat b) -> binop_float op a b
  | (VString a, VString b) -> binop_string op a b
  | (VBool a, VBool b) -> binop_bool op a b
  | _ -> Error (TypeMismatch "Type mismatch in binary operation")

(** Apply function to arguments *)
and apply_function (func : value) (args : value list) : value result =
  match func with
  | VClosure cl ->
    if List.length args <> List.length cl.cl_params then
      Error (TypeMismatch "Argument count mismatch")
    else
      let bindings = List.map2 (fun param arg ->
        (param.p_name.name, arg)
      ) cl.cl_params args in
      let env' = extend_env_list bindings cl.cl_env in
      eval env' cl.cl_body

  | VBuiltin (_, f) -> f args

  | _ -> Error (TypeMismatch "Expected function")

(** Create initial environment with builtins *)
let create_initial_env () : env =
  let builtins = [
    ("print", VBuiltin ("print", fun args ->
      List.iter (fun v -> print_string (Value.show_value v)) args;
      Ok VUnit
    ));
    ("println", VBuiltin ("println", fun args ->
      List.iter (fun v -> print_endline (Value.show_value v)) args;
      Ok VUnit
    ));
    ("len", VBuiltin ("len", fun args ->
      match args with
      | [VArray arr] -> Ok (VInt (Array.length arr))
      | [VString s] -> Ok (VInt (String.length s))
      | _ -> Error (TypeMismatch "len expects array or string")
    ));
  ] in
  builtins

(** Evaluate a top-level declaration *)
let eval_decl (env : env) (decl : top_level) : env result =
  match decl with
  | TopFn fd ->
    let closure = VClosure {
      cl_params = fd.fd_params;
      cl_body = (match fd.fd_body with
        | FnBlock blk -> ExprBlock blk
        | FnExpr e -> e);
      cl_env = env;
    } in
    Ok (extend_env fd.fd_name.name closure env)

  | TopConst tc ->
    let* v = eval env tc.tc_value in
    Ok (extend_env tc.tc_name.name v env)

  | TopType _ | TopEffect _ | TopTrait _ | TopImpl _ ->
    (* Type declarations don't affect runtime *)
    Ok env

(** Evaluate a program *)
let eval_program (prog : program) : env result =
  let initial_env = create_initial_env () in
  List.fold_left (fun acc decl ->
    let* env = acc in
    eval_decl env decl
  ) (Ok initial_env) prog.prog_decls
