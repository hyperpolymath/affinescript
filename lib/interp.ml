(* SPDX-License-Identifier: PMPL-1.0-or-later *)
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
    (* Start with spread base if present *)
    let* base_fields = match er.er_spread with
      | Some spread_expr ->
        let* spread_val = eval env spread_expr in
        begin match spread_val with
          | VRecord fields -> Ok fields
          | _ -> Error (TypeMismatch "Spread operator requires a record")
        end
      | None -> Ok []
    in
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
    (* Merge: explicit fields override spread fields *)
    let explicit_names = List.map fst field_vals in
    let remaining_base = List.filter (fun (n, _) ->
      not (List.mem n explicit_names)
    ) base_fields in
    Ok (VRecord (field_vals @ remaining_base))

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

  | ExprRowRestrict (base, field) ->
    let* base_val = eval env base in
    begin match base_val with
      | VRecord fields ->
        let filtered = List.filter (fun (n, _) -> n <> field.name) fields in
        Ok (VRecord filtered)
      | _ -> Error (TypeMismatch "Row restriction requires a record")
    end

  | ExprHandle eh ->
    (* Evaluate the body expression. If it performs an effect via
       PerformEffect, we match against the handler arms.
       HandlerReturn arms match the normal return value.
       HandlerOp arms match performed effects. *)
    begin match eval env eh.eh_body with
      | Ok v ->
        (* Normal return — look for a HandlerReturn arm *)
        let return_arm = List.find_opt (fun arm ->
          match arm with HandlerReturn _ -> true | _ -> false
        ) eh.eh_handlers in
        begin match return_arm with
          | Some (HandlerReturn (pat, body)) ->
            let* bindings = match_pattern pat v in
            let env' = extend_env_list bindings env in
            eval env' body
          | _ -> Ok v  (* No return handler — pass through *)
        end
      | Error (PerformEffect (op_name, args)) ->
        (* Effect performed — find matching HandlerOp arm *)
        let op_arm = List.find_opt (fun arm ->
          match arm with
          | HandlerOp (id, _, _) -> id.name = op_name
          | _ -> false
        ) eh.eh_handlers in
        begin match op_arm with
          | Some (HandlerOp (_, pats, body)) ->
            (* Build the resume continuation.  In this tree-walking interpreter
               the continuation is shallow: calling resume(v) returns v as the
               result of the entire `handle` expression.  This is correct for
               the common single-shot, tail-resume pattern.  Full multi-shot
               continuations require either OCaml 5 effects or a CPS transform. *)
            let resume_fn = VBuiltin ("__resume__", fun resume_args ->
              match resume_args with
              | [v] -> Ok v
              | []  -> Ok VUnit
              | vs  -> Ok (VTuple vs)
            ) in
            (* Bind effect argument values to handler patterns.
               Convention: all declared params first, then the continuation as
               the last pattern.  Pass args flat (not wrapped in a tuple) so
               that multi-arg effects bind correctly to separate patterns. *)
            let all_vals = args @ [resume_fn] in
            let n_pats = List.length pats in
            let n_vals = List.length all_vals in
            (* If the handler omits the continuation param, provide it anyway
               so that ExprResume still works via the __resume__ env slot. *)
            let trimmed_vals = List.filteri (fun i _ -> i < n_pats) all_vals in
            let pad_vals =
              if n_vals < n_pats then
                trimmed_vals @ List.init (n_pats - n_vals) (fun _ -> VUnit)
              else trimmed_vals
            in
            let bindings = List.fold_left2 (fun acc pat v ->
              match acc with
              | Ok bs ->
                begin match match_pattern pat v with
                  | Ok new_bs -> Ok (new_bs @ bs)
                  | Error e -> Error e
                end
              | Error e -> Error e
            ) (Ok []) pats pad_vals in
            let* bindings = bindings in
            (* Also bind the resume fn under "__resume__" so that the
               `ExprResume` keyword form can find it regardless of what
               name the programmer chose for the continuation parameter. *)
            let env' =
              extend_env "__resume__" resume_fn
                (extend_env_list bindings env)
            in
            eval env' body
          | _ -> Error (RuntimeError ("Unhandled effect: " ^ op_name))
        end
      | Error e -> Error e
    end

  | ExprResume arg_opt ->
    (* Evaluate the argument, then call the resume continuation bound in the
       environment by the enclosing ExprHandle dispatcher.  The continuation
       is stored under "__resume__" so that the `resume expr` keyword form
       works without the programmer having to name the continuation parameter.
       If called outside a handler (no "__resume__" in env), return the value
       as-is — this matches the surface-syntax intuition "resume x ≈ x". *)
    let* arg_val = match arg_opt with
      | Some e -> eval env e
      | None   -> Ok VUnit
    in
    begin match lookup_env "__resume__" env with
      | Ok resume_fn -> apply_function resume_fn [arg_val]
      | Error _      -> Ok arg_val
    end

  | ExprTry et ->
    (* Evaluate the body block. If it returns an error, match against
       catch arms. Always run finally block if present. *)
    let body_result = eval_block env et.et_body in
    let catch_result = match body_result with
      | Ok v -> Ok v
      | Error (RuntimeError msg) ->
        begin match et.et_catch with
          | Some arms ->
            (* Wrap the error as a variant for pattern matching *)
            let err_val = VVariant ("RuntimeError", Some (VString msg)) in
            eval_match_arms env err_val arms
          | None -> Error (RuntimeError msg)
        end
      | Error (PatternMatchFailure) ->
        begin match et.et_catch with
          | Some arms ->
            let err_val = VVariant ("PatternMatchFailure", None) in
            eval_match_arms env err_val arms
          | None -> Error PatternMatchFailure
        end
      | Error e -> Error e
    in
    (* Run finally block if present (result is discarded) *)
    begin match et.et_finally with
      | Some finally_blk ->
        let _ = eval_block env finally_blk in
        catch_result
      | None -> catch_result
    end

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
      | _ -> Error (RuntimeError "Multiple unsafe operations not yet supported")
    end

  | ExprSpan (e, _) ->
    eval env e

(** Evaluate a list of expressions strictly left-to-right.

    Per ADR-003, all n-ary expression forms (application arguments, tuple
    components, array elements, record fields) evaluate their subexpressions
    in source order. The previous implementation used [List.fold_right] with
    monadic bind, which under OCaml's strict evaluation visited elements
    right-to-left — inconsistent with [ExprBinary] and a latent divergence
    point for future affine enforcement and effect handlers. *)
and eval_list (env : env) (exprs : expr list) : value list result =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | expr :: rest ->
      let* v = eval env expr in
      loop (v :: acc) rest
  in
  loop [] exprs

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
    (* -- Console I/O -------------------------------------------------------- *)
    ("print", VBuiltin ("print", fun args ->
      List.iter (fun v -> print_string (Value.show_value v)) args;
      Ok VUnit
    ));
    ("println", VBuiltin ("println", fun args ->
      List.iter (fun v -> print_endline (Value.show_value v)) args;
      Ok VUnit
    ));
    ("eprint", VBuiltin ("eprint", fun args ->
      List.iter (fun v -> prerr_string (Value.show_value v)) args;
      Ok VUnit
    ));
    ("eprintln", VBuiltin ("eprintln", fun args ->
      List.iter (fun v -> prerr_endline (Value.show_value v)) args;
      Ok VUnit
    ));

    (* -- Collection / string length ---------------------------------------- *)
    ("len", VBuiltin ("len", fun args ->
      match args with
      | [VArray arr] -> Ok (VInt (Array.length arr))
      | [VString s] -> Ok (VInt (String.length s))
      | _ -> Error (TypeMismatch "len expects array or string")
    ));

    (* -- String builtins --------------------------------------------------- *)
    ("string_get", VBuiltin ("string_get", fun args ->
      match args with
      | [VString s; VInt idx] ->
        if idx >= 0 && idx < String.length s then
          Ok (VChar (String.get s idx))
        else
          Error (IndexOutOfBounds (idx, String.length s))
      | _ -> Error (TypeMismatch "string_get expects (String, Int)")
    ));
    ("string_sub", VBuiltin ("string_sub", fun args ->
      match args with
      | [VString s; VInt start; VInt length] ->
        let slen = String.length s in
        let start' = max 0 (min start slen) in
        let length' = max 0 (min length (slen - start')) in
        Ok (VString (String.sub s start' length'))
      | _ -> Error (TypeMismatch "string_sub expects (String, Int, Int)")
    ));
    ("string_find", VBuiltin ("string_find", fun args ->
      match args with
      | [VString haystack; VString needle] ->
        (match String.index_opt haystack (String.get needle 0) with
         | None -> Ok (VInt (-1))
         | Some _ ->
           let hlen = String.length haystack in
           let nlen = String.length needle in
           if nlen = 0 then Ok (VInt 0)
           else if nlen > hlen then Ok (VInt (-1))
           else
             let found = ref (-1) in
             for i = 0 to hlen - nlen do
               if !found = -1 && String.sub haystack i nlen = needle then
                 found := i
             done;
             Ok (VInt !found))
      | _ -> Error (TypeMismatch "string_find expects (String, String)")
    ));
    ("char_to_int", VBuiltin ("char_to_int", fun args ->
      match args with
      | [VChar c] -> Ok (VInt (Char.code c))
      | _ -> Error (TypeMismatch "char_to_int expects Char")
    ));
    ("int_to_char", VBuiltin ("int_to_char", fun args ->
      match args with
      | [VInt n] ->
        if n >= 0 && n <= 127 then Ok (VChar (Char.chr n))
        else Error (RuntimeError "int_to_char: code point out of ASCII range")
      | _ -> Error (TypeMismatch "int_to_char expects Int")
    ));
    ("show", VBuiltin ("show", fun args ->
      match args with
      | [v] -> Ok (VString (Value.show_value v))
      | _ -> Error (TypeMismatch "show expects a single argument")
    ));
    ("to_lowercase", VBuiltin ("to_lowercase", fun args ->
      match args with
      | [VString s] -> Ok (VString (String.lowercase_ascii s))
      | _ -> Error (TypeMismatch "to_lowercase expects String")
    ));
    ("to_uppercase", VBuiltin ("to_uppercase", fun args ->
      match args with
      | [VString s] -> Ok (VString (String.uppercase_ascii s))
      | _ -> Error (TypeMismatch "to_uppercase expects String")
    ));
    ("trim", VBuiltin ("trim", fun args ->
      match args with
      | [VString s] -> Ok (VString (String.trim s))
      | _ -> Error (TypeMismatch "trim expects String")
    ));
    ("int_to_string", VBuiltin ("int_to_string", fun args ->
      match args with
      | [VInt n] -> Ok (VString (string_of_int n))
      | _ -> Error (TypeMismatch "int_to_string expects Int")
    ));
    ("float_to_string", VBuiltin ("float_to_string", fun args ->
      match args with
      | [VFloat f] -> Ok (VString (string_of_float f))
      | _ -> Error (TypeMismatch "float_to_string expects Float")
    ));
    ("parse_int", VBuiltin ("parse_int", fun args ->
      match args with
      | [VString s] ->
        (match int_of_string_opt s with
         | Some n -> Ok (VVariant ("Some", Some (VInt n)))
         | None -> Ok (VVariant ("None", None)))
      | _ -> Error (TypeMismatch "parse_int expects String")
    ));
    ("parse_float", VBuiltin ("parse_float", fun args ->
      match args with
      | [VString s] ->
        (match float_of_string_opt s with
         | Some f -> Ok (VVariant ("Some", Some (VFloat f)))
         | None -> Ok (VVariant ("None", None)))
      | _ -> Error (TypeMismatch "parse_float expects String")
    ));

    (* -- Math builtins ----------------------------------------------------- *)
    ("sqrt", VBuiltin ("sqrt", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.sqrt f))
      | _ -> Error (TypeMismatch "sqrt expects Float")
    ));
    ("cbrt", VBuiltin ("cbrt", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.cbrt f))
      | _ -> Error (TypeMismatch "cbrt expects Float")
    ));
    ("pow_float", VBuiltin ("pow_float", fun args ->
      match args with
      | [VFloat base; VFloat exp] -> Ok (VFloat (Float.pow base exp))
      | _ -> Error (TypeMismatch "pow_float expects (Float, Float)")
    ));
    ("floor", VBuiltin ("floor", fun args ->
      match args with
      | [VFloat f] -> Ok (VInt (Float.to_int (Float.floor f)))
      | _ -> Error (TypeMismatch "floor expects Float")
    ));
    ("ceil", VBuiltin ("ceil", fun args ->
      match args with
      | [VFloat f] -> Ok (VInt (Float.to_int (Float.ceil f)))
      | _ -> Error (TypeMismatch "ceil expects Float")
    ));
    ("round", VBuiltin ("round", fun args ->
      match args with
      | [VFloat f] -> Ok (VInt (Float.to_int (Float.round f)))
      | _ -> Error (TypeMismatch "round expects Float")
    ));
    ("trunc", VBuiltin ("trunc", fun args ->
      match args with
      | [VFloat f] -> Ok (VInt (Float.to_int (Float.of_int (Float.to_int f))))
      | _ -> Error (TypeMismatch "trunc expects Float")
    ));
    ("sin", VBuiltin ("sin", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.sin f))
      | _ -> Error (TypeMismatch "sin expects Float")
    ));
    ("cos", VBuiltin ("cos", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.cos f))
      | _ -> Error (TypeMismatch "cos expects Float")
    ));
    ("tan", VBuiltin ("tan", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.tan f))
      | _ -> Error (TypeMismatch "tan expects Float")
    ));
    ("asin", VBuiltin ("asin", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.asin f))
      | _ -> Error (TypeMismatch "asin expects Float")
    ));
    ("acos", VBuiltin ("acos", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.acos f))
      | _ -> Error (TypeMismatch "acos expects Float")
    ));
    ("atan", VBuiltin ("atan", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.atan f))
      | _ -> Error (TypeMismatch "atan expects Float")
    ));
    ("atan2", VBuiltin ("atan2", fun args ->
      match args with
      | [VFloat y; VFloat x] -> Ok (VFloat (Float.atan2 y x))
      | _ -> Error (TypeMismatch "atan2 expects (Float, Float)")
    ));
    ("exp", VBuiltin ("exp", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.exp f))
      | _ -> Error (TypeMismatch "exp expects Float")
    ));
    ("log", VBuiltin ("log", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.log f))
      | _ -> Error (TypeMismatch "log expects Float")
    ));
    ("log10", VBuiltin ("log10", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.log10 f))
      | _ -> Error (TypeMismatch "log10 expects Float")
    ));
    ("log2", VBuiltin ("log2", fun args ->
      match args with
      | [VFloat f] -> Ok (VFloat (Float.log2 f))
      | _ -> Error (TypeMismatch "log2 expects Float")
    ));

    (* -- I/O builtins ------------------------------------------------------ *)
    ("panic", VBuiltin ("panic", fun args ->
      match args with
      | [VString msg] -> Error (RuntimeError msg)
      | _ -> Error (RuntimeError "panic!")
    ));
    ("read_file", VBuiltin ("read_file", fun args ->
      match args with
      | [VString path] ->
        (try
          let ic = open_in path in
          let n = in_channel_length ic in
          let s = Bytes.create n in
          really_input ic s 0 n;
          close_in ic;
          Ok (VVariant ("Ok", Some (VString (Bytes.to_string s))))
        with
        | Sys_error msg -> Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "read_file expects String")
    ));
    ("write_file", VBuiltin ("write_file", fun args ->
      match args with
      | [VString path; VString content] ->
        (try
          let oc = open_out path in
          output_string oc content;
          close_out oc;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Sys_error msg -> Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "write_file expects (String, String)")
    ));
    ("append_file", VBuiltin ("append_file", fun args ->
      match args with
      | [VString path; VString content] ->
        (try
          let oc = open_out_gen [Open_append; Open_creat] 0o644 path in
          output_string oc content;
          close_out oc;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Sys_error msg -> Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "append_file expects (String, String)")
    ));
    ("file_exists", VBuiltin ("file_exists", fun args ->
      match args with
      | [VString path] -> Ok (VBool (Sys.file_exists path))
      | _ -> Error (TypeMismatch "file_exists expects String")
    ));
    ("is_directory", VBuiltin ("is_directory", fun args ->
      match args with
      | [VString path] -> Ok (VBool (Sys.is_directory path))
      | _ -> Error (TypeMismatch "is_directory expects String")
    ));
    ("getenv", VBuiltin ("getenv", fun args ->
      match args with
      | [VString name] ->
        (match Sys.getenv_opt name with
         | Some v -> Ok (VVariant ("Some", Some (VString v)))
         | None -> Ok (VVariant ("None", None)))
      | _ -> Error (TypeMismatch "getenv expects String")
    ));
    ("getcwd", VBuiltin ("getcwd", fun args ->
      match args with
      | [] -> Ok (VVariant ("Ok", Some (VString (Sys.getcwd ()))))
      | _ -> Error (TypeMismatch "getcwd expects no arguments")
    ));
    ("read_line", VBuiltin ("read_line", fun args ->
      match args with
      | [] ->
        (try Ok (VVariant ("Ok", Some (VString (read_line ()))))
         with End_of_file -> Ok (VVariant ("Err", Some (VString "End of input"))))
      | _ -> Error (TypeMismatch "read_line expects no arguments")
    ));
    ("exit", VBuiltin ("exit", fun args ->
      match args with
      | [VInt code] -> exit code
      | _ -> Error (TypeMismatch "exit expects Int")
    ));

    (* -- Directory operations ------------------------------------------------ *)
    ("list_dir", VBuiltin ("list_dir", fun args ->
      match args with
      | [VString path] ->
        (try
          let handle = Unix.opendir path in
          let entries = ref [] in
          (try while true do
            let entry = Unix.readdir handle in
            if entry <> "." && entry <> ".." then
              entries := entry :: !entries
          done with End_of_file -> ());
          Unix.closedir handle;
          Ok (VVariant ("Ok", Some (VArray (Array.of_list
            (List.rev_map (fun s -> VString s) !entries)))))
        with
        | Unix.Unix_error (_, _, msg) ->
          Ok (VVariant ("Err", Some (VString ("list_dir: " ^ msg))))
        | Sys_error msg ->
          Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "list_dir expects String")
    ));
    ("create_dir", VBuiltin ("create_dir", fun args ->
      match args with
      | [VString path] ->
        (try
          Unix.mkdir path 0o755;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Unix.Unix_error (_, _, msg) ->
          Ok (VVariant ("Err", Some (VString ("create_dir: " ^ msg))))
        | Sys_error msg ->
          Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "create_dir expects String")
    ));
    ("remove_dir", VBuiltin ("remove_dir", fun args ->
      match args with
      | [VString path] ->
        (try
          Unix.rmdir path;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Unix.Unix_error (_, _, msg) ->
          Ok (VVariant ("Err", Some (VString ("remove_dir: " ^ msg))))
        | Sys_error msg ->
          Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "remove_dir expects String")
    ));
    ("setenv", VBuiltin ("setenv", fun args ->
      match args with
      | [VString name; VString value] ->
        (try
          Unix.putenv name value;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Unix.Unix_error (_, _, msg) ->
          Ok (VVariant ("Err", Some (VString ("setenv: " ^ msg)))))
      | _ -> Error (TypeMismatch "setenv expects (String, String)")
    ));
    ("chdir", VBuiltin ("chdir", fun args ->
      match args with
      | [VString path] ->
        (try
          Unix.chdir path;
          Ok (VVariant ("Ok", Some VUnit))
        with
        | Unix.Unix_error (_, _, msg) ->
          Ok (VVariant ("Err", Some (VString ("chdir: " ^ msg))))
        | Sys_error msg ->
          Ok (VVariant ("Err", Some (VString msg))))
      | _ -> Error (TypeMismatch "chdir expects String")
    ));

    (* -- Time --------------------------------------------------------------- *)
    ("time_now", VBuiltin ("time_now", fun _args ->
      Ok (VFloat (Sys.time ()))
    ));

    (* -- Cmd Msg — linear side-effect obligations (Stage 11) --------------- *)
    (* cmd_none : Cmd 'msg — the no-op command; the interpreter represents
       it as VUnit.  At runtime, the TEA loop discards the Cmd after
       extracting the new model from the (model, cmd) pair.
       Linearity is enforced at compile time (quantity checker) not runtime. *)
    ("cmd_none", VUnit);
    (* cmd_perform : (() ->{IO} unit) -> Cmd 'msg — wraps an IO thunk.
       In the interpreter, the Cmd is executed eagerly when constructed.
       In a real browser runtime the thunk would be scheduled by the
       runtime after the update cycle completes. *)
    ("cmd_perform", VBuiltin ("cmd_perform", fun args ->
      match args with
      | [VClosure _ as f] ->
        (* Execute the IO thunk immediately in the interpreter *)
        let* _ = apply_function f [VUnit] in
        Ok VUnit
      | [VBuiltin _ as f] ->
        let* _ = apply_function f [VUnit] in
        Ok VUnit
      | _ ->
        Error (RuntimeError "cmd_perform: expected a zero-argument function")
    ));
    (* -- TEA (The Elm Architecture) interpreter runtime -------------------- *)
    ("tea_run", VBuiltin ("tea_run", fun args ->
      (* tea_run expects a record with fields: init, update, view, subscriptions.
         - init        : () -> (Model, [Cmd])
         - update      : Msg -> Model -> (Model, [Cmd])
         - view        : Model -> Html    (Html is rendered as a string or shown)
         - subscriptions: Model -> [Sub]  (unused at interpreter level)

         The interpreter loop:
           1. Call init() to get the initial model.
           2. Render and print the initial view.
           3. Read lines from stdin; each line becomes a variant Msg.
           4. Call update(msg, model) -> (new_model, cmds).
           5. Print new view. Repeat until EOF. *)
      match args with
      | [VRecord fields] ->
        let get f = match List.assoc_opt f fields with
          | Some v -> Ok v
          | None -> Error (RuntimeError (Printf.sprintf "tea_run: missing field '%s'" f))
        in
        let render_html v =
          let s = match v with
            | VString s -> s
            | VVariant ("Text", Some (VString s)) -> s
            | VVariant ("Node", _) -> "[Html node]"
            | other -> show_value other
          in
          print_endline s
        in
        let* init_fn   = get "init"   in
        let* update_fn = get "update" in
        let* view_fn   = get "view"   in
        (* Call init() -> (model, cmds) — 0-param function, no args *)
        let* init_result = apply_function init_fn [] in
        let model0 = match init_result with
          | VTuple (m :: _) -> m
          | m -> m
        in
        (* Render initial view *)
        let* view0 = apply_function view_fn [model0] in
        render_html view0;
        (* Read-eval-print loop: stdin lines become variant Msgs until EOF *)
        let rec loop model =
          let input = try Some (read_line ()) with End_of_file -> None in
          match input with
          | None -> Ok VUnit  (* EOF — done, clean exit *)
          | Some "" -> loop model  (* blank line — skip *)
          | Some line ->
            let msg = VVariant (String.trim line, None) in
            let* upd_result = apply_function update_fn [msg; model] in
            let new_model = match upd_result with
              | VTuple (m :: _) -> m
              | m -> m
            in
            let* new_view = apply_function view_fn [new_model] in
            render_html new_view;
            loop new_model
        in
        loop model0
      | _ ->
        Error (RuntimeError
          "tea_run expects a record: {init, update, view, subscriptions}")
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

  | TopType td ->
    (* Register enum constructors so they can be used as values / constructor functions.
       Nullary constructors become VVariant values.
       N-ary constructors become VBuiltin constructor functions. *)
    begin match td.td_body with
      | TyEnum variants ->
        let env' = List.fold_left (fun env (vd : variant_decl) ->
          let name = vd.vd_name.name in
          match vd.vd_fields with
          | [] ->
            (* Nullary constructor: bind directly as a VVariant value *)
            extend_env name (VVariant (name, None)) env
          | [_] ->
            (* Single-payload constructor: wrap arg in VVariant *)
            extend_env name
              (VBuiltin (name, fun args ->
                match args with
                | [v] -> Ok (VVariant (name, Some v))
                | _ -> Error (TypeMismatch (Printf.sprintf "%s expects 1 argument" name))))
              env
          | fields ->
            (* Multi-payload constructor: pack args into a VTuple *)
            let n = List.length fields in
            extend_env name
              (VBuiltin (name, fun args ->
                if List.length args = n then
                  Ok (VVariant (name, Some (VTuple args)))
                else
                  Error (TypeMismatch
                    (Printf.sprintf "%s expects %d arguments" name n))))
              env
        ) env variants in
        Ok env'
      | _ ->
        (* Struct, alias, and other type declarations don't create runtime bindings *)
        Ok env
    end

  | TopTrait _ | TopImpl _ ->
    (* Trait and impl declarations don't affect the runtime value environment *)
    Ok env

  | TopEffect ed ->
    (* Register each effect operation as a PerformEffect-raising builtin.
       When an effect op is called from within a `handle` expression, the
       Error(PerformEffect ...) propagates up the call stack until caught by
       the ExprHandle dispatcher.  Unhandled effects surface as RuntimeError. *)
    let env' = List.fold_left (fun env (op : effect_op_decl) ->
      let op_name = op.eod_name.name in
      let builtin = VBuiltin (op_name, fun args ->
        Error (PerformEffect (op_name, args))
      ) in
      extend_env op_name builtin env
    ) env ed.ed_ops in
    Ok env'

(** Evaluate a program *)
let eval_program (prog : program) : env result =
  let initial_env = create_initial_env () in
  List.fold_left (fun acc decl ->
    let* env = acc in
    eval_decl env decl
  ) (Ok initial_env) prog.prog_decls
