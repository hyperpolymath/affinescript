(** Tree-walking interpreter for AffineScript *)

open Ast
open Value

(** Interpreter errors *)
exception Runtime_error of string * Span.t option

let error ?span msg = raise (Runtime_error (msg, span))

(** Get span from expression *)
let expr_span = function
  | ExprSpan (_, span) -> Some span
  | ExprLit (LitInt (_, span)) -> Some span
  | ExprLit (LitFloat (_, span)) -> Some span
  | ExprLit (LitBool (_, span)) -> Some span
  | ExprLit (LitChar (_, span)) -> Some span
  | ExprLit (LitString (_, span)) -> Some span
  | ExprLit (LitUnit span) -> Some span
  | ExprVar { span; _ } -> Some span
  | _ -> None

(** Evaluate a literal *)
let eval_literal = function
  | LitInt (i, _) -> VInt i
  | LitFloat (f, _) -> VFloat f
  | LitBool (b, _) -> VBool b
  | LitChar (c, _) -> VChar c
  | LitString (s, _) -> VString s
  | LitUnit _ -> VUnit

(** Binary operation on integers *)
let int_binop op v1 v2 =
  match v1, v2 with
  | VInt a, VInt b -> (
      match op with
      | OpAdd -> Ok (VInt (a + b))
      | OpSub -> Ok (VInt (a - b))
      | OpMul -> Ok (VInt (a * b))
      | OpDiv ->
          if b = 0 then Error "Division by zero"
          else Ok (VInt (a / b))
      | OpMod ->
          if b = 0 then Error "Modulo by zero"
          else Ok (VInt (a mod b))
      | OpEq -> Ok (VBool (a = b))
      | OpNe -> Ok (VBool (a <> b))
      | OpLt -> Ok (VBool (a < b))
      | OpLe -> Ok (VBool (a <= b))
      | OpGt -> Ok (VBool (a > b))
      | OpGe -> Ok (VBool (a >= b))
      | OpBitAnd -> Ok (VInt (a land b))
      | OpBitOr -> Ok (VInt (a lor b))
      | OpBitXor -> Ok (VInt (a lxor b))
      | OpShl -> Ok (VInt (a lsl b))
      | OpShr -> Ok (VInt (a asr b))
      | OpAnd | OpOr -> Error "Boolean operator on integers"
    )
  | _ -> Error "Type mismatch in integer operation"

(** Binary operation on floats *)
let float_binop op v1 v2 =
  match v1, v2 with
  | VFloat a, VFloat b -> (
      match op with
      | OpAdd -> Ok (VFloat (a +. b))
      | OpSub -> Ok (VFloat (a -. b))
      | OpMul -> Ok (VFloat (a *. b))
      | OpDiv -> Ok (VFloat (a /. b))
      | OpEq -> Ok (VBool (a = b))
      | OpNe -> Ok (VBool (a <> b))
      | OpLt -> Ok (VBool (a < b))
      | OpLe -> Ok (VBool (a <= b))
      | OpGt -> Ok (VBool (a > b))
      | OpGe -> Ok (VBool (a >= b))
      | _ -> Error "Invalid operation on floats"
    )
  | VInt a, VFloat b -> float_binop op (VFloat (Float.of_int a)) (VFloat b)
  | VFloat a, VInt b -> float_binop op (VFloat a) (VFloat (Float.of_int b))
  | _ -> Error "Type mismatch in float operation"

(** Boolean binary operations *)
let bool_binop op v1 v2 =
  match v1, v2 with
  | VBool a, VBool b -> (
      match op with
      | OpAnd -> Ok (VBool (a && b))
      | OpOr -> Ok (VBool (a || b))
      | OpEq -> Ok (VBool (a = b))
      | OpNe -> Ok (VBool (a <> b))
      | _ -> Error "Invalid operation on booleans"
    )
  | _ -> Error "Type mismatch in boolean operation"

(** String operations *)
let string_binop op v1 v2 =
  match v1, v2 with
  | VString a, VString b -> (
      match op with
      | OpAdd -> Ok (VString (a ^ b))
      | OpEq -> Ok (VBool (a = b))
      | OpNe -> Ok (VBool (a <> b))
      | OpLt -> Ok (VBool (a < b))
      | OpLe -> Ok (VBool (a <= b))
      | OpGt -> Ok (VBool (a > b))
      | OpGe -> Ok (VBool (a >= b))
      | _ -> Error "Invalid operation on strings"
    )
  | _ -> Error "Type mismatch in string operation"

(** Evaluate binary expression *)
let eval_binop op v1 v2 =
  match v1, v2 with
  | VInt _, VInt _ -> int_binop op v1 v2
  | VFloat _, _ | _, VFloat _ -> float_binop op v1 v2
  | VBool _, VBool _ -> bool_binop op v1 v2
  | VString _, VString _ -> string_binop op v1 v2
  | _ ->
      match op with
      | OpEq -> Ok (VBool (Value.equal v1 v2))
      | OpNe -> Ok (VBool (not (Value.equal v1 v2)))
      | _ -> Error (Printf.sprintf "Cannot apply operator to %s and %s"
                      (Value.show v1) (Value.show v2))

(** Evaluate unary expression *)
let eval_unop op v =
  match op, v with
  | OpNeg, VInt i -> Ok (VInt (-i))
  | OpNeg, VFloat f -> Ok (VFloat (-.f))
  | OpNot, VBool b -> Ok (VBool (not b))
  | OpBitNot, VInt i -> Ok (VInt (lnot i))
  | OpRef, v -> Ok (VRef (ref v))
  | OpDeref, VRef r -> Ok (!r)
  | OpDeref, _ -> Error "Cannot dereference non-reference value"
  | _ -> Error (Printf.sprintf "Invalid unary operation on %s" (Value.show v))

(** Match a pattern against a value, returning bindings if successful *)
let rec match_pattern pat value : (string * t * bool) list option =
  match pat with
  | PatWildcard _ -> Some []
  | PatVar id -> Some [(id.name, value, false)]
  | PatLit lit ->
      let lit_val = eval_literal lit in
      if Value.equal lit_val value then Some [] else None
  | PatTuple pats -> (
      match value with
      | VTuple values when List.length pats = List.length values ->
          let bindings = List.map2 match_pattern pats values in
          if List.for_all Option.is_some bindings then
            Some (List.concat_map Option.get bindings)
          else None
      | _ -> None
    )
  | PatRecord (fields, has_rest) -> (
      match value with
      | VRecord rec_fields ->
          let match_field (id, pat_opt) =
            match List.assoc_opt id.name rec_fields with
            | Some v -> (
                match pat_opt with
                | None -> Some [(id.name, v, false)]
                | Some p -> match_pattern p v
              )
            | None -> if has_rest then Some [] else None
          in
          let bindings = List.map match_field fields in
          if List.for_all Option.is_some bindings then
            Some (List.concat_map Option.get bindings)
          else None
      | _ -> None
    )
  | PatCon (id, pats) -> (
      match value with
      | VVariant (_, variant, args) when id.name = variant && List.length pats = List.length args ->
          let bindings = List.map2 match_pattern pats args in
          if List.for_all Option.is_some bindings then
            Some (List.concat_map Option.get bindings)
          else None
      | _ -> None
    )
  | PatOr (p1, p2) -> (
      match match_pattern p1 value with
      | Some bindings -> Some bindings
      | None -> match_pattern p2 value
    )
  | PatAs (id, p) -> (
      match match_pattern p value with
      | Some bindings -> Some ((id.name, value, false) :: bindings)
      | None -> None
    )

(** Control flow exceptions *)
exception Return_exn of t
exception Break_exn
exception Continue_exn

(** Main evaluation function *)
let rec eval env expr =
  match expr with
  | ExprSpan (e, _) -> eval env e

  | ExprLit lit -> eval_literal lit

  | ExprVar id -> (
      match Value.consume env id.name with
      | Ok v -> v
      | Error msg -> error ~span:id.span msg
    )

  | ExprLet { el_mut; el_pat; el_value; el_body; _ } ->
      let value = eval env el_value in
      let bindings = match match_pattern el_pat value with
        | Some b -> b
        | None -> error "Pattern match failed in let binding"
      in
      List.iter (fun (name, v, _) ->
        Value.bind env name v ~mutable_:el_mut ~linear:false
      ) bindings;
      (match el_body with
       | Some body -> eval env body
       | None -> VUnit)

  | ExprIf { ei_cond; ei_then; ei_else } ->
      let cond_val = eval env ei_cond in
      (match Value.to_bool cond_val with
       | Ok true -> eval env ei_then
       | Ok false -> (
           match ei_else with
           | Some e -> eval env e
           | None -> VUnit
         )
       | Error msg -> error msg)

  | ExprMatch { em_scrutinee; em_arms } ->
      let value = eval env em_scrutinee in
      let rec try_arms = function
        | [] -> error "No matching pattern in match expression"
        | arm :: rest ->
            match match_pattern arm.ma_pat value with
            | None -> try_arms rest
            | Some bindings ->
                let match_env = child_env env in
                List.iter (fun (name, v, _) ->
                  Value.bind match_env name v ~mutable_:false ~linear:false
                ) bindings;
                (* Check guard if present *)
                let guard_ok = match arm.ma_guard with
                  | None -> true
                  | Some guard ->
                      match Value.to_bool (eval match_env guard) with
                      | Ok b -> b
                      | Error _ -> false
                in
                if guard_ok then eval match_env arm.ma_body
                else try_arms rest
      in
      try_arms em_arms

  | ExprLambda { elam_params; elam_body; _ } ->
      VClosure { params = elam_params; body = elam_body; env }

  | ExprApp (func, args) ->
      let func_val = eval env func in
      let arg_vals = List.map (eval env) args in
      apply func_val arg_vals

  | ExprField (e, field) ->
      let value = eval env e in
      (match value with
       | VRecord fields -> (
           match List.assoc_opt field.name fields with
           | Some v -> v
           | None -> error ~span:field.span (Printf.sprintf "Field '%s' not found" field.name)
         )
       | _ -> error "Cannot access field on non-record value")

  | ExprTupleIndex (e, idx) ->
      let value = eval env e in
      (match value with
       | VTuple values ->
           if idx >= 0 && idx < List.length values then
             List.nth values idx
           else
             error (Printf.sprintf "Tuple index %d out of bounds" idx)
       | _ -> error "Cannot index non-tuple value")

  | ExprIndex (e, idx_expr) ->
      let value = eval env e in
      let idx_val = eval env idx_expr in
      (match value, idx_val with
       | VArray arr, VInt i ->
           if i >= 0 && i < Array.length arr then arr.(i)
           else error (Printf.sprintf "Array index %d out of bounds" i)
       | VString s, VInt i ->
           if i >= 0 && i < String.length s then VChar (String.get s i)
           else error (Printf.sprintf "String index %d out of bounds" i)
       | _ -> error "Invalid indexing operation")

  | ExprTuple exprs ->
      VTuple (List.map (eval env) exprs)

  | ExprArray exprs ->
      VArray (Array.of_list (List.map (eval env) exprs))

  | ExprRecord { er_fields; er_spread } ->
      let base_fields = match er_spread with
        | Some spread_expr ->
            (match eval env spread_expr with
             | VRecord fields -> fields
             | _ -> error "Spread must be a record")
        | None -> []
      in
      let new_fields = List.map (fun (id, expr_opt) ->
        let value = match expr_opt with
          | Some e -> eval env e
          | None ->
              (* Shorthand: {x} means {x: x} *)
              match Value.consume env id.name with
              | Ok v -> v
              | Error msg -> error ~span:id.span msg
        in
        (id.name, value)
      ) er_fields in
      (* New fields override spread fields *)
      let merged = List.fold_left (fun acc (k, v) ->
        (k, v) :: List.remove_assoc k acc
      ) base_fields new_fields in
      VRecord merged

  | ExprRowRestrict (e, field) ->
      let value = eval env e in
      (match value with
       | VRecord fields ->
           VRecord (List.remove_assoc field.name fields)
       | _ -> error "Cannot restrict non-record value")

  | ExprBinary (e1, op, e2) ->
      (* Short-circuit evaluation for && and || *)
      (match op with
       | OpAnd ->
           let v1 = eval env e1 in
           (match Value.to_bool v1 with
            | Ok false -> VBool false
            | Ok true -> eval env e2
            | Error msg -> error msg)
       | OpOr ->
           let v1 = eval env e1 in
           (match Value.to_bool v1 with
            | Ok true -> VBool true
            | Ok false -> eval env e2
            | Error msg -> error msg)
       | _ ->
           let v1 = eval env e1 in
           let v2 = eval env e2 in
           match eval_binop op v1 v2 with
           | Ok v -> v
           | Error msg -> error msg)

  | ExprUnary (op, e) ->
      let v = eval env e in
      (match eval_unop op v with
       | Ok v -> v
       | Error msg -> error msg)

  | ExprBlock blk -> eval_block env blk

  | ExprReturn e_opt ->
      let value = match e_opt with
        | Some e -> eval env e
        | None -> VUnit
      in
      raise (Return_exn value)

  | ExprVariant (type_id, variant_id) ->
      VVariant (type_id.name, variant_id.name, [])

  | ExprTry _ -> error "Try/catch not yet implemented"
  | ExprHandle _ -> error "Effect handlers not yet implemented"
  | ExprResume _ -> error "Resume not yet implemented"
  | ExprUnsafe _ -> error "Unsafe operations not yet implemented"

(** Evaluate a block of statements *)
and eval_block env { blk_stmts; blk_expr } =
  let block_env = child_env env in
  List.iter (eval_stmt block_env) blk_stmts;
  match blk_expr with
  | Some e -> eval block_env e
  | None -> VUnit

(** Evaluate a statement *)
and eval_stmt env = function
  | StmtLet { sl_mut; sl_pat; sl_value; _ } ->
      let value = eval env sl_value in
      let bindings = match match_pattern sl_pat value with
        | Some b -> b
        | None -> error "Pattern match failed in let binding"
      in
      List.iter (fun (name, v, _) ->
        Value.bind env name v ~mutable_:sl_mut ~linear:false
      ) bindings

  | StmtExpr e -> ignore (eval env e)

  | StmtAssign (target, op, value_expr) ->
      let new_value = eval env value_expr in
      (match target with
       | ExprVar id ->
           let final_value = match op with
             | AssignEq -> new_value
             | AssignAdd | AssignSub | AssignMul | AssignDiv as assign_op ->
                 (match Value.lookup env id.name with
                  | Some binding ->
                      let binop = match assign_op with
                        | AssignAdd -> OpAdd
                        | AssignSub -> OpSub
                        | AssignMul -> OpMul
                        | AssignDiv -> OpDiv
                        | AssignEq -> OpAdd (* unreachable *)
                      in
                      (match eval_binop binop binding.value new_value with
                       | Ok v -> v
                       | Error msg -> error msg)
                  | None -> error ~span:id.span (Printf.sprintf "Unbound variable: %s" id.name))
           in
           if not (Value.update env id.name final_value) then
             error ~span:id.span (Printf.sprintf "Cannot assign to immutable variable: %s" id.name)
       | ExprIndex (arr_expr, idx_expr) ->
           let arr = eval env arr_expr in
           let idx = eval env idx_expr in
           (match arr, idx with
            | VArray arr, VInt i ->
                if i >= 0 && i < Array.length arr then
                  arr.(i) <- new_value
                else
                  error (Printf.sprintf "Array index %d out of bounds" i)
            | _ -> error "Invalid assignment target")
       | ExprField (rec_expr, field) ->
           (* Record field update - creates new record *)
           let _ = eval env rec_expr in
           error ~span:field.span "Direct field mutation not supported; use record update syntax"
       | _ -> error "Invalid assignment target")

  | StmtWhile (cond, body) ->
      let rec loop () =
        match Value.to_bool (eval env cond) with
        | Ok true ->
            (try
               ignore (eval_block env body);
               loop ()
             with
             | Break_exn -> ()
             | Continue_exn -> loop ())
        | Ok false -> ()
        | Error msg -> error msg
      in
      loop ()

  | StmtFor (pat, iter_expr, body) ->
      let iter_val = eval env iter_expr in
      let items = match iter_val with
        | VArray arr -> Array.to_list arr
        | VTuple items -> items
        | VString s -> List.init (String.length s) (fun i -> VChar (String.get s i))
        | _ -> error "Cannot iterate over this value"
      in
      List.iter (fun item ->
        match match_pattern pat item with
        | None -> error "Pattern match failed in for loop"
        | Some bindings ->
            let iter_env = child_env env in
            List.iter (fun (name, v, _) ->
              Value.bind iter_env name v ~mutable_:false ~linear:false
            ) bindings;
            try ignore (eval_block iter_env body)
            with
            | Break_exn -> raise Break_exn
            | Continue_exn -> ()
      ) items

(** Apply a function to arguments *)
and apply func_val args =
  match func_val with
  | VClosure { params; body; env } ->
      if List.length params <> List.length args then
        error (Printf.sprintf "Function expects %d arguments, got %d"
                 (List.length params) (List.length args));
      let call_env = child_env env in
      List.iter2 (fun param arg ->
        let linear = match param.p_quantity with
          | Some QOne -> true
          | _ -> false
        in
        Value.bind call_env param.p_name.name arg ~mutable_:false ~linear
      ) params args;
      (try eval call_env body
       with Return_exn v -> v)
  | VBuiltin (_, f) -> f args
  | VVariant (ty, variant, existing_args) ->
      (* Variant constructor application *)
      VVariant (ty, variant, existing_args @ args)
  | _ -> error (Printf.sprintf "Cannot call non-function value: %s" (Value.show func_val))

(** Evaluate a function declaration and bind it *)
let eval_fn_decl env (decl : fn_decl) =
  let closure = VClosure {
    params = decl.fd_params;
    body = (match decl.fd_body with
            | FnBlock blk -> ExprBlock blk
            | FnExpr e -> e);
    env;
  } in
  Value.bind env decl.fd_name.name closure ~mutable_:false ~linear:false

(** Evaluate a type declaration (registers constructors) *)
let eval_type_decl env (decl : type_decl) =
  match decl.td_body with
  | TyEnum variants ->
      List.iter (fun (v : variant_decl) ->
        let constructor =
          if v.vd_fields = [] then
            VVariant (decl.td_name.name, v.vd_name.name, [])
          else
            (* Return a constructor function *)
            VBuiltin (v.vd_name.name, fun args ->
              VVariant (decl.td_name.name, v.vd_name.name, args))
        in
        Value.bind env v.vd_name.name constructor ~mutable_:false ~linear:false
      ) variants
  | _ -> ()  (* Structs/aliases don't create value bindings *)

(** Evaluate a top-level declaration *)
let eval_top_level env = function
  | TopFn decl -> eval_fn_decl env decl
  | TopType decl -> eval_type_decl env decl
  | TopConst { tc_name; tc_value; _ } ->
      let value = eval env tc_value in
      Value.bind env tc_name.name value ~mutable_:false ~linear:false
  | TopEffect _ -> ()  (* Effects are compile-time only *)
  | TopTrait _ -> ()   (* Traits are compile-time only *)
  | TopImpl _ -> ()    (* Impls are compile-time only *)

(** Evaluate a complete program *)
let eval_program env (prog : program) =
  List.iter (eval_top_level env) prog.prog_decls
