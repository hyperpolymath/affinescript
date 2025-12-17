(** Bidirectional type checker for AffineScript *)

open Ast
open Types

(** Type errors *)
type error =
  | TypeMismatch of ty * ty * Span.t option
  | UnboundVariable of string * Span.t
  | UnboundType of string * Span.t
  | NotAFunction of ty * Span.t option
  | WrongArity of int * int * Span.t option
  | OccursCheck of tyvar_id * ty
  | CannotUnify of ty * ty
  | InfiniteType of tyvar_id * ty
  | RowMismatch of string * Span.t option
  | MissingField of string * Span.t option
  | DuplicateField of string * Span.t
  | NotMutable of string * Span.t
  | LinearityViolation of string * Span.t
  | EffectNotAllowed of effect * Span.t option

exception Type_error of error

let error_to_string = function
  | TypeMismatch (expected, got, _) ->
      Printf.sprintf "Type mismatch: expected %s, got %s"
        (show_ty expected) (show_ty got)
  | UnboundVariable (name, _) ->
      Printf.sprintf "Unbound variable: %s" name
  | UnboundType (name, _) ->
      Printf.sprintf "Unbound type: %s" name
  | NotAFunction (ty, _) ->
      Printf.sprintf "Expected function, got %s" (show_ty ty)
  | WrongArity (expected, got, _) ->
      Printf.sprintf "Expected %d arguments, got %d" expected got
  | OccursCheck (id, ty) ->
      Printf.sprintf "Occurs check failed: ?%d in %s" id (show_ty ty)
  | CannotUnify (t1, t2) ->
      Printf.sprintf "Cannot unify %s with %s" (show_ty t1) (show_ty t2)
  | InfiniteType (id, ty) ->
      Printf.sprintf "Infinite type: ?%d = %s" id (show_ty ty)
  | RowMismatch (label, _) ->
      Printf.sprintf "Row mismatch at label: %s" label
  | MissingField (field, _) ->
      Printf.sprintf "Missing field: %s" field
  | DuplicateField (field, _) ->
      Printf.sprintf "Duplicate field: %s" field
  | NotMutable (name, _) ->
      Printf.sprintf "Cannot mutate immutable binding: %s" name
  | LinearityViolation (name, _) ->
      Printf.sprintf "Linear variable used multiple times: %s" name
  | EffectNotAllowed (eff, _) ->
      Printf.sprintf "Effect not allowed: %a" pp_eff eff

(** Type environment *)
type env = {
  env_vars: (string, scheme) Hashtbl.t;
  env_types: (string, ty) Hashtbl.t;
  env_parent: env option;
  env_effects: effect;  (** Allowed effects in this context *)
}

let empty_env () = {
  env_vars = Hashtbl.create 32;
  env_types = Hashtbl.create 16;
  env_parent = None;
  env_effects = EEmpty;
}

let child_env parent = {
  env_vars = Hashtbl.create 16;
  env_types = Hashtbl.create 8;
  env_parent = Some parent;
  env_effects = parent.env_effects;
}

let with_effects env eff = { env with env_effects = eff }

(** Add variable binding to environment *)
let bind_var env name scheme =
  Hashtbl.replace env.env_vars name scheme

(** Add type binding to environment *)
let bind_type env name ty =
  Hashtbl.replace env.env_types name ty

(** Lookup variable in environment *)
let rec lookup_var env name =
  match Hashtbl.find_opt env.env_vars name with
  | Some scheme -> Some scheme
  | None -> Option.bind env.env_parent (fun p -> lookup_var p name)

(** Lookup type in environment *)
let rec lookup_type env name =
  match Hashtbl.find_opt env.env_types name with
  | Some ty -> Some ty
  | None -> Option.bind env.env_parent (fun p -> lookup_type p name)

(** Type checker state *)
type state = {
  mutable subst: subst;
  mutable errors: error list;
}

let create_state () = {
  subst = empty_subst ();
  errors = [];
}

let add_error st err =
  st.errors <- err :: st.errors

(** Instantiate a type scheme with fresh type variables *)
let instantiate st scheme =
  let ty_map = Hashtbl.create 8 in
  List.iter (fun (name, _) ->
    Hashtbl.replace ty_map name (TVar (fresh_tyvar ()))
  ) scheme.sc_tyvars;
  let rec subst_rigid ty =
    match ty with
    | TRigid name ->
        (match Hashtbl.find_opt ty_map name with
         | Some t -> t
         | None -> ty)
    | TApp (name, args) -> TApp (name, List.map subst_rigid args)
    | TArrow (t1, t2, eff) -> TArrow (subst_rigid t1, subst_rigid t2, eff)
    | TForall (v, k, t) -> TForall (v, k, subst_rigid t)
    | TTuple ts -> TTuple (List.map subst_rigid ts)
    | TRecord row -> TRecord (subst_rigid_row row)
    | TRef t -> TRef (subst_rigid t)
    | TMut t -> TMut (subst_rigid t)
    | TOwn t -> TOwn (subst_rigid t)
    | TRefined (t, r) -> TRefined (subst_rigid t, r)
    | TQuantified (q, t) -> TQuantified (q, subst_rigid t)
    | _ -> ty
  and subst_rigid_row = function
    | REmpty -> REmpty
    | RVar id -> RVar id
    | RExtend (l, t, r) -> RExtend (l, subst_rigid t, subst_rigid_row r)
  in
  subst_rigid scheme.sc_type

(** Generalize a type to a scheme *)
let generalize env ty =
  let env_vars = Hashtbl.fold (fun _ scheme acc ->
    let ty = scheme.sc_type in
    free_tyvars ty @ acc
  ) env.env_vars [] in
  let ty_vars = free_tyvars ty in
  let free = List.filter (fun v -> not (List.mem v env_vars)) ty_vars in
  if free = [] then
    mono ty
  else
    let tyvars = List.mapi (fun i id ->
      let name = Printf.sprintf "t%d" i in
      (name, KType)
    ) (List.sort_uniq compare free) in
    { sc_tyvars = tyvars; sc_rowvars = []; sc_effvars = []; sc_type = ty }

(** Unification *)
let rec unify st t1 t2 =
  let t1 = apply_subst st.subst t1 in
  let t2 = apply_subst st.subst t2 in
  match t1, t2 with
  | TUnit, TUnit -> ()
  | TBool, TBool -> ()
  | TInt, TInt -> ()
  | TNat, TNat -> ()
  | TFloat, TFloat -> ()
  | TChar, TChar -> ()
  | TString, TString -> ()
  | TNever, _ -> ()  (* Never unifies with anything *)
  | _, TNever -> ()
  | TVar id1, TVar id2 when id1 = id2 -> ()
  | TVar id, t | t, TVar id ->
      if occurs id t then
        raise (Type_error (InfiniteType (id, t)))
      else
        extend_ty_subst st.subst id t
  | TRigid n1, TRigid n2 when n1 = n2 -> ()
  | TApp (n1, args1), TApp (n2, args2) when n1 = n2 ->
      if List.length args1 <> List.length args2 then
        raise (Type_error (CannotUnify (t1, t2)));
      List.iter2 (unify st) args1 args2
  | TArrow (a1, r1, e1), TArrow (a2, r2, e2) ->
      unify st a1 a2;
      unify st r1 r2;
      unify_effect st e1 e2
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 (unify st) ts1 ts2
  | TRecord r1, TRecord r2 ->
      unify_row st r1 r2
  | TRef t1, TRef t2 -> unify st t1 t2
  | TMut t1, TMut t2 -> unify st t1 t2
  | TOwn t1, TOwn t2 -> unify st t1 t2
  | TQuantified (q1, t1), TQuantified (q2, t2) when q1 = q2 ->
      unify st t1 t2
  | _ -> raise (Type_error (CannotUnify (t1, t2)))

and unify_row st r1 r2 =
  let r1 = apply_row_subst st.subst r1 in
  let r2 = apply_row_subst st.subst r2 in
  match r1, r2 with
  | REmpty, REmpty -> ()
  | RVar id1, RVar id2 when id1 = id2 -> ()
  | RVar id, r | r, RVar id ->
      extend_row_subst st.subst id r
  | RExtend (l1, t1, r1'), RExtend (l2, t2, r2') when l1 = l2 ->
      unify st t1 t2;
      unify_row st r1' r2'
  | RExtend (l1, t1, r1'), r2 ->
      (* Row rewriting: find l1 in r2 *)
      let rec find_and_remove label = function
        | REmpty -> None
        | RVar _ as r -> Some (TVar (fresh_tyvar ()), r)  (* Extend with fresh *)
        | RExtend (l, t, rest) when l = label ->
            Some (t, rest)
        | RExtend (l, t, rest) ->
            match find_and_remove label rest with
            | Some (found_t, rest') -> Some (found_t, RExtend (l, t, rest'))
            | None -> None
      in
      (match find_and_remove l1 r2 with
       | Some (t2, r2') ->
           unify st t1 t2;
           unify_row st r1' r2'
       | None ->
           raise (Type_error (RowMismatch (l1, None))))
  | _ -> raise (Type_error (CannotUnify (TRecord r1, TRecord r2)))

and unify_effect st e1 e2 =
  let e1 = apply_eff_subst st.subst e1 in
  let e2 = apply_eff_subst st.subst e2 in
  match e1, e2 with
  | EEmpty, EEmpty -> ()
  | EVar id1, EVar id2 when id1 = id2 -> ()
  | EVar id, e | e, EVar id ->
      extend_eff_subst st.subst id e
  | ECon (n1, args1), ECon (n2, args2) when n1 = n2 ->
      List.iter2 (unify st) args1 args2
  | EUnion (e1a, e1b), e2 ->
      (* Effect subsumption - both parts must be in e2 *)
      unify_effect st e1a e2;
      unify_effect st e1b e2
  | e1, EUnion (e2a, e2b) ->
      unify_effect st e1 e2a;
      unify_effect st e1 e2b
  | _ -> ()  (* Effects are more permissive *)

(** Convert AST type expression to internal type *)
let rec ast_to_type env (ty_expr: type_expr) : ty =
  match ty_expr with
  | TyVar id | TyCon id ->
      (match id.name with
       | "Int" -> TInt
       | "Bool" -> TBool
       | "Float" -> TFloat
       | "Char" -> TChar
       | "String" -> TString
       | "Unit" -> TUnit
       | "Never" -> TNever
       | "Nat" -> TNat
       | name ->
           match lookup_type env name with
           | Some ty -> ty
           | None -> TRigid name)  (* Assume it's a type variable *)
  | TyApp (id, args) ->
      let arg_types = List.map (function
        | TyArg t -> ast_to_type env t
        | NatArg _ -> TInt  (* Simplification: treat nat args as int *)
      ) args in
      TApp (id.name, arg_types)
  | TyArrow (t1, t2, eff_opt) ->
      let eff = match eff_opt with
        | None -> EEmpty
        | Some e -> ast_to_effect env e
      in
      TArrow (ast_to_type env t1, ast_to_type env t2, eff)
  | TyDepArrow { da_param_ty; da_ret_ty; da_eff; _ } ->
      (* Simplify dependent arrow to regular arrow for now *)
      let eff = match da_eff with
        | None -> EEmpty
        | Some e -> ast_to_effect env e
      in
      TArrow (ast_to_type env da_param_ty, ast_to_type env da_ret_ty, eff)
  | TyTuple tys ->
      TTuple (List.map (ast_to_type env) tys)
  | TyRecord (fields, rest) ->
      let row = List.fold_right (fun rf acc ->
        RExtend (rf.rf_name.name, ast_to_type env rf.rf_ty, acc)
      ) fields (match rest with
        | None -> REmpty
        | Some _ -> RVar (fresh_rowvar ()))
      in
      TRecord row
  | TyOwn t -> TOwn (ast_to_type env t)
  | TyRef t -> TRef (ast_to_type env t)
  | TyMut t -> TMut (ast_to_type env t)
  | TyRefined (t, _) -> ast_to_type env t  (* Ignore refinement for now *)
  | TyHole -> TVar (fresh_tyvar ())

and ast_to_effect _env (eff_expr: effect_expr) : effect =
  match eff_expr with
  | EffVar id -> ECon (id.name, [])
  | EffCon (id, _) -> ECon (id.name, [])
  | EffUnion (e1, e2) ->
      EUnion (ast_to_effect _env e1, ast_to_effect _env e2)

(** Synthesize type of expression *)
let rec synth st env (expr: expr) : ty =
  match expr with
  | ExprSpan (e, _) -> synth st env e

  | ExprLit lit -> synth_literal lit

  | ExprVar id ->
      (match lookup_var env id.name with
       | Some scheme -> instantiate st scheme
       | None ->
           add_error st (UnboundVariable (id.name, id.span));
           TVar (fresh_tyvar ()))

  | ExprLet { el_mut = _; el_pat; el_ty; el_value; el_body } ->
      let value_ty = match el_ty with
        | Some ty_expr ->
            let expected = ast_to_type env ty_expr in
            check st env el_value expected;
            expected
        | None -> synth st env el_value
      in
      let env' = child_env env in
      bind_pattern env' el_pat value_ty;
      (match el_body with
       | Some body -> synth st env' body
       | None -> TUnit)

  | ExprIf { ei_cond; ei_then; ei_else } ->
      check st env ei_cond TBool;
      let then_ty = synth st env ei_then in
      (match ei_else with
       | Some else_e ->
           check st env else_e then_ty;
           then_ty
       | None -> TUnit)

  | ExprMatch { em_scrutinee; em_arms } ->
      let scrutinee_ty = synth st env em_scrutinee in
      (match em_arms with
       | [] -> TVar (fresh_tyvar ())
       | arm :: rest ->
           let env' = child_env env in
           check_pattern st env' arm.ma_pat scrutinee_ty;
           Option.iter (fun guard -> check st env' guard TBool) arm.ma_guard;
           let result_ty = synth st env' arm.ma_body in
           List.iter (fun arm ->
             let arm_env = child_env env in
             check_pattern st arm_env arm.ma_pat scrutinee_ty;
             Option.iter (fun guard -> check st arm_env guard TBool) arm.ma_guard;
             check st arm_env arm.ma_body result_ty
           ) rest;
           result_ty)

  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
      let env' = child_env env in
      let param_types = List.map (fun p ->
        let ty = ast_to_type env p.p_ty in
        bind_var env' p.p_name.name (mono ty);
        ty
      ) elam_params in
      let body_ty = match elam_ret_ty with
        | Some ret_ty ->
            let expected = ast_to_type env ret_ty in
            check st env' elam_body expected;
            expected
        | None -> synth st env' elam_body
      in
      List.fold_right (fun param_ty acc ->
        TArrow (param_ty, acc, env.env_effects)
      ) param_types body_ty

  | ExprApp (func, args) ->
      let func_ty = synth st env func in
      synth_app st env func_ty args

  | ExprField (e, field) ->
      let record_ty = synth st env e in
      synth_field st record_ty field.name

  | ExprTupleIndex (e, idx) ->
      let tuple_ty = synth st env e in
      synth_tuple_index st tuple_ty idx

  | ExprIndex (arr, idx) ->
      let arr_ty = synth st env arr in
      check st env idx TInt;
      synth_index st arr_ty

  | ExprTuple exprs ->
      TTuple (List.map (synth st env) exprs)

  | ExprArray exprs ->
      (match exprs with
       | [] -> TApp ("Array", [TVar (fresh_tyvar ())])
       | e :: rest ->
           let elem_ty = synth st env e in
           List.iter (fun e -> check st env e elem_ty) rest;
           TApp ("Array", [elem_ty]))

  | ExprRecord { er_fields; er_spread } ->
      let base_row = match er_spread with
        | Some spread ->
            let spread_ty = synth st env spread in
            (match apply_subst st.subst spread_ty with
             | TRecord row -> row
             | _ -> REmpty)
        | None -> REmpty
      in
      let row = List.fold_right (fun (field_id, expr_opt) acc ->
        let ty = match expr_opt with
          | Some e -> synth st env e
          | None ->
              (* Shorthand: {x} means {x: x} *)
              match lookup_var env field_id.name with
              | Some scheme -> instantiate st scheme
              | None ->
                  add_error st (UnboundVariable (field_id.name, field_id.span));
                  TVar (fresh_tyvar ())
        in
        RExtend (field_id.name, ty, acc)
      ) er_fields base_row in
      TRecord row

  | ExprRowRestrict (e, field) ->
      let record_ty = synth st env e in
      synth_row_restrict st record_ty field.name

  | ExprBinary (e1, op, e2) ->
      synth_binary st env op e1 e2

  | ExprUnary (op, e) ->
      synth_unary st env op e

  | ExprBlock blk ->
      synth_block st env blk

  | ExprReturn e_opt ->
      (match e_opt with
       | Some e -> ignore (synth st env e)
       | None -> ());
      TNever  (* Return diverges *)

  | ExprVariant (type_id, _variant_id) ->
      (* For now, return a type variable that will be unified *)
      TApp (type_id.name, [])

  | ExprTry _ -> TVar (fresh_tyvar ())  (* TODO *)
  | ExprHandle _ -> TVar (fresh_tyvar ())  (* TODO *)
  | ExprResume _ -> TVar (fresh_tyvar ())  (* TODO *)
  | ExprUnsafe _ -> TVar (fresh_tyvar ())  (* TODO *)

and synth_literal = function
  | LitInt _ -> TInt
  | LitFloat _ -> TFloat
  | LitBool _ -> TBool
  | LitChar _ -> TChar
  | LitString _ -> TString
  | LitUnit _ -> TUnit

and synth_app st env func_ty args =
  let func_ty = apply_subst st.subst func_ty in
  match func_ty, args with
  | _, [] -> func_ty
  | TArrow (param_ty, ret_ty, _), arg :: rest ->
      check st env arg param_ty;
      synth_app st env ret_ty rest
  | TVar id, arg :: rest ->
      let arg_ty = synth st env arg in
      let ret_ty = TVar (fresh_tyvar ()) in
      extend_ty_subst st.subst id (TArrow (arg_ty, ret_ty, EEmpty));
      synth_app st env ret_ty rest
  | _, _ ->
      add_error st (NotAFunction (func_ty, None));
      TVar (fresh_tyvar ())

and synth_field st record_ty field_name =
  let record_ty = apply_subst st.subst record_ty in
  match record_ty with
  | TRecord row ->
      let rec find_field = function
        | REmpty ->
            add_error st (MissingField (field_name, None));
            TVar (fresh_tyvar ())
        | RVar _ ->
            TVar (fresh_tyvar ())  (* Unknown row - return fresh *)
        | RExtend (l, t, rest) ->
            if l = field_name then t
            else find_field rest
      in
      find_field row
  | TVar id ->
      let field_ty = TVar (fresh_tyvar ()) in
      let rest_row = RVar (fresh_rowvar ()) in
      let record_row = RExtend (field_name, field_ty, rest_row) in
      extend_ty_subst st.subst id (TRecord record_row);
      field_ty
  | _ ->
      add_error st (MissingField (field_name, None));
      TVar (fresh_tyvar ())

and synth_tuple_index st tuple_ty idx =
  let tuple_ty = apply_subst st.subst tuple_ty in
  match tuple_ty with
  | TTuple ts when idx >= 0 && idx < List.length ts ->
      List.nth ts idx
  | _ -> TVar (fresh_tyvar ())

and synth_index st arr_ty =
  let arr_ty = apply_subst st.subst arr_ty in
  match arr_ty with
  | TApp ("Array", [elem_ty]) -> elem_ty
  | TString -> TChar
  | _ -> TVar (fresh_tyvar ())

and synth_row_restrict st record_ty field_name =
  let record_ty = apply_subst st.subst record_ty in
  match record_ty with
  | TRecord row ->
      let rec remove_field = function
        | REmpty -> REmpty
        | RVar id -> RVar id
        | RExtend (l, t, rest) ->
            if l = field_name then rest
            else RExtend (l, t, remove_field rest)
      in
      TRecord (remove_field row)
  | _ -> TVar (fresh_tyvar ())

and synth_binary st env op e1 e2 =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv | OpMod ->
      (* Try int first, then float *)
      let t1 = synth st env e1 in
      let t2 = synth st env e2 in
      (try unify st t1 TInt; unify st t2 TInt; TInt
       with Type_error _ ->
         try unify st t1 TFloat; unify st t2 TFloat; TFloat
         with Type_error _ ->
           (* For + also allow string concat *)
           if op = OpAdd then begin
             try unify st t1 TString; unify st t2 TString; TString
             with Type_error _ -> TInt
           end else TInt)
  | OpEq | OpNe ->
      let t1 = synth st env e1 in
      check st env e2 t1;
      TBool
  | OpLt | OpLe | OpGt | OpGe ->
      let t1 = synth st env e1 in
      check st env e2 t1;
      TBool
  | OpAnd | OpOr ->
      check st env e1 TBool;
      check st env e2 TBool;
      TBool
  | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr ->
      check st env e1 TInt;
      check st env e2 TInt;
      TInt

and synth_unary st env op e =
  match op with
  | OpNeg ->
      let t = synth st env e in
      (try unify st t TInt; TInt
       with Type_error _ -> unify st t TFloat; TFloat)
  | OpNot -> check st env e TBool; TBool
  | OpBitNot -> check st env e TInt; TInt
  | OpRef -> TRef (synth st env e)
  | OpDeref ->
      let t = synth st env e in
      let elem_ty = TVar (fresh_tyvar ()) in
      (try unify st t (TRef elem_ty); elem_ty
       with Type_error _ ->
         try unify st t (TMut elem_ty); elem_ty
         with Type_error _ -> elem_ty)

and synth_block st env { blk_stmts; blk_expr } =
  let env' = child_env env in
  List.iter (check_stmt st env') blk_stmts;
  match blk_expr with
  | Some e -> synth st env' e
  | None -> TUnit

and check_stmt st env = function
  | StmtLet { sl_mut = _; sl_pat; sl_ty; sl_value } ->
      let value_ty = match sl_ty with
        | Some ty_expr ->
            let expected = ast_to_type env ty_expr in
            check st env sl_value expected;
            expected
        | None -> synth st env sl_value
      in
      bind_pattern env sl_pat value_ty
  | StmtExpr e ->
      ignore (synth st env e)
  | StmtAssign (target, _, value) ->
      let target_ty = synth st env target in
      check st env value target_ty
  | StmtWhile (cond, body) ->
      check st env cond TBool;
      ignore (synth_block st env body)
  | StmtFor (pat, iter, body) ->
      let iter_ty = synth st env iter in
      let elem_ty = match apply_subst st.subst iter_ty with
        | TApp ("Array", [t]) -> t
        | TString -> TChar
        | _ -> TVar (fresh_tyvar ())
      in
      let env' = child_env env in
      bind_pattern env' pat elem_ty;
      ignore (synth_block st env' body)

(** Check expression against expected type *)
and check st env expr expected =
  let actual = synth st env expr in
  try unify st actual expected
  with Type_error _ ->
    add_error st (TypeMismatch (expected, actual, None))

(** Bind pattern variables with types *)
and bind_pattern env pat ty =
  match pat with
  | PatWildcard _ -> ()
  | PatVar id -> bind_var env id.name (mono ty)
  | PatLit _ -> ()
  | PatCon (_, pats) ->
      (* For now, assume tuple-like destructuring *)
      (match ty with
       | TTuple ts when List.length ts = List.length pats ->
           List.iter2 (bind_pattern env) pats ts
       | _ -> ())
  | PatTuple pats ->
      (match ty with
       | TTuple ts when List.length ts = List.length pats ->
           List.iter2 (bind_pattern env) pats ts
       | _ -> ())
  | PatRecord (fields, _) ->
      (match ty with
       | TRecord row ->
           List.iter (fun (field_id, pat_opt) ->
             let field_ty = find_row_field row field_id.name in
             match pat_opt with
             | Some p -> bind_pattern env p field_ty
             | None -> bind_var env field_id.name (mono field_ty)
           ) fields
       | _ -> ())
  | PatOr (p1, _) -> bind_pattern env p1 ty
  | PatAs (id, p) ->
      bind_var env id.name (mono ty);
      bind_pattern env p ty

and find_row_field row field_name =
  match row with
  | REmpty -> TVar (fresh_tyvar ())
  | RVar _ -> TVar (fresh_tyvar ())
  | RExtend (l, t, rest) ->
      if l = field_name then t
      else find_row_field rest field_name

and check_pattern st env pat expected =
  match pat with
  | PatWildcard _ -> ()
  | PatVar id -> bind_var env id.name (mono expected)
  | PatLit lit ->
      let lit_ty = synth_literal lit in
      (try unify st lit_ty expected with _ -> ())
  | PatCon (_, pats) ->
      (* Simplified: assume it's a variant with tuple fields *)
      (match apply_subst st.subst expected with
       | TTuple ts when List.length ts = List.length pats ->
           List.iter2 (check_pattern st env) pats ts
       | _ -> List.iter (fun p -> check_pattern st env p (TVar (fresh_tyvar ()))) pats)
  | PatTuple pats ->
      (match apply_subst st.subst expected with
       | TTuple ts when List.length ts = List.length pats ->
           List.iter2 (check_pattern st env) pats ts
       | _ -> ())
  | PatRecord (fields, _) ->
      List.iter (fun (field_id, pat_opt) ->
        let field_ty = match apply_subst st.subst expected with
          | TRecord row -> find_row_field row field_id.name
          | _ -> TVar (fresh_tyvar ())
        in
        match pat_opt with
        | Some p -> check_pattern st env p field_ty
        | None -> bind_var env field_id.name (mono field_ty)
      ) fields
  | PatOr (p1, p2) ->
      check_pattern st env p1 expected;
      check_pattern st env p2 expected
  | PatAs (id, p) ->
      bind_var env id.name (mono expected);
      check_pattern st env p expected

(** Type check a function declaration *)
let check_fn_decl st env (decl: fn_decl) =
  let env' = child_env env in
  (* Add type parameters *)
  List.iter (fun tp ->
    bind_type env' tp.tp_name.name (TRigid tp.tp_name.name)
  ) decl.fd_type_params;
  (* Add parameters *)
  let param_types = List.map (fun p ->
    let ty = ast_to_type env' p.p_ty in
    bind_var env' p.p_name.name (mono ty);
    ty
  ) decl.fd_params in
  (* Check return type *)
  let ret_ty = match decl.fd_ret_ty with
    | Some ty_expr -> ast_to_type env' ty_expr
    | None -> TVar (fresh_tyvar ())
  in
  (* Check body *)
  let env_with_effects = match decl.fd_eff with
    | Some eff -> with_effects env' (ast_to_effect env' eff)
    | None -> env'
  in
  (match decl.fd_body with
   | FnBlock blk -> check st env_with_effects (ExprBlock blk) ret_ty
   | FnExpr e -> check st env_with_effects e ret_ty);
  (* Return function type *)
  List.fold_right (fun param_ty acc ->
    TArrow (param_ty, acc, env_with_effects.env_effects)
  ) param_types ret_ty

(** Type check a program *)
let check_program (prog: program) =
  let st = create_state () in
  let env = empty_env () in
  (* Add built-in types *)
  bind_type env "Int" TInt;
  bind_type env "Bool" TBool;
  bind_type env "Float" TFloat;
  bind_type env "Char" TChar;
  bind_type env "String" TString;
  bind_type env "Unit" TUnit;
  bind_type env "Never" TNever;
  bind_type env "Nat" TNat;
  (* Add built-in functions *)
  bind_var env "print" (mono (TArrow (TVar (fresh_tyvar ()), TUnit, EEmpty)));
  bind_var env "println" (mono (TArrow (TVar (fresh_tyvar ()), TUnit, EEmpty)));
  bind_var env "str" (mono (TArrow (TVar (fresh_tyvar ()), TString, EEmpty)));
  bind_var env "len" (mono (TArrow (TVar (fresh_tyvar ()), TInt, EEmpty)));
  bind_var env "type_of" (mono (TArrow (TVar (fresh_tyvar ()), TString, EEmpty)));
  bind_var env "range" (mono (TArrow (TInt, TApp ("Array", [TInt]), EEmpty)));
  bind_var env "map" (mono (TArrow (TArrow (TVar 0, TVar 1, EEmpty), TArrow (TApp ("Array", [TVar 0]), TApp ("Array", [TVar 1]), EEmpty), EEmpty)));
  bind_var env "filter" (mono (TArrow (TArrow (TVar 0, TBool, EEmpty), TArrow (TApp ("Array", [TVar 0]), TApp ("Array", [TVar 0]), EEmpty), EEmpty)));
  bind_var env "fold" (mono (TArrow (TArrow (TVar 0, TArrow (TVar 1, TVar 0, EEmpty), EEmpty), TArrow (TVar 0, TArrow (TApp ("Array", [TVar 1]), TVar 0, EEmpty), EEmpty), EEmpty)));
  (* First pass: collect type declarations *)
  List.iter (fun decl ->
    match decl with
    | TopType td ->
        bind_type env td.td_name.name (TApp (td.td_name.name, []))
    | _ -> ()
  ) prog.prog_decls;
  (* Second pass: collect function declarations *)
  List.iter (fun decl ->
    match decl with
    | TopFn fd ->
        let fn_ty = TVar (fresh_tyvar ()) in
        bind_var env fd.fd_name.name (mono fn_ty)
    | TopConst { tc_name; tc_ty; _ } ->
        bind_var env tc_name.name (mono (ast_to_type env tc_ty))
    | _ -> ()
  ) prog.prog_decls;
  (* Third pass: type check all declarations *)
  List.iter (fun decl ->
    match decl with
    | TopFn fd ->
        let fn_ty = check_fn_decl st env fd in
        (* Update the binding with the inferred type *)
        bind_var env fd.fd_name.name (generalize env fn_ty)
    | TopConst { tc_name; tc_ty; tc_value; _ } ->
        let expected = ast_to_type env tc_ty in
        check st env tc_value expected
    | _ -> ()
  ) prog.prog_decls;
  List.rev st.errors
