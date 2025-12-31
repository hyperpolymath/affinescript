(* SPDX-License-Identifier: Apache-2.0 OR MIT *)
(* Copyright 2024 AffineScript Contributors *)

(** Bidirectional type checker.

    This module implements bidirectional type checking for AffineScript.
    It uses synthesis (inference) and checking modes, with the unification
    engine handling type variable instantiation.
*)

open Ast
open Types

(** Type checking errors *)
type type_error =
  | UnificationFailed of Unify.unify_error * Span.t
  | ExpectedFunction of ty * Span.t
  | ExpectedRecord of ty * Span.t
  | ExpectedTuple of ty * Span.t
  | UndefinedField of string * Span.t
  | ArityMismatch of int * int * Span.t
  | CannotInfer of Span.t
  | TypeAnnotationRequired of Span.t
  | InvalidPattern of Span.t
  | QuantityError of string * Span.t
  | EffectError of string * Span.t
  | BorrowError of string * Span.t
[@@deriving show]

type 'a result = ('a, type_error) Result.t

(** Type checking context *)
type context = {
  (** Symbol table with resolved names *)
  symbols : Symbol.t;

  (** Current let-generalization level *)
  level : int;

  (** Variable types *)
  var_types : (Symbol.symbol_id, scheme) Hashtbl.t;

  (** Current effect context *)
  current_effect : effect;
}

(* Result bind - define before use *)
let ( let* ) = Result.bind

(** Create a new type checking context *)
let create_context (symbols : Symbol.t) : context =
  {
    symbols;
    level = 0;
    var_types = Hashtbl.create 64;
    current_effect = EPure;
  }

(** Enter a new let-binding level *)
let enter_level (ctx : context) : context =
  { ctx with level = ctx.level + 1 }

(** Generalize a type at the current level *)
let generalize (ctx : context) (ty : ty) : scheme =
  (* Collect all unbound type variables at level > ctx.level *)
  let rec collect_tyvars (ty : ty) (acc : (tyvar * kind) list) : (tyvar * kind) list =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, lvl) when lvl > ctx.level ->
          if List.mem_assoc v acc then acc
          else (v, KType) :: acc  (* TODO: Track actual kinds *)
        | _ -> acc
      end
    | TApp (t, args) ->
      List.fold_left (fun acc t -> collect_tyvars t acc) (collect_tyvars t acc) args
    | TArrow (a, b, _) ->
      collect_tyvars b (collect_tyvars a acc)
    | TTuple ts ->
      List.fold_left (fun acc t -> collect_tyvars t acc) acc ts
    | TRecord row | TVariant row ->
      collect_row_tyvars row acc
    | TForall (_, _, body) | TExists (_, _, body) ->
      collect_tyvars body acc
    | TRef t | TMut t | TOwn t ->
      collect_tyvars t acc
    | TRefined (t, _) ->
      collect_tyvars t acc
    | _ -> acc
  and collect_row_tyvars (row : row) (acc : (tyvar * kind) list) : (tyvar * kind) list =
    match repr_row row with
    | REmpty -> acc
    | RExtend (_, ty, rest) ->
      collect_row_tyvars rest (collect_tyvars ty acc)
    | RVar _ -> acc
  in
  let tyvars = collect_tyvars ty [] in
  { sc_tyvars = tyvars; sc_effvars = []; sc_rowvars = []; sc_body = ty }

(** Instantiate a type scheme *)
let instantiate (ctx : context) (scheme : scheme) : ty =
  let subst = List.map (fun (v, _k) ->
    (v, fresh_tyvar ctx.level)
  ) scheme.sc_tyvars in
  let rec apply_subst (ty : ty) : ty =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, _) ->
          begin match List.assoc_opt v subst with
            | Some ty' -> ty'
            | None -> ty
          end
        | Link _ -> failwith "instantiate: unexpected Link"
      end
    | TApp (t, args) ->
      TApp (apply_subst t, List.map apply_subst args)
    | TArrow (a, b, eff) ->
      TArrow (apply_subst a, apply_subst b, eff)
    | TTuple ts ->
      TTuple (List.map apply_subst ts)
    | TRecord row ->
      TRecord (apply_subst_row row)
    | TVariant row ->
      TVariant (apply_subst_row row)
    | TForall (v, k, body) ->
      TForall (v, k, apply_subst body)
    | TRef t -> TRef (apply_subst t)
    | TMut t -> TMut (apply_subst t)
    | TOwn t -> TOwn (apply_subst t)
    | TRefined (t, p) -> TRefined (apply_subst t, p)
    | t -> t
  and apply_subst_row (row : row) : row =
    match repr_row row with
    | REmpty -> REmpty
    | RExtend (l, ty, rest) ->
      RExtend (l, apply_subst ty, apply_subst_row rest)
    | RVar _ as rv -> rv
  in
  apply_subst scheme.sc_body

(** Look up a variable's type *)
let lookup_var (ctx : context) (id : ident) : ty result =
  match Symbol.lookup ctx.symbols id.name with
  | Some sym ->
    begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
      | Some scheme -> Ok (instantiate ctx scheme)
      | None ->
        (* Variable exists but not yet typed - this shouldn't happen after resolve *)
        Error (CannotInfer id.span)
    end
  | None ->
    Error (CannotInfer id.span)

(** Bind a variable with a type *)
let bind_var (ctx : context) (id : ident) (ty : ty) : unit =
  match Symbol.lookup ctx.symbols id.name with
  | Some sym ->
    let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None -> ()

(** Bind a variable with a scheme (polymorphic) *)
let bind_var_scheme (ctx : context) (id : ident) (scheme : scheme) : unit =
  match Symbol.lookup ctx.symbols id.name with
  | Some sym ->
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None -> ()

(** Convert AST type to internal type *)
let rec ast_to_ty (ctx : context) (ty : type_expr) : ty =
  match ty with
  | TyVar _id -> fresh_tyvar ctx.level  (* TODO: Look up type variable *)
  | TyCon id ->
    begin match id.name with
      | "Unit" -> ty_unit
      | "Bool" -> ty_bool
      | "Int" -> ty_int
      | "Float" -> ty_float
      | "Char" -> ty_char
      | "String" -> ty_string
      | "Never" -> ty_never
      | name -> TCon name
    end
  | TyApp (id, args) ->
    TApp (TCon id.name, List.map (ast_to_ty_arg ctx) args)
  | TyArrow (a, b, eff) ->
    let eff' = match eff with
      | Some e -> ast_to_eff ctx e
      | None -> EPure
    in
    TArrow (ast_to_ty ctx a, ast_to_ty ctx b, eff')
  | TyTuple tys ->
    TTuple (List.map (ast_to_ty ctx) tys)
  | TyRecord (fields, rest) ->
    let row = List.fold_right (fun field acc ->
      RExtend (field.rf_name.name, ast_to_ty ctx field.rf_ty, acc)
    ) fields (match rest with
      | Some _ -> fresh_rowvar ctx.level
      | None -> REmpty
    ) in
    TRecord row
  | TyOwn t -> TOwn (ast_to_ty ctx t)
  | TyRef t -> TRef (ast_to_ty ctx t)
  | TyMut t -> TMut (ast_to_ty ctx t)
  | TyRefined (t, _pred) ->
    (* TODO: Convert predicate *)
    TRefined (ast_to_ty ctx t, PTrue)
  | TyDepArrow _ -> fresh_tyvar ctx.level  (* TODO: Handle dependent arrows *)
  | TyHole -> fresh_tyvar ctx.level

and ast_to_ty_arg (ctx : context) (arg : type_arg) : ty =
  match arg with
  | TyArg ty -> ast_to_ty ctx ty
  | NatArg _ -> TNat (NLit 0)  (* TODO: Convert nat expr *)

and ast_to_eff (ctx : context) (eff : effect_expr) : effect =
  match eff with
  | EffCon (id, _) -> ESingleton id.name
  | EffVar _id -> fresh_effvar ctx.level
  | EffUnion (e1, e2) -> EUnion [ast_to_eff ctx e1; ast_to_eff ctx e2]

(** Get span from an expression *)
let rec expr_span (expr : expr) : Span.t =
  match expr with
  | ExprSpan (_, span) -> span
  | ExprLit lit -> lit_span lit
  | ExprVar id -> id.span
  | ExprLet { el_pat; _ } -> pattern_span el_pat
  | ExprIf { ei_cond; _ } -> expr_span ei_cond
  | ExprMatch { em_scrutinee; _ } -> expr_span em_scrutinee
  | ExprLambda { elam_params; _ } ->
    begin match elam_params with
      | p :: _ -> p.p_name.span
      | [] -> Span.dummy
    end
  | ExprApp (f, _) -> expr_span f
  | ExprField (e, _) -> expr_span e
  | ExprTupleIndex (e, _) -> expr_span e
  | ExprIndex (e, _) -> expr_span e
  | ExprTuple exprs ->
    begin match exprs with
      | e :: _ -> expr_span e
      | [] -> Span.dummy
    end
  | ExprArray exprs ->
    begin match exprs with
      | e :: _ -> expr_span e
      | [] -> Span.dummy
    end
  | ExprRecord { er_fields; _ } ->
    begin match er_fields with
      | (id, _) :: _ -> id.span
      | [] -> Span.dummy
    end
  | ExprRowRestrict (e, _) -> expr_span e
  | ExprBinary (e, _, _) -> expr_span e
  | ExprUnary (_, e) -> expr_span e
  | ExprBlock { blk_stmts; blk_expr } ->
    begin match blk_stmts with
      | StmtLet { sl_pat; _ } :: _ -> pattern_span sl_pat
      | StmtExpr e :: _ -> expr_span e
      | StmtAssign (e, _, _) :: _ -> expr_span e
      | StmtWhile (e, _) :: _ -> expr_span e
      | StmtFor (p, _, _) :: _ -> pattern_span p
      | [] -> match blk_expr with Some e -> expr_span e | None -> Span.dummy
    end
  | ExprReturn _ -> Span.dummy
  | ExprTry _ -> Span.dummy
  | ExprHandle { eh_body; _ } -> expr_span eh_body
  | ExprResume _ -> Span.dummy
  | ExprUnsafe _ -> Span.dummy
  | ExprVariant (id, _) -> id.span

and lit_span (lit : literal) : Span.t =
  match lit with
  | LitInt (_, span) -> span
  | LitFloat (_, span) -> span
  | LitBool (_, span) -> span
  | LitChar (_, span) -> span
  | LitString (_, span) -> span
  | LitUnit span -> span

and pattern_span (pat : pattern) : Span.t =
  match pat with
  | PatWildcard span -> span
  | PatVar id -> id.span
  | PatLit lit -> lit_span lit
  | PatCon (id, _) -> id.span
  | PatTuple pats ->
    begin match pats with
      | p :: _ -> pattern_span p
      | [] -> Span.dummy
    end
  | PatRecord ((id, _) :: _, _) -> id.span
  | PatRecord ([], _) -> Span.dummy
  | PatOr (p1, _) -> pattern_span p1
  | PatAs (id, _) -> id.span

(** Synthesize (infer) the type of an expression *)
let rec synth (ctx : context) (expr : expr) : (ty * effect) result =
  match expr with
  | ExprVar id ->
    let* ty = lookup_var ctx id in
    Ok (ty, EPure)

  | ExprLit lit ->
    let ty = synth_literal lit in
    Ok (ty, EPure)

  | ExprApp (func, args) ->
    let span = expr_span expr in
    let* (func_ty, func_eff) = synth ctx func in
    synth_app ctx func_ty func_eff args span

  | ExprLambda lam ->
    (* For lambdas, we need annotations or we infer fresh variables *)
    let param_tys = List.map (fun param ->
      (param.p_name, ast_to_ty ctx param.p_ty)
    ) lam.elam_params in
    (* Bind parameters *)
    List.iter (fun (id, ty) -> bind_var ctx id ty) param_tys;
    (* Infer body *)
    let* (body_ty, body_eff) = synth ctx lam.elam_body in
    (* Build arrow type *)
    let ty = List.fold_right (fun (_, param_ty) acc ->
      TArrow (param_ty, acc, body_eff)
    ) param_tys body_ty in
    Ok (ty, EPure)

  | ExprLet lb ->
    (* Infer RHS at higher level for generalization *)
    let ctx' = enter_level ctx in
    let* (rhs_ty, rhs_eff) = synth ctx' lb.el_value in
    (* Generalize *)
    let scheme = generalize ctx rhs_ty in
    (* Bind pattern *)
    let* () = bind_pattern ctx lb.el_pat scheme in
    (* Infer body if present *)
    begin match lb.el_body with
      | Some body ->
        let* (body_ty, body_eff) = synth ctx body in
        Ok (body_ty, union_eff [rhs_eff; body_eff])
      | None ->
        Ok (ty_unit, rhs_eff)
    end

  | ExprIf ei ->
    let* cond_eff = check ctx ei.ei_cond ty_bool in
    let* (then_ty, then_eff) = synth ctx ei.ei_then in
    begin match ei.ei_else with
      | Some else_expr ->
        let* else_eff = check ctx else_expr then_ty in
        Ok (then_ty, union_eff [cond_eff; then_eff; else_eff])
      | None ->
        Ok (ty_unit, union_eff [cond_eff; then_eff])
    end

  | ExprMatch em ->
    let* (scrut_ty, scrut_eff) = synth ctx em.em_scrutinee in
    begin match em.em_arms with
      | [] -> Error (CannotInfer (expr_span expr))
      | first_arm :: rest_arms ->
        let* () = check_pattern ctx first_arm.ma_pat scrut_ty in
        let* (arm_ty, arm_eff) = synth ctx first_arm.ma_body in
        let* effs = List.fold_left (fun acc arm ->
          let* effs = acc in
          let* () = check_pattern ctx arm.ma_pat scrut_ty in
          let* eff = check ctx arm.ma_body arm_ty in
          Ok (eff :: effs)
        ) (Ok [arm_eff]) rest_arms in
        Ok (arm_ty, union_eff (scrut_eff :: effs))
    end

  | ExprTuple exprs ->
    let* results = synth_list ctx exprs in
    let tys = List.map fst results in
    let effs = List.map snd results in
    Ok (TTuple tys, union_eff effs)

  | ExprArray exprs ->
    begin match exprs with
      | [] -> Ok (TApp (TCon "Array", [fresh_tyvar ctx.level]), EPure)
      | first :: rest ->
        let* (elem_ty, first_eff) = synth ctx first in
        let* effs = List.fold_left (fun acc e ->
          let* effs = acc in
          let* eff = check ctx e elem_ty in
          Ok (eff :: effs)
        ) (Ok [first_eff]) rest in
        Ok (TApp (TCon "Array", [elem_ty]), union_eff effs)
    end

  | ExprRecord er ->
    let* field_results = synth_record_fields ctx er.er_fields in
    let row = List.fold_right (fun (name, ty, _eff) acc ->
      RExtend (name, ty, acc)
    ) field_results REmpty in
    let effs = List.map (fun (_, _, eff) -> eff) field_results in
    Ok (TRecord row, union_eff effs)

  | ExprField (base, field) ->
    let span = expr_span expr in
    let* (base_ty, base_eff) = synth ctx base in
    begin match repr base_ty with
      | TRecord row ->
        begin match find_field field.name row with
          | Some ty -> Ok (ty, base_eff)
          | None -> Error (UndefinedField (field.name, span))
        end
      | TVar _ as tv ->
        let field_ty = fresh_tyvar ctx.level in
        let rest = fresh_rowvar ctx.level in
        let row = RExtend (field.name, field_ty, rest) in
        begin match Unify.unify tv (TRecord row) with
          | Ok () -> Ok (field_ty, base_eff)
          | Error e -> Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (ExpectedRecord (base_ty, span))
    end

  | ExprTupleIndex (base, idx) ->
    let span = expr_span expr in
    let* (base_ty, base_eff) = synth ctx base in
    begin match repr base_ty with
      | TTuple tys when idx >= 0 && idx < List.length tys ->
        Ok (List.nth tys idx, base_eff)
      | TTuple _ ->
        Error (ArityMismatch (idx + 1, 0, span))
      | _ ->
        Error (ExpectedTuple (base_ty, span))
    end

  | ExprIndex (arr, idx_expr) ->
    let span = expr_span expr in
    let* (arr_ty, arr_eff) = synth ctx arr in
    let* idx_eff = check ctx idx_expr ty_int in
    begin match repr arr_ty with
      | TApp (TCon "Array", [elem_ty]) ->
        Ok (elem_ty, union_eff [arr_eff; idx_eff])
      | TVar _ as tv ->
        let elem_ty = fresh_tyvar ctx.level in
        begin match Unify.unify tv (TApp (TCon "Array", [elem_ty])) with
          | Ok () -> Ok (elem_ty, union_eff [arr_eff; idx_eff])
          | Error e -> Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (CannotInfer span)
    end

  | ExprBlock blk ->
    synth_block ctx blk

  | ExprBinary (left, op, right) ->
    let span = expr_span expr in
    synth_binop ctx left op right span

  | ExprUnary (op, operand) ->
    synth_unary ctx op operand

  | ExprReturn e_opt ->
    (* Return types need context from enclosing function *)
    begin match e_opt with
      | Some e ->
        let* (ty, eff) = synth ctx e in
        Ok (ty, eff)
      | None ->
        Ok (ty_unit, EPure)
    end

  | ExprHandle eh ->
    let* (body_ty, _body_eff) = synth ctx eh.eh_body in
    (* TODO: Check handlers and compute resulting effect *)
    Ok (body_ty, EPure)

  | ExprResume e_opt ->
    begin match e_opt with
      | Some e ->
        let* (ty, eff) = synth ctx e in
        Ok (ty, eff)
      | None ->
        Ok (ty_unit, EPure)
    end

  | ExprTry et ->
    let* (body_ty, body_eff) = synth_block ctx et.et_body in
    (* TODO: Check catch arms and finally block *)
    Ok (body_ty, body_eff)

  | ExprRowRestrict (base, _field) ->
    let* (base_ty, base_eff) = synth ctx base in
    (* Row restriction removes a field from a record type *)
    Ok (base_ty, base_eff)  (* TODO: Proper row restriction *)

  | ExprUnsafe _ ->
    Ok (fresh_tyvar ctx.level, EPure)

  | ExprVariant (ty_id, _variant_id) ->
    Ok (TCon ty_id.name, EPure)

  | ExprSpan (e, _span) ->
    synth ctx e

and synth_app (ctx : context) (func_ty : ty) (func_eff : effect)
    (args : expr list) (span : Span.t) : (ty * effect) result =
  match args with
  | [] -> Ok (func_ty, func_eff)
  | arg :: rest ->
    begin match repr func_ty with
      | TArrow (param_ty, ret_ty, call_eff) ->
        let* arg_eff = check ctx arg param_ty in
        synth_app ctx ret_ty (union_eff [func_eff; arg_eff; call_eff]) rest span
      | TVar _ as tv ->
        let param_ty = fresh_tyvar ctx.level in
        let ret_ty = fresh_tyvar ctx.level in
        let call_eff = fresh_effvar ctx.level in
        begin match Unify.unify tv (TArrow (param_ty, ret_ty, call_eff)) with
          | Ok () ->
            let* arg_eff = check ctx arg param_ty in
            synth_app ctx ret_ty (union_eff [func_eff; arg_eff; call_eff]) rest span
          | Error e ->
            Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (ExpectedFunction (func_ty, span))
    end

(** Check an expression against an expected type *)
and check (ctx : context) (expr : expr) (expected : ty) : effect result =
  match (expr, repr expected) with
  (* Lambda checking *)
  | (ExprLambda lam, TArrow (param_ty, ret_ty, arr_eff)) ->
    begin match lam.elam_params with
      | [param] ->
        bind_var ctx param.p_name param_ty;
        let* body_eff = check ctx lam.elam_body ret_ty in
        begin match Unify.unify_eff body_eff arr_eff with
          | Ok () -> Ok EPure
          | Error e -> Error (UnificationFailed (e, Span.dummy))
        end
      | _ ->
        (* Multi-param lambdas: fall through to subsumption *)
        check_subsumption ctx expr expected
    end

  (* If checking *)
  | (ExprIf ei, _) ->
    let* cond_eff = check ctx ei.ei_cond ty_bool in
    let* then_eff = check ctx ei.ei_then expected in
    begin match ei.ei_else with
      | Some else_expr ->
        let* else_eff = check ctx else_expr expected in
        Ok (union_eff [cond_eff; then_eff; else_eff])
      | None ->
        (* If without else must have unit type *)
        begin match Unify.unify expected ty_unit with
          | Ok () -> Ok (union_eff [cond_eff; then_eff])
          | Error e -> Error (UnificationFailed (e, expr_span expr))
        end
    end

  (* Tuple checking *)
  | (ExprTuple exprs, TTuple tys) when List.length exprs = List.length tys ->
    let* effs = check_list ctx exprs tys in
    Ok (union_eff effs)

  (* Match checking *)
  | (ExprMatch em, _) ->
    let* (scrut_ty, scrut_eff) = synth ctx em.em_scrutinee in
    let* effs = List.fold_left (fun acc arm ->
      let* effs = acc in
      let* () = check_pattern ctx arm.ma_pat scrut_ty in
      let* eff = check ctx arm.ma_body expected in
      Ok (eff :: effs)
    ) (Ok [scrut_eff]) em.em_arms in
    Ok (union_eff effs)

  (* Block checking *)
  | (ExprBlock blk, _) ->
    check_block ctx blk expected

  (* Subsumption: synth and unify *)
  | _ ->
    check_subsumption ctx expr expected

and check_subsumption (ctx : context) (expr : expr) (expected : ty) : effect result =
  let* (actual, eff) = synth ctx expr in
  match Unify.unify actual expected with
  | Ok () -> Ok eff
  | Error e -> Error (UnificationFailed (e, expr_span expr))

and synth_list (ctx : context) (exprs : expr list) : ((ty * effect) list) result =
  List.fold_right (fun expr acc ->
    match acc with
    | Error e -> Error e
    | Ok results ->
      match synth ctx expr with
      | Error e -> Error e
      | Ok result -> Ok (result :: results)
  ) exprs (Ok [])

and check_list (ctx : context) (exprs : expr list) (tys : ty list) : (effect list) result =
  List.fold_right2 (fun expr ty acc ->
    match acc with
    | Error e -> Error e
    | Ok effs ->
      match check ctx expr ty with
      | Error e -> Error e
      | Ok eff -> Ok (eff :: effs)
  ) exprs tys (Ok [])

and synth_record_fields (ctx : context) (fields : (ident * expr option) list)
    : ((string * ty * effect) list) result =
  List.fold_right (fun (id, expr_opt) acc ->
    match acc with
    | Error e -> Error e
    | Ok results ->
      match expr_opt with
      | Some expr ->
        begin match synth ctx expr with
          | Error e -> Error e
          | Ok (ty, eff) -> Ok ((id.name, ty, eff) :: results)
        end
      | None ->
        (* Punning: {x} is short for {x: x} *)
        begin match lookup_var ctx id with
          | Error e -> Error e
          | Ok ty -> Ok ((id.name, ty, EPure) :: results)
        end
  ) fields (Ok [])

and synth_block (ctx : context) (blk : block) : (ty * effect) result =
  let* effs = List.fold_left (fun acc stmt ->
    let* effs = acc in
    let* eff = synth_stmt ctx stmt in
    Ok (eff :: effs)
  ) (Ok []) blk.blk_stmts in
  match blk.blk_expr with
  | Some e ->
    let* (ty, eff) = synth ctx e in
    Ok (ty, union_eff (eff :: effs))
  | None ->
    Ok (ty_unit, union_eff effs)

and check_block (ctx : context) (blk : block) (expected : ty) : effect result =
  let* effs = List.fold_left (fun acc stmt ->
    let* effs = acc in
    let* eff = synth_stmt ctx stmt in
    Ok (eff :: effs)
  ) (Ok []) blk.blk_stmts in
  match blk.blk_expr with
  | Some e ->
    let* eff = check ctx e expected in
    Ok (union_eff (eff :: effs))
  | None ->
    begin match Unify.unify expected ty_unit with
      | Ok () -> Ok (union_eff effs)
      | Error e -> Error (UnificationFailed (e, Span.dummy))
    end

and synth_stmt (ctx : context) (stmt : stmt) : effect result =
  match stmt with
  | StmtLet sl ->
    let ctx' = enter_level ctx in
    let* (rhs_ty, rhs_eff) = synth ctx' sl.sl_value in
    let scheme = generalize ctx rhs_ty in
    let* () = bind_pattern ctx sl.sl_pat scheme in
    Ok rhs_eff
  | StmtExpr e ->
    let* (_, eff) = synth ctx e in
    Ok eff
  | StmtAssign (lhs, _op, rhs) ->
    let* (lhs_ty, lhs_eff) = synth ctx lhs in
    let* rhs_eff = check ctx rhs lhs_ty in
    Ok (union_eff [lhs_eff; rhs_eff])
  | StmtWhile (cond, body) ->
    let* cond_eff = check ctx cond ty_bool in
    let* (_, body_eff) = synth_block ctx body in
    Ok (union_eff [cond_eff; body_eff])
  | StmtFor (pat, iter, body) ->
    let* (iter_ty, iter_eff) = synth ctx iter in
    (* Assume iterator yields element type *)
    let elem_ty = fresh_tyvar ctx.level in
    let* () = check_pattern ctx pat elem_ty in
    let* (_, body_eff) = synth_block ctx body in
    let _ = iter_ty in  (* Silence unused warning for now *)
    Ok (union_eff [iter_eff; body_eff])

and synth_binop (ctx : context) (left : expr) (op : binary_op) (right : expr)
    (span : Span.t) : (ty * effect) result =
  let* (left_ty, left_eff) = synth ctx left in
  let* (right_ty, right_eff) = synth ctx right in
  let eff = union_eff [left_eff; right_eff] in
  match op with
  | OpAdd | OpSub | OpMul | OpDiv | OpMod ->
    begin match Unify.unify left_ty ty_int, Unify.unify right_ty ty_int with
      | Ok (), Ok () -> Ok (ty_int, eff)
      | Error e, _ | _, Error e -> Error (UnificationFailed (e, span))
    end
  | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe ->
    begin match Unify.unify left_ty right_ty with
      | Ok () -> Ok (ty_bool, eff)
      | Error e -> Error (UnificationFailed (e, span))
    end
  | OpAnd | OpOr ->
    begin match Unify.unify left_ty ty_bool, Unify.unify right_ty ty_bool with
      | Ok (), Ok () -> Ok (ty_bool, eff)
      | Error e, _ | _, Error e -> Error (UnificationFailed (e, span))
    end
  | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr ->
    begin match Unify.unify left_ty ty_int, Unify.unify right_ty ty_int with
      | Ok (), Ok () -> Ok (ty_int, eff)
      | Error e, _ | _, Error e -> Error (UnificationFailed (e, span))
    end

and synth_unary (ctx : context) (op : unary_op) (operand : expr) : (ty * effect) result =
  let* (operand_ty, operand_eff) = synth ctx operand in
  match op with
  | OpNeg ->
    begin match Unify.unify operand_ty ty_int with
      | Ok () -> Ok (ty_int, operand_eff)
      | Error _ ->
        begin match Unify.unify operand_ty ty_float with
          | Ok () -> Ok (ty_float, operand_eff)
          | Error e -> Error (UnificationFailed (e, expr_span operand))
        end
    end
  | OpNot ->
    begin match Unify.unify operand_ty ty_bool with
      | Ok () -> Ok (ty_bool, operand_eff)
      | Error e -> Error (UnificationFailed (e, expr_span operand))
    end
  | OpBitNot ->
    begin match Unify.unify operand_ty ty_int with
      | Ok () -> Ok (ty_int, operand_eff)
      | Error e -> Error (UnificationFailed (e, expr_span operand))
    end
  | OpRef ->
    Ok (TRef operand_ty, operand_eff)
  | OpDeref ->
    begin match repr operand_ty with
      | TRef t | TMut t | TOwn t -> Ok (t, operand_eff)
      | _ -> Error (CannotInfer (expr_span operand))
    end

and bind_pattern (ctx : context) (pat : pattern) (scheme : scheme) : unit result =
  match pat with
  | PatVar id ->
    bind_var_scheme ctx id scheme;
    Ok ()
  | PatWildcard _ -> Ok ()
  | PatLit _ -> Ok ()  (* Literal patterns don't bind *)
  | PatTuple pats ->
    begin match scheme.sc_body with
      | TTuple tys when List.length pats = List.length tys ->
        List.fold_left2 (fun acc pat ty ->
          match acc with
          | Error e -> Error e
          | Ok () ->
            let sc = { scheme with sc_body = ty } in
            bind_pattern ctx pat sc
        ) (Ok ()) pats tys
      | _ -> Error (InvalidPattern Span.dummy)
    end
  | PatRecord (fields, _has_rest) ->
    begin match scheme.sc_body with
      | TRecord row ->
        List.fold_left (fun acc (field_id, pat_opt) ->
          match acc with
          | Error e -> Error e
          | Ok () ->
            match find_field field_id.name row with
            | Some ty ->
              begin match pat_opt with
                | Some p ->
                  let sc = { scheme with sc_body = ty } in
                  bind_pattern ctx p sc
                | None ->
                  bind_var ctx field_id ty;
                  Ok ()
              end
            | None -> Error (InvalidPattern field_id.span)
        ) (Ok ()) fields
      | _ -> Error (InvalidPattern Span.dummy)
    end
  | PatCon (_con, pats) ->
    (* TODO: Look up constructor type and bind subpatterns *)
    List.fold_left (fun acc pat ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        let sc = { scheme with sc_body = fresh_tyvar ctx.level } in
        bind_pattern ctx pat sc
    ) (Ok ()) pats
  | PatOr (p1, p2) ->
    (* Both branches must bind the same variables with same types *)
    let* () = bind_pattern ctx p1 scheme in
    bind_pattern ctx p2 scheme
  | PatAs (id, pat) ->
    bind_var_scheme ctx id scheme;
    bind_pattern ctx pat scheme

and check_pattern (ctx : context) (pat : pattern) (expected : ty) : unit result =
  let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = expected } in
  bind_pattern ctx pat scheme

and synth_literal (lit : literal) : ty =
  match lit with
  | LitUnit _ -> ty_unit
  | LitBool _ -> ty_bool
  | LitInt _ -> ty_int
  | LitFloat _ -> ty_float
  | LitChar _ -> ty_char
  | LitString _ -> ty_string

and find_field (name : string) (row : row) : ty option =
  match repr_row row with
  | REmpty -> None
  | RExtend (l, ty, rest) ->
    if l = name then Some ty
    else find_field name rest
  | RVar _ -> None

and union_eff (effs : effect list) : effect =
  let effs = List.filter (fun e -> e <> EPure) effs in
  match effs with
  | [] -> EPure
  | [e] -> e
  | es -> EUnion es

(** Type check a declaration *)
let check_decl (ctx : context) (decl : top_level) : unit result =
  match decl with
  | TopFn fd ->
    (* Create function type from signature *)
    let param_tys = List.map (fun param ->
      (param.p_name, ast_to_ty ctx param.p_ty)
    ) fd.fd_params in
    let ret_ty = match fd.fd_ret_ty with
      | Some ty -> ast_to_ty ctx ty
      | None -> fresh_tyvar ctx.level
    in
    (* Build function type *)
    let func_ty = List.fold_right (fun (_, param_ty) acc ->
      TArrow (param_ty, acc, EPure)
    ) param_tys ret_ty in
    (* Bind function name *)
    bind_var ctx fd.fd_name func_ty;
    (* Bind parameters *)
    List.iter (fun (id, ty) -> bind_var ctx id ty) param_tys;
    (* Check body *)
    begin match fd.fd_body with
      | FnBlock blk ->
        let* _ = check_block ctx blk ret_ty in
        Ok ()
      | FnExpr e ->
        let* _ = check ctx e ret_ty in
        Ok ()
    end

  | TopType _ ->
    (* TODO: Check type definitions *)
    Ok ()

  | TopEffect _ ->
    (* TODO: Register effect *)
    Ok ()

  | TopTrait _ ->
    (* TODO: Check trait definitions *)
    Ok ()

  | TopImpl _ ->
    (* TODO: Check implementations *)
    Ok ()

  | TopConst tc ->
    let expected = ast_to_ty ctx tc.tc_ty in
    let* _ = check ctx tc.tc_value expected in
    Ok ()

(** Type check a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  let ctx = create_context symbols in
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> check_decl ctx decl
  ) (Ok ()) program.prog_decls

(* TODO: Phase 1 implementation
   - [ ] Better error messages with suggestions
   - [ ] Type annotations on let bindings
   - [ ] Effect inference integration
   - [ ] Quantity checking integration
   - [ ] Trait resolution
   - [ ] Module type checking
*)
