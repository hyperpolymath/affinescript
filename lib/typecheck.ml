(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

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
  | KindError of string * Span.t
[@@deriving show]

type 'a result = ('a, type_error) Result.t

(** Type checking context *)
type context = {
  (** Symbol table with resolved names *)
  symbols : Symbol.t;

  (** Current let-generalization level *)
  mutable level : int;

  (** Variable types *)
  var_types : (Symbol.symbol_id, scheme) Hashtbl.t;

  (** Current effect context *)
  current_effect : eff;

  (** Constraint store for dependent types *)
  constraints : Constraint.constraint_store;
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
    constraints = Constraint.empty_store;
  }

(** Enter a new let-binding level *)
let enter_level (ctx : context) : context =
  { ctx with level = ctx.level + 1 }

(** Save current variable bindings for a list of names *)
let save_bindings (ctx : context) (names : ident list) : (Symbol.symbol_id * scheme) list =
  List.filter_map (fun id ->
    match Symbol.lookup ctx.symbols id.name with
    | Some sym ->
      begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
        | Some scheme -> Some (sym.sym_id, scheme)
        | None -> None
      end
    | None -> None
  ) names

(** Restore variable bindings from saved list *)
let restore_bindings (ctx : context) (saved : (Symbol.symbol_id * scheme) list) : unit =
  List.iter (fun (sym_id, scheme) ->
    Hashtbl.replace ctx.var_types sym_id scheme
  ) saved

(** Remove variable bindings for a list of names *)
let remove_bindings (ctx : context) (names : ident list) : unit =
  List.iter (fun id ->
    match Symbol.lookup ctx.symbols id.name with
    | Some sym -> Hashtbl.remove ctx.var_types sym.sym_id
    | None -> ()
  ) names

(** Add a nat constraint to context *)
let add_nat_constraint (ctx : context) (c : Constraint.nat_constraint) : context =
  { ctx with constraints = Constraint.add_nat_constraint c ctx.constraints }

(** Add an assumption to context *)
let add_assumption (ctx : context) (p : predicate) : context =
  { ctx with constraints = Constraint.add_assumption p ctx.constraints }

(** Check if a predicate is entailed by context *)
let check_predicate (ctx : context) (p : predicate) : bool =
  Constraint.entails ctx.constraints p

(** Add nat variable binding to context *)
let bind_nat_var (ctx : context) (name : string) (value : nat_expr) : context =
  let subst = (name, value) :: ctx.constraints.nat_subst in
  { ctx with constraints = { ctx.constraints with nat_subst = subst } }

(** Generalize a type at the current level *)
let generalize (ctx : context) (ty : ty) : scheme =
  (* Collect all unbound type variables at level > ctx.level *)
  let rec collect_tyvars (ty : ty) (acc : (tyvar * kind) list) : (tyvar * kind) list =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, lvl) when lvl > ctx.level ->
          if List.mem_assoc v acc then acc
          else (v, KType) :: acc  (* Kinds inferred during unification *)
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
  (* Collect all unbound row variables at level > ctx.level *)
  let rec collect_rowvars (ty : ty) (acc : rowvar list) : rowvar list =
    match repr ty with
    | TRecord row | TVariant row ->
      collect_row_rowvars row acc
    | TApp (t, args) ->
      List.fold_left (fun acc t -> collect_rowvars t acc) (collect_rowvars t acc) args
    | TArrow (a, b, _) ->
      collect_rowvars b (collect_rowvars a acc)
    | TTuple ts ->
      List.fold_left (fun acc t -> collect_rowvars t acc) acc ts
    | TForall (_, _, body) | TExists (_, _, body) ->
      collect_rowvars body acc
    | TRef t | TMut t | TOwn t ->
      collect_rowvars t acc
    | TRefined (t, _) ->
      collect_rowvars t acc
    | _ -> acc
  and collect_row_rowvars (row : row) (acc : rowvar list) : rowvar list =
    match repr_row row with
    | REmpty -> acc
    | RExtend (_, ty, rest) ->
      collect_row_rowvars rest (collect_rowvars ty acc)
    | RVar r ->
      begin match !r with
        | RUnbound (v, lvl) when lvl > ctx.level ->
          if List.mem v acc then acc
          else v :: acc
        | _ -> acc
      end
  in
  (* Collect all unbound effect variables at level > ctx.level *)
  let rec collect_effvars (ty : ty) (acc : effvar list) : effvar list =
    match repr ty with
    | TArrow (a, b, eff) ->
      collect_effvars b (collect_effvars a (collect_eff_effvars eff acc))
    | TDepArrow (_, a, b, eff) ->
      collect_effvars b (collect_effvars a (collect_eff_effvars eff acc))
    | TApp (t, args) ->
      List.fold_left (fun acc t -> collect_effvars t acc) (collect_effvars t acc) args
    | TTuple ts ->
      List.fold_left (fun acc t -> collect_effvars t acc) acc ts
    | TRecord row | TVariant row ->
      collect_row_effvars row acc
    | TForall (_, _, body) | TExists (_, _, body) ->
      collect_effvars body acc
    | TRef t | TMut t | TOwn t ->
      collect_effvars t acc
    | TRefined (t, _) ->
      collect_effvars t acc
    | _ -> acc
  and collect_row_effvars (row : row) (acc : effvar list) : effvar list =
    match repr_row row with
    | REmpty -> acc
    | RExtend (_, ty, rest) ->
      collect_row_effvars rest (collect_effvars ty acc)
    | RVar _ -> acc
  and collect_eff_effvars (eff : eff) (acc : effvar list) : effvar list =
    match repr_eff eff with
    | EPure -> acc
    | ESingleton _ -> acc
    | EUnion effs ->
      List.fold_left (fun acc e -> collect_eff_effvars e acc) acc effs
    | EVar r ->
      begin match !r with
        | EUnbound (v, lvl) when lvl > ctx.level ->
          if List.mem v acc then acc
          else v :: acc
        | _ -> acc
      end
  in
  let tyvars = collect_tyvars ty [] in
  let rowvars = collect_rowvars ty [] in
  let effvars = collect_effvars ty [] in
  { sc_tyvars = tyvars; sc_effvars = effvars; sc_rowvars = rowvars; sc_body = ty }

(** Instantiate a type scheme *)
let instantiate (ctx : context) (scheme : scheme) : ty =
  (* Create substitution for type variables *)
  let ty_subst = List.map (fun (v, _k) ->
    (v, fresh_tyvar ctx.level)
  ) scheme.sc_tyvars in
  (* Create substitution for row variables *)
  let row_subst = List.map (fun v ->
    (v, fresh_rowvar ctx.level)
  ) scheme.sc_rowvars in
  (* Create substitution for effect variables *)
  let eff_subst = List.map (fun v ->
    (v, fresh_effvar ctx.level)
  ) scheme.sc_effvars in
  let rec apply_subst (ty : ty) : ty =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, _) ->
          begin match List.assoc_opt v ty_subst with
            | Some ty' -> ty'
            | None -> ty
          end
        | Link _ -> failwith "instantiate: unexpected Link"
      end
    | TApp (t, args) ->
      TApp (apply_subst t, List.map apply_subst args)
    | TArrow (a, b, eff) ->
      TArrow (apply_subst a, apply_subst b, apply_subst_eff eff)
    | TDepArrow (name, a, b, eff) ->
      TDepArrow (name, apply_subst a, apply_subst b, apply_subst_eff eff)
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
    | RVar r ->
      begin match !r with
        | RUnbound (v, _) ->
          begin match List.assoc_opt v row_subst with
            | Some row' -> row'
            | None -> RVar r
          end
        | RLink _ -> failwith "instantiate: unexpected RLink"
      end
  and apply_subst_eff (eff : eff) : eff =
    match repr_eff eff with
    | EPure -> EPure
    | ESingleton s -> ESingleton s
    | EUnion effs -> EUnion (List.map apply_subst_eff effs)
    | EVar r ->
      begin match !r with
        | EUnbound (v, _) ->
          begin match List.assoc_opt v eff_subst with
            | Some eff' -> eff'
            | None -> EVar r
          end
        | ELink _ -> failwith "instantiate: unexpected ELink"
      end
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
    (* Symbol not found in current scope chain - try searching all_symbols by name as fallback *)
    (* This handles parameters defined in exited function scopes during resolution *)
    (* Choose the most recent symbol (highest ID) to handle shadowing correctly *)
    let matching_symbols = Hashtbl.fold (fun _id sym acc ->
      if sym.Symbol.sym_name = id.name && sym.Symbol.sym_kind = Symbol.SKVariable then
        sym :: acc
      else
        acc
    ) ctx.symbols.Symbol.all_symbols [] in
    let sorted_symbols = List.sort (fun a b -> compare b.Symbol.sym_id a.Symbol.sym_id) matching_symbols in
    begin match sorted_symbols with
      | sym :: _ ->
        begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
          | Some scheme -> Ok (instantiate ctx scheme)
          | None -> Error (CannotInfer id.span)
        end
      | [] -> Error (CannotInfer id.span)
    end

(** Bind a variable with a type *)
let bind_var (ctx : context) (id : ident) (ty : ty) : unit =
  match Symbol.lookup ctx.symbols id.name with
  | Some sym ->
    let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None ->
    (* Symbol not found in current scope chain - try searching all_symbols by name as fallback *)
    (* This handles parameters defined in exited function scopes during resolution *)
    (* Choose the most recent symbol (highest ID) to handle shadowing correctly *)
    let matching_symbols = Hashtbl.fold (fun _id sym acc ->
      if sym.Symbol.sym_name = id.name && sym.Symbol.sym_kind = Symbol.SKVariable then
        sym :: acc
      else
        acc
    ) ctx.symbols.Symbol.all_symbols [] in
    let sorted_symbols = List.sort (fun a b -> compare b.Symbol.sym_id a.Symbol.sym_id) matching_symbols in
    begin match sorted_symbols with
      | sym :: _ ->
        let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
        Hashtbl.replace ctx.var_types sym.sym_id scheme
      | [] -> ()
    end

(** Bind a variable with a scheme (polymorphic) *)
let bind_var_scheme (ctx : context) (id : ident) (scheme : scheme) : unit =
  match Symbol.lookup ctx.symbols id.name with
  | Some sym ->
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None ->
    (* Symbol not found in current scope chain - try searching all_symbols by name as fallback *)
    (* This handles variables defined in exited scopes during resolution *)
    (* Choose the most recent symbol (highest ID) to handle shadowing correctly *)
    let matching_symbols = Hashtbl.fold (fun _id sym acc ->
      if sym.Symbol.sym_name = id.name && sym.Symbol.sym_kind = Symbol.SKVariable then
        sym :: acc
      else
        acc
    ) ctx.symbols.Symbol.all_symbols [] in
    let sorted_symbols = List.sort (fun a b -> compare b.Symbol.sym_id a.Symbol.sym_id) matching_symbols in
    begin match sorted_symbols with
      | sym :: _ ->
        Hashtbl.replace ctx.var_types sym.sym_id scheme
      | [] -> ()
    end

(** Convert AST type to internal type *)
let rec ast_to_ty (ctx : context) (ty : type_expr) : ty =
  match ty with
  | TyVar id ->
    (* Look up type variable in symbol table *)
    begin match Symbol.lookup ctx.symbols id.name with
      | Some sym when sym.sym_kind = Symbol.SKTypeVar ->
        fresh_tyvar ctx.level  (* Type variable instantiated fresh each use *)
      | _ -> fresh_tyvar ctx.level
    end
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
  | TyRefined (t, pred) ->
    TRefined (ast_to_ty ctx t, ast_to_pred pred)
  | TyDepArrow da ->
    let param_ty = ast_to_ty ctx da.da_param_ty in
    let ret_ty = ast_to_ty ctx da.da_ret_ty in
    let eff = match da.da_eff with
      | Some e -> ast_to_eff ctx e
      | None -> EPure
    in
    TDepArrow (da.da_param.name, param_ty, ret_ty, eff)
  | TyHole -> fresh_tyvar ctx.level

and ast_to_ty_arg (ctx : context) (arg : type_arg) : ty =
  match arg with
  | TyArg ty -> ast_to_ty ctx ty
  | NatArg n -> TNat (ast_to_nat n)

(** Convert AST nat expr to internal nat expr *)
and ast_to_nat (n : Ast.nat_expr) : nat_expr =
  match n with
  | Ast.NatLit (i, _) -> NLit i
  | Ast.NatVar id -> NVar id.name
  | Ast.NatAdd (a, b) -> NAdd (ast_to_nat a, ast_to_nat b)
  | Ast.NatSub (a, b) -> NSub (ast_to_nat a, ast_to_nat b)
  | Ast.NatMul (a, b) -> NMul (ast_to_nat a, ast_to_nat b)
  | Ast.NatLen id -> NLen id.name
  | Ast.NatSizeof _ -> NLit 0  (* sizeof requires type info, defaulting *)

(** Convert AST predicate to internal predicate *)
and ast_to_pred (p : Ast.predicate) : predicate =
  match p with
  | Ast.PredCmp (a, op, b) ->
    let a' = ast_to_nat a in
    let b' = ast_to_nat b in
    begin match op with
      | Ast.Lt -> PLt (a', b')
      | Ast.Le -> PLe (a', b')
      | Ast.Gt -> PGt (a', b')
      | Ast.Ge -> PGe (a', b')
      | Ast.Eq -> PEq (a', b')
      | Ast.Ne -> PNot (PEq (a', b'))
    end
  | Ast.PredNot p -> PNot (ast_to_pred p)
  | Ast.PredAnd (p1, p2) -> PAnd (ast_to_pred p1, ast_to_pred p2)
  | Ast.PredOr (p1, p2) -> POr (ast_to_pred p1, ast_to_pred p2)

and ast_to_eff (ctx : context) (e : effect_expr) : eff =
  match e with
  | EffCon (id, _) -> ESingleton id.name
  | EffVar _id -> fresh_effvar ctx.level
  | EffUnion (e1, e2) -> EUnion [ast_to_eff ctx e1; ast_to_eff ctx e2]

(** Kind checking *)

(** Infer the kind of a type *)
let rec infer_kind (ctx : context) (ty : ty) : kind result =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (v, _) ->
        (* Look up the kind from type variable binding *)
        (* For now, assume type variables have kind Type *)
        let _ = v in
        Ok KType
      | Link t -> infer_kind ctx t
    end
  | TCon name ->
    (* Built-in type constructors *)
    begin match name with
      | "Int" | "Bool" | "String" | "Unit" -> Ok KType
      | "Vec" -> Ok (KArrow (KNat, KArrow (KType, KType)))
      | "Array" -> Ok (KArrow (KType, KType))
      | "List" -> Ok (KArrow (KType, KType))
      | "Option" -> Ok (KArrow (KType, KType))
      | "Result" -> Ok (KArrow (KType, KArrow (KType, KType)))
      | _ ->
        (* User-defined type constructors - look up in symbol table *)
        Ok KType  (* Default to Type for now *)
    end
  | TApp (t, args) ->
    (* Type application: check constructor has arrow kind *)
    let* con_kind = infer_kind ctx t in
    check_kind_app ctx con_kind args
  | TArrow (a, b, _) ->
    (* Function types have kind Type *)
    let* _ = check_kind ctx a KType in
    let* _ = check_kind ctx b KType in
    Ok KType
  | TDepArrow (_, a, b, _) ->
    let* _ = check_kind ctx a KType in
    let* _ = check_kind ctx b KType in
    Ok KType
  | TTuple tys ->
    (* Tuples have kind Type if all components do *)
    let* _ = List.fold_left (fun acc t ->
      let* _ = acc in
      check_kind ctx t KType
    ) (Ok ()) tys in
    Ok KType
  | TRecord _ | TVariant _ ->
    (* Records and variants have kind Type *)
    Ok KType
  | TForall (_v, k, body) ->
    (* Polymorphic types have the kind of their body *)
    let* _ = check_kind ctx body KType in
    let _ = k in
    Ok KType
  | TExists (_v, k, body) ->
    let* _ = check_kind ctx body KType in
    let _ = k in
    Ok KType
  | TRef t | TMut t | TOwn t ->
    let* _ = check_kind ctx t KType in
    Ok KType
  | TRefined (t, _) ->
    (* Refined types have same kind as base type *)
    infer_kind ctx t
  | TNat _ ->
    (* Type-level naturals have kind Nat *)
    Ok KNat

(** Check a type has an expected kind *)
and check_kind (ctx : context) (ty : ty) (expected : kind) : unit result =
  let* inferred = infer_kind ctx ty in
  if inferred = expected then
    Ok ()
  else
    Error (KindError (
      Printf.sprintf "Kind mismatch: expected %s, got %s"
        (show_kind expected) (show_kind inferred),
      Span.dummy))

(** Check type application kinds *)
and check_kind_app (ctx : context) (con_kind : kind) (args : ty list) : kind result =
  match (con_kind, args) with
  | (k, []) -> Ok k
  | (KArrow (k_arg, k_ret), arg :: rest) ->
    let* _ = check_kind ctx arg k_arg in
    check_kind_app ctx k_ret rest
  | (k, _ :: _) ->
    Error (KindError (
      Printf.sprintf "Expected arrow kind for type application, got %s"
        (show_kind k),
      Span.dummy))

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
let rec synth (ctx : context) (expr : expr) : (ty * eff) result =
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
    (* Save current bindings for parameter names to restore later *)
    let param_names = List.map fst param_tys in
    let saved = save_bindings ctx param_names in
    (* Bind parameters *)
    List.iter (fun (id, ty) -> bind_var ctx id ty) param_tys;
    (* Infer body *)
    let* (body_ty, body_eff) = synth ctx lam.elam_body in
    (* Restore original bindings *)
    remove_bindings ctx param_names;
    restore_bindings ctx saved;
    (* Build arrow type *)
    let ty = List.fold_right (fun (_, param_ty) acc ->
      TArrow (param_ty, acc, body_eff)
    ) param_tys body_ty in
    Ok (ty, EPure)

  | ExprLet lb ->
    (* Infer RHS at higher level for generalization *)
    let ctx' = enter_level ctx in
    let* (rhs_ty, rhs_eff) = synth ctx' lb.el_value in
    (* If mutable, wrap type in TMut *)
    let bind_ty = if lb.el_mut then TMut rhs_ty else rhs_ty in
    (* Generalize (note: mutable bindings typically shouldn't be generalized) *)
    let scheme = if lb.el_mut then
      (* Mutable bindings: no generalization *)
      { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = bind_ty }
    else
      (* Immutable bindings: generalize *)
      generalize ctx bind_ty
    in
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
    (* Handle spread if present *)
    let* (base_row, spread_eff) = match er.er_spread with
      | Some spread_expr ->
        let* (spread_ty, spread_eff) = synth ctx spread_expr in
        begin match repr spread_ty with
          | TRecord row -> Ok (row, spread_eff)
          | TVar _ as tv ->
            let row = fresh_rowvar ctx.level in
            begin match Unify.unify tv (TRecord row) with
              | Ok () -> Ok (row, spread_eff)
              | Error e -> Error (UnificationFailed (e, expr_span spread_expr))
            end
          | _ -> Error (ExpectedRecord (spread_ty, expr_span spread_expr))
        end
      | None -> Ok (REmpty, EPure)
    in
    (* Build row by extending base with new fields *)
    let row = List.fold_right (fun (name, ty, _eff) acc ->
      RExtend (name, ty, acc)
    ) field_results base_row in
    let field_effs = List.map (fun (_, _, eff) -> eff) field_results in
    Ok (TRecord row, union_eff (spread_eff :: field_effs))

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
    let* (body_ty, body_eff) = synth ctx eh.eh_body in
    (* Check each handler arm and compute resulting effect *)
    let* handler_effs = List.fold_left (fun acc handler ->
      let* effs = acc in
      match handler with
      | HandlerReturn (pat, handler_body) ->
        let* () = check_pattern ctx pat body_ty in
        let* (_, eff) = synth ctx handler_body in
        Ok (eff :: effs)
      | HandlerOp (_op, pats, handler_body) ->
        (* Bind pattern variables for operation arguments *)
        List.iter (fun pat ->
          let _ = check_pattern ctx pat (fresh_tyvar ctx.level) in ()
        ) pats;
        let* (_, eff) = synth ctx handler_body in
        Ok (eff :: effs)
    ) (Ok [body_eff]) eh.eh_handlers in
    (* Effect after handling: body effect minus handled effects *)
    Ok (body_ty, union_eff handler_effs)

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
    (* Check catch arms if present *)
    let* catch_effs = match et.et_catch with
      | Some arms ->
        List.fold_left (fun acc arm ->
          let* effs = acc in
          let* () = check_pattern ctx arm.ma_pat (fresh_tyvar ctx.level) in
          let* () = match arm.ma_guard with
            | Some g -> let* _ = check ctx g ty_bool in Ok ()
            | None -> Ok ()
          in
          let* eff = check ctx arm.ma_body body_ty in
          Ok (eff :: effs)
        ) (Ok []) arms
      | None -> Ok []
    in
    (* Check finally block if present *)
    let* finally_eff = match et.et_finally with
      | Some blk ->
        let* (_, eff) = synth_block ctx blk in
        Ok eff
      | None -> Ok EPure
    in
    Ok (body_ty, union_eff (body_eff :: finally_eff :: catch_effs))

  | ExprRowRestrict (base, field) ->
    let span = expr_span expr in
    let* (base_ty, base_eff) = synth ctx base in
    (* Row restriction removes a field from a record type *)
    begin match repr base_ty with
      | TRecord row ->
        let restricted = restrict_row field.name row in
        Ok (TRecord restricted, base_eff)
      | TVar _ as tv ->
        (* Generate a record type with the field and a fresh rest *)
        let rest = fresh_rowvar ctx.level in
        let field_ty = fresh_tyvar ctx.level in
        let row = RExtend (field.name, field_ty, rest) in
        begin match Unify.unify tv (TRecord row) with
          | Ok () -> Ok (TRecord rest, base_eff)
          | Error e -> Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (ExpectedRecord (base_ty, span))
    end

  | ExprUnsafe ops ->
    synth_unsafe_ops ctx ops

  | ExprVariant (ty_id, variant_id) ->
    (* Look up the variant constructor in the symbol table *)
    begin match Symbol.lookup ctx.symbols variant_id.name with
      | Some sym when sym.sym_kind = Symbol.SKConstructor ->
        (* Get the constructor's type from var_types *)
        begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
          | Some scheme -> Ok (instantiate ctx scheme, EPure)
          | None -> Ok (TCon ty_id.name, EPure)
        end
      | _ ->
        (* Constructor not found or not a constructor - return the type *)
        Ok (TCon ty_id.name, EPure)
    end

  | ExprSpan (e, _span) ->
    synth ctx e

and synth_app (ctx : context) (func_ty : ty) (func_eff : eff)
    (args : expr list) (span : Span.t) : (ty * eff) result =
  match args with
  | [] -> Ok (func_ty, func_eff)
  | arg :: rest ->
    begin match repr func_ty with
      | TArrow (param_ty, ret_ty, call_eff) ->
        let* arg_eff = check ctx arg param_ty in
        synth_app ctx ret_ty (union_eff [func_eff; arg_eff; call_eff]) rest span

      (* Dependent arrow: substitute argument in return type *)
      | TDepArrow (param_name, param_ty, ret_ty, call_eff) ->
        let* arg_eff = check ctx arg param_ty in
        (* Try to extract nat expression from argument *)
        let arg_nat = extract_nat_from_expr arg in
        let (ret_ty', _subst) = Constraint.instantiate_dep_arrow param_name arg_nat ret_ty in
        synth_app ctx ret_ty' (union_eff [func_eff; arg_eff; call_eff]) rest span

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

(** Extract nat expression from an expression (for dependent types) *)
and extract_nat_from_expr (expr : expr) : nat_expr =
  match expr with
  | ExprLit (LitInt (n, _)) -> NLit n
  | ExprVar id -> NVar id.name
  | ExprBinary (e1, OpAdd, e2) -> NAdd (extract_nat_from_expr e1, extract_nat_from_expr e2)
  | ExprBinary (e1, OpSub, e2) -> NSub (extract_nat_from_expr e1, extract_nat_from_expr e2)
  | ExprBinary (e1, OpMul, e2) -> NMul (extract_nat_from_expr e1, extract_nat_from_expr e2)
  | _ -> NVar "_"  (* Unknown/complex expression *)

(** Check an expression against an expected type *)
and check (ctx : context) (expr : expr) (expected : ty) : eff result =
  match (expr, repr expected) with
  (* Lambda checking *)
  | (ExprLambda lam, TArrow (param_ty, ret_ty, arr_eff)) ->
    begin match lam.elam_params with
      | [param] ->
        (* Save binding for this parameter name *)
        let saved = save_bindings ctx [param.p_name] in
        bind_var ctx param.p_name param_ty;
        let* body_eff = check ctx lam.elam_body ret_ty in
        (* Restore original binding *)
        remove_bindings ctx [param.p_name];
        restore_bindings ctx saved;
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

  (* Refined type checking *)
  | (_, TRefined (base_ty, pred)) ->
    (* Check against base type first *)
    let* eff = check ctx expr base_ty in
    (* Then verify refinement predicate *)
    if check_predicate ctx pred then
      Ok eff
    else
      Error (QuantityError ("Refinement predicate not satisfied", expr_span expr))

  (* Subsumption: synth and unify *)
  | _ ->
    check_subsumption ctx expr expected

and check_subsumption (ctx : context) (expr : expr) (expected : ty) : eff result =
  let* (actual, eff) = synth ctx expr in
  (* Check for refined type subsumption *)
  begin match (repr actual, repr expected) with
    | (TRefined (base1, pred1), TRefined (base2, pred2)) ->
      (* Must have same base type and pred1 => pred2 *)
      begin match Unify.unify base1 base2 with
        | Ok () ->
          if check_predicate ctx (PImpl (pred1, pred2)) then
            Ok eff
          else
            Error (QuantityError ("Refinement not strong enough", expr_span expr))
        | Error e -> Error (UnificationFailed (e, expr_span expr))
      end
    | (_, TRefined (base, pred)) ->
      (* Checking non-refined against refined *)
      begin match Unify.unify actual base with
        | Ok () ->
          if check_predicate ctx pred then
            Ok eff
          else
            Error (QuantityError ("Refinement predicate not satisfied", expr_span expr))
        | Error e -> Error (UnificationFailed (e, expr_span expr))
      end
    | _ ->
      match Unify.unify actual expected with
      | Ok () -> Ok eff
      | Error e -> Error (UnificationFailed (e, expr_span expr))
  end

and synth_list (ctx : context) (exprs : expr list) : ((ty * eff) list) result =
  List.fold_right (fun expr acc ->
    match acc with
    | Error e -> Error e
    | Ok results ->
      match synth ctx expr with
      | Error e -> Error e
      | Ok result -> Ok (result :: results)
  ) exprs (Ok [])

and check_list (ctx : context) (exprs : expr list) (tys : ty list) : (eff list) result =
  List.fold_right2 (fun expr ty acc ->
    match acc with
    | Error e -> Error e
    | Ok effs ->
      match check ctx expr ty with
      | Error e -> Error e
      | Ok eff -> Ok (eff :: effs)
  ) exprs tys (Ok [])

and synth_record_fields (ctx : context) (fields : (ident * expr option) list)
    : ((string * ty * eff) list) result =
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

and synth_block (ctx : context) (blk : block) : (ty * eff) result =
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

and check_block (ctx : context) (blk : block) (expected : ty) : eff result =
  (* Check if last statement is a return statement *)
  let last_is_return = match List.rev blk.blk_stmts with
    | StmtExpr (ExprReturn _) :: _ -> true
    | _ -> false
  in
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
    (* If last statement is a return, the block can return the expected type *)
    if last_is_return then
      Ok (union_eff effs)
    else
      begin match Unify.unify expected ty_unit with
        | Ok () -> Ok (union_eff effs)
        | Error e -> Error (UnificationFailed (e, Span.dummy))
      end

and synth_stmt (ctx : context) (stmt : stmt) : eff result =
  match stmt with
  | StmtLet sl ->
    let ctx' = enter_level ctx in
    let* (rhs_ty, rhs_eff) = synth ctx' sl.sl_value in
    (* If mutable, wrap type in TMut *)
    let bind_ty = if sl.sl_mut then TMut rhs_ty else rhs_ty in
    (* Generalize only if immutable *)
    let scheme = if sl.sl_mut then
      { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = bind_ty }
    else
      generalize ctx bind_ty
    in
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
    (span : Span.t) : (ty * eff) result =
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

and synth_unary (ctx : context) (op : unary_op) (operand : expr) : (ty * eff) result =
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
  | PatCon (con, pats) ->
    (* Look up constructor and bind subpatterns with actual types *)
    let param_tys = match Symbol.lookup ctx.symbols con.name with
      | Some sym when sym.sym_kind = Symbol.SKConstructor ->
        (* Try to get constructor type from var_types *)
        begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
          | Some con_scheme ->
            (* Extract parameter types from constructor type *)
            let con_ty = instantiate ctx con_scheme in
            extract_constructor_param_types con_ty (List.length pats)
          | None ->
            (* Constructor type not available, use fresh tyvars *)
            List.map (fun _ -> fresh_tyvar ctx.level) pats
        end
      | _ ->
        (* Constructor not found, use fresh tyvars *)
        List.map (fun _ -> fresh_tyvar ctx.level) pats
    in
    List.fold_left2 (fun acc pat ty ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        let sc = { scheme with sc_body = ty } in
        bind_pattern ctx pat sc
    ) (Ok ()) pats param_tys
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

(** Remove a field from a row, returning the restricted row *)
and restrict_row (name : string) (row : row) : row =
  match repr_row row with
  | REmpty -> REmpty
  | RExtend (l, ty, rest) ->
    if l = name then rest
    else RExtend (l, ty, restrict_row name rest)
  | RVar _ as rv -> rv

(** Extract parameter types from a constructor type *)
and extract_constructor_param_types (ty : ty) (expected_count : int) : ty list =
  let rec go ty acc =
    match repr ty with
    | TArrow (param_ty, ret_ty, _) ->
      go ret_ty (param_ty :: acc)
    | _ -> List.rev acc
  in
  let params = go ty [] in
  (* If we got the expected number of params, use them *)
  if List.length params = expected_count then params
  (* Otherwise, generate fresh tyvars *)
  else List.init expected_count (fun _ -> fresh_tyvar 0)

and union_eff (effs : eff list) : eff =
  let effs = List.filter (fun e -> e <> EPure) effs in
  match effs with
  | [] -> EPure
  | [e] -> e
  | es -> EUnion es

(** Type check unsafe operations *)
and synth_unsafe_ops (ctx : context) (ops : unsafe_op list) : (ty * eff) result =
  (* Process each unsafe operation and collect effects *)
  let* (last_ty, effs) = List.fold_left (fun acc op ->
    let* (_, effs) = acc in
    match op with
    | UnsafeRead e ->
      let* (ty, eff) = synth ctx e in
      (* UnsafeRead dereferences a raw pointer *)
      begin match repr ty with
        | TRef t | TMut t | TOwn t -> Ok (t, eff :: effs)
        | TVar _ as tv ->
          let inner_ty = fresh_tyvar ctx.level in
          begin match Unify.unify tv (TRef inner_ty) with
            | Ok () -> Ok (inner_ty, eff :: effs)
            | Error _ -> Ok (fresh_tyvar ctx.level, eff :: effs)
          end
        | _ -> Ok (fresh_tyvar ctx.level, eff :: effs)
      end
    | UnsafeWrite (ptr, value) ->
      let* (ptr_ty, ptr_eff) = synth ctx ptr in
      let* (val_ty, val_eff) = synth ctx value in
      (* UnsafeWrite writes through a mutable pointer *)
      begin match repr ptr_ty with
        | TMut t ->
          begin match Unify.unify t val_ty with
            | Ok () -> Ok (ty_unit, val_eff :: ptr_eff :: effs)
            | Error _ -> Ok (ty_unit, val_eff :: ptr_eff :: effs)
          end
        | TVar _ as tv ->
          begin match Unify.unify tv (TMut val_ty) with
            | Ok () -> Ok (ty_unit, val_eff :: ptr_eff :: effs)
            | Error _ -> Ok (ty_unit, val_eff :: ptr_eff :: effs)
          end
        | _ -> Ok (ty_unit, val_eff :: ptr_eff :: effs)
      end
    | UnsafeOffset (ptr, offset) ->
      let* (ptr_ty, ptr_eff) = synth ctx ptr in
      let* offset_eff = check ctx offset ty_int in
      (* UnsafeOffset computes pointer arithmetic *)
      Ok (ptr_ty, offset_eff :: ptr_eff :: effs)
    | UnsafeTransmute (from_ty, to_ty, e) ->
      let from_ty' = ast_to_ty ctx from_ty in
      let to_ty' = ast_to_ty ctx to_ty in
      let* e_eff = check ctx e from_ty' in
      (* UnsafeTransmute reinterprets bits *)
      Ok (to_ty', e_eff :: effs)
    | UnsafeForget e ->
      let* (_, eff) = synth ctx e in
      (* UnsafeForget prevents destructor from running *)
      Ok (ty_unit, eff :: effs)
    | UnsafeAssume pred ->
      (* UnsafeAssume adds a predicate to the constraint context *)
      let pred' = ast_to_pred pred in
      let _ = add_assumption ctx pred' in
      Ok (ty_unit, effs)
  ) (Ok (ty_unit, [])) ops in
  Ok (last_ty, union_eff effs)

(** Type check a declaration *)
let rec check_decl (ctx : context) (decl : top_level) : unit result =
  match decl with
  | TopFn fd ->
    (* Check that type parameters have valid kinds if annotated *)
    let* () = List.fold_left (fun acc tp ->
      let* () = acc in
      match tp.tp_kind with
      | None -> Ok ()  (* No kind annotation, default to Type *)
      | Some _k ->
        (* Kind annotations are syntactically valid by parsing *)
        (* TODO: Check that kind is sensible for this position *)
        Ok ()
    ) (Ok ()) fd.fd_type_params in

    (* Enter new level for function signature variables *)
    let outer_level = ctx.level in
    ctx.level <- ctx.level + 1;

    (* Create function type from signature and check kinds *)
    let* param_tys = List.fold_left (fun acc param ->
      let* tys = acc in
      let param_ty = ast_to_ty ctx param.p_ty in
      (* Check parameter type is well-kinded (should have kind Type) *)
      let* _ = check_kind ctx param_ty KType in
      Ok ((param.p_name, param_ty) :: tys)
    ) (Ok []) fd.fd_params in
    let param_tys = List.rev param_tys in

    let* ret_ty = match fd.fd_ret_ty with
      | Some ty ->
        let ret_ty = ast_to_ty ctx ty in
        (* Check return type is well-kinded *)
        let* _ = check_kind ctx ret_ty KType in
        Ok ret_ty
      | None -> Ok (fresh_tyvar ctx.level)
    in

    (* Build function type with fresh effect variables *)
    let func_eff = fresh_effvar ctx.level in
    let func_ty = List.fold_right (fun (_, param_ty) acc ->
      TArrow (param_ty, acc, func_eff)
    ) param_tys ret_ty in
    (* Exit level for generalization *)
    ctx.level <- outer_level;
    (* Generalize function type to make it polymorphic *)
    let func_scheme = generalize ctx func_ty in
    (* Bind function name with polymorphic scheme *)
    bind_var_scheme ctx fd.fd_name func_scheme;
    (* Bind parameters (using fallback lookup that searches all_symbols) *)
    List.iter (fun (id, ty) -> bind_var ctx id ty) param_tys;
    (* Check body and unify effect *)
    begin match fd.fd_body with
      | FnBlock blk ->
        let* body_eff = check_block ctx blk ret_ty in
        begin match Unify.unify_eff func_eff body_eff with
          | Ok () -> Ok ()
          | Error e -> Error (UnificationFailed (e, Span.dummy))
        end
      | FnExpr e ->
        let* body_eff = check ctx e ret_ty in
        begin match Unify.unify_eff func_eff body_eff with
          | Ok () -> Ok ()
          | Error e -> Error (UnificationFailed (e, Span.dummy))
        end
    end

  | TopType td ->
    (* Check type definitions - validate type body is well-formed *)
    (* Check that type parameters have valid kinds if annotated *)
    let* () = List.fold_left (fun acc tp ->
      let* () = acc in
      match tp.tp_kind with
      | None -> Ok ()  (* No kind annotation, default to Type *)
      | Some _k ->
        (* Kind annotations are syntactically valid by parsing *)
        (* TODO: Check that kind is sensible for this position *)
        Ok ()
    ) (Ok ()) td.td_type_params in

    (* Check type body and validate kinds *)
    begin match td.td_body with
      | TyAlias ty ->
        let ty' = ast_to_ty ctx ty in
        (* Check the alias type is well-kinded *)
        let* _ = infer_kind ctx ty' in
        Ok ()
      | TyStruct fields ->
        (* Check all field types are well-kinded *)
        List.fold_left (fun acc field ->
          let* () = acc in
          let ty = ast_to_ty ctx field.sf_ty in
          let* _ = check_kind ctx ty KType in
          Ok ()
        ) (Ok ()) fields
      | TyEnum variants ->
        (* Check all variant field types are well-kinded *)
        List.fold_left (fun acc variant ->
          let* () = acc in
          List.fold_left (fun acc2 ty_expr ->
            let* () = acc2 in
            let ty = ast_to_ty ctx ty_expr in
            let* _ = check_kind ctx ty KType in
            Ok ()
          ) (Ok ()) variant.vd_fields
        ) (Ok ()) variants
    end

  | TopEffect ed ->
    (* Register effect operations in context *)
    List.iter (fun op ->
      let param_tys = List.map (fun p -> ast_to_ty ctx p.p_ty) op.eod_params in
      let ret_ty = match op.eod_ret_ty with
        | Some ty -> ast_to_ty ctx ty
        | None -> ty_unit
      in
      (* Build operation type *)
      let op_ty = List.fold_right (fun param_ty acc ->
        TArrow (param_ty, acc, ESingleton ed.ed_name.name)
      ) param_tys ret_ty in
      bind_var ctx op.eod_name op_ty
    ) ed.ed_ops;
    Ok ()

  | TopTrait td ->
    (* Check trait definitions - validate method signatures *)
    List.iter (fun item ->
      match item with
      | TraitFn fs ->
        List.iter (fun p -> let _ = ast_to_ty ctx p.p_ty in ()) fs.fs_params;
        Option.iter (fun ty -> let _ = ast_to_ty ctx ty in ()) fs.fs_ret_ty
      | TraitFnDefault fd ->
        let _ = check_decl ctx (TopFn fd) in ()
      | TraitType _ -> ()
    ) td.trd_items;
    Ok ()

  | TopImpl ib ->
    (* Check implementations - validate methods against trait *)
    let self_ty = ast_to_ty ctx ib.ib_self_ty in
    List.iter (fun item ->
      match item with
      | ImplFn fd ->
        (* Bind self type for method body *)
        let _ = Symbol.define ctx.symbols "Self"
            Symbol.SKType Span.dummy Private in
        bind_var ctx { name = "Self"; span = Span.dummy } self_ty;
        let _ = check_decl ctx (TopFn fd) in ()
      | ImplType (_name, ty) ->
        let _ = ast_to_ty ctx ty in ()
    ) ib.ib_items;
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

(* Phase 1 complete. Future enhancements (Phase 2+):
   - Better error messages with suggestions (Phase 2)
   - Advanced trait resolution with overlapping impls (Phase 2)
   - Full dependent type checking (Phase 3)
   - Module type checking with signatures (Phase 2)
*)
