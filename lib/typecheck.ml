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
  match Symbol.lookup ctx.symbols id.id_name with
  | Some sym ->
    begin match Hashtbl.find_opt ctx.var_types sym.sym_id with
      | Some scheme -> Ok (instantiate ctx scheme)
      | None ->
        (* Variable exists but not yet typed - this shouldn't happen after resolve *)
        Error (CannotInfer id.id_span)
    end
  | None ->
    Error (CannotInfer id.id_span)

(** Bind a variable with a type *)
let bind_var (ctx : context) (id : ident) (ty : ty) : unit =
  match Symbol.lookup ctx.symbols id.id_name with
  | Some sym ->
    let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None -> ()

(** Bind a variable with a scheme (polymorphic) *)
let bind_var_scheme (ctx : context) (id : ident) (scheme : scheme) : unit =
  match Symbol.lookup ctx.symbols id.id_name with
  | Some sym ->
    Hashtbl.replace ctx.var_types sym.sym_id scheme
  | None -> ()

(** Convert AST type to internal type *)
let rec ast_to_ty (ctx : context) (ty : type_expr) : ty =
  match ty with
  | TyVar id -> fresh_tyvar ctx.level  (* TODO: Look up type variable *)
  | TyCon id ->
    begin match id.id_name with
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
    TApp (TCon id.id_name, List.map (ast_to_ty_arg ctx) args)
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
      RExtend (field.rf_name.id_name, ast_to_ty ctx field.rf_ty, acc)
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
  | _ -> fresh_tyvar ctx.level  (* TODO: Handle other cases *)

and ast_to_ty_arg (ctx : context) (arg : type_arg) : ty =
  match arg with
  | TaType ty -> ast_to_ty ctx ty
  | TaNat _ -> TNat (NLit 0)  (* TODO: Convert nat expr *)

and ast_to_eff (ctx : context) (eff : effect_expr) : effect =
  match eff with
  | EffNamed id -> ESingleton id.id_name
  | EffVar id -> fresh_effvar ctx.level
  | EffUnion effs -> EUnion (List.map (ast_to_eff ctx) effs)
  | EffApp (id, _) -> ESingleton id.id_name

(** Synthesize (infer) the type of an expression *)
let rec synth (ctx : context) (expr : expr) : (ty * effect) result =
  match expr with
  | EVar id ->
    let* ty = lookup_var ctx id in
    Ok (ty, EPure)

  | ELit lit ->
    let ty = synth_literal lit in
    Ok (ty, EPure)

  | EApp (func, arg, span) ->
    let* (func_ty, func_eff) = synth ctx func in
    begin match repr func_ty with
      | TArrow (param_ty, ret_ty, call_eff) ->
        let* arg_eff = check ctx arg param_ty in
        Ok (ret_ty, union_eff [func_eff; arg_eff; call_eff])
      | TVar _ as tv ->
        (* Infer function type *)
        let param_ty = fresh_tyvar ctx.level in
        let ret_ty = fresh_tyvar ctx.level in
        let call_eff = fresh_effvar ctx.level in
        begin match Unify.unify tv (TArrow (param_ty, ret_ty, call_eff)) with
          | Ok () ->
            let* arg_eff = check ctx arg param_ty in
            Ok (ret_ty, union_eff [func_eff; arg_eff; call_eff])
          | Error e ->
            Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (ExpectedFunction (func_ty, span))
    end

  | ELam lam ->
    (* For lambdas, we need annotations or we infer fresh variables *)
    let param_tys = List.map (fun (id, ty_opt, _q) ->
      match ty_opt with
      | Some ty -> (id, ast_to_ty ctx ty)
      | None -> (id, fresh_tyvar ctx.level)
    ) lam.lam_params in
    (* Bind parameters *)
    List.iter (fun (id, ty) -> bind_var ctx id ty) param_tys;
    (* Infer body *)
    let* (body_ty, body_eff) = synth ctx lam.lam_body in
    (* Build arrow type *)
    let ty = List.fold_right (fun (_, param_ty) acc ->
      TArrow (param_ty, acc, body_eff)
    ) param_tys body_ty in
    Ok (ty, EPure)

  | ELet lb ->
    (* Infer RHS at higher level for generalization *)
    let ctx' = enter_level ctx in
    let* (rhs_ty, rhs_eff) = synth ctx' lb.lb_rhs in
    (* Generalize *)
    let scheme = generalize ctx rhs_ty in
    (* Bind pattern *)
    let* () = bind_pattern ctx lb.lb_pat scheme in
    (* Infer body *)
    let* (body_ty, body_eff) = synth ctx lb.lb_body in
    Ok (body_ty, union_eff [rhs_eff; body_eff])

  | EIf (cond, then_, else_, span) ->
    let* cond_eff = check ctx cond ty_bool in
    let* (then_ty, then_eff) = synth ctx then_ in
    let* else_eff = check ctx else_ then_ty in
    Ok (then_ty, union_eff [cond_eff; then_eff; else_eff])

  | ETuple (exprs, _) ->
    let* results = synth_list ctx exprs in
    let tys = List.map fst results in
    let effs = List.map snd results in
    Ok (TTuple tys, union_eff effs)

  | ERecord (fields, _) ->
    let* field_results = synth_fields ctx fields in
    let row = List.fold_right (fun (name, ty, _eff) acc ->
      RExtend (name, ty, acc)
    ) field_results REmpty in
    let effs = List.map (fun (_, _, eff) -> eff) field_results in
    Ok (TRecord row, union_eff effs)

  | ERecordAccess (expr, field, span) ->
    let* (expr_ty, expr_eff) = synth ctx expr in
    begin match repr expr_ty with
      | TRecord row ->
        begin match find_field field.id_name row with
          | Some ty -> Ok (ty, expr_eff)
          | None -> Error (UndefinedField (field.id_name, span))
        end
      | TVar _ as tv ->
        let field_ty = fresh_tyvar ctx.level in
        let rest = fresh_rowvar ctx.level in
        let row = RExtend (field.id_name, field_ty, rest) in
        begin match Unify.unify tv (TRecord row) with
          | Ok () -> Ok (field_ty, expr_eff)
          | Error e -> Error (UnificationFailed (e, span))
        end
      | _ ->
        Error (ExpectedRecord (expr_ty, span))
    end

  | EBlock (exprs, _) ->
    synth_block ctx exprs

  | EBinOp (left, op, right, span) ->
    synth_binop ctx left op right span

  | EPerform (op, arg, span) ->
    (* TODO: Look up effect operation type *)
    let* (_arg_ty, arg_eff) = synth ctx arg in
    let eff = ESingleton op.id_name in
    let ret_ty = fresh_tyvar ctx.level in
    Ok (ret_ty, union_eff [arg_eff; eff])

  | EHandle (body, handler, span) ->
    let* (body_ty, body_eff) = synth ctx body in
    (* TODO: Check handler and compute resulting effect *)
    let result_ty = match handler.h_return with
      | Some _ -> fresh_tyvar ctx.level
      | None -> body_ty
    in
    Ok (result_ty, EPure)  (* TODO: Proper effect computation *)

  | _ ->
    Error (CannotInfer (Span.dummy))

(** Check an expression against an expected type *)
and check (ctx : context) (expr : expr) (expected : ty) : effect result =
  match (expr, repr expected) with
  (* Lambda checking *)
  | (ELam lam, TArrow (param_ty, ret_ty, arr_eff)) ->
    (* Bind parameters with expected types *)
    begin match lam.lam_params with
      | [(id, _, _)] ->
        bind_var ctx id param_ty;
        let* body_eff = check ctx lam.lam_body ret_ty in
        begin match Unify.unify_eff body_eff arr_eff with
          | Ok () -> Ok EPure
          | Error e -> Error (UnificationFailed (e, Span.dummy))
        end
      | _ ->
        (* TODO: Multi-param lambdas *)
        check_subsumption ctx expr expected
    end

  (* If checking *)
  | (EIf (cond, then_, else_, _), _) ->
    let* cond_eff = check ctx cond ty_bool in
    let* then_eff = check ctx then_ expected in
    let* else_eff = check ctx else_ expected in
    Ok (union_eff [cond_eff; then_eff; else_eff])

  (* Tuple checking *)
  | (ETuple (exprs, _), TTuple tys) when List.length exprs = List.length tys ->
    let* effs = check_list ctx exprs tys in
    Ok (union_eff effs)

  (* Subsumption: synth and unify *)
  | _ ->
    check_subsumption ctx expr expected

and check_subsumption (ctx : context) (expr : expr) (expected : ty) : effect result =
  let* (actual, eff) = synth ctx expr in
  match Unify.unify actual expected with
  | Ok () -> Ok eff
  | Error e -> Error (UnificationFailed (e, Span.dummy))

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

and synth_fields (ctx : context) (fields : (ident * expr) list)
    : ((string * ty * effect) list) result =
  List.fold_right (fun (id, expr) acc ->
    match acc with
    | Error e -> Error e
    | Ok results ->
      match synth ctx expr with
      | Error e -> Error e
      | Ok (ty, eff) -> Ok ((id.id_name, ty, eff) :: results)
  ) fields (Ok [])

and synth_block (ctx : context) (exprs : expr list) : (ty * effect) result =
  match exprs with
  | [] -> Ok (ty_unit, EPure)
  | [e] -> synth ctx e
  | e :: rest ->
    let* (_, eff1) = synth ctx e in
    let* (ty, eff2) = synth_block ctx rest in
    Ok (ty, union_eff [eff1; eff2])

and synth_binop (ctx : context) (left : expr) (op : binop) (right : expr)
    (span : Span.t) : (ty * effect) result =
  let* (left_ty, left_eff) = synth ctx left in
  let* (right_ty, right_eff) = synth ctx right in
  let eff = union_eff [left_eff; right_eff] in
  match op with
  | BAdd | BSub | BMul | BDiv | BMod ->
    begin match Unify.unify left_ty ty_int, Unify.unify right_ty ty_int with
      | Ok (), Ok () -> Ok (ty_int, eff)
      | Error e, _ | _, Error e -> Error (UnificationFailed (e, span))
    end
  | BEq | BNe | BLt | BLe | BGt | BGe ->
    begin match Unify.unify left_ty right_ty with
      | Ok () -> Ok (ty_bool, eff)
      | Error e -> Error (UnificationFailed (e, span))
    end
  | BAnd | BOr ->
    begin match Unify.unify left_ty ty_bool, Unify.unify right_ty ty_bool with
      | Ok (), Ok () -> Ok (ty_bool, eff)
      | Error e, _ | _, Error e -> Error (UnificationFailed (e, span))
    end
  | _ ->
    (* TODO: Other operators *)
    Ok (fresh_tyvar ctx.level, eff)

and bind_pattern (ctx : context) (pat : pattern) (scheme : scheme) : unit result =
  match pat with
  | PVar id ->
    bind_var_scheme ctx id scheme;
    Ok ()
  | PWild _ -> Ok ()
  | PTuple (pats, _) ->
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
  | _ ->
    (* TODO: Other patterns *)
    Ok ()

and synth_literal (lit : literal) : ty =
  match lit with
  | LUnit _ -> ty_unit
  | LBool _ -> ty_bool
  | LInt _ -> ty_int
  | LFloat _ -> ty_float
  | LChar _ -> ty_char
  | LString _ -> ty_string

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

(* Result bind *)
let ( let* ) = Result.bind

(** Type check a declaration *)
let check_decl (ctx : context) (decl : decl) : unit result =
  match decl with
  | DFun fd ->
    (* Create function type from signature *)
    let param_tys = List.map (fun (id, ty_opt, _q) ->
      match ty_opt with
      | Some ty -> (id, ast_to_ty ctx ty)
      | None -> (id, fresh_tyvar ctx.level)
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
    (* Check body if present *)
    begin match fd.fd_body with
      | Some body ->
        let* _ = check ctx body ret_ty in
        Ok ()
      | None -> Ok ()
    end

  | DType _ ->
    (* TODO: Check type definitions *)
    Ok ()

  | DEffect _ ->
    (* TODO: Register effect *)
    Ok ()

  | DTrait _ ->
    (* TODO: Check trait definitions *)
    Ok ()

  | DImpl _ ->
    (* TODO: Check implementations *)
    Ok ()

  | DModule (_, decls, _) ->
    List.fold_left (fun acc d ->
      match acc with
      | Error e -> Error e
      | Ok () -> check_decl ctx d
    ) (Ok ()) decls

  | DImport _ ->
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
