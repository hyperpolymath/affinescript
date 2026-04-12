(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Bidirectional type checker for AffineScript.

    Implements a Hindley-Milner-style type checker with bidirectional
    mode switching (synth/check), let-polymorphism via levels, and
    integration with the unification engine ({!Unify}).

    Design:
    - [synth]: infer a type for an expression (mode ⇒)
    - [check]: verify an expression against an expected type (mode ⇐)
    - At application sites we synth the function and check the argument
    - At let-bindings we generalize to produce polymorphic schemes
    - Effect annotations are unified alongside types

    Limitations (Phase 1):
    - Quantity checking is a separate pass (see {!Quantity})
    - Refinement types are checked structurally, not via SMT
    - Trait resolution uses name matching (see {!Trait})
    - Effect inference is basic: we unify declared effects
*)

open Types
open Ast

let string_of_binary_op = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpMod -> "%"
  | OpEq -> "=="
  | OpNe -> "!="
  | OpLt -> "<"
  | OpLe -> "<="
  | OpGt -> ">"
  | OpGe -> ">="
  | OpAnd -> "&&"
  | OpOr -> "||"
  | OpBitAnd -> "&"
  | OpBitOr -> "|"
  | OpBitXor -> "^"
  | OpShl -> "<<"
  | OpShr -> ">>"
  | OpConcat -> "++"

let rec expr_summary (expr : expr) : string =
  match expr with
  | ExprVar id -> id.name
  | ExprLit (LitBool (b, _)) -> string_of_bool b
  | ExprLit (LitInt (i, _)) -> string_of_int i
  | ExprLit (LitFloat (f, _)) -> string_of_float f
  | ExprLit (LitChar (c, _)) -> Printf.sprintf "%c" c
  | ExprLit (LitString (s, _)) -> Printf.sprintf "\"%s\"" s
  | ExprLit (LitUnit _) -> "()"
  | ExprBinary (l, op, r) ->
    Printf.sprintf "(%s %s %s)" (expr_summary l) (string_of_binary_op op) (expr_summary r)
  | ExprUnary (_, inner) -> expr_summary inner
  | ExprApp (f, args) ->
    let fdesc = expr_summary f in
    let argsdesc = match args with
      | [] -> ""
      | hd :: _ -> expr_summary hd
    in
    Printf.sprintf "%s(%s...)" fdesc argsdesc
  | ExprField (e, id) ->
    Printf.sprintf "%s.%s" (expr_summary e) id.name
  | ExprTuple _ -> "<tuple>"
  | ExprRecord _ -> "<record>"
  | ExprBlock _ -> "{...}"
  | ExprIf _ -> "if(...)"
  | ExprLet _ -> "let(...)"
  | ExprMatch _ -> "match(...)"
  | ExprRowRestrict _ -> "row_restrict"
  | ExprTupleIndex _ -> "tuple_index"
  | ExprIndex _ -> "index"
  | ExprArray _ -> "array"
  | ExprReturn _ -> "return"
  | ExprTry _ -> "try"
  | ExprHandle _ -> "handle"
  | ExprResume _ -> "resume"
  | ExprUnsafe _ -> "unsafe"
  | ExprVariant (id, _) -> id.name
  | ExprSpan (inner, _) -> expr_summary inner
  | _ -> "<expr>"

(** {1 Errors} *)

(** Type checking error *)
type type_error =
  | UnboundVariable of string
  | TypeMismatch of { expected : ty; got : ty }
  | OccursCheck of string * ty
  | NotImplemented of string
  | ArityMismatch of { name : string; expected : int; got : int }
  | NotAFunction of ty
  | FieldNotFound of { field : string; record_ty : ty }
  | TupleIndexOutOfBounds of { index : int; length : int }
  | DuplicateField of string
  | UnificationError of Unify.unify_error
  | PatternTypeMismatch of string
  | BranchTypeMismatch of { then_ty : ty; else_ty : ty }
  | QuantityError of Quantity.quantity_error * Span.t
      (** QTT quantity violation detected after type checking. *)

(** Format a type error for human consumption. *)
let show_type_error = function
  | UnboundVariable v -> "Unbound variable: " ^ v
  | TypeMismatch { expected; got } ->
    Printf.sprintf "Type mismatch: expected %s, got %s"
      (ty_to_string expected) (ty_to_string got)
  | OccursCheck (v, ty) ->
    Printf.sprintf "Occurs check: %s in %s" v (ty_to_string ty)
  | NotImplemented msg -> "Not implemented: " ^ msg
  | ArityMismatch { name; expected; got } ->
    Printf.sprintf "Function %s expects %d arguments, got %d" name expected got
  | NotAFunction ty ->
    Printf.sprintf "Expected a function type, got %s" (ty_to_string ty)
  | FieldNotFound { field; record_ty } ->
    Printf.sprintf "Field '%s' not found in type %s" field (ty_to_string record_ty)
  | TupleIndexOutOfBounds { index; length } ->
    Printf.sprintf "Tuple index %d out of bounds (tuple has %d elements)" index length
  | DuplicateField f ->
    Printf.sprintf "Duplicate field: %s" f
  | UnificationError ue ->
    Printf.sprintf "Unification error: %s" (Unify.show_unify_error ue)
  | PatternTypeMismatch msg ->
    Printf.sprintf "Pattern type mismatch: %s" msg
  | BranchTypeMismatch { then_ty; else_ty } ->
    Printf.sprintf "Branch type mismatch: then-branch has type %s, else-branch has type %s"
      (ty_to_string then_ty) (ty_to_string else_ty)
  | QuantityError (qerr, _span) ->
    Printf.sprintf "Quantity error: %s" (Quantity.format_quantity_error qerr)

let format_type_error = show_type_error

(** {1 Context} *)

(** Type checking context.

    [level] tracks the current let-nesting depth for generalization.
    Variables bound at a deeper level than the current one can be
    generalized when we exit back to a shallower level. *)
type context = {
  var_types : (Symbol.symbol_id, scheme) Hashtbl.t;
  (** Symbol-ID-keyed type map — used by resolve.ml for imports *)
  name_types : (string, scheme) Hashtbl.t;
  (** Name-keyed type map — used by the type checker for lookups *)
  type_env : (string, ty) Hashtbl.t;
  (** Named type constructors → their definition types *)
  constructor_env : (string, ty) Hashtbl.t;
  (** Data constructors (enum variants) → their function types *)
  symbols : Symbol.t;
  mutable level : int;
  mutable current_eff : eff;
  (** The current effect context — unified with declared effects *)
  trait_registry : Trait.trait_registry;
  (** Trait registry — stores trait definitions and impls for dispatch *)
}

type 'a result = ('a, type_error) Result.t

let ( let* ) = Result.bind

(** Lift a unification result into a type-checking result. *)
let unify_or_err (t1 : ty) (t2 : ty) : unit result =
  match Unify.unify t1 t2 with
  | Ok () -> Ok ()
  | Error ue -> Error (UnificationError ue)

let unify_eff_or_err (e1 : eff) (e2 : eff) : unit result =
  match Unify.unify_eff e1 e2 with
  | Ok () -> Ok ()
  | Error ue -> Error (UnificationError ue)

(** {1 Context management} *)

let create_context (symbols : Symbol.t) : context =
  {
    var_types = Hashtbl.create 128;
    name_types = Hashtbl.create 128;
    type_env = Hashtbl.create 64;
    constructor_env = Hashtbl.create 64;
    symbols;
    level = 0;
    current_eff = fresh_effvar 0;
    trait_registry = Trait.create_registry ();
  }

(** Enter a deeper let-level. *)
let enter_level (ctx : context) : unit =
  ctx.level <- ctx.level + 1

(** Exit a let-level. *)
let exit_level (ctx : context) : unit =
  ctx.level <- ctx.level - 1

(** Bind a variable to a monomorphic type (no generalization). *)
let bind_var (ctx : context) (name : string) (ty : ty) : unit =
  let sc = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
  Hashtbl.replace ctx.name_types name sc

(** Bind a variable to a polymorphic scheme. *)
let bind_scheme (ctx : context) (name : string) (sc : scheme) : unit =
  Hashtbl.replace ctx.name_types name sc

(** {1 Instantiation and generalization} *)

(** Instantiate a polymorphic scheme by replacing bound variables
    with fresh unification variables at the current level. *)
let instantiate (level : int) (sc : scheme) : ty =
  let subst = Hashtbl.create 8 in
  (* Create fresh type variables for each quantified tyvar *)
  List.iter (fun (tv, _kind) ->
    let fresh = fresh_tyvar level in
    Hashtbl.replace subst tv fresh
  ) sc.sc_tyvars;
  (* Apply substitution to the body *)
  let rec apply_subst ty =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, _) ->
          begin match Hashtbl.find_opt subst v with
            | Some fresh -> fresh
            | None -> ty
          end
        | Link t -> apply_subst t
      end
    | TCon _ -> ty
    | TApp (t, args) ->
      TApp (apply_subst t, List.map apply_subst args)
    | TArrow (a, q, b, e) ->
      TArrow (apply_subst a, q, apply_subst b, e)
    | TTuple tys ->
      TTuple (List.map apply_subst tys)
    | TRecord row ->
      TRecord (apply_subst_row row)
    | TVariant row ->
      TVariant (apply_subst_row row)
    | TForall (v, k, body) ->
      TForall (v, k, apply_subst body)
    | TExists (v, k, body) ->
      TExists (v, k, apply_subst body)
    | TRef t -> TRef (apply_subst t)
    | TMut t -> TMut (apply_subst t)
    | TOwn t -> TOwn (apply_subst t)
  and apply_subst_row row =
    match repr_row row with
    | REmpty -> REmpty
    | RExtend (l, ty, rest) ->
      RExtend (l, apply_subst ty, apply_subst_row rest)
    | RVar _ -> row
  in
  apply_subst sc.sc_body

(** Generalize a type by quantifying over all type variables
    that were introduced at a level deeper than the context's
    current level. *)
let generalize (ctx : context) (ty : ty) : scheme =
  let tyvars = ref [] in
  let rec collect ty =
    match repr ty with
    | TVar r ->
      begin match !r with
        | Unbound (v, lev) when lev > ctx.level ->
          if not (List.exists (fun (v', _) -> v' = v) !tyvars) then
            tyvars := (v, Types.KType) :: !tyvars
        | Unbound _ -> ()
        | Link t -> collect t
      end
    | TCon _ -> ()
    | TApp (t, args) ->
      collect t; List.iter collect args
    | TArrow (a, _, b, _) ->
      collect a; collect b
    | TTuple tys ->
      List.iter collect tys
    | TRecord row | TVariant row ->
      collect_row row
    | TForall (_, _, body) | TExists (_, _, body) ->
      collect body
    | TRef t | TMut t | TOwn t ->
      collect t
  and collect_row row =
    match repr_row row with
    | REmpty -> ()
    | RExtend (_, ty, rest) ->
      collect ty; collect_row rest
    | RVar _ -> ()
  in
  collect ty;
  { sc_tyvars = !tyvars; sc_effvars = []; sc_rowvars = [];
    sc_body = ty }

(** Look up a variable, returning a fresh instantiation of its scheme. *)
let lookup_var (ctx : context) (name : string) : ty result =
  match Hashtbl.find_opt ctx.name_types name with
  | Some sc -> Ok (instantiate ctx.level sc)
  | None -> Error (UnboundVariable name)

(** {1 Kind checking} *)

let rec infer_kind (ctx : context) (ty : ty) : kind result =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (_, _) -> Ok KType
      | Link t -> infer_kind ctx t
    end
  | TCon name ->
    begin match name with
      | "Array" | "Option" | "List" | "Vec" | "Cmd" -> Ok (KArrow (KType, KType))
      | "Result" -> Ok (KArrow (KType, KArrow (KType, KType)))
      | _ -> Ok KType
    end
  | TApp (head, args) ->
    let* k = infer_kind ctx head in
    check_kind_app ctx k args
  | TArrow (_, _, _, _) | TTuple _ | TRecord _ | TVariant _ ->
    Ok KType
  | TForall (_, _, body) | TExists (_, _, body) ->
    let* _ = infer_kind ctx body in
    Ok KType
  | TRef t | TMut t | TOwn t ->
    infer_kind ctx t

and check_kind (ctx : context) (ty : ty) (expected : kind) : unit result =
  let* got = infer_kind ctx ty in
  if got = expected then Ok ()
  else Error (NotImplemented (Printf.sprintf "Kind mismatch: expected %s, got %s" (show_kind expected) (show_kind got)))

and check_kind_app (ctx : context) (k : kind) (args : ty list) : kind result =
  match args with
  | [] -> Ok k
  | arg :: rest ->
    begin match k with
      | KArrow (param_k, ret_k) ->
        let* () = check_kind ctx arg param_k in
        check_kind_app ctx ret_k rest
      | _ -> Error (NotImplemented "Too many arguments for kind")
    end

(** {1 AST type_expr → internal ty conversion} *)

let lower_quantity (q : Ast.quantity) : Types.quantity =
  match q with
  | QZero -> QZero
  | QOne -> QOne
  | QOmega -> QOmega

(** Convert an AST [type_expr] to an internal [ty].

    Type variables are looked up in [type_env]; unknown names become
    fresh unification variables so that inference can resolve them. *)
let rec lower_type_expr (ctx : context) (te : type_expr) : ty =
  match te with
  | TyVar { name; _ } ->
    begin match Hashtbl.find_opt ctx.type_env name with
      | Some ty -> ty
      | None ->
        (* Unresolved type variable: create a fresh unification var *)
        let tv = fresh_tyvar ctx.level in
        Hashtbl.replace ctx.type_env name tv;
        tv
    end
  | TyCon { name; _ } ->
    begin match name with
      | "Int" -> ty_int
      | "Float" -> ty_float
      | "Bool" -> ty_bool
      | "String" -> ty_string
      | "Char" -> ty_char
      | "Unit" | "()" -> ty_unit
      | "Never" -> ty_never
      | _ ->
        begin match Hashtbl.find_opt ctx.type_env name with
          | Some ty -> ty
          | None -> TCon name
        end
    end
  | TyApp ({ name; _ }, args) ->
    let head = match Hashtbl.find_opt ctx.type_env name with
      | Some ty -> ty
      | None -> TCon name
    in
    let arg_tys = List.map (fun arg ->
      match arg with
      | TyArg te -> lower_type_expr ctx te
    ) args in
    TApp (head, arg_tys)
  | TyArrow (a, q_opt, b, eff_opt) ->
    let a' = lower_type_expr ctx a in
    let b' = lower_type_expr ctx b in
    let q = match q_opt with
      | Some q -> lower_quantity q
      | None -> QOmega  (* Default to unrestricted *)
    in
    let eff = match eff_opt with
      | Some e -> lower_effect_expr ctx e
      | None -> EPure
    in
    TArrow (a', q, b', eff)
  | TyTuple [] ->
    ty_unit
  | TyTuple tes ->
    TTuple (List.map (lower_type_expr ctx) tes)
  | TyRecord (fields, _row_var) ->
    let row = List.fold_right (fun (rf : row_field) acc ->
      RExtend (rf.rf_name.name, lower_type_expr ctx rf.rf_ty, acc)
    ) fields REmpty in
    TRecord row
  | TyOwn te -> TOwn (lower_type_expr ctx te)
  | TyRef te -> TRef (lower_type_expr ctx te)
  | TyMut te -> TMut (lower_type_expr ctx te)
  | TyHole ->
    fresh_tyvar ctx.level

and lower_effect_expr (ctx : context) (ee : effect_expr) : eff =
  match ee with
  | EffVar { name; _ } ->
    begin match name with
      | "Pure" -> EPure
      | _ -> ESingleton name
    end
  | EffCon ({ name; _ }, _args) ->
    ESingleton name
  | EffUnion (e1, e2) ->
    EUnion [lower_effect_expr ctx e1; lower_effect_expr ctx e2]

(** {1 Binary/unary operator typing} *)

(** Return the type of a binary operator given its operand types. *)
let type_of_binop (op : binary_op) : ty * ty * ty =
  match op with
  (* Arithmetic: Int -> Int -> Int *)
  | OpAdd | OpSub | OpMul | OpDiv | OpMod ->
    (ty_int, ty_int, ty_int)
  (* Comparison: Int -> Int -> Bool *)
  | OpLt | OpLe | OpGt | OpGe ->
    (ty_int, ty_int, ty_bool)
  (* Equality: polymorphic, but we approximate as 'a -> 'a -> Bool *)
  | OpEq | OpNe ->
    let tv = fresh_tyvar 0 in
    (tv, tv, ty_bool)
  (* Logical: Bool -> Bool -> Bool *)
  | OpAnd | OpOr ->
    (ty_bool, ty_bool, ty_bool)
  (* Bitwise: Int -> Int -> Int *)
  | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr ->
    (ty_int, ty_int, ty_int)
  | OpConcat ->
    (ty_string, ty_string, ty_string)

let type_of_unop (op : unary_op) : ty * ty =
  match op with
  | OpNeg -> (ty_int, ty_int)
  | OpNot -> (ty_bool, ty_bool)
  | OpBitNot -> (ty_int, ty_int)
  | OpRef ->
    let tv = fresh_tyvar 0 in
    (tv, TRef tv)
  | OpDeref ->
    let tv = fresh_tyvar 0 in
    (TRef tv, tv)

(** {1 Pattern typing} *)

(** Type-check a pattern and return the list of bindings it introduces.
    Each binding is [(name, ty)].  The pattern is checked against [expected_ty]
    when in checking mode. *)
let rec check_pattern (ctx : context) (pat : pattern) (expected : ty)
    : ((string * ty) list) result =
  match pat with
  | PatWildcard _ ->
    Ok []
  | PatVar { name; _ } ->
    Ok [(name, expected)]
  | PatLit lit ->
    let lit_ty = type_of_literal lit in
    let* () = unify_or_err expected lit_ty in
    Ok []
  | PatTuple pats ->
    let n = List.length pats in
    let elem_tys = List.init n (fun _ -> fresh_tyvar ctx.level) in
    let* () = unify_or_err expected (TTuple elem_tys) in
    let* bindings_list = check_patterns ctx pats elem_tys in
    Ok (List.concat bindings_list)
  | PatCon ({ name; _ }, sub_pats) ->
    (* Look up the constructor type *)
    begin match Hashtbl.find_opt ctx.constructor_env name with
      | Some ctor_ty ->
        let ctor_ty' = instantiate ctx.level
          { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ctor_ty } in
        (* Constructor type should be T1 -> T2 -> ... -> ResultType *)
        let rec peel_arrows ty pats acc =
          match pats with
          | [] ->
            let* () = unify_or_err expected ty in
            Ok (List.rev acc)
          | p :: rest ->
            begin match repr ty with
              | TArrow (param_ty, _, ret_ty, _) ->
                let* binds = check_pattern ctx p param_ty in
                peel_arrows ret_ty rest (binds :: acc)
              | _ ->
                Error (ArityMismatch { name; expected = List.length sub_pats;
                                       got = List.length pats - List.length rest })
            end
        in
        let* bindings_list = peel_arrows ctor_ty' sub_pats [] in
        Ok (List.concat bindings_list)
      | None ->
        (* Unknown constructor — if it has no arguments, treat as a variant tag *)
        if sub_pats = [] then begin
          (* Unify expected with a variant containing this tag *)
          Ok []
        end else
          Error (UnboundVariable name)
    end
  | PatRecord (fields, _has_rest) ->
    let bindings = ref [] in
    List.iter (fun (({ name; _ } : ident), pat_opt) ->
      let field_ty = fresh_tyvar ctx.level in
      begin match pat_opt with
        | Some sub_pat ->
          begin match check_pattern ctx sub_pat field_ty with
            | Ok bs -> bindings := bs @ !bindings
            | Error _ -> ()
          end
        | None ->
          bindings := (name, field_ty) :: !bindings
      end
    ) fields;
    Ok !bindings
  | PatOr (p1, _p2) ->
    (* Both branches must produce the same bindings; we check the first *)
    check_pattern ctx p1 expected
  | PatAs ({ name; _ }, sub_pat) ->
    let* binds = check_pattern ctx sub_pat expected in
    Ok ((name, expected) :: binds)

and check_patterns (ctx : context) (pats : pattern list) (tys : ty list)
    : ((string * ty) list list) result =
  match pats, tys with
  | [], [] -> Ok []
  | p :: ps, t :: ts ->
    let* binds = check_pattern ctx p t in
    let* rest = check_patterns ctx ps ts in
    Ok (binds :: rest)
  | _ -> Error (PatternTypeMismatch "pattern/type list length mismatch")

(** {1 Literal typing} *)

and type_of_literal (lit : literal) : ty =
  match lit with
  | LitInt _ -> ty_int
  | LitFloat _ -> ty_float
  | LitBool _ -> ty_bool
  | LitChar _ -> ty_char
  | LitString _ -> ty_string
  | LitUnit _ -> ty_unit

(** {1 Expression synthesis (mode ⇒)} *)

(** Synthesize a type for an expression. *)
let rec synth (ctx : context) (expr : expr) : ty result =
  match expr with
  (* Literals *)
  | ExprLit lit ->
    Ok (type_of_literal lit)

  (* Variables — instantiate their scheme *)
  | ExprVar { name; _ } ->
    lookup_var ctx name

  (* Let bindings: let pat = e1 in e2 *)
  | ExprLet { el_pat; el_ty; el_value; el_body; el_mut = _; el_quantity = _ } ->
    (* Synthesize or check the value *)
    enter_level ctx;
    let* val_ty = begin match el_ty with
      | Some te ->
        let ann_ty = lower_type_expr ctx te in
        let* () = check ctx el_value ann_ty in
        Ok ann_ty
      | None ->
        synth ctx el_value
    end in
    exit_level ctx;
    (* Generalize the value type *)
    let sc = generalize ctx val_ty in
    (* Bind pattern variables *)
    let* bindings = check_pattern ctx el_pat val_ty in
    let old_bindings = List.map (fun (name, _) ->
      (name, Hashtbl.find_opt ctx.name_types name)
    ) bindings in
    List.iter (fun (name, _ty) ->
      bind_scheme ctx name sc
    ) bindings;
    (* Type-check the body if present, else return Unit *)
    let result = begin match el_body with
      | Some body -> synth ctx body
      | None -> Ok ty_unit
    end in
    (* Restore old bindings *)
    List.iter (fun (name, old) ->
      match old with
      | Some sc' -> Hashtbl.replace ctx.name_types name sc'
      | None -> Hashtbl.remove ctx.name_types name
    ) old_bindings;
    result

  (* If-then-else *)
  | ExprIf { ei_cond; ei_then; ei_else } ->
    let* () = check ctx ei_cond ty_bool in
    let* then_ty = synth ctx ei_then in
    begin match ei_else with
      | Some else_expr ->
        let* else_ty = synth ctx else_expr in
        let* () = unify_or_err then_ty else_ty in
        Ok then_ty
      | None ->
        let () =
          if ty_to_string then_ty <> ty_to_string ty_unit then
            Format.eprintf "If without else returns %s; then=%s cond=%s\n%!"
              (ty_to_string then_ty) (expr_summary ei_then) (expr_summary ei_cond)
          else
            ()
        in
        (* No else branch: result is Unit *)
        let* () = unify_or_err then_ty ty_unit in
        Ok ty_unit
    end

  (* Match expressions *)
  | ExprMatch { em_scrutinee; em_arms } ->
    let* scrut_ty = synth ctx em_scrutinee in
    let result_ty = fresh_tyvar ctx.level in
    let* () = List.fold_left (fun acc (arm : match_arm) ->
      let* () = acc in
      let* bindings = check_pattern ctx arm.ma_pat scrut_ty in
      (* Save and bind pattern variables *)
      let old = List.map (fun (n, _) ->
        (n, Hashtbl.find_opt ctx.name_types n)
      ) bindings in
      List.iter (fun (n, t) -> bind_var ctx n t) bindings;
      (* Check guard if present *)
      let* () = match arm.ma_guard with
        | Some guard -> check ctx guard ty_bool
        | None -> Ok ()
      in
      (* Body must unify with result type *)
      let* arm_ty = synth ctx arm.ma_body in
      let* () = unify_or_err result_ty arm_ty in
      (* Restore *)
      List.iter (fun (n, old_sc) ->
        match old_sc with
        | Some sc -> Hashtbl.replace ctx.name_types n sc
        | None -> Hashtbl.remove ctx.name_types n
      ) old;
      Ok ()
    ) (Ok ()) em_arms in
    Ok result_ty

  (* Lambda *)
  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
    let param_tys = List.map (fun (p : param) ->
      lower_type_expr ctx p.p_ty
    ) elam_params in
    (* Save old bindings, bind params *)
    let old = List.map2 (fun (p : param) ty ->
      let old = Hashtbl.find_opt ctx.name_types p.p_name.name in
      bind_var ctx p.p_name.name ty;
      (p.p_name.name, old)
    ) elam_params param_tys in
    (* Synthesize or check body *)
    let* body_ty = begin match elam_ret_ty with
      | Some te ->
        let ret_ty = lower_type_expr ctx te in
        let* () = check ctx elam_body ret_ty in
        Ok ret_ty
      | None ->
        synth ctx elam_body
    end in
    (* Restore *)
    List.iter (fun (n, old_sc) ->
      match old_sc with
      | Some sc -> Hashtbl.replace ctx.name_types n sc
      | None -> Hashtbl.remove ctx.name_types n
    ) old;
    (* Build the arrow type: curried for multi-param.
       Each arrow carries the declared quantity of the corresponding parameter.
       If the parameter has no explicit annotation, we default to QOmega
       (unrestricted), matching the convention used by check_fn_decl. *)
    let eff = fresh_effvar ctx.level in
    let param_qty_pairs = List.map2 (fun (p : param) param_ty ->
      let q = match p.p_quantity with
        | Some q -> lower_quantity q
        | None   -> QOmega
      in
      (q, param_ty)
    ) elam_params param_tys in
    let ty = List.fold_right (fun (q, param_ty) acc ->
      TArrow (param_ty, q, acc, eff)
    ) param_qty_pairs body_ty in
    Ok ty

  (* Function application *)
  | ExprApp (fn_expr, args) ->
    let* fn_ty = synth ctx fn_expr in
    apply_args ctx fn_ty args

  (* Tuple *)
  | ExprTuple exprs ->
    let* tys = synth_list ctx exprs in
    Ok (TTuple tys)

  (* Array *)
  | ExprArray exprs ->
    begin match exprs with
      | [] ->
        let elem_ty = fresh_tyvar ctx.level in
        Ok (TApp (TCon "Array", [elem_ty]))
      | first :: rest ->
        let* first_ty = synth ctx first in
        let* () = List.fold_left (fun acc e ->
          let* () = acc in
          check ctx e first_ty
        ) (Ok ()) rest in
        Ok (TApp (TCon "Array", [first_ty]))
    end

  (* Record literal *)
  | ExprRecord { er_fields; er_spread = _ } ->
    let* field_tys = List.fold_left (fun acc (({ name; _ } : ident), expr_opt) ->
      let* fields = acc in
      let* ty = begin match expr_opt with
        | Some e -> synth ctx e
        | None -> lookup_var ctx name
      end in
      Ok ((name, ty) :: fields)
    ) (Ok []) er_fields in
    let row = List.fold_right (fun (name, ty) acc ->
      RExtend (name, ty, acc)
    ) field_tys REmpty in
    Ok (TRecord row)

  (* Field access — first try record-field projection, then trait method lookup *)
  | ExprField (obj, { name = field; _ }) ->
    let* obj_ty = synth ctx obj in
    let field_ty = fresh_tyvar ctx.level in
    let rest_row = fresh_rowvar ctx.level in
    let expected_record = TRecord (RExtend (field, field_ty, rest_row)) in
    begin match Unify.unify (repr obj_ty) expected_record with
    | Ok () -> Ok field_ty
    | Error _ ->
      (* Record projection failed — try trait method dispatch.
         We search all registered impls for a method named [field]
         whose self type unifies with [obj_ty]. *)
      begin match Trait.find_method_for_type ctx.trait_registry obj_ty field with
      | Some (_impl, method_decl) ->
        (* Build the method's monomorphic type from the fn_decl.
           Parameters: each p_ty is lowered to an internal ty.
           Return type defaults to a fresh type variable when omitted.
           Effects are left as a fresh effect variable (unannotated). *)
        let param_tys = List.map (fun (p : Ast.param) ->
          lower_type_expr ctx p.p_ty
        ) method_decl.Ast.fd_params in
        let ret_ty = match method_decl.Ast.fd_ret_ty with
          | Some te -> lower_type_expr ctx te
          | None -> fresh_tyvar ctx.level
        in
        let eff = fresh_effvar ctx.level in
        (* Fold params into a curried arrow, right-to-left *)
        let method_ty = List.fold_right (fun (param_and_ty) acc ->
          let (p, pt) = (param_and_ty : Ast.param * ty) in
          let q = match p.Ast.p_quantity with
            | Some q -> lower_quantity q
            | None -> QOmega
          in
          TArrow (pt, q, acc, eff)
        ) (List.combine method_decl.Ast.fd_params param_tys) ret_ty in
        Ok method_ty
      | None ->
        (* Neither record field nor trait method — report a field-not-found error *)
        Error (FieldNotFound { field; record_ty = obj_ty })
      end
    end

  (* Tuple indexing *)
  | ExprTupleIndex (tup, idx) ->
    let* tup_ty = synth ctx tup in
    begin match repr tup_ty with
      | TTuple tys ->
        if idx >= 0 && idx < List.length tys then
          Ok (List.nth tys idx)
        else
          Error (TupleIndexOutOfBounds { index = idx; length = List.length tys })
      | _ ->
        (* Create a tuple type with enough slots *)
        let n = idx + 1 in
        let elem_tys = List.init n (fun _ -> fresh_tyvar ctx.level) in
        let* () = unify_or_err tup_ty (TTuple elem_tys) in
        Ok (List.nth elem_tys idx)
    end

  (* Array indexing *)
  | ExprIndex (arr, idx_expr) ->
    let* arr_ty = synth ctx arr in
    let* () = check ctx idx_expr ty_int in
    let elem_ty = fresh_tyvar ctx.level in
    let* () = unify_or_err arr_ty (TApp (TCon "Array", [elem_ty])) in
    Ok elem_ty

  (* Binary operators *)
  | ExprBinary (lhs, op, rhs) ->
    let (lhs_ty, rhs_ty, result_ty) = type_of_binop op in
    let* () = check ctx lhs lhs_ty in
    let* () = check ctx rhs rhs_ty in
    Ok result_ty

  (* Unary operators *)
  | ExprUnary (op, operand) ->
    let (operand_ty, result_ty) = type_of_unop op in
    let* () = check ctx operand operand_ty in
    Ok result_ty

  (* Block *)
  | ExprBlock blk ->
    synth_block ctx blk

  (* Return — return type is Never (it doesn't produce a value locally) *)
  | ExprReturn expr_opt ->
    begin match expr_opt with
      | Some e ->
        let* _ty = synth ctx e in
        Ok ty_never
      | None ->
        Ok ty_never
    end

  (* Variant constructor: Type::Variant *)
  | ExprVariant ({ name = _type_name; _ }, { name = variant_name; _ }) ->
    begin match Hashtbl.find_opt ctx.constructor_env variant_name with
      | Some ctor_ty -> Ok ctor_ty
      | None ->
        (* Unknown variant — return a fresh variable *)
        Ok (fresh_tyvar ctx.level)
    end

  (* Row restriction *)
  | ExprRowRestrict (obj, { name = field; _ }) ->
    let* obj_ty = synth ctx obj in
    let field_ty = fresh_tyvar ctx.level in
    let rest_row = fresh_rowvar ctx.level in
    let* () = unify_or_err obj_ty (TRecord (RExtend (field, field_ty, rest_row))) in
    Ok (TRecord rest_row)

  (* Span wrapper — unwrap and recurse *)
  | ExprSpan (inner, _span) ->
    synth ctx inner

  (* Effect handling *)
  | ExprHandle { eh_body; _ } ->
    synth ctx eh_body

  (* Resume *)
  | ExprResume expr_opt ->
    begin match expr_opt with
      | Some e -> synth ctx e
      | None -> Ok ty_unit
    end

  (* Try-catch-finally.
     Body type is synthesised first and becomes the overall result type.
     Each catch arm is type-checked against a fresh error-type variable
     (the effect system will constrain this once effect inference is
     complete) and its body type must unify with the result type.
     The finally block (if present) is checked for unit — its value is
     discarded at runtime; a non-unit finally type is a type error. *)
  | ExprTry { et_body; et_catch; et_finally } ->
    let* body_ty = synth_block ctx et_body in
    let result_ty = fresh_tyvar ctx.level in
    let* () = unify_or_err result_ty body_ty in
    let* () = match et_catch with
      | None -> Ok ()
      | Some arms ->
          (* All catch arms match against a single opaque error type. *)
          let err_ty = fresh_tyvar ctx.level in
          List.fold_left (fun acc (arm : match_arm) ->
            let* () = acc in
            let* bindings = check_pattern ctx arm.ma_pat err_ty in
            (* Save bindings that will be shadowed, then install new ones. *)
            let old = List.map (fun (n, _) ->
              (n, Hashtbl.find_opt ctx.name_types n)
            ) bindings in
            List.iter (fun (n, t) -> bind_var ctx n t) bindings;
            let* arm_ty = synth ctx arm.ma_body in
            let* () = unify_or_err result_ty arm_ty in
            (* Restore previous bindings. *)
            List.iter (fun (n, old_sc) ->
              match old_sc with
              | Some sc -> Hashtbl.replace ctx.name_types n sc
              | None    -> Hashtbl.remove ctx.name_types n
            ) old;
            Ok ()
          ) (Ok ()) arms
    in
    let* () = match et_finally with
      | None -> Ok ()
      | Some blk ->
          (* Finally must be unit — its value is always discarded. *)
          let* fin_ty = synth_block ctx blk in
          unify_or_err fin_ty ty_unit
    in
    Ok result_ty

  (* Unsafe *)
  | ExprUnsafe _ ->
    Ok (fresh_tyvar ctx.level)

(** Apply a function type to a list of arguments. *)
and apply_args (ctx : context) (fn_ty : ty) (args : expr list) : ty result =
  match args with
  | [] -> Ok fn_ty
  | arg :: rest ->
    let fn_ty' = repr fn_ty in
    match fn_ty' with
      | TArrow (param_ty, _q, ret_ty, _eff) ->
        let* () = check ctx arg param_ty in
        apply_args ctx ret_ty rest
      | TVar _ ->
        (* Unknown function type: create fresh arrow *)
        let param_ty = fresh_tyvar ctx.level in
        let ret_ty = fresh_tyvar ctx.level in
        let eff = fresh_effvar ctx.level in
        let q = lower_quantity QOmega in (* Default for unknown *)
        let* () = unify_or_err fn_ty' (TArrow (param_ty, q, ret_ty, eff)) in
        let* () = check ctx arg param_ty in
        apply_args ctx ret_ty rest
      | _ ->
        Error (NotAFunction fn_ty')
and synth_list (ctx : context) (exprs : expr list) : (ty list) result =
  List.fold_right (fun e acc ->
    let* tys = acc in
    let* ty = synth ctx e in
    Ok (ty :: tys)
  ) exprs (Ok [])

(** {1 Block and statement typing} *)

(** Returns true if an expression always diverges — i.e., it never produces
    a value in normal control flow. Used to give blocks a Never type when
    all paths exit via return. *)
and always_diverges (e : expr) : bool =
  match e with
  | ExprReturn _ -> true
  | ExprBlock blk -> block_always_diverges blk
  | ExprIf { ei_cond = _; ei_then; ei_else = Some else_e } ->
    always_diverges ei_then && always_diverges else_e
  | _ -> false

(** Returns true if a block always diverges (all paths return). *)
and block_always_diverges (blk : block) : bool =
  match blk.blk_expr with
  | Some e -> always_diverges e
  | None ->
    begin match List.rev blk.blk_stmts with
    | StmtExpr e :: _ -> always_diverges e
    | _ -> false
    end

and synth_block (ctx : context) (blk : block) : ty result =
  (* Type-check each statement for side effects *)
  let* () = List.fold_left (fun acc stmt ->
    let* () = acc in
    check_stmt ctx stmt
  ) (Ok ()) blk.blk_stmts in
  (* The block's type is the type of the final expression, or Unit *)
  match blk.blk_expr with
  | Some e -> synth ctx e
  | None ->
    (* When all paths through the block diverge (every code path ends with
       return/break/etc.), the block has type Never rather than Unit. This
       allows functions declared as returning T to have bodies that exclusively
       use `return expr;` rather than a final expression. *)
    if block_always_diverges blk
    then Ok ty_never
    else Ok ty_unit

and check_stmt (ctx : context) (stmt : stmt) : unit result =
  match stmt with
  | StmtLet { sl_pat; sl_ty; sl_value; sl_mut = _; sl_quantity = _ } ->
    enter_level ctx;
    let* val_ty = begin match sl_ty with
      | Some te ->
        let ann_ty = lower_type_expr ctx te in
        let* () = check ctx sl_value ann_ty in
        Ok ann_ty
      | None ->
        synth ctx sl_value
    end in
    exit_level ctx;
    let sc = generalize ctx val_ty in
    let* bindings = check_pattern ctx sl_pat val_ty in
    List.iter (fun (name, _ty) ->
      bind_scheme ctx name sc
    ) bindings;
    Ok ()
  | StmtExpr e ->
    let* _ty = synth ctx e in
    Ok ()
  | StmtAssign (lhs, _op, rhs) ->
    let* lhs_ty = synth ctx lhs in
    check ctx rhs lhs_ty
  | StmtWhile (cond, body) ->
    let* () = check ctx cond ty_bool in
    let* _ty = synth_block ctx body in
    Ok ()
  | StmtFor (pat, iter_expr, body) ->
    let* iter_ty = synth ctx iter_expr in
    let elem_ty = fresh_tyvar ctx.level in
    let* () = unify_or_err iter_ty (TApp (TCon "Array", [elem_ty])) in
    let* bindings = check_pattern ctx pat elem_ty in
    let old = List.map (fun (n, _) ->
      (n, Hashtbl.find_opt ctx.name_types n)
    ) bindings in
    List.iter (fun (n, t) -> bind_var ctx n t) bindings;
    let* _ty = synth_block ctx body in
    List.iter (fun (n, old_sc) ->
      match old_sc with
      | Some sc -> Hashtbl.replace ctx.name_types n sc
      | None -> Hashtbl.remove ctx.name_types n
    ) old;
    Ok ()

(** {1 Checking mode (mode ⇐)} *)

(** Check that an expression has the expected type. *)
and check (ctx : context) (expr : expr) (expected : ty) : unit result =
  match expr with
  (* Lambda against arrow type: check mode is more precise.
     We peel the expected arrow type one param at a time.  For each param:
     - If the param has an explicit quantity annotation, we verify it is
       consistent with the arrow quantity from the expected type.
     - The arrow quantity from the expected type is used as the definitive
       quantity for binding (so an unannotated lambda param correctly inherits
       the quantity from its context, e.g. a @linear annotation on the let). *)
  | ExprLambda { elam_params; elam_body; elam_ret_ty = _ }
    when (match repr expected with TArrow _ -> true | _ -> false) ->
    let rec peel_arrows ty params =
      match params, repr ty with
      | [], _ -> Ok ()
      | p :: rest, TArrow (param_ty, q, ret_ty, _eff) ->
        (* Validate explicit quantity annotation against the expected arrow. *)
        let* () = match p.p_quantity with
          | Some pq ->
            let pq' = lower_quantity pq in
            if pq' = q then Ok ()
            else Error (TypeMismatch {
              expected = TArrow (param_ty, q,   ret_ty, EPure);
              got      = TArrow (param_ty, pq', ret_ty, EPure);
            })
          | None -> Ok ()
        in
        bind_var ctx p.p_name.name param_ty;
        peel_arrows ret_ty rest
      | _ -> synth_and_unify ctx expr expected
    in
    let old = List.map (fun (p : param) ->
      (p.p_name.name, Hashtbl.find_opt ctx.name_types p.p_name.name)
    ) elam_params in
    let* () = peel_arrows expected elam_params in
    (* Now check the body against the final return type *)
    let final_ret = List.fold_left (fun ty _ ->
      match repr ty with
      | TArrow (_, _, ret, _) -> ret
      | _ -> ty
    ) expected elam_params in
    let* () = check ctx elam_body final_ret in
    (* Restore *)
    List.iter (fun (n, old_sc) ->
      match old_sc with
      | Some sc -> Hashtbl.replace ctx.name_types n sc
      | None -> Hashtbl.remove ctx.name_types n
    ) old;
    Ok ()

  (* If without else against Unit *)
  | ExprIf { ei_cond; ei_then; ei_else = None } ->
    let* () = check ctx ei_cond ty_bool in
    let* () = check ctx ei_then ty_unit in
    unify_or_err expected ty_unit

  (* Default: synth and unify *)
  | _ ->
    synth_and_unify ctx expr expected

and synth_and_unify (ctx : context) (expr : expr) (expected : ty) : unit result =
  let* got = synth ctx expr in
  unify_or_err expected got

(** {1 Declaration checking} *)

(** Register built-in types and functions. *)
let register_builtins (ctx : context) : unit =
  (* Arithmetic builtins *)
  let int_binop = TArrow (ty_int, QOmega, TArrow (ty_int, QOmega, ty_int, EPure), EPure) in
  let float_binop = TArrow (ty_float, QOmega, TArrow (ty_float, QOmega, ty_float, EPure), EPure) in
  bind_var ctx "print" (TArrow (ty_string, QOmega, ty_unit, ESingleton "IO"));
  bind_var ctx "println" (TArrow (ty_string, QOmega, ty_unit, ESingleton "IO"));
  bind_var ctx "read_line" (TArrow (ty_unit, QOmega, ty_string, ESingleton "IO"));
  bind_var ctx "int_to_string" (TArrow (ty_int, QOmega, ty_string, EPure));
  bind_var ctx "int" (TArrow (ty_float, QOmega, ty_int, EPure));
  bind_var ctx "float" (TArrow (ty_int, QOmega, ty_float, EPure));
  bind_var ctx "float_to_string" (TArrow (ty_float, QOmega, ty_string, EPure));
  bind_var ctx "string_length" (TArrow (ty_string, QOmega, ty_int, EPure));
  bind_var ctx "sqrt" (TArrow (ty_float, QOmega, ty_float, EPure));
  bind_var ctx "abs" int_binop;
  bind_var ctx "max" int_binop;
  bind_var ctx "min" int_binop;
  bind_var ctx "pow_float" float_binop;
  bind_var ctx "len" (let tv = fresh_tyvar 0 in
    TArrow (TApp (TCon "Array", [tv]), QOmega, ty_int, EPure));
  bind_var ctx "panic" (TArrow (ty_string, QOmega, ty_never, EPure));
  bind_var ctx "exit" (TArrow (ty_int, QOmega, ty_never, ESingleton "IO"));
  (* TEA runtime — accepts any record, returns unit with IO effect *)
  let tea_tv = fresh_tyvar 0 in
  bind_var ctx "tea_run" (TArrow (tea_tv, QOmega, ty_unit, ESingleton "IO"));
  (* Cmd Msg — linear side-effect obligation type (Stage 11).
     Cmd has kind * → *.
     cmd_none : Cmd 'a — the no-op command; a linear value obligating zero IO.
     cmd_perform : (unit ->{IO} unit) -> Cmd 'a — wraps an IO action as a Cmd.
     Cmd values bound with a [Cmd _] type annotation are automatically
     treated as QOne (linear) by the quantity checker. *)
  let cmd_tv  = fresh_tyvar 0 in
  let cmd_tv2 = fresh_tyvar 0 in
  bind_var ctx "cmd_none"
    (TApp (TCon "Cmd", [cmd_tv]));
  bind_var ctx "cmd_perform"
    (TArrow (TArrow (ty_unit, QOmega, ty_unit, ESingleton "IO"),
             QOmega,
             TApp (TCon "Cmd", [cmd_tv2]),
             EPure))

(** Check a top-level function declaration. *)
let check_fn_decl (ctx : context) (fd : fn_decl) : unit result =
  (* Lower parameter types *)
  let* param_tys = List.fold_left (fun acc (p : param) ->
    let* tys = acc in
    let ty = lower_type_expr ctx p.p_ty in
    let* () = check_kind ctx ty KType in
    Ok (ty :: tys)
  ) (Ok []) fd.fd_params in
  let param_tys = List.rev param_tys in
  (* Lower return type *)
  let* ret_ty = match fd.fd_ret_ty with
    | Some te ->
      let ty = lower_type_expr ctx te in
      let* () = check_kind ctx ty KType in
      Ok ty
    | None -> Ok (fresh_tyvar ctx.level)
  in
  (* Lower effect *)
  (* Build the function type *)
  let fn_eff = match fd.fd_eff with
    | Some ee -> lower_effect_expr ctx ee
    | None -> fresh_effvar ctx.level
  in
  let fn_ty = List.fold_right2 (fun param_ty (p : param) acc ->
    let q = match p.p_quantity with
      | Some q -> lower_quantity q
      | None -> QOmega
    in
    TArrow (param_ty, q, acc, fn_eff)
  ) param_tys fd.fd_params ret_ty in
  (* Bind the function name (allows recursion) *)
  bind_var ctx fd.fd_name.name fn_ty;
  (* Bind parameters *)
  let old = List.map2 (fun (p : param) ty ->
    let old = Hashtbl.find_opt ctx.name_types p.p_name.name in
    bind_var ctx p.p_name.name ty;
    (p.p_name.name, old)
  ) fd.fd_params param_tys in
  (* Check the body against the return type *)
  let* () = begin match fd.fd_body with
    | FnBlock blk ->
      let* body_ty = synth_block ctx blk in
      unify_or_err ret_ty body_ty
    | FnExpr e ->
      check ctx e ret_ty
  end in
  (* Restore parameter bindings *)
  List.iter (fun (n, old_sc) ->
    match old_sc with
    | Some sc -> Hashtbl.replace ctx.name_types n sc
    | None -> Hashtbl.remove ctx.name_types n
  ) old;
  (* Generalize and rebind the function with its polymorphic type *)
  let sc = generalize ctx fn_ty in
  bind_scheme ctx fd.fd_name.name sc;
  Ok ()

(** Register a type declaration in the context. *)
let register_type_decl (ctx : context) (td : type_decl) : unit result =
  let* ty = match td.td_body with
    | TyAlias te ->
      let ty = lower_type_expr ctx te in
      let* () = check_kind ctx ty KType in
      Ok ty
    | TyStruct fields ->
      (* Register struct as a record type constructor *)
      let row = List.fold_right (fun (sf : struct_field) acc ->
        RExtend (sf.sf_name.name, lower_type_expr ctx sf.sf_ty, acc)
      ) fields REmpty in
      let ty = TRecord row in
      let* () = check_kind ctx ty KType in
      Ok ty
    | TyEnum variants ->
      (* Register each variant as a constructor *)
      let result_ty = match td.td_type_params with
        | [] -> TCon td.td_name.name
        | params ->
          let tparams = List.map (fun tp ->
            match tp.tp_name.name with
            | _ ->
              let tv = fresh_tyvar 0 in
              (* Note: In a full impl we'd map param name to tv in a local env *)
              tv
          ) params in
          TApp (TCon td.td_name.name, tparams)
      in
      List.iter (fun (vd : variant_decl) ->
        let ctor_ty = List.fold_right (fun field_te acc ->
          TArrow (lower_type_expr ctx field_te, QOmega, acc, EPure)
        ) vd.vd_fields result_ty in
        (* If it has type params, we should really bind a TForall scheme *)
        let sc = match td.td_type_params with
          | [] -> { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ctor_ty }
          | _ -> generalize ctx ctor_ty
        in
        Hashtbl.replace ctx.constructor_env vd.vd_name.name ctor_ty;
        (* Also bind as a variable for ExprVar references *)
        if vd.vd_fields = [] then
          bind_scheme ctx vd.vd_name.name sc
        else
          bind_scheme ctx vd.vd_name.name sc
      ) variants;
      Ok (TCon td.td_name.name)
  in
  Hashtbl.replace ctx.type_env td.td_name.name ty;
  Ok ()

(** Register an effect declaration. *)
let register_effect_decl (ctx : context) (ed : effect_decl) : unit result =
  List.iter (fun (op : effect_op_decl) ->
    let param_tys = List.map (fun (p : param) ->
      lower_type_expr ctx p.p_ty
    ) op.eod_params in
    let ret_ty = match op.eod_ret_ty with
      | Some te -> lower_type_expr ctx te
      | None -> ty_unit
    in
    let eff = ESingleton ed.ed_name.name in
    let op_ty = List.fold_right (fun pty acc ->
      TArrow (pty, QOmega, acc, eff)
    ) param_tys ret_ty in
    bind_var ctx op.eod_name.name op_ty
  ) ed.ed_ops;
  Ok ()

(** Check a single top-level declaration. *)
let check_decl (ctx : context) (decl : top_level) : (unit, type_error) Result.t =
  match decl with
  | TopFn fd ->
    check_fn_decl ctx fd
  | TopType td ->
    register_type_decl ctx td
  | TopEffect ed ->
    register_effect_decl ctx ed
  | TopTrait _td ->
    (* Trait declarations are registered in the forward pass; nothing to
       re-check here since there are no bodies to type-check in Phase 1. *)
    Ok ()
  | TopImpl ib ->
    (* Type-check each method body in the impl against the trait's declared
       method signatures (where those exist).  Methods whose trait signature
       cannot be found are still checked for internal consistency.

       We also verify that the impl satisfies its trait (all required methods
       are present) using [Trait.check_impl_satisfies_trait]. *)
    let self_ty = lower_type_expr ctx ib.ib_self_ty in
    (* Make Self available as a type alias inside method bodies *)
    let old_self = Hashtbl.find_opt ctx.type_env "Self" in
    Hashtbl.replace ctx.type_env "Self" self_ty;
    (* Build a synthetic trait_impl record for the satisfaction check *)
    let synth_impl = {
      Trait.ti_trait_name = (match ib.ib_trait_ref with
        | Some tr -> tr.Ast.tr_name.name
        | None -> "");
      Trait.ti_trait_args = (match ib.ib_trait_ref with
        | Some tr -> tr.Ast.tr_args
        | None -> []);
      Trait.ti_self_ty = self_ty;
      Trait.ti_type_params = ib.ib_type_params;
      Trait.ti_methods = List.filter_map (fun item ->
        match item with
        | Ast.ImplFn fd -> Some (fd.Ast.fd_name.name, fd)
        | Ast.ImplType _ -> None
      ) ib.ib_items;
      Trait.ti_assoc_types = [];
      Trait.ti_where = ib.ib_where;
    } in
    (* Verify method presence (trait satisfaction) — convert trait errors to
       type errors so they surface through the standard pipeline. *)
    let* () = match ib.ib_trait_ref with
      | None -> Ok ()  (* Inherent impl — no trait to satisfy *)
      | Some _ ->
        begin match Trait.check_impl_satisfies_trait ctx.trait_registry synth_impl with
        | Ok () -> Ok ()
        | Error re ->
          Error (NotImplemented (Trait.show_resolution_error re))
        end
    in
    (* Type-check each method body *)
    let result = List.fold_left (fun acc item ->
      let* () = acc in
      match item with
      | Ast.ImplFn fd ->
        (* Type-check the method body using the standard function checker *)
        check_fn_decl ctx fd
      | Ast.ImplType _ ->
        (* Associated type definitions carry no body to check *)
        Ok ()
    ) (Ok ()) ib.ib_items in
    (* Restore the previous Self binding *)
    begin match old_self with
    | Some ty -> Hashtbl.replace ctx.type_env "Self" ty
    | None -> Hashtbl.remove ctx.type_env "Self"
    end;
    result
  | TopConst { tc_name; tc_ty; tc_value; _ } ->
    let expected = lower_type_expr ctx tc_ty in
    let* () = check ctx tc_value expected in
    bind_var ctx tc_name.name expected;
    Ok ()

(** {1 Program-level entry point} *)

(** Type-check an entire program.

    First registers all type and effect declarations (forward pass),
    then checks all function declarations and constants. *)
let check_program (symbols : Symbol.t) (prog : Ast.program)
    : (context, type_error) Result.t =
  let ctx = create_context symbols in
  register_builtins ctx;
  (* Forward pass: register all types, effects, traits, impls, and
     function signatures so that mutually recursive declarations resolve. *)
  let* () = List.fold_left (fun acc decl ->
    let* () = acc in
    match decl with
    | TopType td -> register_type_decl ctx td
    | TopEffect ed -> register_effect_decl ctx ed
    | TopFn fd ->
      (* Pre-register function with a fresh type for mutual recursion *)
      let fn_ty = fresh_tyvar ctx.level in
      bind_var ctx fd.fd_name.name fn_ty;
      Ok ()
    | TopTrait td ->
      (* Register trait definition in the trait registry so that
         find_impl / find_method_for_type can locate it. *)
      Trait.register_trait ctx.trait_registry td;
      Ok ()
    | TopImpl ib ->
      (* Lower the self type and register the impl in the trait registry.
         This makes impl methods visible to ExprField trait fallback. *)
      let self_ty = lower_type_expr ctx ib.ib_self_ty in
      Trait.register_impl ctx.trait_registry ib self_ty;
      Ok ()
    | _ -> Ok ()
  ) (Ok ()) prog.prog_decls in
  (* Check pass: verify all declarations *)
  let result = List.fold_left (fun acc decl ->
    let* () = acc in
    check_decl ctx decl
  ) (Ok ()) prog.prog_decls in
  match result with
  | Ok () ->
    (* Quantity checking pass: verify QTT quantity annotations.
       This runs after type checking succeeds so that we report type
       errors first (they are more fundamental). *)
    begin match Quantity.check_program_quantities prog with
    | Ok () -> Ok ctx
    | Error (qerr, span) ->
      Error (QuantityError (qerr, span))
    end
  | Error e -> Error e
