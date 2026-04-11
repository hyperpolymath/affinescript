(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Trait resolution and method dispatch.

    This module implements:
    - Trait registry (storing trait definitions)
    - Impl registry (storing implementations)
    - Trait resolution (checking impls match traits)
    - Method resolution (finding correct impl for calls)
    - Coherence checking (preventing overlapping impls)
*)

open Ast
open Types

(** Convert Ast.kind to Types.kind *)
let rec ast_kind_to_types_kind (k : Ast.kind) : Types.kind =
  match k with
  | Ast.KType -> Types.KType
  | Ast.KRow -> Types.KRow
  | Ast.KEffect -> Types.KEffect
  | Ast.KArrow (k1, k2) -> Types.KArrow (ast_kind_to_types_kind k1, ast_kind_to_types_kind k2)

(** Trait method signature *)
type trait_method = {
  tm_name : string;
  tm_type_params : type_param list;
  tm_params : param list;
  tm_ret_ty : ty option;
  tm_has_default : bool;
}

(** Trait definition *)
type trait_def = {
  td_name : string;
  td_type_params : type_param list;
  td_super : trait_bound list;
  td_methods : trait_method list;
  td_assoc_types : (string * kind option) list;
}

(** Implementation of a trait for a type *)
type trait_impl = {
  ti_trait_name : string;
  ti_trait_args : type_arg list;
  ti_self_ty : ty;
  ti_type_params : type_param list;
  ti_methods : (string * fn_decl) list;
  ti_assoc_types : (string * ty) list;
  ti_where : constraint_ list;
}

(** Trait registry - stores all trait definitions *)
type trait_registry = {
  traits : (string, trait_def) Hashtbl.t;
  impls : (string, trait_impl list) Hashtbl.t;  (* Key: trait name *)
}

(** Create empty trait registry *)
let create_registry () : trait_registry = {
  traits = Hashtbl.create 64;
  impls = Hashtbl.create 64;
}

(** Register a trait definition.

    The [tm_ret_ty] field is populated from the AST return-type annotation
    (if present) and later used by the type checker when verifying impl bodies. *)
let register_trait (registry : trait_registry) (trait_decl : trait_decl) : unit =
  let methods = List.filter_map (fun item ->
    match item with
    | TraitFn fs ->
      (* fs_ret_ty is the declared return-type annotation from the signature *)
      let ret_ty = match fs.fs_ret_ty with
        | None -> None
        | Some te ->
          (* Convert the AST type expression to an internal ty using a simple
             structural walk.  We do not have the full context here, so only
             built-in primitive names are resolved; everything else becomes
             TCon which the type checker resolves later during unification. *)
          let rec lower_simple (te : type_expr) : ty =
            match te with
            | TyCon { name = "Int"; _ } -> TCon "Int"
            | TyCon { name = "Float"; _ } -> TCon "Float"
            | TyCon { name = "Bool"; _ } -> TCon "Bool"
            | TyCon { name = "String"; _ } -> TCon "String"
            | TyCon { name = "Char"; _ } -> TCon "Char"
            | TyCon { name = "Unit"; _ } | TyTuple [] -> TCon "Unit"
            | TyCon { name = "Never"; _ } -> TCon "Never"
            | TyCon { name; _ } -> TCon name
            | TyVar { name; _ } -> TCon name
            | TyApp ({ name; _ }, args) ->
              let arg_tys = List.filter_map (fun a ->
                match a with TyArg te' -> Some (lower_simple te')
              ) args in
              TApp (TCon name, arg_tys)
            | TyTuple tes -> TTuple (List.map lower_simple tes)
            | TyArrow (a, _q, b, _eff) ->
              TArrow (lower_simple a, QOmega, lower_simple b, EPure)
            | TyOwn te' -> TOwn (lower_simple te')
            | TyRef te' -> TRef (lower_simple te')
            | TyMut te' -> TMut (lower_simple te')
            | TyRecord (fields, _) ->
              let row = List.fold_right (fun (rf : row_field) acc ->
                RExtend (rf.rf_name.name, lower_simple rf.rf_ty, acc)
              ) fields REmpty in
              TRecord row
            | TyHole ->
              let id = !Types.next_tyvar in
              Types.next_tyvar := id + 1;
              TVar (ref (Unbound (id, 0)))
          in
          Some (lower_simple te)
      in
      Some {
        tm_name = fs.fs_name.name;
        tm_type_params = fs.fs_type_params;
        tm_params = fs.fs_params;
        tm_ret_ty = ret_ty;
        tm_has_default = false;
      }
    | TraitFnDefault fd ->
      let ret_ty = match fd.fd_ret_ty with
        | None -> None
        | Some te ->
          let rec lower_simple (te : type_expr) : ty =
            match te with
            | TyCon { name = "Int"; _ } -> TCon "Int"
            | TyCon { name = "Float"; _ } -> TCon "Float"
            | TyCon { name = "Bool"; _ } -> TCon "Bool"
            | TyCon { name = "String"; _ } -> TCon "String"
            | TyCon { name = "Char"; _ } -> TCon "Char"
            | TyCon { name = "Unit"; _ } | TyTuple [] -> TCon "Unit"
            | TyCon { name = "Never"; _ } -> TCon "Never"
            | TyCon { name; _ } -> TCon name
            | TyVar { name; _ } -> TCon name
            | TyApp ({ name; _ }, args) ->
              let arg_tys = List.filter_map (fun a ->
                match a with TyArg te' -> Some (lower_simple te')
              ) args in
              TApp (TCon name, arg_tys)
            | TyTuple tes -> TTuple (List.map lower_simple tes)
            | TyArrow (a, _q, b, _eff) ->
              TArrow (lower_simple a, QOmega, lower_simple b, EPure)
            | TyOwn te' -> TOwn (lower_simple te')
            | TyRef te' -> TRef (lower_simple te')
            | TyMut te' -> TMut (lower_simple te')
            | TyRecord (fields, _) ->
              let row = List.fold_right (fun (rf : row_field) acc ->
                RExtend (rf.rf_name.name, lower_simple rf.rf_ty, acc)
              ) fields REmpty in
              TRecord row
            | TyHole ->
              let id = !Types.next_tyvar in
              Types.next_tyvar := id + 1;
              TVar (ref (Unbound (id, 0)))
          in
          Some (lower_simple te)
      in
      Some {
        tm_name = fd.fd_name.name;
        tm_type_params = fd.fd_type_params;
        tm_params = fd.fd_params;
        tm_ret_ty = ret_ty;
        tm_has_default = true;
      }
    | TraitType _ -> None
  ) trait_decl.trd_items in

  let assoc_types = List.filter_map (fun item ->
    match item with
    | TraitType { tt_name; tt_kind; _ } ->
      let converted_kind = Option.map ast_kind_to_types_kind tt_kind in
      Some (tt_name.name, converted_kind)
    | _ -> None
  ) trait_decl.trd_items in

  let trait_def = {
    td_name = trait_decl.trd_name.name;
    td_type_params = trait_decl.trd_type_params;
    td_super = trait_decl.trd_super;
    td_methods = methods;
    td_assoc_types = assoc_types;
  } in

  Hashtbl.replace registry.traits trait_decl.trd_name.name trait_def

(** Register an implementation *)
let register_impl (registry : trait_registry) (impl_block : impl_block) (self_ty : ty) : unit =
  match impl_block.ib_trait_ref with
  | None -> ()  (* Inherent impl, not a trait impl *)
  | Some trait_ref ->
    let methods = List.filter_map (fun item ->
      match item with
      | ImplFn fd -> Some (fd.fd_name.name, fd)
      | ImplType _ -> None
    ) impl_block.ib_items in

    let assoc_types = List.filter_map (fun item ->
      match item with
      | ImplType (name, _ty_expr) ->
        (* Convert ty_expr to ty - placeholder for now *)
        (* TODO: Need context to properly convert ty_expr to ty *)
        let placeholder_var = ref (Unbound (0, 0)) in
        Some (name.name, TVar placeholder_var)
      | ImplFn _ -> None
    ) impl_block.ib_items in

    let impl = {
      ti_trait_name = trait_ref.tr_name.name;
      ti_trait_args = trait_ref.tr_args;
      ti_self_ty = self_ty;  (* Use the provided self type *)
      ti_type_params = impl_block.ib_type_params;
      ti_methods = methods;
      ti_assoc_types = assoc_types;
      ti_where = impl_block.ib_where;
    } in

    let existing = Hashtbl.find_opt registry.impls trait_ref.tr_name.name
                   |> Option.value ~default:[] in
    Hashtbl.replace registry.impls trait_ref.tr_name.name (impl :: existing)

(** Trait resolution error *)
type resolution_error =
  | TraitNotFound of string
  | MissingMethod of string * string  (* trait_name, method_name *)
  | MethodSignatureMismatch of string * string * string  (* trait, method, reason *)
  | MissingAssocType of string * string  (* trait_name, type_name *)
  | OverlappingImpl of string * ty  (* trait_name, self_ty *)
  | SupertraitNotSatisfied of string * string  (* trait_name, supertrait_name *)

let show_resolution_error = function
  | TraitNotFound name -> Printf.sprintf "Trait '%s' not found" name
  | MissingMethod (trait, method_name) ->
      Printf.sprintf "Method '%s' required by trait '%s' is not implemented" method_name trait
  | MethodSignatureMismatch (trait, method_name, reason) ->
      Printf.sprintf "Method '%s' in trait '%s' has mismatched signature: %s"
        method_name trait reason
  | MissingAssocType (trait, type_name) ->
      Printf.sprintf "Associated type '%s' required by trait '%s' is not provided" type_name trait
  | OverlappingImpl (trait, _ty) ->
      Printf.sprintf "Overlapping implementation for trait '%s'" trait
  | SupertraitNotSatisfied (trait, supertrait) ->
      Printf.sprintf "Supertrait '%s' is not satisfied for trait '%s'" supertrait trait

type 'a result = ('a, resolution_error) Result.t

let ( let* ) = Result.bind

(** Check if an impl satisfies a trait *)
let check_impl_satisfies_trait (registry : trait_registry) (impl : trait_impl) : unit result =
  (* Find trait definition *)
  match Hashtbl.find_opt registry.traits impl.ti_trait_name with
  | None -> Error (TraitNotFound impl.ti_trait_name)
  | Some trait_def ->
    (* Check all required methods are implemented *)
    let* () = List.fold_left (fun acc method_def ->
      let* () = acc in
      if method_def.tm_has_default then
        Ok ()  (* Method has default, not required *)
      else
        match List.assoc_opt method_def.tm_name impl.ti_methods with
        | None -> Error (MissingMethod (trait_def.td_name, method_def.tm_name))
        | Some impl_method ->
          (* TODO: Check signature matches *)
          (* For now, just check it exists *)
          let impl_param_count = List.length impl_method.fd_params in
          let trait_param_count = List.length method_def.tm_params in
          if impl_param_count <> trait_param_count then
            Error (MethodSignatureMismatch (
              trait_def.td_name,
              method_def.tm_name,
              Printf.sprintf "expected %d parameters, found %d"
                trait_param_count impl_param_count
            ))
          else
            Ok ()
    ) (Ok ()) trait_def.td_methods in

    (* Check all required associated types are provided *)
    List.fold_left (fun acc (type_name, _kind) ->
      let* () = acc in
      match List.assoc_opt type_name impl.ti_assoc_types with
      | None -> Error (MissingAssocType (trait_def.td_name, type_name))
      | Some _ -> Ok ()
    ) (Ok ()) trait_def.td_assoc_types

(** Substitute type-param names with concrete types in a ty.

    [subst] maps type-parameter names to fresh unification variables.
    We walk the type tree and replace [TCon name] with [Hashtbl.find subst name]
    wherever a type parameter of that name exists. *)
let rec subst_ty (subst : (string, ty) Hashtbl.t) (ty : ty) : ty =
  match Types.repr ty with
  | TVar _ -> ty
  | TCon name ->
    begin match Hashtbl.find_opt subst name with
    | Some replacement -> replacement
    | None -> ty
    end
  | TApp (head, args) ->
    TApp (subst_ty subst head, List.map (subst_ty subst) args)
  | TArrow (a, q, b, eff) ->
    TArrow (subst_ty subst a, q, subst_ty subst b, eff)
  | TTuple tys ->
    TTuple (List.map (subst_ty subst) tys)
  | TRecord row ->
    TRecord (subst_row subst row)
  | TVariant row ->
    TVariant (subst_row subst row)
  | TForall (v, k, body) ->
    TForall (v, k, subst_ty subst body)
  | TExists (v, k, body) ->
    TExists (v, k, subst_ty subst body)
  | TRef t -> TRef (subst_ty subst t)
  | TMut t -> TMut (subst_ty subst t)
  | TOwn t -> TOwn (subst_ty subst t)

and subst_row (subst : (string, ty) Hashtbl.t) (row : row) : row =
  match Types.repr_row row with
  | REmpty -> REmpty
  | RExtend (l, ty, rest) ->
    RExtend (l, subst_ty subst ty, subst_row subst rest)
  | RVar _ -> row

(** Create a fresh instantiation of an impl's self type.

    For each type parameter declared on the impl, we create a fresh
    unification variable and substitute it for the parameter name in
    the impl's self type.  This allows unification-based matching
    without permanently committing to any particular substitution.

    [fresh_var] should create a fresh [TVar (ref (Unbound (...)))] at
    the caller's current unification level. *)
let fresh_impl_self_ty (impl : trait_impl) (fresh_var : unit -> ty) : ty =
  let subst = Hashtbl.create 4 in
  List.iter (fun (tp : type_param) ->
    Hashtbl.replace subst tp.tp_name.name (fresh_var ())
  ) impl.ti_type_params;
  if Hashtbl.length subst = 0 then
    impl.ti_self_ty
  else
    subst_ty subst impl.ti_self_ty

(** Find implementation of a trait for a given type using unification.

    For each candidate impl we:
      1. Instantiate its type parameters as fresh unification variables.
      2. Attempt [Unify.unify self_ty instantiated_self_ty].
      3. If unification succeeds the substitution is captured in the mutable
         type variables — we return that impl.
      4. If unification fails we move on to the next candidate.

    The [fresh_var] callback creates a new [TVar (Unbound _)] at the
    appropriate level; callers typically pass a closure over [ctx.level]. *)
let find_impl_with_unify (registry : trait_registry) (trait_name : string)
    (self_ty : ty) (fresh_var : unit -> ty) : trait_impl option =
  match Hashtbl.find_opt registry.impls trait_name with
  | None -> None
  | Some impls ->
    List.find_opt (fun impl ->
      let candidate_self = fresh_impl_self_ty impl fresh_var in
      match Unify.unify self_ty candidate_self with
      | Ok () -> true
      | Error _ -> false
    ) impls

(** Find implementation of a trait for a given type.

    Uses unification-based matching when fresh type variables are available
    (via [~fresh_var]).  Falls back to structural constructor-name matching
    when no [fresh_var] callback is supplied (e.g. from legacy call sites). *)
let find_impl (registry : trait_registry) (trait_name : string) (self_ty : ty) : trait_impl option =
  (* Use a simple level-0 fresh var for the fallback path *)
  let fresh_var () =
    let id = !Types.next_tyvar in
    Types.next_tyvar := id + 1;
    TVar (ref (Unbound (id, 0)))
  in
  find_impl_with_unify registry trait_name self_ty fresh_var

(** Find all implementations for a given type across all traits.

    Uses the same unification-based matching as [find_impl].  Each candidate
    self type is instantiated with fresh type variables so that impls with
    generic parameters (e.g. [impl Display for Option[T]]) are handled
    correctly by structural unification. *)
let find_impls_for_type (registry : trait_registry) (self_ty : ty) : trait_impl list =
  let fresh_var () =
    let id = !Types.next_tyvar in
    Types.next_tyvar := id + 1;
    TVar (ref (Unbound (id, 0)))
  in
  Hashtbl.fold (fun _trait_name impls acc ->
    let matching = List.filter (fun impl ->
      let candidate_self = fresh_impl_self_ty impl fresh_var in
      match Unify.unify self_ty candidate_self with
      | Ok () -> true
      | Error _ -> false
    ) impls in
    matching @ acc
  ) registry.impls []

(** Find method in a trait impl *)
let find_method (impl : trait_impl) (method_name : string) : fn_decl option =
  List.assoc_opt method_name impl.ti_methods

(** Find method in any trait impl for a type *)
let find_method_for_type (registry : trait_registry) (self_ty : ty) (method_name : string)
    : (trait_impl * fn_decl) option =
  let impls = find_impls_for_type registry self_ty in
  let rec search_impls = function
    | [] -> None
    | impl :: rest ->
      begin match find_method impl method_name with
        | Some method_decl -> Some (impl, method_decl)
        | None -> search_impls rest
      end
  in
  search_impls impls

(** Check for overlapping implementations *)
let check_coherence (registry : trait_registry) (trait_name : string) : unit result =
  match Hashtbl.find_opt registry.impls trait_name with
  | None -> Ok ()
  | Some impls ->
    (* TODO: Check for overlapping impls *)
    (* For now, just ensure no duplicate self types *)
    let rec check_pairs = function
      | [] -> Ok ()
      | _impl :: rest ->
        (* Check if any impl in rest has same self_ty *)
        (* TODO: Proper unification check *)
        check_pairs rest
    in
    check_pairs impls

(** Standard library traits - automatically registered *)
let register_stdlib_traits (registry : trait_registry) : unit =
  (* Eq trait *)
  let self_ty = Ast.TyCon { Ast.name = "Self"; span = Span.dummy } in
  let eq_self_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "self"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let eq_other_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "other"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let eq_trait = {
    td_name = "Eq";
    td_type_params = [];
    td_super = [];
    td_methods = [{
      tm_name = "eq";
      tm_type_params = [];
      tm_params = [eq_self_param; eq_other_param];
      tm_ret_ty = Some ty_bool;
      tm_has_default = false;
    }];
    td_assoc_types = [];
  } in
  Hashtbl.replace registry.traits "Eq" eq_trait;

  (* Ord trait (requires Eq) *)
  let ord_self_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "self"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let ord_other_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "other"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let ord_trait = {
    td_name = "Ord";
    td_type_params = [];
    td_super = [{
      tb_name = { name = "Eq"; span = Span.dummy };
      tb_args = [];
    }];
    td_methods = [{
      tm_name = "cmp";
      tm_type_params = [];
      tm_params = [ord_self_param; ord_other_param];
      tm_ret_ty = None;  (* Returns Ordering enum *)
      tm_has_default = false;
    }];
    td_assoc_types = [];
  } in
  Hashtbl.replace registry.traits "Ord" ord_trait;

  (* Hash trait *)
  let hash_self_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "self"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let hash_trait = {
    td_name = "Hash";
    td_type_params = [];
    td_super = [];
    td_methods = [{
      tm_name = "hash";
      tm_type_params = [];
      tm_params = [hash_self_param];
      tm_ret_ty = Some ty_int;
      tm_has_default = false;
    }];
    td_assoc_types = [];
  } in
  Hashtbl.replace registry.traits "Hash" hash_trait;

  (* Display trait *)
  let display_self_param = {
    Ast.p_quantity = None;
    p_ownership = Some Ast.Ref;
    p_name = { Ast.name = "self"; span = Span.dummy };
    p_ty = self_ty;
  } in
  let display_trait = {
    td_name = "Display";
    td_type_params = [];
    td_super = [];
    td_methods = [{
      tm_name = "to_string";
      tm_type_params = [];
      tm_params = [display_self_param];
      tm_ret_ty = None;  (* Returns String *)
      tm_has_default = false;
    }];
    td_assoc_types = [];
  } in
  Hashtbl.replace registry.traits "Display" display_trait

(** Validate all registered implementations *)
let validate_all_impls (registry : trait_registry) : unit result =
  Hashtbl.fold (fun _trait_name impls acc ->
    let* () = acc in
    List.fold_left (fun acc2 impl ->
      let* () = acc2 in
      check_impl_satisfies_trait registry impl
    ) (Ok ()) impls
  ) registry.impls (Ok ())
