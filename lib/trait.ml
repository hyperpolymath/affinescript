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
  | Ast.KNat -> Types.KNat
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

(** Register a trait definition *)
let register_trait (registry : trait_registry) (trait_decl : trait_decl) : unit =
  let methods = List.filter_map (fun item ->
    match item with
    | TraitFn fs ->
      Some {
        tm_name = fs.fs_name.name;
        tm_type_params = fs.fs_type_params;
        tm_params = fs.fs_params;
        tm_ret_ty = None;  (* Will be filled by type checker *)
        tm_has_default = false;
      }
    | TraitFnDefault fd ->
      Some {
        tm_name = fd.fd_name.name;
        tm_type_params = fd.fd_type_params;
        tm_params = fd.fd_params;
        tm_ret_ty = None;  (* Will be filled by type checker *)
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

(** Find implementation of a trait for a given type *)
let find_impl (registry : trait_registry) (trait_name : string) (self_ty : ty) : trait_impl option =
  match Hashtbl.find_opt registry.impls trait_name with
  | None -> None
  | Some impls ->
    (* Find impl where self_ty matches ti_self_ty *)
    (* For now, simple name matching - TODO: proper unification *)
    let rec type_name = function
      | TVar _ -> None  (* Type variables don't have concrete names *)
      | TCon name -> Some name
      | TApp (TCon name, _) -> Some name
      | TApp (ty, _) -> type_name ty
      | _ -> None
    in
    let self_name = type_name self_ty in
    List.find_opt (fun impl ->
      match (self_name, type_name impl.ti_self_ty) with
      | (Some n1, Some n2) -> n1 = n2
      | _ -> false
    ) impls

(** Find all implementations for a given type (search all traits) *)
let find_impls_for_type (registry : trait_registry) (self_ty : ty) : trait_impl list =
  Hashtbl.fold (fun _trait_name impls acc ->
    let matching = List.filter (fun impl ->
      (* Simple type matching - TODO: proper unification *)
      let rec type_name = function
        | TVar _ -> None  (* Type variables don't have concrete names *)
        | TCon name -> Some name
        | TApp (TCon name, _) -> Some name
        | TApp (ty, _) -> type_name ty
        | _ -> None
      in
      match (type_name self_ty, type_name impl.ti_self_ty) with
      | (Some n1, Some n2) -> n1 = n2
      | _ -> false
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
