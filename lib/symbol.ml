(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Symbol table for name resolution.

    This module provides the symbol table infrastructure for tracking
    bindings during name resolution and type checking.
*)

open Ast

(** Unique identifier for symbols *)
type symbol_id = int
[@@deriving show, eq, ord]

(** Symbol kinds *)
type symbol_kind =
  | SKVariable      (** Local or global variable *)
  | SKFunction      (** Function definition *)
  | SKType          (** Type definition *)
  | SKTypeVar       (** Type variable *)
  | SKEffect        (** Effect definition *)
  | SKEffectOp      (** Effect operation *)
  | SKTrait         (** Trait definition *)
  | SKModule        (** Module *)
  | SKConstructor   (** Data constructor *)
[@@deriving show, eq]

(** A symbol entry in the symbol table *)
type symbol = {
  sym_id : symbol_id;
  sym_name : string;
  sym_kind : symbol_kind;
  sym_span : Span.t;
  sym_visibility : visibility;
  sym_type : type_expr option;  (** Filled during type checking *)
  sym_quantity : quantity option;
}
[@@deriving show]

(** Symbol table scope *)
type scope = {
  scope_parent : scope option;
  scope_symbols : (string, symbol) Hashtbl.t;
  scope_kind : scope_kind;
}

and scope_kind =
  | ScopeGlobal
  | ScopeModule of string
  | ScopeFunction of string
  | ScopeBlock
  | ScopeMatch
  | ScopeHandler

(** The symbol table *)
type t = {
  mutable current_scope : scope;
  mutable next_id : symbol_id;
  all_symbols : (symbol_id, symbol) Hashtbl.t;
}

(** Create a new symbol table *)
let create () : t =
  let global_scope = {
    scope_parent = None;
    scope_symbols = Hashtbl.create 64;
    scope_kind = ScopeGlobal;
  } in
  {
    current_scope = global_scope;
    next_id = 0;
    all_symbols = Hashtbl.create 256;
  }

(** Generate a fresh symbol ID *)
let fresh_id (table : t) : symbol_id =
  let id = table.next_id in
  table.next_id <- id + 1;
  id

(** Enter a new scope *)
let enter_scope (table : t) (kind : scope_kind) : unit =
  let new_scope = {
    scope_parent = Some table.current_scope;
    scope_symbols = Hashtbl.create 16;
    scope_kind = kind;
  } in
  table.current_scope <- new_scope

(** Exit the current scope *)
let exit_scope (table : t) : unit =
  match table.current_scope.scope_parent with
  | Some parent -> table.current_scope <- parent
  | None -> failwith "Cannot exit global scope"

(** Define a new symbol in the current scope *)
let define (table : t) (name : string) (kind : symbol_kind)
    (span : Span.t) (vis : visibility) : symbol =
  let sym = {
    sym_id = fresh_id table;
    sym_name = name;
    sym_kind = kind;
    sym_span = span;
    sym_visibility = vis;
    sym_type = None;
    sym_quantity = None;
  } in
  Hashtbl.replace table.current_scope.scope_symbols name sym;
  Hashtbl.replace table.all_symbols sym.sym_id sym;
  sym

(** Look up a symbol in the current scope and parents *)
let rec lookup_in_scope (scope : scope) (name : string) : symbol option =
  match Hashtbl.find_opt scope.scope_symbols name with
  | Some sym -> Some sym
  | None ->
    match scope.scope_parent with
    | Some parent -> lookup_in_scope parent name
    | None -> None

(** Look up a symbol by name *)
let lookup (table : t) (name : string) : symbol option =
  lookup_in_scope table.current_scope name

(** Look up a symbol by ID *)
let lookup_by_id (table : t) (id : symbol_id) : symbol option =
  Hashtbl.find_opt table.all_symbols id

(** Check if a name is defined in the current scope (not parents) *)
let is_defined_locally (table : t) (name : string) : bool =
  Hashtbl.mem table.current_scope.scope_symbols name

(** Update a symbol's type *)
let set_type (table : t) (id : symbol_id) (ty : type_expr) : unit =
  match Hashtbl.find_opt table.all_symbols id with
  | Some sym ->
    let updated = { sym with sym_type = Some ty } in
    Hashtbl.replace table.all_symbols id updated
  | None -> ()

(** Update a symbol's quantity *)
let set_quantity (table : t) (id : symbol_id) (q : quantity) : unit =
  match Hashtbl.find_opt table.all_symbols id with
  | Some sym ->
    let updated = { sym with sym_quantity = Some q } in
    Hashtbl.replace table.all_symbols id updated
  | None -> ()

(** Look up a qualified path (Foo.Bar.x) *)
let lookup_qualified (table : t) (path : string list) : symbol option =
  match path with
  | [] -> None
  | [name] -> lookup table name
  | _modules ->
    (* For qualified paths, we need to traverse module scopes *)
    (* Currently, we flatten to the final name since modules aren't fully implemented *)
    let final_name = List.hd (List.rev path) in
    lookup table final_name

(** Check if a symbol is visible from the current scope *)
let is_visible (table : t) (sym : symbol) : bool =
  match sym.sym_visibility with
  | Private ->
    (* Private symbols are only visible in the same scope *)
    Hashtbl.mem table.current_scope.scope_symbols sym.sym_name
  | Public -> true
  | PubCrate -> true  (* Within same crate, always visible *)
  | PubSuper ->
    (* Visible in parent module - check if we're in a child scope *)
    begin match table.current_scope.scope_parent with
      | Some _ -> true
      | None -> false
    end
  | PubIn _path ->
    (* Visible in specified path - for now, treat as public *)
    true

(** Register an import, making a symbol available under a new name *)
let register_import (table : t) (sym : symbol) (alias : string option) : symbol =
  let name = match alias with
    | Some n -> n
    | None -> sym.sym_name
  in
  let imported = { sym with sym_name = name } in
  Hashtbl.replace table.current_scope.scope_symbols name imported;
  imported

(** Look up an effect operation *)
let lookup_effect_op (table : t) (effect_name : string) (op_name : string) : symbol option =
  (* First find the effect, then look for the operation *)
  match lookup table effect_name with
  | Some eff_sym when eff_sym.sym_kind = SKEffect ->
    (* Effect found, now look for the operation *)
    lookup table op_name
  | _ -> None

(* Phase 1 complete. Future enhancements (Phase 2+):
   - Full module system with nested namespaces (Phase 2)
   - Glob imports with filtering (Phase 2)
   - Re-exports and visibility inheritance (Phase 2)
*)
