(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Type checking stub for AffineScript REPL.

    This is a minimal stub to allow the REPL to compile.
    Full type checking will be implemented later.
*)

open Types
open Ast

(** Type checking context *)
type context = {
  var_types : (Symbol.symbol_id, scheme) Hashtbl.t;
  type_env : (string, ty) Hashtbl.t;
  symbols : Symbol.t;
}

(** Type checking error *)
type type_error =
  | UnboundVariable of string
  | TypeMismatch of { expected : ty; got : ty }
  | OccursCheck of string * ty
  | NotImplemented of string

(** Show type error *)
let show_type_error = function
  | UnboundVariable v -> "Unbound variable: " ^ v
  | TypeMismatch { expected; got } ->
    Printf.sprintf "Type mismatch: expected %s, got %s"
      (show_ty expected) (show_ty got)
  | OccursCheck (v, ty) ->
    Printf.sprintf "Occurs check: %s in %s" v (show_ty ty)
  | NotImplemented msg -> "Not implemented: " ^ msg

(** Create a new type checking context *)
let create_context (symbols : Symbol.t) : context =
  {
    var_types = Hashtbl.create 100;
    type_env = Hashtbl.create 100;
    symbols;
  }

(** Synthesize type for an expression (stub) *)
let synth (ctx : context) (expr : expr) : (ty, type_error) result =
  let _ = (ctx, expr) in
  Error (NotImplemented "Type synthesis not yet implemented")

(** Check expression against a type (stub) *)
let check (ctx : context) (expr : expr) (ty : ty) : (unit, type_error) result =
  let _ = (ctx, expr, ty) in
  Error (NotImplemented "Type checking not yet implemented")

(** Generalize a type to a type scheme *)
let generalize (ctx : context) (ty : ty) : scheme =
  let _ = ctx in
  (* For now, just wrap in a scheme with no quantified variables *)
  { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty }

(** Check a top-level declaration (stub) *)
let check_decl (ctx : context) (decl : top_level) : (unit, type_error) result =
  let _ = (ctx, decl) in
  Ok ()

(** Check a program (stub) *)
let check_program (symbols : Symbol.t) (prog : Ast.program) : (context, type_error) result =
  let ctx = create_context symbols in
  let _ = prog in
  Ok ctx

(** Format type error for display *)
let format_type_error (err : type_error) : string =
  show_type_error err
