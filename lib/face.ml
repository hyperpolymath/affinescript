(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Face-aware error formatter (ADR-010).

    The compiler's internal error representation is canonical and
    face-agnostic.  This module is the single point where canonical error
    terms are mapped to face-specific vocabulary before being shown to the
    user.

    Architecture (ADR-010 §2):
    {v
      compiler  →  face.ml  →  terminal / LSP / IDE
    v}

    Adding a new face: add a variant to {!face} and a branch to each
    [format_*_for_face] function below.  No compiler internals change.
*)

(** Active parser/error face. *)
type face =
  | Canonical   (** Standard AffineScript syntax and vocabulary. *)
  | Python      (** Python-style surface syntax; Python-friendly messages. *)

(* ─── Helpers ────────────────────────────────────────────────────────── *)

(** Render a type to a face-appropriate string.
    The canonical [Types.ty_to_string] output is always valid here;
    face-specific names for built-in types can be added per face. *)
let render_ty (face : face) (ty : Types.ty) : string =
  let s = Types.ty_to_string ty in
  match face with
  | Canonical -> s
  | Python ->
    (* Map a few canonical names to Python-familiar names. *)
    let s = if s = "Unit" then "None" else s in
    let s = if s = "Bool" then "bool" else s in
    s

(* ─── Quantity / ownership errors ────────────────────────────────────── *)

(** Format a QTT quantity error for the given face.

    Python-face replaces the @linear/@erased/@unrestricted annotation
    vocabulary with "single-use"/"erased"/"unrestricted" and frames
    errors in terms a Python developer can recognise. *)
let format_quantity_error (face : face) (err : Quantity.quantity_error) : string =
  match face with
  | Canonical -> Quantity.format_quantity_error err
  | Python ->
    (match err with
    | Quantity.LinearVariableUnused id ->
      Printf.sprintf
        "Ownership error: single-use variable '%s' must be used exactly once, \
         but was never used\n\
         hint: either use '%s', or prefix its name with '_' to suppress this error"
        id.name id.name
    | Quantity.LinearVariableUsedMultiple id ->
      Printf.sprintf
        "Ownership error: single-use variable '%s' can only be used once, \
         but was used more than once\n\
         hint: clone or copy the value before reusing it, \
         or declare '%s' as an unrestricted variable"
        id.name id.name
    | Quantity.ErasedVariableUsed id ->
      Printf.sprintf
        "Ownership error: erased variable '%s' was declared as compile-time-only \
         (@erased / :0) and cannot appear at runtime"
        id.name
    | Quantity.QuantityMismatch (id, q, _u) ->
      let decl = match q with
        | Ast.QZero -> "erased (compile-time only)"
        | Ast.QOne -> "single-use"
        | Ast.QOmega -> "unrestricted"
      in
      Printf.sprintf
        "Ownership error: '%s' was declared as %s but used inconsistently"
        id.name decl)

(* ─── Unification errors ─────────────────────────────────────────────── *)

let format_unify_error (face : face) (ue : Unify.unify_error) : string =
  match face with
  | Canonical -> Unify.show_unify_error ue
  | Python ->
    (match ue with
    | Unify.TypeMismatch (expected, got) ->
      Printf.sprintf "Type error: expected %s, got %s"
        (render_ty face expected)
        (render_ty face got)
    | Unify.OccursCheck _ ->
      "Type error: recursive type — a type cannot contain itself"
    | Unify.RowMismatch _ ->
      "Type error: record field mismatch"
    | Unify.RowOccursCheck _ ->
      "Type error: recursive record type"
    | Unify.EffectMismatch _ ->
      "Type error: effect set mismatch"
    | Unify.EffectOccursCheck _ ->
      "Type error: recursive effect type"
    | Unify.KindMismatch _ ->
      "Type error: kind mismatch (e.g. used a type where a row was expected)"
    | Unify.LabelNotFound (label, _) ->
      Printf.sprintf "Type error: field '%s' not found in record" label)

(* ─── Type errors ────────────────────────────────────────────────────── *)

(** Format a type-checker error for the given face. *)
let format_type_error (face : face) (err : Typecheck.type_error) : string =
  match face with
  | Canonical -> Typecheck.format_type_error err
  | Python ->
    (match err with
    | Typecheck.UnboundVariable v ->
      Printf.sprintf "Name not found: '%s'\n\
                      hint: check spelling or add a 'def %s(...)' declaration" v v
    | Typecheck.TypeMismatch { expected; got } ->
      Printf.sprintf "Type error: expected %s but got %s"
        (render_ty face expected)
        (render_ty face got)
    | Typecheck.OccursCheck (v, ty) ->
      Printf.sprintf "Type error: cannot construct infinite type %s = %s"
        v (render_ty face ty)
    | Typecheck.NotImplemented msg ->
      Printf.sprintf "Compiler limitation: %s" msg
    | Typecheck.ArityMismatch { name; expected; got } ->
      Printf.sprintf "'%s' takes %d argument%s but was called with %d"
        name expected (if expected = 1 then "" else "s") got
    | Typecheck.NotAFunction ty ->
      Printf.sprintf "Cannot call a value of type %s (it is not a function)"
        (render_ty face ty)
    | Typecheck.FieldNotFound { field; record_ty } ->
      Printf.sprintf "Field '%s' does not exist on type %s"
        field (render_ty face record_ty)
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      Printf.sprintf "Tuple index %d is out of range (tuple has %d element%s)"
        index length (if length = 1 then "" else "s")
    | Typecheck.DuplicateField f ->
      Printf.sprintf "Duplicate field '%s' in record literal" f
    | Typecheck.UnificationError ue ->
      format_unify_error face ue
    | Typecheck.PatternTypeMismatch msg ->
      Printf.sprintf "Pattern error: %s" msg
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      Printf.sprintf
        "if/else type mismatch: the if-branch returns %s \
         but the else-branch returns %s — both branches must return the same type"
        (render_ty face then_ty)
        (render_ty face else_ty)
    | Typecheck.QuantityError (qerr, _span) ->
      format_quantity_error face qerr)

(* ─── Resolve errors ─────────────────────────────────────────────────── *)

(** Format a name-resolution error for the given face. *)
let format_resolve_error (face : face) (err : Resolve.resolve_error) : string =
  match face with
  | Canonical -> Resolve.show_resolve_error err
  | Python ->
    (match err with
    | Resolve.UndefinedVariable id ->
      Printf.sprintf "Name not found: '%s'\n\
                      hint: define it with 'def %s(...):' or 'let %s = ...'"
        id.name id.name id.name
    | Resolve.UndefinedType id ->
      Printf.sprintf "Type not found: '%s'\n\
                      hint: define it with 'class %s:' or 'type %s = ...'"
        id.name id.name id.name
    | Resolve.UndefinedEffect id ->
      Printf.sprintf "Effect not found: '%s'" id.name
    | Resolve.UndefinedModule id ->
      Printf.sprintf "Module not found: '%s'\n\
                      hint: use 'import %s' to bring it into scope" id.name id.name
    | Resolve.DuplicateDefinition id ->
      Printf.sprintf "'%s' is already defined in this scope" id.name
    | Resolve.VisibilityError (id, msg) ->
      Printf.sprintf "'%s' is not accessible here: %s" id.name msg
    | Resolve.ImportError msg ->
      Printf.sprintf "Import error: %s" msg)
