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
  | Js          (** JavaScript/TypeScript-style syntax; JS-friendly messages. *)
  | Pseudocode  (** Natural-language-adjacent pseudocode; beginner-friendly messages. *)
  | Lucid       (** PureScript/Haskell-style surface; Haskell-family vocabulary. *)
  | Cafe        (** CoffeeScript-style surface; concise JS-family vocabulary. *)

(* ─── Helpers ────────────────────────────────────────────────────────── *)

(** Render a type to a face-appropriate string.
    The canonical [Types.ty_to_string] output is always valid here;
    face-specific names for built-in types can be added per face. *)
let render_ty (face : face) (ty : Types.ty) : string =
  let s = Types.ty_to_string ty in
  match face with
  | Canonical -> s
  | Python ->
    let s = if s = "Unit" then "None" else s in
    let s = if s = "Bool" then "bool" else s in
    s
  | Js ->
    (* Map canonical names to JS-familiar equivalents. *)
    let s = if s = "Unit" then "null" else s in
    let s = if s = "Bool" then "boolean" else s in
    (* Option[T] rendered as "T | null" per META.a2ml ADR note *)
    let s = Str.global_replace (Str.regexp {|Option\[\(.*\)\]|}) {|\1 | null|} s in
    s
  | Pseudocode ->
    let s = if s = "Unit" then "nothing" else s in
    let s = if s = "Bool" then "Boolean" else s in
    s
  | Lucid ->
    (* PureScript/Haskell vocabulary: Unit stays Unit, Bool → Boolean,
       Option[T] → Maybe T (PureScript spelling). *)
    let s = if s = "Bool" then "Boolean" else s in
    let s = Str.global_replace (Str.regexp {|Option\[\(.*\)\]|}) {|Maybe \1|} s in
    s
  | Cafe ->
    (* CoffeeScript inherits JS types; render in JS-style with the
       optional postfix [?] for Option (CoffeeScript existential). *)
    let s = if s = "Unit" then "null" else s in
    let s = if s = "Bool" then "Boolean" else s in
    let s = Str.global_replace (Str.regexp {|Option\[\(.*\)\]|}) {|\1?|} s in
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
  | Js ->
    (match err with
    | Quantity.LinearVariableUnused id ->
      Printf.sprintf
        "Resource leak: '%s' is a single-use (own) value that was never consumed.\n\
         hint: call a function that takes ownership of '%s', or use the '_' prefix \
         to suppress this warning"
        id.name id.name
    | Quantity.LinearVariableUsedMultiple id ->
      Printf.sprintf
        "Use-after-move: '%s' is a single-use (own) value but was used more than once.\n\
         hint: clone the value before the second use, or restructure to consume it once"
        id.name
    | Quantity.ErasedVariableUsed id ->
      Printf.sprintf
        "Compile-time-only value '%s' cannot appear at runtime (it was declared with \
         @erased / own at quantity :0)"
        id.name
    | Quantity.QuantityMismatch (id, q, _u) ->
      let decl = match q with
        | Ast.QZero -> "compile-time only"
        | Ast.QOne -> "single-use (own)"
        | Ast.QOmega -> "freely shareable"
      in
      Printf.sprintf "Resource error: '%s' was declared as %s but used inconsistently"
        id.name decl)
  | Pseudocode ->
    (match err with
    | Quantity.LinearVariableUnused id ->
      Printf.sprintf
        "ERROR: '%s' was declared as a single-use resource but was never used.\n\
         You must use '%s' exactly once in the body."
        id.name id.name
    | Quantity.LinearVariableUsedMultiple id ->
      Printf.sprintf
        "ERROR: '%s' is a single-use resource — you used it more than once.\n\
         A single-use resource can only appear in one place."
        id.name
    | Quantity.ErasedVariableUsed id ->
      Printf.sprintf
        "ERROR: '%s' only exists at compile time and cannot be used at runtime."
        id.name
    | Quantity.QuantityMismatch (id, q, _u) ->
      let decl = match q with
        | Ast.QZero -> "compile-time only"
        | Ast.QOne -> "single-use"
        | Ast.QOmega -> "ordinary"
      in
      Printf.sprintf "ERROR: '%s' was declared as %s but used in an inconsistent way."
        id.name decl)
  | Lucid ->
    (match err with
    | Quantity.LinearVariableUnused id ->
      Printf.sprintf
        "Linearity error: linear variable '%s' is never consumed.\n\
         A linear value must be consumed exactly once. Bind it to '_' \
         or pass it to a function that takes ownership."
        id.name
    | Quantity.LinearVariableUsedMultiple id ->
      Printf.sprintf
        "Linearity error: linear variable '%s' is used more than once.\n\
         A linear value cannot be duplicated. Use Data.Clone (clone) to \
         produce an independent copy, or refactor so it flows through a \
         single consumer."
        id.name
    | Quantity.ErasedVariableUsed id ->
      Printf.sprintf
        "Erasure error: '%s' has multiplicity 0 and exists only at compile time. \
         It cannot appear in a runtime expression."
        id.name
    | Quantity.QuantityMismatch (id, q, _u) ->
      let decl = match q with
        | Ast.QZero -> "Erased (multiplicity 0)"
        | Ast.QOne -> "Linear (multiplicity 1)"
        | Ast.QOmega -> "Unrestricted (multiplicity ω)"
      in
      Printf.sprintf
        "Multiplicity error: '%s' was declared %s but is used at a different multiplicity."
        id.name decl)
  | Cafe ->
    (match err with
    | Quantity.LinearVariableUnused id ->
      Printf.sprintf
        "Resource leak: '%s' is a one-shot value but was never used.\n\
         hint: pass it to something that consumes it, or rename to '_%s'."
        id.name id.name
    | Quantity.LinearVariableUsedMultiple id ->
      Printf.sprintf
        "Resource error: '%s' is a one-shot value — you used it more than once.\n\
         hint: clone before the second use, or restructure so it's consumed only once."
        id.name
    | Quantity.ErasedVariableUsed id ->
      Printf.sprintf
        "Compile-time value '%s' cannot show up at runtime."
        id.name
    | Quantity.QuantityMismatch (id, q, _u) ->
      let decl = match q with
        | Ast.QZero -> "compile-time"
        | Ast.QOne -> "one-shot"
        | Ast.QOmega -> "shareable"
      in
      Printf.sprintf "Resource error: '%s' was declared %s but used inconsistently."
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
  | Js ->
    (match ue with
    | Unify.TypeMismatch (expected, got) ->
      Printf.sprintf "Type error: Type '%s' is not assignable to type '%s'"
        (render_ty face got)
        (render_ty face expected)
    | Unify.OccursCheck _ ->
      "Type error: circular type reference — a type cannot reference itself"
    | Unify.RowMismatch _ ->
      "Type error: object type mismatch — missing or extra fields"
    | Unify.RowOccursCheck _ ->
      "Type error: circular object type"
    | Unify.EffectMismatch _ ->
      "Type error: effect signature mismatch — function may perform undeclared effects"
    | Unify.EffectOccursCheck _ ->
      "Type error: circular effect type"
    | Unify.KindMismatch _ ->
      "Type error: type argument kind mismatch"
    | Unify.LabelNotFound (label, _) ->
      Printf.sprintf "Property '%s' does not exist on this type" label)
  | Pseudocode ->
    (match ue with
    | Unify.TypeMismatch (expected, got) ->
      Printf.sprintf "TYPE MISMATCH: expected %s but got %s"
        (render_ty face expected)
        (render_ty face got)
    | Unify.OccursCheck _ ->
      "TYPE ERROR: a type cannot refer to itself"
    | Unify.RowMismatch _ ->
      "TYPE ERROR: the record does not have the expected fields"
    | Unify.RowOccursCheck _ ->
      "TYPE ERROR: circular record type"
    | Unify.EffectMismatch _ ->
      "EFFECT ERROR: the function uses effects that were not declared"
    | Unify.EffectOccursCheck _ ->
      "EFFECT ERROR: circular effect type"
    | Unify.KindMismatch _ ->
      "TYPE ERROR: wrong kind of type argument"
    | Unify.LabelNotFound (label, _) ->
      Printf.sprintf "FIELD ERROR: the record has no field named '%s'" label)
  | Lucid ->
    (match ue with
    | Unify.TypeMismatch (expected, got) ->
      Printf.sprintf "Could not match type '%s' with type '%s'"
        (render_ty face got)
        (render_ty face expected)
    | Unify.OccursCheck _ ->
      "Cannot construct the infinite type — a type variable refers to itself"
    | Unify.RowMismatch _ ->
      "Could not match record type — labels differ"
    | Unify.RowOccursCheck _ ->
      "Cannot construct the infinite record type"
    | Unify.EffectMismatch _ ->
      "Could not match effect rows — function performs effects not in its signature"
    | Unify.EffectOccursCheck _ ->
      "Cannot construct the infinite effect row"
    | Unify.KindMismatch _ ->
      "Kind mismatch — type-level argument has the wrong kind"
    | Unify.LabelNotFound (label, _) ->
      Printf.sprintf "No field '%s' in record" label)
  | Cafe ->
    (match ue with
    | Unify.TypeMismatch (expected, got) ->
      Printf.sprintf "Type doesn't fit: expected %s, got %s"
        (render_ty face expected)
        (render_ty face got)
    | Unify.OccursCheck _ ->
      "Type loops back on itself"
    | Unify.RowMismatch _ ->
      "Object shape doesn't match — missing or extra fields"
    | Unify.RowOccursCheck _ ->
      "Object type loops back on itself"
    | Unify.EffectMismatch _ ->
      "Effect mismatch: function does effects that weren't declared"
    | Unify.EffectOccursCheck _ ->
      "Effect type loops back on itself"
    | Unify.KindMismatch _ ->
      "Type argument is the wrong kind"
    | Unify.LabelNotFound (label, _) ->
      Printf.sprintf "No field '%s' on this object" label)

(* ─── Type errors ────────────────────────────────────────────────────── *)

(** Format a type-checker error for the given face. *)
let format_type_error (face : face) (err : Typecheck.type_error) : string =
  match face with
  | Canonical -> Typecheck.format_type_error err
  | Js ->
    (match err with
    | Typecheck.UnboundVariable v ->
      Printf.sprintf "Cannot find name '%s'.\n\
                      Did you mean to declare it with 'const %s = ...' ?" v v
    | Typecheck.TypeMismatch { expected; got } ->
      Printf.sprintf "Type '%s' is not assignable to type '%s'."
        (render_ty face got)
        (render_ty face expected)
    | Typecheck.OccursCheck (v, ty) ->
      Printf.sprintf "Type '%s' circularly references itself: %s = %s"
        v v (render_ty face ty)
    | Typecheck.NotImplemented msg ->
      Printf.sprintf "Compiler limitation: %s" msg
    | Typecheck.ArityMismatch { name; expected; got } ->
      Printf.sprintf "Expected %d argument%s for '%s', but got %d."
        expected (if expected = 1 then "" else "s") name got
    | Typecheck.NotAFunction ty ->
      Printf.sprintf "This expression is not callable (type: %s)."
        (render_ty face ty)
    | Typecheck.FieldNotFound { field; record_ty } ->
      Printf.sprintf "Property '%s' does not exist on type '%s'."
        field (render_ty face record_ty)
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      Printf.sprintf "Tuple index %d out of range (length %d)." index length
    | Typecheck.DuplicateField f ->
      Printf.sprintf "Duplicate identifier '%s' in object literal." f
    | Typecheck.UnificationError ue -> format_unify_error face ue
    | Typecheck.PatternTypeMismatch msg ->
      Printf.sprintf "Pattern error: %s" msg
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      Printf.sprintf
        "Type error: if-true branch returns '%s', if-false branch returns '%s'. \
         Both branches must return the same type."
        (render_ty face then_ty)
        (render_ty face else_ty)
    | Typecheck.QuantityError (qerr, _span) ->
      format_quantity_error face qerr)
  | Pseudocode ->
    (match err with
    | Typecheck.UnboundVariable v ->
      Printf.sprintf "ERROR: '%s' has not been defined.\n\
                      Declare it with 'set %s to ...' or as a function parameter." v v
    | Typecheck.TypeMismatch { expected; got } ->
      Printf.sprintf "TYPE MISMATCH: expected %s but the expression has type %s."
        (render_ty face expected)
        (render_ty face got)
    | Typecheck.OccursCheck (v, ty) ->
      Printf.sprintf "TYPE ERROR: '%s' refers to itself (circular type): %s"
        v (render_ty face ty)
    | Typecheck.NotImplemented msg ->
      Printf.sprintf "Not yet supported: %s" msg
    | Typecheck.ArityMismatch { name; expected; got } ->
      Printf.sprintf "'%s' takes %d input%s but was given %d."
        name expected (if expected = 1 then "" else "s") got
    | Typecheck.NotAFunction ty ->
      Printf.sprintf "Cannot call this — it is a %s, not a function."
        (render_ty face ty)
    | Typecheck.FieldNotFound { field; record_ty } ->
      Printf.sprintf "The record type %s has no field called '%s'."
        (render_ty face record_ty) field
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      Printf.sprintf "Cannot access element %d — the tuple only has %d element%s."
        index length (if length = 1 then "" else "s")
    | Typecheck.DuplicateField f ->
      Printf.sprintf "The field '%s' appears more than once in this record." f
    | Typecheck.UnificationError ue -> format_unify_error face ue
    | Typecheck.PatternTypeMismatch msg ->
      Printf.sprintf "Pattern error: %s" msg
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      Printf.sprintf
        "TYPE MISMATCH: the 'if' branch produces %s but the 'else' branch produces %s. \
         Both branches must produce the same type."
        (render_ty face then_ty)
        (render_ty face else_ty)
    | Typecheck.QuantityError (qerr, _span) ->
      format_quantity_error face qerr)
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
  | Lucid ->
    (match err with
    | Typecheck.UnboundVariable v ->
      Printf.sprintf "Variable not in scope: '%s'\n\
                      hint: bring it into scope with 'import' or define it with '%s = ...'."
        v v
    | Typecheck.TypeMismatch { expected; got } ->
      Printf.sprintf "Could not match expected type '%s' with actual type '%s'"
        (render_ty face expected)
        (render_ty face got)
    | Typecheck.OccursCheck (v, ty) ->
      Printf.sprintf "Cannot construct the infinite type: %s ~ %s"
        v (render_ty face ty)
    | Typecheck.NotImplemented msg ->
      Printf.sprintf "Compiler limitation: %s" msg
    | Typecheck.ArityMismatch { name; expected; got } ->
      Printf.sprintf "'%s' takes %d argument%s but was applied to %d"
        name expected (if expected = 1 then "" else "s") got
    | Typecheck.NotAFunction ty ->
      Printf.sprintf "This is not a function: it has type %s" (render_ty face ty)
    | Typecheck.FieldNotFound { field; record_ty } ->
      Printf.sprintf "No field '%s' in record of type '%s'"
        field (render_ty face record_ty)
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      Printf.sprintf "Tuple index %d out of range (tuple has %d element%s)"
        index length (if length = 1 then "" else "s")
    | Typecheck.DuplicateField f ->
      Printf.sprintf "Duplicate label '%s' in record" f
    | Typecheck.UnificationError ue ->
      format_unify_error face ue
    | Typecheck.PatternTypeMismatch msg ->
      Printf.sprintf "Pattern match error: %s" msg
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      Printf.sprintf
        "Branches of 'if' have different types: then-branch returns %s, \
         else-branch returns %s — both branches must agree."
        (render_ty face then_ty)
        (render_ty face else_ty)
    | Typecheck.QuantityError (qerr, _span) ->
      format_quantity_error face qerr)
  | Cafe ->
    (match err with
    | Typecheck.UnboundVariable v ->
      Printf.sprintf "Can't find '%s'.\n\
                      hint: declare it first with '%s = ...'." v v
    | Typecheck.TypeMismatch { expected; got } ->
      Printf.sprintf "Type doesn't fit: expected %s, got %s."
        (render_ty face expected)
        (render_ty face got)
    | Typecheck.OccursCheck (v, ty) ->
      Printf.sprintf "Type '%s' loops back on itself: %s = %s"
        v v (render_ty face ty)
    | Typecheck.NotImplemented msg ->
      Printf.sprintf "Compiler limitation: %s" msg
    | Typecheck.ArityMismatch { name; expected; got } ->
      Printf.sprintf "'%s' wants %d argument%s but got %d."
        name expected (if expected = 1 then "" else "s") got
    | Typecheck.NotAFunction ty ->
      Printf.sprintf "This isn't callable — it's a %s." (render_ty face ty)
    | Typecheck.FieldNotFound { field; record_ty } ->
      Printf.sprintf "No field '%s' on type %s." field (render_ty face record_ty)
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      Printf.sprintf "Tuple index %d out of range (length %d)." index length
    | Typecheck.DuplicateField f ->
      Printf.sprintf "Field '%s' appears twice." f
    | Typecheck.UnificationError ue ->
      format_unify_error face ue
    | Typecheck.PatternTypeMismatch msg ->
      Printf.sprintf "Pattern error: %s" msg
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      Printf.sprintf
        "Branches don't match: then→%s, else→%s. Both branches need the same type."
        (render_ty face then_ty)
        (render_ty face else_ty)
    | Typecheck.QuantityError (qerr, _span) ->
      format_quantity_error face qerr)

(* ─── Borrow errors ──────────────────────────────────────────────────── *)

(** Format a borrow error, optionally translated for the active face. *)
let format_borrow_error (face : face) (err : Borrow.borrow_error) : string =
  match face with
  | Canonical   -> Borrow.format_borrow_error err
  | Python      ->
    (* Python-face vocabulary: ownership → "value lifetime" *)
    let msg = Borrow.format_borrow_error err in
    (* Rephrase the most common variant *)
    (match err with
     | Borrow.UseAfterMove _ ->
       "Lifetime error: " ^ msg
     | Borrow.CannotBorrowAsMutable _ ->
       "Mutation error: " ^ msg
     | _ -> "Ownership error: " ^ msg)
  | Js          -> Borrow.format_borrow_error err
  | Pseudocode  -> Borrow.format_borrow_error err
  | Lucid       ->
    (* Linear-Haskell vocabulary: borrowing is ownership transfer. *)
    let msg = Borrow.format_borrow_error err in
    (match err with
     | Borrow.UseAfterMove _ ->
       "Linearity error (use-after-move): " ^ msg
     | Borrow.CannotBorrowAsMutable _ ->
       "Aliasing error (mutable borrow blocked): " ^ msg
     | _ -> "Linearity error: " ^ msg)
  | Cafe        ->
    let msg = Borrow.format_borrow_error err in
    (match err with
     | Borrow.UseAfterMove _ -> "Use-after-move: " ^ msg
     | Borrow.CannotBorrowAsMutable _ -> "Cannot borrow as mutable: " ^ msg
     | _ -> "Borrow error: " ^ msg)

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
  | Js ->
    (match err with
    | Resolve.UndefinedVariable id ->
      Printf.sprintf "Cannot find name '%s'.\n\
                      Did you forget to declare it with 'const %s = ...' ?" id.name id.name
    | Resolve.UndefinedType id ->
      Printf.sprintf "Cannot find type '%s'.\n\
                      Define it with 'type %s = ...' or import it." id.name id.name
    | Resolve.UndefinedEffect id ->
      Printf.sprintf "Effect '%s' is not in scope." id.name
    | Resolve.UndefinedModule id ->
      Printf.sprintf "Module '%s' not found.\n\
                      Add 'import { ... } from \"%s\"' at the top of the file."
        id.name id.name
    | Resolve.DuplicateDefinition id ->
      Printf.sprintf "Identifier '%s' has already been declared." id.name
    | Resolve.VisibilityError (id, msg) ->
      Printf.sprintf "'%s' is not accessible here: %s" id.name msg
    | Resolve.ImportError msg ->
      Printf.sprintf "Import error: %s" msg)
  | Pseudocode ->
    (match err with
    | Resolve.UndefinedVariable id ->
      Printf.sprintf "ERROR: '%s' is not defined in this scope.\n\
                      Define it with 'set %s to ...' before using it." id.name id.name
    | Resolve.UndefinedType id ->
      Printf.sprintf "ERROR: the type '%s' is not defined." id.name
    | Resolve.UndefinedEffect id ->
      Printf.sprintf "ERROR: the effect '%s' is not defined." id.name
    | Resolve.UndefinedModule id ->
      Printf.sprintf "ERROR: no module named '%s' is available." id.name
    | Resolve.DuplicateDefinition id ->
      Printf.sprintf "ERROR: '%s' has already been defined in this block." id.name
    | Resolve.VisibilityError (id, msg) ->
      Printf.sprintf "ERROR: '%s' is not accessible here: %s" id.name msg
    | Resolve.ImportError msg ->
      Printf.sprintf "IMPORT ERROR: %s" msg)
  | Lucid ->
    (match err with
    | Resolve.UndefinedVariable id ->
      Printf.sprintf "Variable not in scope: '%s'\n\
                      hint: import the module that exports it, or define it with '%s = ...'."
        id.name id.name
    | Resolve.UndefinedType id ->
      Printf.sprintf "Type constructor not in scope: '%s'\n\
                      hint: declare 'data %s = ...' or import it."
        id.name id.name
    | Resolve.UndefinedEffect id ->
      Printf.sprintf "Effect '%s' not in scope" id.name
    | Resolve.UndefinedModule id ->
      Printf.sprintf "Could not find module '%s'\n\
                      hint: 'import %s' at the top of the file."
        id.name id.name
    | Resolve.DuplicateDefinition id ->
      Printf.sprintf "Multiple declarations of '%s'" id.name
    | Resolve.VisibilityError (id, msg) ->
      Printf.sprintf "'%s' is not exported from its module: %s" id.name msg
    | Resolve.ImportError msg ->
      Printf.sprintf "Import error: %s" msg)
  | Cafe ->
    (match err with
    | Resolve.UndefinedVariable id ->
      Printf.sprintf "Can't find '%s'.\n\
                      hint: assign to it first with '%s = ...'." id.name id.name
    | Resolve.UndefinedType id ->
      Printf.sprintf "Can't find type '%s' — declare or import it." id.name
    | Resolve.UndefinedEffect id ->
      Printf.sprintf "Effect '%s' isn't in scope." id.name
    | Resolve.UndefinedModule id ->
      Printf.sprintf "No module '%s' — require it first." id.name
    | Resolve.DuplicateDefinition id ->
      Printf.sprintf "'%s' is already defined here." id.name
    | Resolve.VisibilityError (id, msg) ->
      Printf.sprintf "'%s' isn't accessible: %s" id.name msg
    | Resolve.ImportError msg ->
      Printf.sprintf "Import error: %s" msg)
