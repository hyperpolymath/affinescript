(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   F4_ErrorFaithful.v
   ══════════════════
   Mechanizes obligation **F-4** (error-vocabulary faithfulness) for a concrete
   model, discharging the statement from Siblings_Stated.v — axiom-free.

   Models `lib/face.ml`'s `format_*_for_face`: a face changes only the *words*
   of an error message; it must preserve the canonical error's *class* (which
   error) and *referent* (the offending identifier/site). The model makes
   `render` carry class + referent through unchanged while choosing
   face-specific vocabulary, and proves the rendering preserves both — so a
   face can never make error X read as a different error Y.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import ASFormal.Siblings_Stated.

Inductive face := FCanonical | FPython | FJs | FPseudo | FLucid | FCafe.

(* A canonical error: which error (class) and what it is about (referent). *)
Record canon_error := mkErr { ce_class : nat; ce_referent : nat }.

(* A rendered message: the preserved class + referent, plus face-chosen words. *)
Record rendered := mkRendered { r_class : nat; r_referent : nat; r_vocab : nat }.

(* Face-specific vocabulary for a given error class (the part that legitimately
   varies between faces — e.g. "single-use" vs "linear" vs "one-shot"). *)
Definition face_vocab (f : face) (cls : nat) : nat :=
  match f with
  | FCanonical => cls
  | FPython    => cls + 1
  | FJs        => cls + 2
  | FPseudo    => cls + 3
  | FLucid     => cls + 4
  | FCafe      => cls + 5
  end.

(* The face renderer: vocabulary may change; class and referent are carried
   through verbatim. *)
Definition render (f : face) (e : canon_error) : rendered :=
  mkRendered (ce_class e) (ce_referent e) (face_vocab f (ce_class e)).

Definition rendered_class    (r : rendered) : nat := r_class r.
Definition rendered_referent (r : rendered) : nat := r_referent r.

Theorem error_faithful :
  forall f e,
    rendered_class    (render f e) = ce_class e /\
    rendered_referent (render f e) = ce_referent e.
Proof. intros f e; split; reflexivity. Qed.

(* ── discharge the stated obligation ───────────────────────────────────── *)

Definition F4_faithful_discharged
  : F4_error_faithful canon_error face rendered render
      ce_class ce_referent rendered_class rendered_referent
  := error_faithful.

Print Assumptions F4_faithful_discharged.
