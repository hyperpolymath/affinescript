(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   Siblings_Stated.v
   ═════════════════
   Wave-0 *statements* (NOT proofs) of the remaining Wave-0 obligations from
   docs/PROOF-NEEDS.adoc — P-2, P-3, F-3, F-4 — as parametric Coq propositions.

   Each obligation abstracts the not-yet-modelled syntax/judgments via a
   `Section` with `Variable`s and records the property as `Definition
   ..._statement : Prop`. At `End`, the Variables are discharged into ordinary
   universally-quantified propositions, so:
     * the file is complete and **axiom-free** (no `Admitted`, no `Axiom`,
       no `Parameter`); and
     * NOTHING is claimed proven — these are signatures, the Coq analogue of
       solo-core's statement-only Idris2 skeleton.

   Each is now DISCHARGED for a concrete model (small fragments, not the full
   language): P-2 in P2_Progress.v, P-3 in P3_BorrowSound.v, F-3 in
   F3_PragmaDecidable.v, F-4 in F4_ErrorFaithful.v — each ending in a
   `*_discharged : <this statement> ... := <proof>` line. F-1 is discharged in
   F1_TransformerPreservation.v; K-1 in K1_CodegenPreservation.v (and the grown
   K1Let_CodegenPreservation.v). Growing these models toward the real language
   (functions/QTT for P-2, the real borrow graph for P-3) is Wave 1.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

(* ═══════════════════ P-2 — Solo progress + preservation ══════════════════ *)
Section P2_Solo.
  Variable term : Type.
  Variable ty   : Type.
  Variable ctx  : Type.
  Variable empty    : ctx.
  Variable has_type : ctx -> term -> ty -> Prop.
  Variable value    : term -> Prop.
  Variable step     : term -> term -> Prop.

  (* A well-typed closed term is a value or can step. *)
  Definition P2_progress : Prop :=
    forall t a, has_type empty t a -> value t \/ (exists t', step t t').

  (* Typing is preserved by a step (subject reduction). *)
  Definition P2_preservation : Prop :=
    forall g t t' a, has_type g t a -> step t t' -> has_type g t' a.
End P2_Solo.

(* ═══════════ P-3 — borrow-graph soundness (must reject #554) ══════════════ *)
Section P3_Borrow.
  Variable term : Type.
  Variable borrow_ok       : term -> Prop.   (* the borrow checker accepts t *)
  Variable uses_after_move : term -> Prop.   (* t observes a moved value at runtime *)
  Variable example_554     : term.           (* let r = pick(a); consume(a); *r *)

  (* Soundness: an accepted program never uses-after-move. *)
  Definition P3_borrow_soundness : Prop :=
    forall t, borrow_ok t -> ~ uses_after_move t.

  (* The #554 obligation made explicit: the witness program DOES use-after-move,
     so a sound checker must REJECT it. (Today's checker accepts it — that gap
     is exactly #554 / the Polonius residual #553.) This is a direct corollary
     of P3_borrow_soundness applied to example_554. *)
  Definition P3_rejects_554 : Prop :=
    uses_after_move example_554 -> ~ borrow_ok example_554.
End P3_Borrow.

(* ═════════ F-3 — face pragma detection: deterministic + functional ═══════ *)
Section F3_Pragma.
  Variable source : Type.
  Variable face   : Type.
  Variable detect        : source -> option face.   (* Face_pragma.detect_in_source *)
  Variable pragma_region : source -> source.        (* the leading lines actually scanned *)

  (* Totality and determinism are automatic for a Coq function `detect`; the
     real content is *locality* — the result depends only on the scanned
     region, never on bytes past the first code token. *)
  Definition F3_pragma_local : Prop :=
    forall s1 s2, pragma_region s1 = pragma_region s2 -> detect s1 = detect s2.

  (* The alias table (rattle→Python, jaffa→Js, …) is a function: a name resolves
     to at most one face. *)
  Variable name         : Type.
  Variable resolve_name : name -> option face.
  Definition F3_alias_functional : Prop :=
    forall n f1 f2, resolve_name n = Some f1 -> resolve_name n = Some f2 -> f1 = f2.
End F3_Pragma.

(* ═══════════ F-4 — error-vocabulary faithfulness (a simulation) ══════════ *)
Section F4_ErrorVocab.
  Variable canon_error : Type.   (* canonical compiler error term *)
  Variable face        : Type.
  Variable rendered    : Type.   (* face-specific rendered message *)
  Variable render : face -> canon_error -> rendered.

  (* Observables recoverable from a canonical error and from a rendering. *)
  Variable err_class          : canon_error -> nat.   (* the error's kind *)
  Variable err_referent       : canon_error -> nat.   (* the offending identifier/site *)
  Variable rendered_class     : rendered -> nat.
  Variable rendered_referent  : rendered -> nat.

  (* Faithfulness: a face rendering preserves the canonical error's class and
     referent — a face can never make error X read as a different error Y.
     (OCaml's exhaustiveness checks `render` is total; this is the *semantic*
     obligation it does not check.) *)
  Definition F4_error_faithful : Prop :=
    forall f e,
      rendered_class    (render f e) = err_class e /\
      rendered_referent (render f e) = err_referent e.
End F4_ErrorVocab.
