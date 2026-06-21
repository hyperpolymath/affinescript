(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   F1_TransformerPreservation.v
   ════════════════════════════
   Wave-0 seed of obligation **F-1** from docs/PROOF-NEEDS.adoc: the *real*
   same-cube theorem — each face's surface→canonical transform `T_F` preserves
   the typed-WASM denotation. F-1 is the front-end twin of K-1; this file
   *composes on top of* K-1 (it `Require`s it) exactly as the inventory says.

   A "face" is an alternate surface syntax (rattle/jaffa/pseudo/lucid/cafe/…)
   whose parser yields a surface AST `fexp`; `elaborate : fexp -> sexp` is the
   transform `T_F` into the canonical AST that K-1's `compile` consumes. We give
   the face its OWN big-step semantics `feval` (what a reader of that surface
   expects) and prove the transform preserves it — then chain K-1 to get
   end-to-end codegen preservation, and state the cross-face "same cube".

   Like K-1, everything here is complete and axiom-free (no `Admitted`).
   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.

   Check:  coqc -Q . ASFormal F1_TransformerPreservation.v   (after K-1)
*)

Require Import List.
Import ListNotations.
Require Import ASFormal.K1_CodegenPreservation.

(* ═══════════ A face surface: canonical forms + parser sugar ═══════════════ *)

(* `FSum3`/`FAll3` are surface sugar a face parser may emit (n-ary fold spelt
   as one node) — they stand in for any derived surface form that elaborates
   to a tree of canonical operations. *)
Inductive fexp : Type :=
| FNat  (n : nat)
| FBool (b : bool)
| FAdd  (a b : fexp)
| FAnd  (a b : fexp)
| FSum3 (a b c : fexp)    (* sugar for  a + (b + c)  *)
| FAll3 (a b c : fexp).   (* sugar for  a && (b && c) *)

(* The transform T_F : surface AST -> canonical AST (`sexp` from K-1). *)
Fixpoint elaborate (f : fexp) : sexp :=
  match f with
  | FNat n     => SNat n
  | FBool b    => SBool b
  | FAdd a b   => SAdd (elaborate a) (elaborate b)
  | FAnd a b   => SAnd (elaborate a) (elaborate b)
  | FSum3 a b c => SAdd (elaborate a) (SAdd (elaborate b) (elaborate c))
  | FAll3 a b c => SAnd (elaborate a) (SAnd (elaborate b) (elaborate c))
  end.

(* The face's own big-step semantics — what a reader of this surface expects,
   defined directly (not via elaboration), so that agreement is a theorem. *)
Fixpoint feval (f : fexp) : option sval :=
  match f with
  | FNat n  => Some (VNat n)
  | FBool b => Some (VBool b)
  | FAdd a b =>
      match feval a, feval b with
      | Some (VNat x), Some (VNat y) => Some (VNat (x + y))
      | _, _ => None
      end
  | FAnd a b =>
      match feval a, feval b with
      | Some (VBool x), Some (VBool y) => Some (VBool (andb x y))
      | _, _ => None
      end
  | FSum3 a b c =>
      match feval a, feval b, feval c with
      | Some (VNat x), Some (VNat y), Some (VNat z) => Some (VNat (x + (y + z)))
      | _, _, _ => None
      end
  | FAll3 a b c =>
      match feval a, feval b, feval c with
      | Some (VBool x), Some (VBool y), Some (VBool z) => Some (VBool (andb x (andb y z)))
      | _, _, _ => None
      end
  end.

(* ════════════ F-1: the transform preserves meaning ═══════════════════════ *)

(* Transform semantics-preservation: the face's own semantics agrees, on the
   nose, with elaborating-then-evaluating canonically. *)
Theorem f1_transform_preserves_eval : forall f, feval f = seval (elaborate f).
Proof.
  induction f as [ n | b | a IHa b IHb | a IHa b IHb
                 | a IHa b IHb c IHc | a IHa b IHb c IHc ]; simpl.
  - reflexivity.
  - reflexivity.
  - rewrite IHa, IHb; reflexivity.
  - rewrite IHa, IHb; reflexivity.
  - (* FSum3: flat 3-way match vs nested — destruct to align *)
    rewrite IHa, IHb, IHc;
    destruct (seval (elaborate a)) as [[x | ] | ];
    destruct (seval (elaborate b)) as [[y | ] | ];
    destruct (seval (elaborate c)) as [[z | ] | ]; reflexivity.
  - rewrite IHa, IHb, IHc;
    destruct (seval (elaborate a)) as [[ | x] | ];
    destruct (seval (elaborate b)) as [[ | y] | ];
    destruct (seval (elaborate c)) as [[ | z] | ]; reflexivity.
Qed.

(* End-to-end (the F-1 "same cube" at the value level): compiling the
   elaborated face program agrees with the face's own semantics — obtained by
   chaining the transform lemma with K-1's backend theorem. *)
Theorem f1_codegen_preservation : forall f v,
  feval f = Some v ->
  wexec (compile (elaborate f)) [] = Some [obs v].
Proof.
  intros f v Hf.
  apply k1_preservation_holds.
  rewrite <- f1_transform_preserves_eval.
  exact Hf.
Qed.

(* Cross-face same-cube: two faces whose transforms land on the same canonical
   AST emit byte-identical code (hence the same wasm). The observational
   version for the trailing-statement/tail split is mechanized separately in
   invariant-path/proofs/SameCube.agda (F-2). *)
Corollary f1_same_cube : forall f1 f2,
  elaborate f1 = elaborate f2 ->
  compile (elaborate f1) = compile (elaborate f2).
Proof.
  intros f1 f2 H; rewrite H; reflexivity.
Qed.

Print Assumptions f1_transform_preserves_eval.
Print Assumptions f1_codegen_preservation.
