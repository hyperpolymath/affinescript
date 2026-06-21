(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   AffineUsage.v
   ═════════════
   Wave 2: the affine-usage layer over the QTT semiring (QttSemiring.v).

   Reads the multiplicity of a variable in a term as the QTT abstraction of its
   free-occurrence count: `usage x t := qof (count x t)`. Proves that usage is
   compositional under the semiring — through an application the usages ADD:

       usage x (app f a) = qplus (usage x f) (usage x a)         (from qof_plus)

   — and defines **affine well-formedness** (every binder consumes its variable
   at most once, `usage x b ≤ 1`). The payoff theorems: the identity `λx.x` is
   affine, while self-application `λx. x x` is NOT — its variable totals `ω`,
   which exceeds the affine bound. That is the mechanized statement of "a
   linear/affine value may not be duplicated".

   Advances **P-4** (docs/PROOF-NEEDS.adoc). Axiom-free, no `Admitted`. The full
   *typed* QTT calculus (context splitting Γ = Γ₁ + Γ₂, progress+preservation
   tracking quantities) is the next increment, on top of this + P2_Stlc.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import Lia.
Require Import ASFormal.QttSemiring.

Definition id := nat.

Inductive tm :=
| var (x : id)
| app (f a : tm)
| lam (x : id) (b : tm).

(* Free-occurrence count of x in t. *)
Fixpoint count (x : id) (t : tm) : nat :=
  match t with
  | var y   => if Nat.eqb x y then 1 else 0
  | app f a => count x f + count x a
  | lam y b => if Nat.eqb x y then 0 else count x b
  end.

(* The multiplicity at which x is used in t, as a QTT quantity. *)
Definition usage (x : id) (t : tm) : quant := qof (count x t).

(* ── usage is compositional under the semiring ─────────────────────────── *)

(* Through an application, usages ADD (qplus) — the crux of the affine reading:
   a variable used once in `f` and once in `a` is used ω in `f a`. *)
Theorem usage_app : forall x f a,
  usage x (app f a) = qplus (usage x f) (usage x a).
Proof. intros x f a; unfold usage; simpl; apply qof_plus. Qed.

Lemma usage_var_same : forall x, usage x (var x) = One.
Proof. intro x; unfold usage; simpl; rewrite Nat.eqb_refl; reflexivity. Qed.

Lemma usage_var_diff : forall x y, x <> y -> usage x (var y) = Zero.
Proof.
  intros x y H; unfold usage; simpl.
  apply Nat.eqb_neq in H; rewrite H; reflexivity.
Qed.

Lemma usage_lam_bound : forall x b, usage x (lam x b) = Zero.
Proof. intros x b; unfold usage; simpl; rewrite Nat.eqb_refl; reflexivity. Qed.

(* ── affine well-formedness ────────────────────────────────────────────── *)

Fixpoint affine (t : tm) : Prop :=
  match t with
  | var _   => True
  | app f a => affine f /\ affine a
  | lam x b => qle (usage x b) One /\ affine b
  end.

(* The identity is affine: its bound variable is used exactly once. *)
Example affine_id : forall x, affine (lam x (var x)).
Proof.
  intro x; simpl; split.
  - rewrite usage_var_same; apply qle_refl.
  - exact I.
Qed.

(* Self-application λx. x x is NOT affine: x is used twice, totalling ω, which
   exceeds the affine bound `≤ 1`. *)
Example not_affine_selfapp : forall x, ~ affine (lam x (app (var x) (var x))).
Proof.
  intro x; simpl; intros [Hle _].
  unfold usage, qle in Hle; simpl in Hle.
  rewrite !Nat.eqb_refl in Hle; simpl in Hle; lia.
Qed.

Print Assumptions usage_app.
Print Assumptions not_affine_selfapp.
