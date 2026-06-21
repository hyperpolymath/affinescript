(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   QttDynamic.v
   ════════════
   Wave 3, the DYNAMIC half of P-4. QttTyping.v proved the *static* quantity
   discipline sound — the typing context records exactly the usage
   (`usage x t = Γ x`). This file proves that discipline is preserved **under
   reduction**: a small-step β-reduction relation on the QTT term language
   (`AffineUsage.tm`) carries the whole usage profile forward unchanged.

   Headline — `step_preserves_usage`:

       t → t'  →  ∀ z, usage z t' = usage z t

   so the quantity accounting is a dynamic invariant. Composed with QttTyping's
   `usage_soundness` it yields the capstone-completing corollary
   `qtt_context_preserved`:

       qtt Γ t A → t → t' → ∀ x, usage x t' = Γ x

   — a well-typed term's QTT context stays valid across a reduction step.

   Two further results expose the QTT *scaling* law that makes substructurality
   bite. Substituting v for a variable used at multiplicity q scales v's usage by
   q (`usage_subst_scaling`, via the new ×-homomorphism `qof_mult`); and an
   affine redex never increases any variable's usage (`affine_no_increase`) — the
   dynamic face of the affine bound. β here substitutes a *closed* value (the
   call-by-value, closed-program convention P2_Stlc.v also uses), so capture
   cannot arise and the development stays axiom-free — no Admitted, no axioms.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import Lia.
Require Import ASFormal.QttSemiring.
Require Import ASFormal.AffineUsage.
Require Import ASFormal.QttTyping.

(* ── qof is also a homomorphism for multiplication ──────────────────────────
   QttSemiring gives qof_plus (occurrences add); the scaling law below needs the
   × analogue (occurrences multiply through a binder of a given multiplicity). *)
Lemma qof_mult : forall m n, qof (m * n) = qmult (qof m) (qof n).
Proof.
  intros m n; destruct m as [|[|m]]; destruct n as [|[|n]];
    try reflexivity; rewrite Nat.mul_0_r; reflexivity.
Qed.

(* ── capture-permitting substitution (only ever applied to closed values) ─── *)
Fixpoint subst (x : id) (s t : tm) : tm :=
  match t with
  | var y   => if Nat.eqb x y then s else var y
  | app f a => app (subst x s f) (subst x s a)
  | lam y b => if Nat.eqb x y then lam y b else lam y (subst x s b)
  end.

Definition closed (t : tm) : Prop := forall z, count z t = 0.

(* ── small-step β-reduction (call-by-value, closed argument) ──────────────── *)
Inductive value : tm -> Prop :=
| v_lam : forall x b, value (lam x b).

Inductive step : tm -> tm -> Prop :=
| ST_AppAbs : forall x b v,
    value v -> closed v -> step (app (lam x b) v) (subst x v b)
| ST_App1 : forall f f' a, step f f' -> step (app f a) (app f' a)
| ST_App2 : forall f a a', value f -> step a a' -> step (app f a) (app f a').

(* ── substituting a closed value: counts untouched off x, zeroed at x ─────── *)

Lemma count_subst_closed_diff : forall x v b z,
  closed v -> z <> x -> count z (subst x v b) = count z b.
Proof.
  intros x v b z Hc Hzx.
  induction b as [ y | f IHf a IHa | y b IHb ].
  - (* var y *) cbn [subst]. destruct (Nat.eqb x y) eqn:E.
    + rewrite (Hc z). apply Nat.eqb_eq in E; subst y.
      cbn. apply Nat.eqb_neq in Hzx. rewrite Hzx. reflexivity.
    + reflexivity.
  - (* app *) simpl. rewrite IHf, IHa. reflexivity.
  - (* lam y b *) cbn [subst]. destruct (Nat.eqb x y) eqn:E.
    + reflexivity.
    + simpl. destruct (Nat.eqb z y); [ reflexivity | exact IHb ].
Qed.

Lemma count_subst_closed_same : forall x v b,
  closed v -> count x (subst x v b) = 0.
Proof.
  intros x v b Hc.
  induction b as [ y | f IHf a IHa | y b IHb ].
  - cbn [subst]. destruct (Nat.eqb x y) eqn:E.
    + apply (Hc x).
    + cbn. rewrite E. reflexivity.
  - simpl. rewrite IHf, IHa. reflexivity.
  - cbn [subst]. destruct (Nat.eqb x y) eqn:E.
    + cbn. rewrite E. reflexivity.
    + cbn. rewrite E. exact IHb.
Qed.

(* ── headline: reduction preserves the full usage profile ─────────────────── *)

Theorem step_preserves_usage : forall t t',
  step t t' -> forall z, usage z t' = usage z t.
Proof.
  intros t t' Hs; induction Hs as
    [ x b v Hval Hcl | f f' a Hs IH | f a a' Hval Hs IH ]; intro z.
  - (* ST_AppAbs *)
    rewrite usage_app.
    assert (Hzv : usage z v = Zero) by (unfold usage; rewrite Hcl; reflexivity).
    rewrite Hzv, qplus_0_r.
    destruct (Nat.eqb z x) eqn:E.
    + apply Nat.eqb_eq in E; subst z.
      rewrite usage_lam_bound.
      unfold usage; rewrite (count_subst_closed_same x v b Hcl); reflexivity.
    + assert (Hzx : z <> x) by (apply Nat.eqb_neq; exact E).
      unfold usage. rewrite (count_subst_closed_diff x v b z Hcl Hzx).
      simpl. rewrite E. reflexivity.
  - (* ST_App1 *) rewrite !usage_app, (IH z). reflexivity.
  - (* ST_App2 *) rewrite !usage_app, (IH z). reflexivity.
Qed.

(* ── the QTT scaling law (binder-free fragment, open v) ────────────────────
   Substituting v for x scales v's usage by x's multiplicity:
       usage z (subst x v b) = usage z b  +  (usage x b · usage z v).
   Stated on the lam-free fragment so capture cannot arise even for open v —
   enough to exhibit the qmult scaling that the closed-value headline hides. *)

Fixpoint lam_free (t : tm) : Prop :=
  match t with
  | var _   => True
  | app f a => lam_free f /\ lam_free a
  | lam _ _ => False
  end.

Lemma usage_subst_scaling : forall x v b z,
  z <> x -> lam_free b ->
  usage z (subst x v b) = qplus (usage z b) (qmult (usage x b) (usage z v)).
Proof.
  intros x v b; induction b as [ y | f IHf a IHa | y b IHb ];
    intros z Hzx Hlf.
  - (* var y *) cbn [subst]. destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y.
      rewrite (usage_var_diff z x Hzx), (usage_var_same x), qmult_1_l, qplus_0_l.
      reflexivity.
    + assert (Hxy : x <> y) by (apply Nat.eqb_neq; exact E).
      rewrite (usage_var_diff x y Hxy), qmult_0_l, qplus_0_r. reflexivity.
  - (* app f a *) destruct Hlf as [Hlf Hla].
    cbn [subst].
    rewrite (usage_app z (subst x v f) (subst x v a)).
    rewrite (IHf z Hzx Hlf), (IHa z Hzx Hla).
    rewrite (usage_app z f a), (usage_app x f a).
    destruct (usage z f), (usage x f), (usage z a), (usage x a), (usage z v);
      reflexivity.
  - (* lam *) cbn in Hlf; destruct Hlf.
Qed.

(* An affine redex (bound variable used ≤ 1×) never increases any variable's
   usage: the dynamic counterpart of the static affine bound. *)
Corollary affine_no_increase : forall x v b z,
  z <> x -> lam_free b -> qle (usage x b) One ->
  qle (usage z (subst x v b)) (qplus (usage z b) (usage z v)).
Proof.
  intros x v b z Hzx Hlf Haff.
  rewrite usage_subst_scaling by assumption.
  revert Haff; unfold qle.
  destruct (usage x b), (usage z b), (usage z v); simpl; lia.
Qed.

(* A *linear* redex (bound variable used exactly once) preserves usage exactly,
   matching the closed-value headline but now for open v. *)
Corollary linear_exact : forall x v b z,
  z <> x -> lam_free b -> usage x b = One ->
  usage z (subst x v b) = qplus (usage z b) (usage z v).
Proof.
  intros x v b z Hzx Hlf Hlin.
  rewrite usage_subst_scaling by assumption.
  rewrite Hlin, qmult_1_l. reflexivity.
Qed.

(* ── tie back to the Wave-3 static capstone ───────────────────────────────── *)

(* A well-typed term's QTT context stays valid across a reduction step:
   static usage_soundness (qtt Γ t A → usage x t = Γ x) composed with the
   dynamic invariant. Static + dynamic ⇒ the quantity discipline is sound. *)
Corollary qtt_context_preserved : forall G t A t',
  qtt G t A -> step t t' -> forall x, usage x t' = G x.
Proof.
  intros G t A t' HT Hs x.
  rewrite (step_preserves_usage t t' Hs x).
  exact (usage_soundness G t A HT x).
Qed.

Print Assumptions step_preserves_usage.
Print Assumptions usage_subst_scaling.
Print Assumptions qtt_context_preserved.
