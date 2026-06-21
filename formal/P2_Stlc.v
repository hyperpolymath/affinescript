(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   P2_Stlc.v
   ═════════
   P-2, GROWN (Wave 1). Adds what the first-order P2_Progress.v lacked:
   **functions, binders, and the substitution lemma**. Full progress +
   preservation for the simply-typed lambda calculus (base type `TUnit` plus
   `→`), call-by-value, named (`nat`) variables.

   Crucially **funext-free**: contexts are compared only on a term's *free*
   variables (`context_invariance`), never by function equality — so the
   development uses NO `functional_extensionality` and `Print Assumptions` stays
   "Closed under the global context". No `Admitted`, no axioms.

   Scope: simply-typed (no QTT/affine quantities yet) — the substructural
   context-splitting discipline on top of this is the next increment.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import ASFormal.Siblings_Stated.

Definition id := nat.

Inductive ty := TUnit | TArrow (A B : ty).

Inductive tm :=
| var  (x : id)
| app  (f a : tm)
| abs  (x : id) (A : ty) (b : tm)   (* A is the domain type *)
| tunit.

Inductive value : tm -> Prop :=
| v_abs  : forall x A b, value (abs x A b)
| v_unit : value tunit.

(* Substitution. Only ever applied with a closed value (the beta-redex
   argument), so capture cannot arise — established by the typing lemmas. *)
Fixpoint subst (x : id) (s t : tm) : tm :=
  match t with
  | var y     => if Nat.eqb x y then s else var y
  | abs y A b => abs y A (if Nat.eqb x y then b else subst x s b)
  | app f a   => app (subst x s f) (subst x s a)
  | tunit     => tunit
  end.

Inductive step : tm -> tm -> Prop :=
| ST_AppAbs : forall x A b v, value v -> step (app (abs x A b) v) (subst x v b)
| ST_App1   : forall f f' a, step f f' -> step (app f a) (app f' a)
| ST_App2   : forall f a a', value f -> step a a' -> step (app f a) (app f a').

Definition context := id -> option ty.
Definition empty : context := fun _ => None.
Definition extend (G : context) (x : id) (T : ty) : context :=
  fun y => if Nat.eqb x y then Some T else G y.

Inductive has_type : context -> tm -> ty -> Prop :=
| T_Var  : forall G x T, G x = Some T -> has_type G (var x) T
| T_Abs  : forall G x A B b, has_type (extend G x A) b B -> has_type G (abs x A b) (TArrow A B)
| T_App  : forall G f a A B, has_type G f (TArrow A B) -> has_type G a A -> has_type G (app f a) B
| T_Unit : forall G, has_type G tunit TUnit.

(* ── free variables, free-in-context ───────────────────────────────────── *)

Inductive afi : id -> tm -> Prop :=
| afi_var  : forall x, afi x (var x)
| afi_app1 : forall x f a, afi x f -> afi x (app f a)
| afi_app2 : forall x f a, afi x a -> afi x (app f a)
| afi_abs  : forall x y A b, y <> x -> afi x b -> afi x (abs y A b).

#[local] Hint Constructors afi : core.

Lemma free_in_context : forall x t S G,
  afi x t -> has_type G t S -> exists U, G x = Some U.
Proof.
  intros x t S G Hafi; generalize dependent S; generalize dependent G.
  induction Hafi; intros G S HT; inversion HT; subst.
  - eauto.
  - eapply IHHafi; eauto.
  - eapply IHHafi; eauto.
  - edestruct IHHafi as [U HU]; eauto.
    unfold extend in HU. destruct (Nat.eqb y x) eqn:E.
    + apply Nat.eqb_eq in E; subst y; exfalso; apply H; reflexivity.
    + eauto.
Qed.

Corollary typable_empty_closed : forall x t S,
  afi x t -> has_type empty t S -> False.
Proof.
  intros x t S Hafi HT.
  destruct (free_in_context x t S empty Hafi HT) as [U HU]; discriminate HU.
Qed.

(* ── context invariance (the funext-free substitute for map equality) ──── *)

Lemma context_invariance : forall G G' t S,
  has_type G t S ->
  (forall x, afi x t -> G x = G' x) ->
  has_type G' t S.
Proof.
  intros G G' t S HT; generalize dependent G'.
  induction HT; intros G' Hf.
  - apply T_Var. rewrite <- (Hf x (afi_var x)). assumption.
  - apply T_Abs. apply IHHT. intros z Hz. unfold extend.
    destruct (Nat.eqb x z) eqn:E; auto.
    apply Hf. apply Nat.eqb_neq in E. auto.
  - eapply T_App; [apply IHHT1 | apply IHHT2]; intros z Hz; apply Hf; auto.
  - apply T_Unit.
Qed.

(* ── substitution preserves typing ─────────────────────────────────────── *)

Lemma subst_preserves_typing : forall G x U t v S,
  has_type (extend G x U) t S ->
  has_type empty v U ->
  has_type G (subst x v t) S.
Proof.
  intros G x U t v S Ht Hv; generalize dependent S; generalize dependent G.
  induction t as [ y | f IHf a IHa | y A b IHb | ];
    intros G S Ht; simpl; inversion Ht; subst.
  - (* var y *)
    unfold extend in H1. destruct (Nat.eqb x y) eqn:E.
    + injection H1 as H1. rewrite <- H1.
      apply (context_invariance empty G); [assumption |].
      intros z Hz. exfalso; apply (typable_empty_closed z v U Hz Hv).
    + apply T_Var; assumption.
  - (* app f a *)
    eapply T_App; [apply IHf | apply IHa]; eassumption.
  - (* abs y A b *)
    destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y.
      apply T_Abs.
      apply (context_invariance (extend (extend G x U) x A) (extend G x A));
        [assumption |].
      intros z Hz. unfold extend. destruct (Nat.eqb x z); reflexivity.
    + apply T_Abs. apply IHb.
      apply (context_invariance (extend (extend G x U) y A)
                                (extend (extend G y A) x U)); [assumption |].
      intros z Hz. unfold extend.
      destruct (Nat.eqb y z) eqn:E1; destruct (Nat.eqb x z) eqn:E2;
        try reflexivity.
      exfalso. apply Nat.eqb_neq in E. apply E.
      apply Nat.eqb_eq in E1; apply Nat.eqb_eq in E2.
      rewrite E2, <- E1; reflexivity.
  - (* tunit *) apply T_Unit.
Qed.

(* ── canonical forms ───────────────────────────────────────────────────── *)

Lemma canon_arrow : forall v A B,
  value v -> has_type empty v (TArrow A B) -> exists x b, v = abs x A b.
Proof.
  intros v A B Hv HT; destruct Hv.
  - inversion HT; subst; eauto.
  - inversion HT.
Qed.

(* ── progress ──────────────────────────────────────────────────────────── *)

Theorem progress : forall t S, has_type empty t S -> value t \/ exists t', step t t'.
Proof.
  intros t S HT; remember empty as G eqn:HG.
  induction HT.
  - subst G; discriminate H.
  - left; apply v_abs.
  - right; subst G.
    destruct IHHT1 as [Hvf | [f' Hf]]; [reflexivity | |].
    + destruct (canon_arrow _ _ _ Hvf HT1) as [x [b ->]].
      destruct IHHT2 as [Hva | [a' Ha]]; [reflexivity | |].
      * eexists; apply ST_AppAbs; assumption.
      * eexists; apply ST_App2; [apply v_abs | eassumption].
    + eexists; apply ST_App1; eassumption.
  - left; apply v_unit.
Qed.

(* ── preservation ──────────────────────────────────────────────────────── *)

Theorem preservation : forall t t' S,
  has_type empty t S -> step t t' -> has_type empty t' S.
Proof.
  intros t t' S HT Hstep; generalize dependent S.
  induction Hstep; intros S HT; inversion HT; subst.
  - (* ST_AppAbs *)
    match goal with H : has_type _ (abs _ _ _) _ |- _ => inversion H; subst end.
    eapply subst_preserves_typing; eassumption.
  - (* ST_App1 *) eapply T_App; [ apply IHHstep; eassumption | eassumption ].
  - (* ST_App2 *) eapply T_App; [ eassumption | apply IHHstep; eassumption ].
Qed.

(* ── discharge the stated obligations (closed-term, ctx:=unit) ──────────── *)

Definition P2_progress_discharged
  : P2_progress tm ty unit tt
      (fun (_ : unit) (t : tm) (T : ty) => has_type empty t T) value step
  := progress.

Definition P2_preservation_discharged
  : P2_preservation tm ty unit
      (fun (_ : unit) (t : tm) (T : ty) => has_type empty t T) step
  := fun (_ : unit) t t' T HT Hs => preservation t t' T HT Hs.

Print Assumptions progress.
Print Assumptions preservation.
