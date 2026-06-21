(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   QttTyping.v
   ═══════════
   Wave 3 (capstone): a **quantitative type system** that fuses the three Wave-1/2
   pieces — the term structure + typing discipline of P2_Stlc, the multiplicity
   semiring of QttSemiring, and the occurrence-count usage of AffineUsage — into
   one judgment whose context *tracks quantities*.

   `qtt Γ t A` : Γ : uctx (a usage context, var ↦ quantity) records how much each
   variable is used in t. Q_Var consumes one variable once; Q_App ADDS the two
   subcontexts (`Γ₁ + Γ₂`, the QTT context-splitting read backwards); Q_Lam binds
   a variable at the multiplicity it is used.

   The capstone theorem **usage_soundness**: the typing context records EXACTLY
   the QTT usage of every variable —

       qtt Γ t A  →  ∀ x, usage x t = Γ x

   — so the type system's quantity bookkeeping is sound w.r.t. actual occurrence
   counts. Corollary **linear_uses_once**: a term typed in a singleton context
   uses exactly that one variable, exactly once. Axiom-free, no Admitted.

   Scope: the *static* quantity discipline is now unified and proven sound
   against usage. Operational progress+preservation that PRESERVES the quantity
   accounting under reduction (the dynamic half) is the remaining step.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import ASFormal.QttSemiring.
Require Import ASFormal.AffineUsage.

(* Multiplicity-annotated types: a function records how often it uses its arg. *)
Inductive qty := QBase | QArr (q : quant) (A B : qty).

(* Usage contexts. *)
Definition uctx := id -> quant.
Definition uadd    (G1 G2 : uctx) : uctx := fun x => qplus (G1 x) (G2 x).
Definition usingle (x : id)       : uctx := fun y => if Nat.eqb x y then One else Zero.
Definition uext    (G : uctx) (x : id) (q : quant) : uctx :=
  fun y => if Nat.eqb x y then q else G y.

(* Quantitative typing: the context records each variable's usage. *)
Inductive qtt : uctx -> tm -> qty -> Prop :=
| Q_Var : forall x A, qtt (usingle x) (var x) A
| Q_App : forall G1 G2 f a q A B,
    qtt G1 f (QArr q A B) -> qtt G2 a A -> qtt (uadd G1 G2) (app f a) B
| Q_Lam : forall G x q A B b,
    G x = Zero -> qtt (uext G x q) b B -> qtt G (lam x b) (QArr q A B).

(* ── capstone: the typing context records exactly the usage ─────────────── *)

Theorem usage_soundness : forall G t A,
  qtt G t A -> forall x, usage x t = G x.
Proof.
  intros G t A H.
  induction H as [ x A
                 | G1 G2 f a q A B Hf IHf Ha IHa
                 | G x q A B b Hz Hb IHb ]; intro z.
  - (* Q_Var *)
    unfold usage, usingle; simpl. rewrite (Nat.eqb_sym x z).
    destruct (Nat.eqb z x); reflexivity.
  - (* Q_App *)
    rewrite usage_app, (IHf z), (IHa z); unfold uadd; reflexivity.
  - (* Q_Lam *)
    destruct (Nat.eqb z x) eqn:E.
    + apply Nat.eqb_eq in E; subst z.
      rewrite usage_lam_bound; symmetry; exact Hz.
    + assert (usage z (lam x b) = usage z b) as Hlz
        by (unfold usage; simpl; rewrite E; reflexivity).
      rewrite Hlz, (IHb z); unfold uext; rewrite (Nat.eqb_sym x z), E; reflexivity.
Qed.

(* A term typed in a singleton context uses exactly that one variable, once —
   the essence of linearity, now read off the typing. *)
Corollary linear_uses_once : forall x t A,
  qtt (usingle x) t A ->
  usage x t = One /\ (forall y, y <> x -> usage y t = Zero).
Proof.
  intros x t A H; split.
  - rewrite (usage_soundness _ _ _ H x); unfold usingle; rewrite Nat.eqb_refl; reflexivity.
  - intros y Hy. rewrite (usage_soundness _ _ _ H y); unfold usingle.
    assert (x <> y) as Hxy by (intro Hc; apply Hy; symmetry; exact Hc).
    apply Nat.eqb_neq in Hxy; rewrite Hxy; reflexivity.
Qed.

Print Assumptions usage_soundness.
Print Assumptions linear_uses_once.
