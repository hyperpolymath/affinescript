(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   QttSemiring.v
   ═════════════
   Wave 2: the **QTT quantity semiring {0, 1, ω}** — the algebraic core of
   AffineScript's affine / quantitative type system (the `Quantity` module of
   the Solo calculus). Mechanizes the multiplicity semiring with its `+`
   (context addition) and `·` (scaling) operations, the usage order
   `0 ≤ 1 ≤ ω`, and the bridge to occurrence counts:

       qof (m + n) = qplus (qof m) (qof n)

   i.e. *occurrences add under QTT addition*. So using a **linear** (`1`)
   variable in BOTH the function and the argument of an application totals `ω`
   (`qplus One One = Omega`), exceeding the affine bound — the algebraic reason
   a linear value cannot be used twice.

   Advances **P-4** (docs/PROOF-NEEDS.adoc) from prose to a mechanized core.
   All laws hold by exhaustive case analysis — axiom-free, no `Admitted`.
   A usage-counted linear *calculus* (the "1-var used exactly once" typing
   theorem) builds on this in AffineUsage.v.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import Lia.

Inductive quant := Zero | One | Omega.

(* Context addition: how usages combine when a variable is used in two places.
   1 + 1 = ω is the crux — two linear uses exceed the affine bound. *)
Definition qplus (p q : quant) : quant :=
  match p with
  | Zero  => q
  | One   => match q with Zero => One | _ => Omega end
  | Omega => Omega
  end.

(* Scaling: how a usage is multiplied when passing through a binder of a given
   multiplicity. *)
Definition qmult (p q : quant) : quant :=
  match p with
  | Zero  => Zero
  | One   => q
  | Omega => match q with Zero => Zero | _ => Omega end
  end.

(* ── semiring laws (exhaustive case analysis) ──────────────────────────── *)

Lemma qplus_0_l : forall q, qplus Zero q = q.       Proof. reflexivity. Qed.
Lemma qplus_0_r : forall q, qplus q Zero = q.       Proof. destruct q; reflexivity. Qed.
Lemma qplus_comm : forall p q, qplus p q = qplus q p.
Proof. destruct p, q; reflexivity. Qed.
Lemma qplus_assoc : forall p q r, qplus p (qplus q r) = qplus (qplus p q) r.
Proof. destruct p, q, r; reflexivity. Qed.

Lemma qmult_0_l : forall q, qmult Zero q = Zero.    Proof. reflexivity. Qed.
Lemma qmult_0_r : forall q, qmult q Zero = Zero.    Proof. destruct q; reflexivity. Qed.
Lemma qmult_1_l : forall q, qmult One q = q.        Proof. reflexivity. Qed.
Lemma qmult_1_r : forall q, qmult q One = q.        Proof. destruct q; reflexivity. Qed.
Lemma qmult_comm : forall p q, qmult p q = qmult q p.
Proof. destruct p, q; reflexivity. Qed.
Lemma qmult_assoc : forall p q r, qmult p (qmult q r) = qmult (qmult p q) r.
Proof. destruct p, q, r; reflexivity. Qed.

Lemma qdistrib_l : forall p q r, qmult p (qplus q r) = qplus (qmult p q) (qmult p r).
Proof. destruct p, q, r; reflexivity. Qed.
Lemma qdistrib_r : forall p q r, qmult (qplus p q) r = qplus (qmult p r) (qmult q r).
Proof. destruct p, q, r; reflexivity. Qed.

(* ── the usage order  0 ≤ 1 ≤ ω  ───────────────────────────────────────── *)

Definition rank (q : quant) : nat :=
  match q with Zero => 0 | One => 1 | Omega => 2 end.
Definition qle (p q : quant) : Prop := rank p <= rank q.

Lemma qle_refl  : forall q, qle q q.                  Proof. intro; apply Nat.le_refl. Qed.
Lemma qle_trans : forall p q r, qle p q -> qle q r -> qle p r.
Proof. unfold qle; intros p q r H1 H2; eapply Nat.le_trans; eauto. Qed.
Lemma qle_zero_min : forall q, qle Zero q.            Proof. intro q; unfold qle; apply Nat.le_0_l. Qed.
Lemma qle_omega_max : forall q, qle q Omega.          Proof. destruct q; unfold qle; simpl; lia. Qed.

(* ── the affine facts ──────────────────────────────────────────────────── *)

(* Using a linear (1) value twice overflows to ω — i.e. exceeds the affine
   bound `≤ 1`. This is the algebraic heart of "a linear value is used once". *)
Lemma linear_used_twice : qplus One One = Omega.      Proof. reflexivity. Qed.
Lemma affine_bound_exceeded : ~ qle (qplus One One) One.
Proof. unfold qle; rewrite linear_used_twice; simpl; lia. Qed.

(* Erased (0) variables contribute nothing and absorb under scaling. *)
Lemma erased_scales_away : forall q, qmult Zero q = Zero.   Proof. reflexivity. Qed.

(* ── bridge: occurrence counts abstract to quantities ──────────────────── *)

Definition qof (n : nat) : quant :=
  match n with 0 => Zero | 1 => One | _ => Omega end.

(* `qof` is a semiring homomorphism on addition: occurrences add under qplus.
   So a variable used m times in `f` and n times in `a` is used (m+n) times in
   `f a`, abstracting to qplus — and if both are 1, the total is ω. *)
Theorem qof_plus : forall m n, qof (m + n) = qplus (qof m) (qof n).
Proof. intros m n; destruct m as [| [| m]]; destruct n as [| [| n]]; reflexivity. Qed.

Corollary qof_linear_twice : qof 1 = One /\ qof (1 + 1) = Omega.
Proof. split; reflexivity. Qed.
