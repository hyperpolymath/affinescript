(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   P2_Progress.v
   ═════════════
   Mechanizes obligation **P-2** (progress + preservation) for a minimal typed
   calculus, discharging both statements from Siblings_Stated.v — axiom-free,
   no Admitted.

   Calculus: types Nat | Bool; terms numbers, booleans, `add`, and `if` (a real
   elimination form, so progress is non-trivial — a `tif` on a non-boolean is
   ruled out only by typing). Small-step, call-by-value. This is the classic
   "arith + bool" type-soundness core.

   Scope (honest): this is the *simply-typed, first-order* core of the Solo
   fragment. Functions/`let`/binders (substitution or environments), products,
   sums, and the QTT/affine quantities are the next increments — solo-core's
   Duet/Ensemble direction. (Codegen preservation WITH `let`/variables is
   already mechanized in K1Let_CodegenPreservation.v.)

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import ASFormal.Siblings_Stated.

Inductive ty := TNat | TBool.

Inductive tm :=
| tnum  (n : nat)
| tbool (b : bool)
| tadd  (a b : tm)
| tif   (c t e : tm).

Inductive value : tm -> Prop :=
| v_num  : forall n, value (tnum n)
| v_bool : forall b, value (tbool b).

Inductive has_type : tm -> ty -> Prop :=
| T_Num  : forall n, has_type (tnum n) TNat
| T_Bool : forall b, has_type (tbool b) TBool
| T_Add  : forall a b, has_type a TNat -> has_type b TNat -> has_type (tadd a b) TNat
| T_If   : forall c t e T,
    has_type c TBool -> has_type t T -> has_type e T -> has_type (tif c t e) T.

Inductive step : tm -> tm -> Prop :=
| S_Add1 : forall a a' b, step a a' -> step (tadd a b) (tadd a' b)
| S_Add2 : forall a b b', value a -> step b b' -> step (tadd a b) (tadd a b')
| S_AddNum : forall m n, step (tadd (tnum m) (tnum n)) (tnum (m + n))
| S_If : forall c c' t e, step c c' -> step (tif c t e) (tif c' t e)
| S_IfTrue  : forall t e, step (tif (tbool true) t e) t
| S_IfFalse : forall t e, step (tif (tbool false) t e) e.

(* ── canonical forms ───────────────────────────────────────────────────── *)

Lemma canon_nat : forall v, value v -> has_type v TNat -> exists n, v = tnum n.
Proof. intros v Hv HT; inversion Hv; subst; inversion HT; subst; eauto. Qed.

Lemma canon_bool : forall v, value v -> has_type v TBool -> exists b, v = tbool b.
Proof. intros v Hv HT; inversion Hv; subst; inversion HT; subst; eauto. Qed.

(* ── progress ──────────────────────────────────────────────────────────── *)

Theorem progress : forall t T, has_type t T -> value t \/ (exists t', step t t').
Proof.
  intros t T HT; induction HT.
  - left; constructor.
  - left; constructor.
  - (* tadd a b *)
    right; destruct IHHT1 as [Hva | [a' Ha]].
    + destruct (canon_nat a Hva HT1) as [m ->].
      destruct IHHT2 as [Hvb | [b' Hb]].
      * destruct (canon_nat b Hvb HT2) as [n ->]; eauto using S_AddNum.
      * eauto using S_Add2, v_num.
    + eauto using S_Add1.
  - (* tif c t e *)
    right; destruct IHHT1 as [Hvc | [c' Hc]].
    + destruct (canon_bool c Hvc HT1) as [[] ->]; eauto using S_IfTrue, S_IfFalse.
    + eauto using S_If.
Qed.

(* ── preservation ──────────────────────────────────────────────────────── *)

Theorem preservation : forall t t' T, has_type t T -> step t t' -> has_type t' T.
Proof.
  intros t t' T HT Hstep; revert T HT.
  induction Hstep; intros T HT; inversion HT; subst.
  - apply T_Add; auto.
  - apply T_Add; auto.
  - constructor.
  - apply T_If; auto.
  - assumption.
  - assumption.
Qed.

(* ── discharge the stated obligations (ctx instantiated to unit) ────────── *)

Definition P2_progress_discharged
  : P2_progress tm ty unit tt (fun (_ : unit) (t : tm) (T : ty) => has_type t T)
                value step
  := progress.

Definition P2_preservation_discharged
  : P2_preservation tm ty unit (fun (_ : unit) (t : tm) (T : ty) => has_type t T)
                    step
  := fun (_ : unit) t t' T HT Hs => preservation t t' T HT Hs.

Print Assumptions P2_progress_discharged.
Print Assumptions P2_preservation_discharged.
