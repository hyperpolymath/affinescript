(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   P3_BorrowSound.v
   ════════════════
   Mechanizes obligation **P-3** (borrow-graph soundness, and the explicit
   "reject #554" obligation) for a concrete model, discharging the statements
   from Siblings_Stated.v — axiom-free, no Admitted.

   A minimal single-resource borrow calculus: a program is a sequence of ops on
   one owned value `a` and one reference `r` borrowed from it. The #554 shape
   `let r = pick(a); consume(a); *r` is `[OBorrow; OMove; OUseRef]`. The dynamic
   semantics tracks whether `r` still aliases a LIVE value; the modelled checker
   is the precise validity analysis (what `lib/borrow.ml` does post-#554-fix —
   the model abstracts the borrow graph as a single validity bit). We prove the
   checker is sound and that it REJECTS the #554 witness.

   Scope: this models the *intended* sound checker on a one-resource fragment;
   the full obligation needs `lib/borrow.ml`'s actual graph + Polonius (#553).

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Import ListNotations.
Require Import ASFormal.Siblings_Stated.

Inductive op := OBorrow | OMove | OUseRef.
Definition prog := list op.

(* Dynamic semantics: `rvalid` = r currently aliases a live value. OBorrow makes
   r valid; OMove consumes a and invalidates r; OUseRef on an invalid r is a
   use-after-move. Returns true iff some OUseRef observes a moved value. *)
Fixpoint runs_uam (rvalid : bool) (p : prog) : bool :=
  match p with
  | []            => false
  | OBorrow :: p' => runs_uam true p'
  | OMove   :: p' => runs_uam false p'   (* moving a invalidates the borrow r *)
  | OUseRef :: p' => if rvalid then runs_uam rvalid p' else true
  end.

Definition uses_after_move (p : prog) : Prop := runs_uam false p = true.

(* The intended sound checker: accept iff the program never uses a moved value. *)
Definition borrow_ok (p : prog) : Prop := runs_uam false p = false.

Definition example_554 : prog := [OBorrow; OMove; OUseRef].

(* Soundness: an accepted program never uses-after-move. *)
Theorem borrow_soundness : forall p, borrow_ok p -> ~ uses_after_move p.
Proof.
  unfold borrow_ok, uses_after_move; intros p H Hc; rewrite H in Hc; discriminate.
Qed.

(* The #554 witness genuinely uses-after-move ... *)
Theorem example_554_is_uam : uses_after_move example_554.
Proof. reflexivity. Qed.

(* ... so the sound checker rejects it (the obligation the fixed checker meets). *)
Theorem rejects_554 : ~ borrow_ok example_554.
Proof. unfold borrow_ok, example_554; simpl; discriminate. Qed.

(* ── discharge the stated obligations ──────────────────────────────────── *)

Definition P3_soundness_discharged
  : P3_borrow_soundness prog borrow_ok uses_after_move
  := borrow_soundness.

Definition P3_rejects_discharged
  : P3_rejects_554 prog borrow_ok uses_after_move example_554
  := fun _ => rejects_554.

Print Assumptions P3_soundness_discharged.
Print Assumptions P3_rejects_discharged.
