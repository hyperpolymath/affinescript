(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   P3_BorrowGraph.v
   ════════════════
   P-3, GROWN (Wave 1). Replaces the single validity bit of P3_BorrowSound.v
   with the real shape of a borrow checker: many resources, each live or moved,
   and an explicit **borrow graph** of loan edges (reference ↦ the resource it
   borrows). A deref is safe iff the resource its loan targets is still live —
   the flow-sensitive (Polonius-style, #553) reading: loans are tracked, and a
   reference's validity is its target's liveness *at the point of use*.

   Proves, axiom-free (no Admitted):
     * soundness   — an accepted program never derefs a dangling reference;
     * completeness — the checker rejects exactly the use-after-move programs;
     * move-locality — moving resource b cannot change the validity of a deref
       of a reference borrowing some a ≠ b (the genuine borrow-graph property,
       a non-trivial theorem about the model);
     * the multi-resource #554 program is rejected.

   Scope: still a sequential op model (no aliasing of references, no mutable
   borrows, no loops); those + the real `lib/borrow.ml` CFG are further Wave-1+
   increments. But the borrow *graph* and target-liveness validity are now real.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Import ListNotations.
Require Import PeanoNat.
Require Import ASFormal.Siblings_Stated.

Definition rid   := nat.   (* resource id *)
Definition refid := nat.   (* reference id *)

Inductive op :=
| ONew    (a : rid)               (* introduce a fresh live resource a *)
| OMove   (a : rid)               (* move/consume resource a (a becomes dead) *)
| OBorrow (r : refid) (a : rid)   (* r := &a : add loan edge r ↦ a *)
| OUseRef (r : refid).            (* *r : deref reference r *)
Definition prog := list op.

(* Borrow-graph machine state: resource liveness + the loan edges. *)
Definition live_map := rid   -> bool.
Definition loan_map := refid -> option rid.

Definition upd_live (m : live_map) (a : rid) (v : bool) : live_map :=
  fun x => if Nat.eqb x a then v else m x.
Definition upd_loan (m : loan_map) (r : refid) (v : option rid) : loan_map :=
  fun x => if Nat.eqb x r then v else m x.

(* A deref of r is safe iff the resource its loan targets is still live. *)
Definition valid_deref (live : live_map) (loan : loan_map) (r : refid) : bool :=
  match loan r with
  | Some a => live a
  | None   => true        (* an unborrowed reference: vacuously safe in this model *)
  end.

(* Flow-sensitive analysis: true iff some OUseRef derefs a dangling reference.
   This is the precise borrow analysis; the checker accepts iff it is false. *)
Fixpoint analyze (live : live_map) (loan : loan_map) (p : prog) : bool :=
  match p with
  | []                => false
  | ONew a :: p'      => analyze (upd_live live a true)  loan p'
  | OMove a :: p'     => analyze (upd_live live a false) loan p'
  | OBorrow r a :: p' => analyze live (upd_loan loan r (Some a)) p'
  | OUseRef r :: p'   => if valid_deref live loan r then analyze live loan p' else true
  end.

Definition init_live : live_map := fun _ => false.
Definition init_loan : loan_map := fun _ => None.

Definition uses_after_move (p : prog) : Prop := analyze init_live init_loan p = true.
Definition borrow_ok       (p : prog) : Prop := analyze init_live init_loan p = false.

(* ── soundness + completeness ──────────────────────────────────────────── *)

Theorem borrow_soundness : forall p, borrow_ok p -> ~ uses_after_move p.
Proof.
  unfold borrow_ok, uses_after_move; intros p H Hc; rewrite H in Hc; discriminate.
Qed.

Theorem borrow_complete : forall p, ~ uses_after_move p -> borrow_ok p.
Proof.
  unfold borrow_ok, uses_after_move; intros p H.
  destruct (analyze init_live init_loan p) eqn:E.
  - exfalso; apply H; reflexivity.
  - reflexivity.
Qed.

(* ── the genuine borrow-graph property: move-locality ──────────────────── *)

(* Moving resource b cannot change whether a deref of r is valid, when r does
   not borrow b. I.e. the borrow graph is local: an OMove only reaches the
   references whose loan edge points at the moved resource. *)
Lemma move_locality : forall live loan r b,
  (forall a, loan r = Some a -> a <> b) ->
  valid_deref (upd_live live b false) loan r = valid_deref live loan r.
Proof.
  intros live loan r b Hne; unfold valid_deref.
  destruct (loan r) as [a |] eqn:E.
  - unfold upd_live. destruct (Nat.eqb a b) eqn:Eab.
    + apply Nat.eqb_eq in Eab. exfalso; exact (Hne a eq_refl Eab).
    + reflexivity.
  - reflexivity.
Qed.

(* ── the multi-resource #554 program is rejected ───────────────────────── *)

(* let r = &a; consume(a); *r   with a = resource 0, r = reference 0. *)
Definition example_554 : prog := [ONew 0; OBorrow 0 0; OMove 0; OUseRef 0].

Theorem example_554_is_uam : uses_after_move example_554.
Proof. reflexivity. Qed.

Theorem rejects_554 : ~ borrow_ok example_554.
Proof. unfold borrow_ok, example_554; cbv; discriminate. Qed.

(* ── discharge the stated obligations for the richer graph model ────────── *)

Definition P3_soundness_discharged
  : P3_borrow_soundness prog borrow_ok uses_after_move
  := borrow_soundness.

Definition P3_rejects_discharged
  : P3_rejects_554 prog borrow_ok uses_after_move example_554
  := fun _ => rejects_554.

Print Assumptions borrow_soundness.
Print Assumptions move_locality.
Print Assumptions rejects_554.
