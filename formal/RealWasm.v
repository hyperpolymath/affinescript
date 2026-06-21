(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealWasm.v
   ══════════
   The REAL-LIFT target IR (see formal/REAL-LIFT.adoc). Grows across rungs.

   R0 — pure i32 numeric stack core (real lib/wasm.ml instr/value_type names),
        stack-machine wexec, arity checker wcheck, soundness wexec_sound.
   R1 — adds **locals** (LocalGet/LocalSet) and the comparison ops I32Eq/I32LtS,
        threading a mutable locals store through wexec. wexec is refactored
        through a per-instruction `step1`, so the append lemma `wexec_app`
        (the sequencing lemma RealCompile.v's preservation proof composes with)
        is one line. wexec_sound is retained for the **pure** (`no_local`)
        fragment: arity-checked local-free code never traps and never touches
        the locals.

   i32 ≔ Z (wrap deferred to rung R-wrap). Axiom-free, no Admitted.
   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Require Import ZArith.
Require Import PeanoNat.
Require Import Lia.
Import ListNotations.

(* lib/wasm.ml `value_type` — full, faithful. *)
Inductive value_type := I32 | I64 | F32 | F64.

(* R0 i32 core + R1 (comparisons + locals), real lib/wasm.ml names. *)
Inductive instr :=
| I32Const (z : Z)
| I32Add | I32Sub | I32Mul
| I32And | I32Or  | I32Xor
| I32Eq  | I32LtS                (* R1: comparisons *)
| I32Eqz
| Drop
| LocalGet (i : nat)             (* R1: locals *)
| LocalSet (i : nat).

Definition stack  := list Z.
Definition locals := list Z.

(* set the i-th local (out of range ⇒ unchanged; callers stay in range). *)
Fixpoint set_nth (i : nat) (v : Z) (l : locals) : locals :=
  match i, l with
  | O,    _ :: r => v :: r
  | S i', x :: r => x :: set_nth i' v r
  | _,    []     => []
  end.

Lemma set_nth_length : forall i v l, length (set_nth i v l) = length l.
Proof. induction i; intros v l; destruct l; simpl; auto. Qed.

Lemma set_nth_eq : forall i v l, i < length l -> nth_error (set_nth i v l) i = Some v.
Proof.
  induction i; intros v l Hlt; destruct l; simpl in *; try lia.
  - reflexivity.
  - apply IHi; lia.
Qed.

Lemma set_nth_neq : forall i j v l, i <> j -> nth_error (set_nth j v l) i = nth_error l i.
Proof.
  induction i; intros j v l Hne; destruct j; destruct l; simpl; try reflexivity.
  - contradiction.
  - apply IHi; lia.
Qed.

(* one-instruction step over (locals, stack). None = trap (underflow / oob). *)
Definition step1 (i : instr) (lo : locals) (st : stack) : option (locals * stack) :=
  match i with
  | I32Const z => Some (lo, z :: st)
  | I32Add => match st with a :: b :: t => Some (lo, Z.add  b a :: t) | _ => None end
  | I32Sub => match st with a :: b :: t => Some (lo, Z.sub  b a :: t) | _ => None end
  | I32Mul => match st with a :: b :: t => Some (lo, Z.mul  b a :: t) | _ => None end
  | I32And => match st with a :: b :: t => Some (lo, Z.land b a :: t) | _ => None end
  | I32Or  => match st with a :: b :: t => Some (lo, Z.lor  b a :: t) | _ => None end
  | I32Xor => match st with a :: b :: t => Some (lo, Z.lxor b a :: t) | _ => None end
  | I32Eq  => match st with a :: b :: t => Some (lo, (if Z.eqb b a then 1 else 0)%Z :: t) | _ => None end
  | I32LtS => match st with a :: b :: t => Some (lo, (if Z.ltb b a then 1 else 0)%Z :: t) | _ => None end
  | I32Eqz => match st with a :: t      => Some (lo, (if Z.eqb a 0 then 1 else 0)%Z :: t) | _ => None end
  | Drop   => match st with _ :: t      => Some (lo, t) | _ => None end
  | LocalGet k => match nth_error lo k with Some v => Some (lo, v :: st) | None => None end
  | LocalSet k => match st with
                  | v :: t => if Nat.ltb k (length lo) then Some (set_nth k v lo, t) else None
                  | [] => None
                  end
  end.

Fixpoint wexec (is : list instr) (lo : locals) (st : stack) : option (locals * stack) :=
  match is with
  | [] => Some (lo, st)
  | i :: r => match step1 i lo st with
              | Some (lo', st') => wexec r lo' st'
              | None => None
              end
  end.

(* sequencing — the lemma the preservation proof composes with. *)
Lemma wexec_app : forall is1 is2 lo st,
  wexec (is1 ++ is2) lo st =
  match wexec is1 lo st with Some (lo', st') => wexec is2 lo' st' | None => None end.
Proof.
  induction is1 as [| i r IH]; intros is2 lo st; simpl.
  - reflexivity.
  - destruct (step1 i lo st) as [[lo' st']|]; [apply IH | reflexivity].
Qed.

(* the run-then-run composition, in the shape the preservation proof applies. *)
Lemma wexec_seq : forall is1 is2 lo st lo1 st1 lo2 st2,
  wexec is1 lo st = Some (lo1, st1) ->
  wexec is2 lo1 st1 = Some (lo2, st2) ->
  wexec (is1 ++ is2) lo st = Some (lo2, st2).
Proof. intros. rewrite wexec_app, H. exact H0. Qed.

(* ── arity discipline (degenerate validation: i32 stack height) ──────────── *)
Definition consumes (i : instr) : nat :=
  match i with
  | I32Const _ => 0
  | LocalGet _ => 0
  | I32Add | I32Sub | I32Mul | I32And | I32Or | I32Xor | I32Eq | I32LtS => 2
  | I32Eqz => 1
  | Drop => 1
  | LocalSet _ => 1
  end.

Definition produces (i : instr) : nat :=
  match i with
  | Drop => 0
  | LocalSet _ => 0
  | _ => 1
  end.

Fixpoint wcheck (is : list instr) (n : nat) : option nat :=
  match is with
  | [] => Some n
  | i :: r =>
      if Nat.leb (consumes i) n
      then wcheck r (n - consumes i + produces i)
      else None
  end.

(* the pure (local-free) fragment. *)
Fixpoint no_local (is : list instr) : Prop :=
  match is with
  | [] => True
  | LocalGet _ :: _ => False
  | LocalSet _ :: _ => False
  | _ :: r => no_local r
  end.

(* ── soundness: arity-checked, local-free code never traps ─────────────────
   (and never touches the locals). The target invariant for the pure core; the
   locals-using case is covered operationally + by RealCompile.v's preservation. *)
Theorem wexec_sound : forall is n m lo st,
  wcheck is n = Some m -> length st = n -> no_local is ->
  exists st', wexec is lo st = Some (lo, st') /\ length st' = m.
Proof.
  induction is as [| i r IH]; intros n m lo st Hchk Hlen Hnl; cbn in Hchk.
  - injection Hchk as <-. exists st. split; [reflexivity | exact Hlen].
  - destruct (Nat.leb (consumes i) n) eqn:Hle; [| discriminate].
    apply Nat.leb_le in Hle.
    destruct i; cbn in Hchk, Hle, Hnl |- *;
      try contradiction;
      try (eapply IH; [exact Hchk | cbn; lia | exact Hnl]).
    all: destruct st as [| a [| b t]]; cbn in Hlen |- *;
         try (exfalso; lia);
         (eapply IH; [exact Hchk | cbn in Hlen |- *; lia | exact Hnl]).
Qed.

(* concrete sanity checks. *)
Example wexec_demo :
  wexec [I32Const 2; I32Const 3; I32Add; I32Const 4; I32Mul] [] [] = Some ([], [20%Z]).
Proof. reflexivity. Qed.

Example wexec_local_demo :
  wexec [I32Const 7; LocalSet 0; LocalGet 0] [0%Z] [] = Some ([7%Z], [7%Z]).
Proof. reflexivity. Qed.

Example wcheck_demo :
  wcheck [I32Const 2; I32Const 3; I32Add; I32Const 4; I32Mul] 0 = Some 1.
Proof. reflexivity. Qed.

Print Assumptions wexec_sound.
