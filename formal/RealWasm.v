(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealWasm.v
   ══════════
   The REAL-LIFT foundation stone (see formal/REAL-LIFT.adoc).

   The toy K-1 (K1_CodegenPreservation.v) compiles to an ad-hoc stack machine
   invented for the proof. The real lift re-targets the *actual* compiler IR —
   lib/wasm.ml's `instr` / `value_type` / `wasm_module`. This file begins that by
   encoding the **R0 fragment**: the pure i32 numeric stack core, with the real
   constructor names from lib/wasm.ml, a stack-machine evaluator `wexec`, a
   stack-arity checker `wcheck`, and a soundness theorem — *arity-checked code
   never gets stuck* (`wexec_sound`). This is the target-side invariant the real
   K-1 preservation proof will rest on (a well-typed wasm body cannot trap on the
   covered fragment).

   Scope of R0 (deliberately the smallest real slice; the ladder in
   REAL-LIFT.adoc adds the rest):
   * i32 only; i32 is modelled as `Z` — **wrap-around (mod 2^32) is deferred**
     (milestone R-wrap). The constructor names and stack discipline are exact.
   * No locals / memory / control / i64 / f64 yet (R1/R2/R-float milestones).

   `value_type` is given in full (faithful to lib/wasm.ml) even though R0's
   evaluator only inhabits I32 — so later milestones extend, not rewrite, it.

   Axiom-free, no Admitted. `.v` is Coq, not V-lang — see formal/README.adoc and
   .hypatia-ignore.
*)

Require Import List.
Require Import ZArith.
Require Import PeanoNat.
Require Import Lia.
Import ListNotations.

(* lib/wasm.ml `value_type` — full, faithful. *)
Inductive value_type := I32 | I64 | F32 | F64.

(* R0 slice of lib/wasm.ml `instr`: the pure i32 numeric stack core.
   Real names; i32 ≔ Z (wrap deferred). *)
Inductive instr :=
| I32Const (z : Z)
| I32Add | I32Sub | I32Mul
| I32And | I32Or  | I32Xor
| I32Eqz
| Drop.

Definition stack := list Z.

(* Big-step stack-machine evaluator. None = stuck (stack underflow). *)
Fixpoint wexec (is : list instr) (st : stack) : option stack :=
  match is with
  | [] => Some st
  | I32Const z :: r => wexec r (z :: st)
  | I32Add :: r => match st with a :: b :: t => wexec r (Z.add b a :: t) | _ => None end
  | I32Sub :: r => match st with a :: b :: t => wexec r (Z.sub b a :: t) | _ => None end
  | I32Mul :: r => match st with a :: b :: t => wexec r (Z.mul b a :: t) | _ => None end
  | I32And :: r => match st with a :: b :: t => wexec r (Z.land b a :: t) | _ => None end
  | I32Or  :: r => match st with a :: b :: t => wexec r (Z.lor  b a :: t) | _ => None end
  | I32Xor :: r => match st with a :: b :: t => wexec r (Z.lxor b a :: t) | _ => None end
  | I32Eqz :: r => match st with a :: t => wexec r ((if Z.eqb a 0 then 1%Z else 0%Z) :: t) | _ => None end
  | Drop   :: r => match st with _ :: t => wexec r t | _ => None end
  end.

(* Stack-arity discipline (a degenerate wasm validation: track the i32 count). *)
Definition consumes (i : instr) : nat :=
  match i with
  | I32Const _ => 0
  | I32Add | I32Sub | I32Mul | I32And | I32Or | I32Xor => 2
  | I32Eqz => 1
  | Drop => 1
  end.

Definition produces (i : instr) : nat :=
  match i with
  | Drop => 0
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

(* ── soundness: arity-checked code never gets stuck ───────────────────────
   If the body checks from height n to height m, then on any stack of height n
   it runs to Some stack of height m. The target invariant the real K-1 needs. *)
Theorem wexec_sound : forall is n m st,
  wcheck is n = Some m -> length st = n ->
  exists st', wexec is st = Some st' /\ length st' = m.
Proof.
  induction is as [| i r IH]; intros n m st Hchk Hlen; cbn in Hchk.
  - injection Hchk as <-. exists st. split; [reflexivity | exact Hlen].
  - destruct (Nat.leb (consumes i) n) eqn:Hle; [| discriminate].
    apply Nat.leb_le in Hle.
    destruct i; cbn in Hchk, Hle |- *.
    + (* I32Const *) eapply IH; [exact Hchk |]. cbn. lia.
    + (* I32Add *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32Sub *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32Mul *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32And *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32Or *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32Xor *) destruct st as [| a [| b t]]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* I32Eqz *) destruct st as [| a t]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
    + (* Drop *) destruct st as [| a t]; cbn in Hlen; try (exfalso; lia).
      eapply IH; [exact Hchk |]. cbn in Hlen |- *; lia.
Qed.

(* A concrete sanity check: (2 + 3) * 4 evaluates to [20], and it arity-checks. *)
Example wexec_demo :
  wexec [I32Const 2; I32Const 3; I32Add; I32Const 4; I32Mul] [] = Some [20%Z].
Proof. reflexivity. Qed.

Example wcheck_demo :
  wcheck [I32Const 2; I32Const 3; I32Add; I32Const 4; I32Mul] 0 = Some 1.
Proof. reflexivity. Qed.

Print Assumptions wexec_sound.
