(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealWasm.v
   ══════════
   The REAL-LIFT target IR (see formal/REAL-LIFT.adoc). Grows across rungs.

   R0 — pure i32 numeric stack core (real lib/wasm.ml instr/value_type names).
   R1 — adds **locals** (LocalGet/LocalSet) + comparison ops I32Eq/I32LtS.
   R2 — adds **structured control**: `IfElse thn els` (lib/wasm.ml's `If`, minus
        the block_type validation annotation). Because `instr` now nests
        `list instr`, a *structural* `wexec` is rejected by Coq's guard checker —
        so `wexec` becomes **fuel-indexed** (decreasing on the `nat` fuel), as
        REAL-LIFT.adoc anticipated for R2. Fuel keeps definitional computation
        (`cbn`/`reflexivity` still work), at the cost of the old one-line
        `wexec_seq`: composition is recovered via monotonicity (`wexec_mono`:
        more fuel preserves a `Some` result) + the additive sequencing lemma
        `wexec_app_some`. (Backward jumps — `Loop`/`Br` — are the next sub-rung.)

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

(* R0 i32 core + R1 (comparisons + locals) + R2 (structured control). *)
Inductive instr :=
| I32Const (z : Z)
| I32Add | I32Sub | I32Mul
| I32And | I32Or  | I32Xor
| I32Eq  | I32LtS                (* R1: comparisons *)
| I32Eqz
| Drop
| LocalGet (i : nat)             (* R1: locals *)
| LocalSet (i : nat)
| IfElse (thn els : list instr). (* R2: lib/wasm.ml `If` (sans block_type) *)

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

(* one straight-line instruction step. IfElse is handled by wexec (it recurses
   into a sub-list), so step1 never sees it — the dead arm returns None. *)
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
  | IfElse _ _ => None
  end.

(* fuel-indexed executor. None = trap OR out of fuel. *)
Fixpoint wexec (fuel : nat) (is : list instr) (lo : locals) (st : stack)
  : option (locals * stack) :=
  match fuel with
  | O => None
  | S f =>
    match is with
    | [] => Some (lo, st)
    | IfElse thn els :: r =>
        match st with
        | c :: t =>
            match wexec f (if Z.eqb c 0 then els else thn) lo t with
            | Some (lo', st') => wexec f r lo' st'
            | None => None
            end
        | [] => None
        end
    | i :: r =>
        match step1 i lo st with
        | Some (lo', st') => wexec f r lo' st'
        | None => None
        end
    end
  end.

(* one-step unfolding (avoids cbn over-reducing the IfElse branch run). *)
Lemma wexec_S_cons : forall f i r lo st,
  wexec (S f) (i :: r) lo st =
  match i with
  | IfElse thn els =>
      match st with
      | c :: t =>
          match wexec f (if Z.eqb c 0 then els else thn) lo t with
          | Some (lo', st') => wexec f r lo' st' | None => None end
      | [] => None end
  | _ => match step1 i lo st with Some (lo', st') => wexec f r lo' st' | None => None end
  end.
Proof. intros; destruct i; reflexivity. Qed.

(* one-step unfolding of a single structured conditional. *)
Lemma wexec_ifelse : forall f c tcode ecode lo st,
  wexec (S f) [IfElse tcode ecode] lo (c :: st) =
  match wexec f (if Z.eqb c 0 then ecode else tcode) lo st with
  | Some (lo', st') => wexec f [] lo' st' | None => None end.
Proof. reflexivity. Qed.

(* ── monotonicity: more fuel never loses a Some result ────────────────────── *)
Lemma wexec_le_S : forall f is lo st r,
  wexec f is lo st = Some r -> wexec (S f) is lo st = Some r.
Proof.
  induction f as [| f IHf]; intros is lo st r Hw; [discriminate Hw|].
  destruct is as [| i r0]; [exact Hw|].
  rewrite wexec_S_cons in Hw; rewrite wexec_S_cons; destruct i;
    try (destruct (step1 _ lo st) as [[lo' st']|];
         [ apply IHf; exact Hw | discriminate Hw ]).
  (* IfElse *)
  destruct st as [| c t]; [discriminate Hw|].
  destruct (wexec f (if Z.eqb c 0 then els else thn) lo t) as [[lo' st']|] eqn:Hb;
    [| discriminate Hw].
  apply IHf in Hb; rewrite Hb; apply IHf; exact Hw.
Qed.

Lemma wexec_mono : forall f f' is lo st r,
  f <= f' -> wexec f is lo st = Some r -> wexec f' is lo st = Some r.
Proof.
  intros f f' is lo st r Hle; induction Hle; intros Hw.
  - exact Hw.
  - apply wexec_le_S, IHHle, Hw.
Qed.

(* ── additive sequencing: run is1 with f1, then is2 with f2 ⇒ is1++is2 with
   f1+f2. The fuel-world replacement for R1's wexec_seq. ─────────────────────── *)
Lemma wexec_app_some : forall f1 is1 lo st lo1 st1,
  wexec f1 is1 lo st = Some (lo1, st1) ->
  forall f2 is2 lo2 st2,
  wexec f2 is2 lo1 st1 = Some (lo2, st2) ->
  wexec (f1 + f2) (is1 ++ is2) lo st = Some (lo2, st2).
Proof.
  induction f1 as [| f1 IHf1];
    intros is1 lo st lo1 st1 Hw1 f2 is2 lo2 st2 Hw2; [discriminate Hw1|].
  destruct is1 as [| i r0].
  - cbn [wexec] in Hw1. injection Hw1 as Hlo Hst; subst lo1 st1.
    cbn [app]. apply wexec_mono with (f := f2); [lia | exact Hw2].
  - rewrite wexec_S_cons in Hw1.
    cbn [app]; rewrite Nat.add_succ_l, wexec_S_cons; destruct i;
      try (destruct (step1 _ lo st) as [[lo' st']|];
           [ exact (IHf1 r0 lo' st' lo1 st1 Hw1 f2 is2 lo2 st2 Hw2)
           | discriminate Hw1 ]).
    (* IfElse *)
    destruct st as [| c t]; [discriminate Hw1|].
    destruct (wexec f1 (if Z.eqb c 0 then els else thn) lo t) as [[lo' st']|] eqn:Hb;
      [| discriminate Hw1].
    rewrite (wexec_mono f1 (f1 + f2) _ lo t _ ltac:(lia) Hb).
    exact (IHf1 r0 lo' st' lo1 st1 Hw1 f2 is2 lo2 st2 Hw2).
Qed.

(* concrete sanity checks (generous fuel; monotonic). *)
Example wexec_demo :
  wexec 10 [I32Const 2; I32Const 3; I32Add; I32Const 4; I32Mul] [] [] = Some ([], [20%Z]).
Proof. reflexivity. Qed.

Example wexec_local_demo :
  wexec 10 [I32Const 7; LocalSet 0; LocalGet 0] [0%Z] [] = Some ([7%Z], [7%Z]).
Proof. reflexivity. Qed.

(* if 0 then 11 else 22  ⇒  22  (condition 0 takes the else branch). *)
Example wexec_if_demo :
  wexec 10 [I32Const 0; IfElse [I32Const 11] [I32Const 22]] [] [] = Some ([], [22%Z]).
Proof. reflexivity. Qed.

Print Assumptions wexec_app_some.
