(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealCompile.v
   ═════════════
   REAL-LIFT rung R1 (see formal/REAL-LIFT.adoc): the first **real**
   ⟦compile p⟧ = ⟦p⟧, on the real RealWasm.v target IR.

   Source: the resolved (de Bruijn LEVEL) core of lib/ast.ml — integer/bool
   literals (ExprLit), variables (ExprVar), let (ExprLet), and binary operators
   (ExprBinary: + - * & | == <). de Bruijn levels are how lib/codegen.ml sees the
   AST after lib/resolve.ml — name resolution itself is obligation P-7. Bool ≔
   0/1, Int ≔ Z, so the single observable is Z.

   `eval` (mirrors lib/interp.ml) is the reference dynamic semantics. `compile`
   (mirrors lib/codegen.ml) lowers `let` to `LocalSet d` into a pre-sized locals
   array (slot = binding depth d; siblings reuse slots). The theorem
   `compile_correct`:

       eval env e = Some v  →  agree env locals  →  d = |env|  →  enough slots →
         wexec (compile d e) locals st = Some (locals', v :: st)
         ∧ |locals'| = |locals| ∧ (low slots < d unchanged)

   and the closed-program corollary `compile_program_correct` runs it from the
   zero-initialised locals array. This RETIRES the toy K1 / K1Let on REAL objects
   — the target is RealWasm's actual lib/wasm.ml instruction names with mutable
   locals, not an ad-hoc machine. Axiom-free, no Admitted.

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Require Import ZArith.
Require Import PeanoNat.
Require Import Lia.
Require Import ASFormal.RealWasm.
Import ListNotations.

(* ── source: the resolved R1 core of lib/ast.ml ───────────────────────────── *)
Inductive lit := LInt (z : Z) | LBool (b : bool).
Inductive bop := BAdd | BSub | BMul | BAnd | BOr | BEq | BLt.
Inductive rexpr :=
| RLit (l : lit)
| RVar (i : nat)              (* de Bruijn LEVEL: outermost binder = 0 *)
| RLet (e1 e2 : rexpr)        (* let _ = e1 in e2 *)
| RBin (b : bop) (e1 e2 : rexpr).

Definition lit_val (l : lit) : Z :=
  match l with LInt z => z | LBool b => if b then 1 else 0 end.

Definition bop_val (b : bop) (a c : Z) : Z :=
  match b with
  | BAdd => Z.add a c | BSub => Z.sub a c | BMul => Z.mul a c
  | BAnd => Z.land a c | BOr => Z.lor a c
  | BEq  => if Z.eqb a c then 1 else 0
  | BLt  => if Z.ltb a c then 1 else 0
  end.

(* ── reference semantics (mirrors lib/interp.ml): env = level-indexed list ── *)
Fixpoint eval (env : list Z) (e : rexpr) : option Z :=
  match e with
  | RLit l => Some (lit_val l)
  | RVar i => nth_error env i
  | RLet e1 e2 =>
      match eval env e1 with
      | Some v1 => eval (env ++ [v1]) e2
      | None => None
      end
  | RBin b e1 e2 =>
      match eval env e1, eval env e2 with
      | Some a, Some c => Some (bop_val b a c)
      | _, _ => None
      end
  end.

(* ── compiler (mirrors lib/codegen.ml): d = binding depth = next free slot ── *)
Definition bop_instr (b : bop) : instr :=
  match b with
  | BAdd => I32Add | BSub => I32Sub | BMul => I32Mul
  | BAnd => I32And | BOr => I32Or
  | BEq  => I32Eq  | BLt => I32LtS
  end.

Fixpoint compile (d : nat) (e : rexpr) : list instr :=
  match e with
  | RLit l => [I32Const (lit_val l)]
  | RVar i => [LocalGet i]
  | RLet e1 e2 => compile d e1 ++ [LocalSet d] ++ compile (S d) e2
  | RBin b e1 e2 => compile d e1 ++ compile d e2 ++ [bop_instr b]
  end.

(* max extra local slots e needs above its starting depth. *)
Fixpoint depth (e : rexpr) : nat :=
  match e with
  | RLit _ | RVar _ => 0
  | RLet e1 e2 => Nat.max (depth e1) (S (depth e2))
  | RBin _ e1 e2 => Nat.max (depth e1) (depth e2)
  end.

(* the binop instruction realises bop_val on the (reversed) operand stack. *)
Lemma step1_bop : forall b lo v1 v2 t,
  step1 (bop_instr b) lo (v2 :: v1 :: t) = Some (lo, bop_val b v1 v2 :: t).
Proof. destruct b; reflexivity. Qed.

(* ── agreement: locals' first |env| slots hold env ────────────────────────── *)
Definition agree (env locals : list Z) : Prop :=
  forall i, i < length env -> nth_error locals i = nth_error env i.

(* ── the preservation theorem ─────────────────────────────────────────────── *)
Lemma compile_correct : forall e env d locals st v,
  eval env e = Some v ->
  length env = d ->
  agree env locals ->
  d + depth e <= length locals ->
  exists locals',
    wexec (compile d e) locals st = Some (locals', v :: st) /\
    length locals' = length locals /\
    (forall i, i < d -> nth_error locals' i = nth_error locals i).
Proof.
  induction e as [ l | i | e1 IH1 e2 IH2 | b e1 IH1 e2 IH2 ];
    intros env d locals st v Heval Hlen Hagree Hbound; cbn [depth] in Hbound.
  - (* RLit *)
    cbn in Heval. injection Heval as Hv; subst v.
    exists locals. cbn [compile wexec step1].
    split; [reflexivity | split; [reflexivity | intros; reflexivity]].
  - (* RVar i *)
    cbn in Heval.
    assert (Hi : i < length env) by (apply nth_error_Some; rewrite Heval; discriminate).
    exists locals. cbn [compile wexec step1].
    rewrite (Hagree i Hi), Heval. cbn.
    split; [reflexivity | split; [reflexivity | intros; reflexivity]].
  - (* RLet e1 e2 *)
    cbn in Heval.
    destruct (eval env e1) as [v1|] eqn:Hev1; cbn in Heval; [| discriminate].
    destruct (IH1 env d locals st v1 Hev1 Hlen Hagree ltac:(lia))
      as [locals1 [Hw1 [Hlen1 Hlow1]]].
    assert (Hd : d < length locals1) by lia.
    assert (Hag2 : agree (env ++ [v1]) (set_nth d v1 locals1)).
    { intros j Hj. rewrite app_length in Hj; cbn in Hj.
      destruct (Nat.eq_dec j d) as [->|Hjd].
      - rewrite set_nth_eq by lia.
        rewrite nth_error_app2 by lia. rewrite Hlen, Nat.sub_diag. reflexivity.
      - rewrite set_nth_neq by lia. rewrite Hlow1 by lia.
        rewrite (Hagree j) by lia. rewrite nth_error_app1 by lia. reflexivity. }
    destruct (IH2 (env ++ [v1]) (S d) (set_nth d v1 locals1) st v Heval
                  ltac:(rewrite app_length; cbn; lia) Hag2
                  ltac:(rewrite set_nth_length; lia))
      as [locals2 [Hw2 [Hlen2 Hlow2]]].
    exists locals2. split; [| split].
    + apply wexec_seq with (lo1:=locals1) (st1:=v1::st); [exact Hw1|].
      apply wexec_seq with (lo1:=set_nth d v1 locals1) (st1:=st); [| exact Hw2].
      cbn [wexec step1].
      assert (Hltb : Nat.ltb d (length locals1) = true) by (apply Nat.ltb_lt; lia).
      rewrite Hltb. reflexivity.
    + rewrite Hlen2, set_nth_length. exact Hlen1.
    + intros j Hj. rewrite Hlow2 by lia. rewrite set_nth_neq by lia. apply Hlow1; lia.
  - (* RBin b e1 e2 *)
    cbn in Heval.
    destruct (eval env e1) as [v1|] eqn:Hev1; cbn in Heval; [| discriminate].
    destruct (eval env e2) as [v2|] eqn:Hev2; cbn in Heval; [| discriminate].
    injection Heval as Hv; subst v.
    destruct (IH1 env d locals st v1 Hev1 Hlen Hagree ltac:(lia))
      as [locals1 [Hw1 [Hlen1 Hlow1]]].
    assert (Hag1 : agree env locals1).
    { intros j Hj. rewrite Hlow1 by lia. apply Hagree; lia. }
    destruct (IH2 env d locals1 (v1 :: st) v2 Hev2 Hlen Hag1
                  ltac:(rewrite Hlen1; lia))
      as [locals2 [Hw2 [Hlen2 Hlow2]]].
    exists locals2. split; [| split].
    + apply wexec_seq with (lo1:=locals1) (st1:=v1::st); [exact Hw1|].
      apply wexec_seq with (lo1:=locals2) (st1:=v2::v1::st); [exact Hw2|].
      cbn [wexec]. rewrite step1_bop. reflexivity.
    + rewrite Hlen2. exact Hlen1.
    + intros j Hj. rewrite Hlow2 by lia. apply Hlow1; lia.
Qed.

(* ── closed-program corollary ─────────────────────────────────────────────── *)
Corollary compile_program_correct : forall e v,
  eval [] e = Some v ->
  exists locals',
    wexec (compile 0 e) (repeat 0%Z (depth e)) [] = Some (locals', [v]).
Proof.
  intros e v Heval.
  destruct (compile_correct e [] 0 (repeat 0%Z (depth e)) [] v Heval)
    as [locals' [Hw _]].
  - reflexivity.
  - intros i Hi; cbn in Hi; lia.
  - rewrite repeat_length; lia.
  - exists locals'; exact Hw.
Qed.

(* concrete: let x = 2+3 in x*4  ⇒  20, end-to-end. *)
Example r1_eval_demo :
  eval [] (RLet (RBin BAdd (RLit (LInt 2)) (RLit (LInt 3)))
                (RBin BMul (RVar 0) (RLit (LInt 4)))) = Some 20%Z.
Proof. reflexivity. Qed.

Example r1_exec_demo :
  wexec (compile 0 (RLet (RBin BAdd (RLit (LInt 2)) (RLit (LInt 3)))
                         (RBin BMul (RVar 0) (RLit (LInt 4))))) [0%Z] []
  = Some ([5%Z], [20%Z]).
Proof. reflexivity. Qed.

Print Assumptions compile_correct.
Print Assumptions compile_program_correct.
