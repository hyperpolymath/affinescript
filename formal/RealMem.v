(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealMem.v
   ═════════
   REAL-LIFT rung **R-mem** (see formal/REAL-LIFT.adoc): linear memory.
   Adds the load/store execution for `lib/wasm.ml`'s `I32Load`/`I32Store` (the
   instructions were added to `instr` in RealWasm.v) and a source heap layer —
   `tuple`/`record`-style cells laid out at consecutive addresses — with the
   build-then-project round-trip proved.

   Memory is **word-addressed** (`mem := list Z`, address = cell index): the
   deterministic layout the codegen's elaboration nodes assume, where tuples /
   records / arrays are runs of i32 cells. (Byte-granular `[len][utf8]` layout
   and IEEE lanes are the later R-str / R-float rungs; bit-exact wrap is R-wrap.)

   Memory ops are *straight-line* (no control), so unlike R2's `wexec` / the
   R2-loops `cexec`, the memory executor `mexec` is a **structural** `Fixpoint`
   on the instruction list — no fuel needed. The heap round-trip is then exactly
   `set_nth`'s get-after-set, reused from RealWasm.

   Axiom-free, no Admitted.
   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Require Import ZArith.
Require Import PeanoNat.
Require Import Lia.
Require Import ASFormal.RealWasm.
Require Import ASFormal.RealCompile.
Import ListNotations.

(* ── word-addressed linear memory ─────────────────────────────────────────── *)
Definition mem := list Z.
Definition mem_get (m : mem) (a : nat) : option Z := nth_error m a.
Definition mem_set (a : nat) (v : Z) (m : mem) : mem := set_nth a v m.

Lemma mem_set_length : forall a v m, length (mem_set a v m) = length m.
Proof. intros; apply set_nth_length. Qed.

Lemma mem_get_set_eq : forall a v m, a < length m -> mem_get (mem_set a v m) a = Some v.
Proof. intros; apply set_nth_eq; assumption. Qed.

Lemma mem_get_set_neq : forall a a' v m,
  a <> a' -> mem_get (mem_set a' v m) a = mem_get m a.
Proof. intros; apply set_nth_neq; assumption. Qed.

(* bump allocator: base = current size; reserve n zeroed cells. *)
Definition alloc (n : nat) (m : mem) : nat * mem := (length m, m ++ repeat 0%Z n).

Lemma alloc_base : forall n m, fst (alloc n m) = length m.
Proof. reflexivity. Qed.

Lemma alloc_length : forall n m, length (snd (alloc n m)) = length m + n.
Proof. intros; cbn. rewrite app_length, repeat_length; reflexivity. Qed.

(* ── memory-aware executor (structural on the instruction list) ───────────────
   I32Load off  : pop addr a, push mem[⌊a⌋+off].
   I32Store off : pop value v then addr a, set mem[⌊a⌋+off] := v.
   everything else: the memory-free `step1` (memory unchanged). Control
   instructions (Block/Loop/Br/BrIf/IfElse) trap via step1 = None — combining
   memory with control is the next sub-rung. *)
Fixpoint mexec (is : list instr) (m : mem) (lo : locals) (st : stack)
  : option (mem * locals * stack) :=
  match is with
  | [] => Some (m, lo, st)
  | a :: r =>
    match a with
    | I32Load off =>
        match st with
        | addr :: t =>
            match mem_get m (Z.to_nat addr + off) with
            | Some v => mexec r m lo (v :: t)
            | None => None
            end
        | [] => None
        end
    | I32Store off =>
        match st with
        | v :: addr :: t => mexec r (mem_set (Z.to_nat addr + off) v m) lo t
        | _ => None
        end
    | _ =>
        match step1 a lo st with
        | Some (lo', st') => mexec r m lo' st'
        | None => None
        end
    end
  end.

(* sequencing: straight-line, so a plain structural split (no fuel/monotonicity). *)
Lemma mexec_app : forall is1 m lo st m1 lo1 st1,
  mexec is1 m lo st = Some (m1, lo1, st1) ->
  forall is2,
  mexec (is1 ++ is2) m lo st = mexec is2 m1 lo1 st1.
Proof.
  induction is1 as [| a r IH]; intros m lo st m1 lo1 st1 H is2.
  - cbn in H. injection H as -> -> ->. reflexivity.
  - cbn [app]. destruct a;
      try (cbn [mexec] in H |- *; destruct (step1 _ lo st) as [[lo' st']|];
           [ apply IH; exact H | discriminate H ]).
    + (* I32Load *)
      cbn [mexec] in H |- *. destruct st as [| addr t]; [discriminate H|].
      destruct (mem_get m (Z.to_nat addr + off)) as [v|]; [| discriminate H].
      apply IH; exact H.
    + (* I32Store *)
      cbn [mexec] in H |- *. destruct st as [| v [| addr t]]; try discriminate H.
      apply IH; exact H.
Qed.

(* ── concrete sanity: store then load the same cell round-trips ─────────────── *)
Example mexec_store_load_demo :
  (* mem = [0;0;0]; store 42 at addr 1, then load addr 1 ⇒ 42 *)
  mexec [I32Const 1; I32Const 42; I32Store 0; I32Const 1; I32Load 0]
        [0;0;0]%Z [] []
  = Some (mem_set 1 42 [0;0;0]%Z, [], [42%Z]).
Proof. reflexivity. Qed.

(* the executor-level store→load round-trip: storing v at a fresh in-range cell
   then loading it back yields v (this is `set_nth`'s get-after-set, lifted
   through `mexec`). *)
Lemma mexec_store_then_load : forall m a v,
  a < length m ->
  mexec [I32Const (Z.of_nat a); I32Const v; I32Store 0;
         I32Const (Z.of_nat a); I32Load 0] m [] []
  = Some (mem_set a v m, [], [v]).
Proof.
  intros m a v Ha.
  cbn [mexec step1].
  rewrite Nat.add_0_r, Nat2Z.id.
  rewrite mem_get_set_eq by exact Ha.
  reflexivity.
Qed.

(* ── heap tuples: build a 2-cell tuple at `base`, then project both fields ────
   This is the codegen's deterministic cell layout (field i at base+i) proved as
   a get-after-set round-trip — the precondition for arrays / records / strings.
   (`base` is the address the bump allocator hands out; a runtime `memory.grow`
   that produces it, and field expressions with control, are the next sub-rung.) *)
Theorem mexec_pair_build_proj : forall m base v0 v1,
  base + 1 < length m ->
  exists m',
    (* build: store v0 at base+0, v1 at base+1 *)
    mexec [I32Const (Z.of_nat base); I32Const v0; I32Store 0;
           I32Const (Z.of_nat base); I32Const v1; I32Store 1] m [] []
      = Some (m', [], [])
    (* project field 0 ⇒ v0 *)
    /\ mexec [I32Const (Z.of_nat base); I32Load 0] m' [] [] = Some (m', [], [v0])
    (* project field 1 ⇒ v1 *)
    /\ mexec [I32Const (Z.of_nat base); I32Load 1] m' [] [] = Some (m', [], [v1]).
Proof.
  intros m base v0 v1 Hb.
  exists (mem_set (base + 1) v1 (mem_set base v0 m)). split; [| split].
  - cbn [mexec step1]. rewrite !Nat2Z.id, Nat.add_0_r. reflexivity.
  - cbn [mexec step1]. rewrite Nat2Z.id, Nat.add_0_r.
    rewrite mem_get_set_neq by lia.
    rewrite mem_get_set_eq by lia. reflexivity.
  - cbn [mexec step1]. rewrite Nat2Z.id.
    rewrite mem_get_set_eq by (rewrite mem_set_length; lia). reflexivity.
Qed.

Print Assumptions mexec_store_then_load.
Print Assumptions mexec_pair_build_proj.
