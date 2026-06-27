(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   RealLoop.v
   ══════════
   REAL-LIFT rung **R2-loops** (see formal/REAL-LIFT.adoc): backward jumps.
   Adds the structured-control execution for `lib/wasm.ml`'s `Block`/`Loop`/
   `Br`/`BrIf` (the instructions were added to `instr` in RealWasm.v) and a
   source `while`/`seq`/`set` statement layer compiled to them, with the
   compiler-correctness simulation proved across a terminating loop.

   The R2 executor `wexec` returns `option (locals*stack)` — it cannot express
   "this code branched to label k", so it cannot run `Br`. R2-loops therefore
   introduces a **branch-aware** executor `cexec` returning an `outcome`
   (Normal / Branch k / Trap). It reuses the very same *fuel* device R2 used for
   forward control: `Loop` re-entry recurses with decremented fuel, so the loop
   is fuel-bounded and `cexec` stays a structural `Fixpoint` (definitional, so
   `cbn`/`reflexivity`/demos still compute).

   Backward jumps + a terminating-loop theorem settle the value-returning-tail
   question behind #601 (F-2) concretely: a `while` is a *statement* (unit), and
   its lowering's `cexec` agreement on the live locals is a theorem (below);
   the expression-tail (value-returning) lowering keeps the value on the stack —
   the two are exhibited and their observable difference is proved.

   i32 ≔ Z (wrap deferred to R-wrap). Axiom-free, no Admitted.
   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import List.
Require Import ZArith.
Require Import PeanoNat.
Require Import Lia.
Require Import ASFormal.RealWasm.
Require Import ASFormal.RealCompile.
Import ListNotations.

(* ── branch-aware outcome ─────────────────────────────────────────────────────
   ONormal  — fell off the end of the instruction list with this state.
   OBranch k — a `Br`/`BrIf` is unwinding toward the k-th enclosing label.
   OTrap    — a stuck straight-line step (e.g. stack underflow).                 *)
Inductive outcome :=
| ONormal (lo : locals) (st : stack)
| OBranch (k : nat) (lo : locals) (st : stack)
| OTrap.

(* fuel-indexed, branch-aware executor. None = out of fuel. Labels:
   - Block/Loop/IfElse each introduce one label, so an OBranch index is
     decremented as it crosses one (S k ↦ k), and consumed (k = 0) by the
     construct it targets.
   - a Block consumed by `Br 0` exits to *after* the block;
   - a Loop consumed by `Br 0` *re-enters* the loop (the back-edge);
   - reaching a construct's end normally (`ONormal`) continues after it.        *)
Fixpoint cexec (fuel : nat) (is : list instr) (lo : locals) (st : stack)
  : option outcome :=
  match fuel with
  | O => None
  | S f =>
    match is with
    | [] => Some (ONormal lo st)
    | a :: r =>
      match a with
      | IfElse thn els =>
          match st with
          | c :: t =>
              match cexec f (if Z.eqb c 0 then els else thn) lo t with
              | Some (ONormal lo' st')      => cexec f r lo' st'
              | Some (OBranch 0 lo' st')    => cexec f r lo' st'      (* if-label consumed *)
              | Some (OBranch (S k) lo' st')=> Some (OBranch k lo' st')
              | Some OTrap                  => Some OTrap
              | None                        => None
              end
          | [] => Some OTrap
          end
      | Block body =>
          match cexec f body lo st with
          | Some (ONormal lo' st')       => cexec f r lo' st'
          | Some (OBranch 0 lo' st')     => cexec f r lo' st'        (* Br 0 ⇒ exit past block *)
          | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
          | Some OTrap                   => Some OTrap
          | None                         => None
          end
      | Loop body =>
          match cexec f body lo st with
          | Some (ONormal lo' st')       => cexec f r lo' st'        (* fell off loop end ⇒ continue *)
          | Some (OBranch 0 lo' st')     => cexec f (Loop body :: r) lo' st'  (* Br 0 ⇒ re-enter *)
          | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
          | Some OTrap                   => Some OTrap
          | None                         => None
          end
      | Br k => Some (OBranch k lo st)
      | BrIf k =>
          match st with
          | c :: t => if Z.eqb c 0 then cexec f r lo t else Some (OBranch k lo t)
          | [] => Some OTrap
          end
      | _ =>
          match step1 a lo st with
          | Some (lo', st') => cexec f r lo' st'
          | None => Some OTrap
          end
      end
    end
  end.

(* ── the canonical `while` lowering, as a stand-alone instruction ────────────
   while (cond <> 0) { body }  ⇒
     Block [ Loop ( <cond>; I32Eqz; BrIf 1; <body>; Br 0 ) ]
   (Br 1 = exit the Block; Br 0 = re-enter the Loop). cond/body here are the
   already-lowered (branch-free) instruction sequences. *)
Definition while_instrs (cond body : list instr) : list instr :=
  [ Block [ Loop ( cond ++ [I32Eqz; BrIf 1] ++ body ++ [Br 0] ) ] ].

(* ── computational sanity (definitional ⇒ reflexivity) ───────────────────────
   countdown: while (local0 <> 0) { local0 := local0 - 1 }, from 3 ⇒ 0. *)
Definition countdown_body : list instr :=
  [LocalGet 0; I32Const 1; I32Sub; LocalSet 0].
Definition countdown_cond : list instr := [LocalGet 0].

Example cexec_countdown_demo :
  cexec 100 (while_instrs countdown_cond countdown_body) [3%Z] []
  = Some (ONormal [0%Z] []).
Proof. reflexivity. Qed.

(* a bare Br exits straight-line code with a branch outcome. *)
Example cexec_br_demo :
  cexec 5 [I32Const 7; Br 2; I32Const 9] [] []
  = Some (OBranch 2 [] [7%Z]).
Proof. reflexivity. Qed.

(* BrIf with a false (0) condition falls through; true branches. *)
Example cexec_brif_false_demo :
  cexec 5 [I32Const 0; BrIf 0; I32Const 9] [] [] = Some (ONormal [] [9%Z]).
Proof. reflexivity. Qed.
Example cexec_brif_true_demo :
  cexec 5 [I32Const 1; BrIf 3; I32Const 9] [] [] = Some (OBranch 3 [] []).
Proof. reflexivity. Qed.

(* ── one-step unfold (keeps cbn from over-reducing Loop self-reference) ─────── *)
Lemma cexec_S_cons : forall f a r lo st,
  cexec (S f) (a :: r) lo st =
  match a with
  | IfElse thn els =>
      match st with
      | c :: t =>
          match cexec f (if Z.eqb c 0 then els else thn) lo t with
          | Some (ONormal lo' st')       => cexec f r lo' st'
          | Some (OBranch 0 lo' st')     => cexec f r lo' st'
          | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
          | Some OTrap                   => Some OTrap
          | None                         => None
          end
      | [] => Some OTrap
      end
  | Block body =>
      match cexec f body lo st with
      | Some (ONormal lo' st')       => cexec f r lo' st'
      | Some (OBranch 0 lo' st')     => cexec f r lo' st'
      | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
      | Some OTrap                   => Some OTrap
      | None                         => None
      end
  | Loop body =>
      match cexec f body lo st with
      | Some (ONormal lo' st')       => cexec f r lo' st'
      | Some (OBranch 0 lo' st')     => cexec f (Loop body :: r) lo' st'
      | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
      | Some OTrap                   => Some OTrap
      | None                         => None
      end
  | Br k => Some (OBranch k lo st)
  | BrIf k =>
      match st with
      | c :: t => if Z.eqb c 0 then cexec f r lo t else Some (OBranch k lo t)
      | [] => Some OTrap
      end
  | _ =>
      match step1 a lo st with
      | Some (lo', st') => cexec f r lo' st'
      | None => Some OTrap
      end
  end.
Proof. intros; destruct a; reflexivity. Qed.

(* ── monotonicity: more fuel never changes a Some result ───────────────────── *)
Lemma cexec_le_S : forall f is lo st o,
  cexec f is lo st = Some o -> cexec (S f) is lo st = Some o.
Proof.
  induction f as [| f IHf]; intros is lo st o H; [discriminate H|].
  destruct is as [| a r]; [exact H|].
  rewrite cexec_S_cons in H; rewrite cexec_S_cons.
  destruct a;
    (* straight-line: step1 path *)
    try (destruct (step1 _ lo st) as [[lo' st']|]; [ apply IHf; exact H | exact H ]).
  - (* IfElse *)
    destruct st as [| c t]; [exact H|].
    destruct (cexec f (if Z.eqb c 0 then els else thn) lo t) as [o'|] eqn:Hb;
      [| discriminate H].
    apply IHf in Hb; rewrite Hb.
    destruct o' as [lo' st'| k' lo' st'|];
      [ apply IHf; exact H
      | destruct k' as [| k'']; [ apply IHf; exact H | exact H ]
      | exact H ].
  - (* Block *)
    destruct (cexec f body lo st) as [o'|] eqn:Hb; [| discriminate H].
    apply IHf in Hb; rewrite Hb.
    destruct o' as [lo' st'| k' lo' st'|];
      [ apply IHf; exact H
      | destruct k' as [| k'']; [ apply IHf; exact H | exact H ]
      | exact H ].
  - (* Loop *)
    destruct (cexec f body lo st) as [o'|] eqn:Hb; [| discriminate H].
    apply IHf in Hb; rewrite Hb.
    destruct o' as [lo' st'| k' lo' st'|];
      [ apply IHf; exact H
      | destruct k' as [| k'']; [ apply IHf; exact H | exact H ]
      | exact H ].
  - (* Br *) exact H.
  - (* BrIf *)
    destruct st as [| c t]; [exact H|].
    destruct (Z.eqb c 0); [ apply IHf; exact H | exact H ].
Qed.

Lemma cexec_mono : forall f f' is lo st o,
  f <= f' -> cexec f is lo st = Some o -> cexec f' is lo st = Some o.
Proof.
  intros f f' is lo st o Hle; induction Hle; intros H.
  - exact H.
  - apply cexec_le_S, IHHle, H.
Qed.

(* ── additive sequencing: when is1 finishes Normal, is2 runs from its state ── *)
Lemma cexec_app_normal : forall f1 is1 lo st lo1 st1,
  cexec f1 is1 lo st = Some (ONormal lo1 st1) ->
  forall f2 is2 o,
  cexec f2 is2 lo1 st1 = Some o ->
  cexec (f1 + f2) (is1 ++ is2) lo st = Some o.
Proof.
  induction f1 as [| f1 IHf1];
    intros is1 lo st lo1 st1 H1 f2 is2 o H2; [discriminate H1|].
  destruct is1 as [| a r0].
  - (* [] : H1 forces lo1=lo, st1=st; is1++is2 = is2 *)
    cbn [cexec] in H1. injection H1 as Hlo Hst; subst lo1 st1.
    cbn [app]. apply cexec_mono with (f := f2); [lia | exact H2].
  - rewrite cexec_S_cons in H1.
    cbn [app]; rewrite Nat.add_succ_l, cexec_S_cons.
    destruct a;
      try (destruct (step1 _ lo st) as [[lo' st']|];
           [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2) | discriminate H1 ]).
    + (* IfElse *)
      destruct st as [| c t]; [discriminate H1|].
      destruct (cexec f1 (if Z.eqb c 0 then els else thn) lo t) as [o'|] eqn:Hb;
        [| discriminate H1].
      rewrite (cexec_mono f1 (f1 + f2) _ lo t _ ltac:(lia) Hb).
      destruct o' as [lo' st'| k' lo' st'|];
        [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2)
        | destruct k' as [| k'']; [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2) | discriminate H1 ]
        | discriminate H1 ].
    + (* Block *)
      destruct (cexec f1 body lo st) as [o'|] eqn:Hb; [| discriminate H1].
      rewrite (cexec_mono f1 (f1 + f2) _ lo st _ ltac:(lia) Hb).
      destruct o' as [lo' st'| k' lo' st'|];
        [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2)
        | destruct k' as [| k'']; [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2) | discriminate H1 ]
        | discriminate H1 ].
    + (* Loop *)
      destruct (cexec f1 body lo st) as [o'|] eqn:Hb; [| discriminate H1].
      rewrite (cexec_mono f1 (f1 + f2) _ lo st _ ltac:(lia) Hb).
      destruct o' as [lo' st'| k' lo' st'|];
        [ exact (IHf1 r0 lo' st' lo1 st1 H1 f2 is2 o H2)
        | destruct k' as [| k''];
            [ exact (IHf1 (Loop body :: r0) lo' st' lo1 st1 H1 f2 is2 o H2)
            | discriminate H1 ]
        | discriminate H1 ].
    + (* Br *) discriminate H1.
    + (* BrIf *)
      destruct st as [| c t]; [discriminate H1|].
      destruct (Z.eqb c 0);
        [ exact (IHf1 r0 lo t lo1 st1 H1 f2 is2 o H2) | discriminate H1 ].
Qed.

(* ── lifting the R2 expression compiler into the branch-aware executor ───────
   Rather than a `branchfree` predicate (mutual instr/list-instr recursion is
   rejected by Coq's guard checker), we re-prove compile-correctness directly in
   `cexec`/ONormal terms — the *same* induction as RealCompile.compile_correct,
   with `cexec_app_normal`/`cexec_le_S` for `wexec_app_some`/`wexec_le_S`. This
   is what lets a `while`'s lowered condition/body (compiled expressions) run
   inside `cexec`. *)
Lemma cexec_bop : forall b lo v1 v2 t,
  cexec 2 [bop_instr b] lo (v2 :: v1 :: t) = Some (ONormal lo (bop_val b v1 v2 :: t)).
Proof. destruct b; reflexivity. Qed.

Lemma cexec_ifelse : forall f c tcode ecode lo st,
  cexec (S f) [IfElse tcode ecode] lo (c :: st) =
  match cexec f (if Z.eqb c 0 then ecode else tcode) lo st with
  | Some (ONormal lo' st')       => cexec f [] lo' st'
  | Some (OBranch 0 lo' st')     => cexec f [] lo' st'
  | Some (OBranch (S k) lo' st') => Some (OBranch k lo' st')
  | Some OTrap                   => Some OTrap
  | None                         => None
  end.
Proof. reflexivity. Qed.

Lemma cexec_compile : forall e env d locals st v,
  eval env e = Some v ->
  length env = d ->
  agree env locals ->
  d + depth e <= length locals ->
  exists fuel locals',
    cexec fuel (compile d e) locals st = Some (ONormal locals' (v :: st)) /\
    length locals' = length locals /\
    (forall i, i < d -> nth_error locals' i = nth_error locals i).
Proof.
  induction e as [ l | i | e1 IH1 e2 IH2 | b e1 IH1 e2 IH2 | c IHc thn IHthn els IHels ];
    intros env d locals st v Heval Hlen Hagree Hbound; cbn [depth] in Hbound.
  - (* RLit *)
    cbn in Heval. injection Heval as Hv; subst v.
    exists 2, locals.
    split; [reflexivity | split; [reflexivity | intros; reflexivity]].
  - (* RVar i *)
    cbn in Heval.
    assert (Hi : i < length env) by (apply nth_error_Some; rewrite Heval; discriminate).
    exists 2, locals. cbn [compile].
    rewrite cexec_S_cons; cbn [step1]. rewrite (Hagree i Hi), Heval.
    split; [reflexivity | split; [reflexivity | intros; reflexivity]].
  - (* RLet e1 e2 *)
    cbn in Heval.
    destruct (eval env e1) as [v1|] eqn:Hev1; cbn in Heval; [| discriminate].
    destruct (IH1 env d locals st v1 Hev1 Hlen Hagree ltac:(lia))
      as [f1 [locals1 [Hw1 [Hlen1 Hlow1]]]].
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
      as [f2 [locals2 [Hw2 [Hlen2 Hlow2]]]].
    exists (f1 + (2 + f2)), locals2. split; [| split].
    + apply cexec_app_normal with (lo1:=locals1) (st1:=v1::st); [exact Hw1|].
      apply cexec_app_normal with (lo1:=set_nth d v1 locals1) (st1:=st); [| exact Hw2].
      rewrite cexec_S_cons. cbn [step1].
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
      as [f1 [locals1 [Hw1 [Hlen1 Hlow1]]]].
    assert (Hag1 : agree env locals1).
    { intros j Hj. rewrite Hlow1 by lia. apply Hagree; lia. }
    destruct (IH2 env d locals1 (v1 :: st) v2 Hev2 Hlen Hag1 ltac:(rewrite Hlen1; lia))
      as [f2 [locals2 [Hw2 [Hlen2 Hlow2]]]].
    exists (f1 + (f2 + 2)), locals2. split; [| split].
    + apply cexec_app_normal with (lo1:=locals1) (st1:=v1::st); [exact Hw1|].
      apply cexec_app_normal with (lo1:=locals2) (st1:=v2::v1::st); [exact Hw2|].
      exact (cexec_bop b locals2 v1 v2 st).
    + rewrite Hlen2. exact Hlen1.
    + intros j Hj. rewrite Hlow2 by lia. apply Hlow1; lia.
  - (* RIf c thn els *)
    change (compile d (RIf c thn els))
      with (compile d c ++ [IfElse (compile d thn) (compile d els)]).
    cbn in Heval.
    destruct (eval env c) as [vc|] eqn:Hevc; cbn in Heval; [| discriminate].
    destruct (IHc env d locals st vc Hevc Hlen Hagree ltac:(lia))
      as [fc [lc [Hwc [Hlenc Hlowc]]]].
    assert (Hagc : agree env lc).
    { intros j Hj. rewrite Hlowc by lia. apply Hagree; lia. }
    destruct (Z.eqb vc 0) eqn:Hvc; cbn in Heval.
    + (* vc = 0 → els *)
      destruct (IHels env d lc st v Heval Hlen Hagc ltac:(lia))
        as [fe [le [Hwe [Hlene Hlowe]]]].
      exists (fc + S (S fe)), le. split; [| split].
      * apply cexec_app_normal with (lo1:=lc) (st1:=vc::st); [exact Hwc|].
        rewrite cexec_ifelse, Hvc.
        rewrite (cexec_le_S _ _ _ _ _ Hwe). reflexivity.
      * rewrite Hlene. exact Hlenc.
      * intros j Hj. rewrite Hlowe by lia. apply Hlowc; lia.
    + (* vc <> 0 → thn *)
      destruct (IHthn env d lc st v Heval Hlen Hagc ltac:(lia))
        as [ft [lt [Hwt [Hlent Hlowt]]]].
      exists (fc + S (S ft)), lt. split; [| split].
      * apply cexec_app_normal with (lo1:=lc) (st1:=vc::st); [exact Hwc|].
        rewrite cexec_ifelse, Hvc.
        rewrite (cexec_le_S _ _ _ _ _ Hwt). reflexivity.
      * rewrite Hlent. exact Hlenc.
      * intros j Hj. rewrite Hlowt by lia. apply Hlowc; lia.
Qed.

(* ── source statements (the value-returning-tail / #601 fragment) ────────────
   A `while` is a *statement* — unit-valued, leaving the value stack as it found
   it. Variables are local slots (the resolved core). Expressions here are the
   RLet-free fragment (depth 0), so no scratch slots are needed and the locals
   array is exactly the live variables. *)
Inductive stmt :=
| SSkip
| SSeq (s1 s2 : stmt)
| SSet (slot : nat) (e : rexpr)          (* local[slot] := eval e *)
| SWhile (cond : rexpr) (body : stmt).   (* while (eval cond <> 0) do body *)

Fixpoint stmt_depth (s : stmt) : nat :=
  match s with
  | SSkip => 0
  | SSeq s1 s2 => Nat.max (stmt_depth s1) (stmt_depth s2)
  | SSet _ e => depth e
  | SWhile cond body => Nat.max (depth cond) (stmt_depth body)
  end.

(* reference semantics (mirrors a statement interpreter); fuel bounds the loop. *)
Fixpoint run_stmt (fuel : nat) (s : stmt) (lo : locals) : option locals :=
  match fuel with
  | O => None
  | S f =>
    match s with
    | SSkip => Some lo
    | SSeq s1 s2 =>
        match run_stmt f s1 lo with Some lo1 => run_stmt f s2 lo1 | None => None end
    | SSet slot e =>
        match eval lo e with
        | Some v => if Nat.ltb slot (length lo) then Some (set_nth slot v lo) else None
        | None => None
        end
    | SWhile cond body =>
        match eval lo cond with
        | Some vc =>
            if Z.eqb vc 0 then Some lo
            else match run_stmt f body lo with
                 | Some lo1 => run_stmt f (SWhile cond body) lo1
                 | None => None
                 end
        | None => None
        end
    end
  end.

(* the lowering (mirrors statement codegen). while ⇒ Block[Loop[..; BrIf 1; ..; Br 0]] *)
Definition loopbody (d : nat) (cond : rexpr) (cbody : list instr) : list instr :=
  compile d cond ++ [I32Eqz; BrIf 1] ++ cbody ++ [Br 0].

Fixpoint compile_stmt (d : nat) (s : stmt) : list instr :=
  match s with
  | SSkip => []
  | SSeq s1 s2 => compile_stmt d s1 ++ compile_stmt d s2
  | SSet slot e => compile d e ++ [LocalSet slot]
  | SWhile cond body => [ Block [ Loop (loopbody d cond (compile_stmt d body)) ] ]
  end.

(* statements preserve the locals-array length (set_nth does). *)
Lemma run_stmt_length : forall fuel s lo lo',
  run_stmt fuel s lo = Some lo' -> length lo' = length lo.
Proof.
  induction fuel as [| f IH]; intros s lo lo' H; [discriminate H|].
  destruct s as [| s1 s2 | slot e | cond body]; cbn [run_stmt] in H.
  - injection H as <-; reflexivity.
  - destruct (run_stmt f s1 lo) as [lo1|] eqn:H1; [| discriminate H].
    apply IH in H1; apply IH in H; lia.
  - destruct (eval lo e) as [v|]; [| discriminate H].
    destruct (Nat.ltb slot (length lo)); [| discriminate H].
    injection H as <-; rewrite set_nth_length; reflexivity.
  - destruct (eval lo cond) as [vc|]; [| discriminate H].
    destruct (Z.eqb vc 0).
    + injection H as <-; reflexivity.
    + destruct (run_stmt f body lo) as [lo1|] eqn:Hb; [| discriminate H].
      apply IH in Hb; apply IH in H; lia.
Qed.

(* nth_error extensionality over Z lists (to turn low-slot agreement at full
   width into equality). *)
Lemma list_ext_Z : forall (l1 l2 : list Z),
  length l1 = length l2 ->
  (forall i, i < length l1 -> nth_error l1 i = nth_error l2 i) ->
  l1 = l2.
Proof.
  induction l1 as [| x r IH]; intros l2 Hlen Hnth.
  - destruct l2; [reflexivity | cbn in Hlen; discriminate].
  - destruct l2 as [| y r2]; [cbn in Hlen; discriminate|].
    cbn in Hlen; injection Hlen as Hlen.
    assert (Hxy : nth_error (x :: r) 0 = nth_error (y :: r2) 0)
      by (apply Hnth; cbn; lia).
    cbn in Hxy; injection Hxy as ->.
    f_equal. apply IH; [exact Hlen|].
    intros i Hi. specialize (Hnth (S i)); cbn in Hnth. apply Hnth; cbn; lia.
Qed.

(* concrete: while (x<>0) { x := x-1 }, from 3 ⇒ 0, as a *source statement*. *)
Definition s_countdown : stmt :=
  SWhile (RVar 0) (SSet 0 (RBin BSub (RVar 0) (RLit (LInt 1)))).
Example run_countdown_demo : run_stmt 100 s_countdown [3%Z] = Some [0%Z].
Proof. reflexivity. Qed.

(* controlled one-step unfolds (cbn [run_stmt] would over-reduce S-headed
   inner fuels). *)
Lemma run_seq_unfold : forall n s1 s2 lo,
  run_stmt (S n) (SSeq s1 s2) lo =
  match run_stmt n s1 lo with Some lo1 => run_stmt n s2 lo1 | None => None end.
Proof. reflexivity. Qed.

Lemma run_while_unfold : forall n cond body lo,
  run_stmt (S n) (SWhile cond body) lo =
  match eval lo cond with
  | Some vc => if Z.eqb vc 0 then Some lo
               else match run_stmt n body lo with
                    | Some lo1 => run_stmt n (SWhile cond body) lo1 | None => None end
  | None => None
  end.
Proof. reflexivity. Qed.

(* run_stmt is monotone in fuel (more fuel ⇒ same Some result). *)
Lemma run_stmt_le_S : forall n s lo lo',
  run_stmt n s lo = Some lo' -> run_stmt (S n) s lo = Some lo'.
Proof.
  induction n as [| n IH]; intros s lo lo' H; [discriminate H|].
  destruct s as [| s1 s2 | slot e | cond body].
  - exact H.
  - rewrite run_seq_unfold in H; rewrite run_seq_unfold.
    destruct (run_stmt n s1 lo) as [lo1|] eqn:H1; [| discriminate H].
    rewrite (IH _ _ _ H1). apply IH; exact H.
  - exact H.
  - rewrite run_while_unfold in H; rewrite run_while_unfold.
    destruct (eval lo cond) as [vc|]; [| discriminate H].
    destruct (Z.eqb vc 0); [exact H|].
    destruct (run_stmt n body lo) as [lo1|] eqn:Hb; [| discriminate H].
    rewrite (IH _ _ _ Hb). apply IH; exact H.
Qed.

Lemma run_stmt_mono : forall n m s lo lo',
  n <= m -> run_stmt n s lo = Some lo' -> run_stmt m s lo = Some lo'.
Proof.
  intros n m s lo lo' Hle; induction Hle; intros H; [exact H|].
  apply run_stmt_le_S, IHHle, H.
Qed.

(* ── one lowered loop iteration ──────────────────────────────────────────────
   Eqz;BrIf 1 on a singleton stack: nonzero ⇒ fall through to the rest;
   zero ⇒ branch out (to the enclosing Block). *)
Lemma cexec_eqz_brif_cont : forall F vc lo rest,
  vc <> 0%Z ->
  cexec (S (S F)) (I32Eqz :: BrIf 1 :: rest) lo (vc :: []) = cexec F rest lo [].
Proof.
  intros F vc lo rest Hvc.
  assert (Hez : Z.eqb vc 0 = false) by (apply Z.eqb_neq; exact Hvc).
  rewrite cexec_S_cons; cbn [step1]. rewrite Hez.
  change (if false then 1%Z else 0%Z) with 0%Z.
  rewrite cexec_S_cons; cbn [Z.eqb]. reflexivity.
Qed.

Lemma cexec_eqz_brif_exit : forall F lo rest,
  cexec (S (S F)) (I32Eqz :: BrIf 1 :: rest) lo (0%Z :: []) = Some (OBranch 1 lo []).
Proof.
  intros F lo rest.
  rewrite cexec_S_cons; cbn [step1].
  change (if Z.eqb 0 0 then 1%Z else 0%Z) with 1%Z.
  rewrite cexec_S_cons; cbn [Z.eqb]. reflexivity.
Qed.

(* cond false ⇒ the loop body unwinds with OBranch 1 (caught by the Block). *)
Lemma cexec_loopbody_false : forall d cond cbody lo,
  length lo = d -> depth cond = 0 -> eval lo cond = Some 0%Z ->
  exists cf, cexec cf (loopbody d cond cbody) lo [] = Some (OBranch 1 lo []).
Proof.
  intros d cond cbody lo Hlen Hcond Hevc.
  destruct (cexec_compile cond lo d lo [] 0%Z Hevc Hlen
              (fun i _ => eq_refl) ltac:(rewrite Hcond; lia))
    as [fc [lc [Hwc [Hlenc Hlow]]]].
  assert (Hlc : lc = lo).
  { apply list_ext_Z; [congruence|]. intros i Hi. apply Hlow.
    rewrite Hlenc, Hlen in Hi; exact Hi. }
  subst lc.
  exists (fc + S (S 0)). unfold loopbody.
  apply cexec_app_normal with (lo1:=lo) (st1:=0%Z::[]); [exact Hwc|].
  cbn [app]. apply cexec_eqz_brif_exit.
Qed.

(* cond true ⇒ run the body, then unwind with OBranch 0 (re-enters the Loop). *)
Lemma cexec_loopbody_true : forall d cond cbody lo lo1 vc,
  length lo = d -> depth cond = 0 -> eval lo cond = Some vc -> vc <> 0%Z ->
  (exists cb, cexec cb cbody lo [] = Some (ONormal lo1 [])) ->
  exists cf, cexec cf (loopbody d cond cbody) lo [] = Some (OBranch 0 lo1 []).
Proof.
  intros d cond cbody lo lo1 vc Hlen Hcond Hevc Hvc [cb Hcb].
  destruct (cexec_compile cond lo d lo [] vc Hevc Hlen
              (fun i _ => eq_refl) ltac:(rewrite Hcond; lia))
    as [fc [lc [Hwc [Hlenc Hlow]]]].
  assert (Hlc : lc = lo).
  { apply list_ext_Z; [congruence|]. intros i Hi. apply Hlow.
    rewrite Hlenc, Hlen in Hi; exact Hi. }
  subst lc.
  exists (fc + S (S (cb + 1))). unfold loopbody.
  apply cexec_app_normal with (lo1:=lo) (st1:=vc::[]); [exact Hwc|].
  cbn [app].
  rewrite (cexec_eqz_brif_cont (cb + 1) vc lo (cbody ++ [Br 0]) Hvc).
  apply cexec_app_normal with (lo1:=lo1) (st1:=@nil Z); [exact Hcb|].
  reflexivity.
Qed.

(* ── the loop simulation: [Loop loopbody] realizes a terminating while ──────── *)
Lemma cexec_loop : forall fuel cond body lo lo' d,
  d = length lo -> depth cond = 0 ->
  run_stmt fuel (SWhile cond body) lo = Some lo' ->
  (forall n lo0 lo0', n < fuel -> length lo0 = d ->
      run_stmt n body lo0 = Some lo0' ->
      exists cf, cexec cf (compile_stmt d body) lo0 [] = Some (ONormal lo0' [])) ->
  exists cf, cexec cf [ Loop (loopbody d cond (compile_stmt d body)) ] lo []
             = Some (OBranch 0 lo' []).
Proof.
  induction fuel as [| f IHfuel]; intros cond body lo lo' d Hd Hcond Hrun Hbody.
  - discriminate Hrun.
  - cbn [run_stmt] in Hrun.
    destruct (eval lo cond) as [vc|] eqn:Hevc; [| discriminate Hrun].
    destruct (Z.eqb vc 0) eqn:Hvc.
    + (* cond false: exit; lo' = lo *)
      apply Z.eqb_eq in Hvc; subst vc.
      injection Hrun as Hrun; subst lo'.
      destruct (cexec_loopbody_false d cond (compile_stmt d body) lo
                  (eq_sym Hd) Hcond Hevc) as [ce Hce].
      exists (S ce). rewrite cexec_S_cons, Hce. reflexivity.
    + (* cond true: body → lo1, then recurse *)
      apply Z.eqb_neq in Hvc.
      destruct (run_stmt f body lo) as [lo1|] eqn:Hbrun; [| discriminate Hrun].
      assert (Hlen1 : length lo1 = d).
      { rewrite (run_stmt_length f body lo lo1 Hbrun); exact (eq_sym Hd). }
      destruct (Hbody f lo lo1 (Nat.lt_succ_diag_r f) (eq_sym Hd) Hbrun) as [cb Hcb].
      destruct (cexec_loopbody_true d cond (compile_stmt d body) lo lo1 vc
                  (eq_sym Hd) Hcond Hevc Hvc (ex_intro _ cb Hcb)) as [ce Hce].
      destruct (IHfuel cond body lo1 lo' d (eq_sym Hlen1) Hcond Hrun
                  (fun n lo0 lo0' Hn => Hbody n lo0 lo0' (Nat.lt_lt_succ_r n f Hn)))
        as [cw Hcw].
      exists (S (Nat.max ce cw)). rewrite cexec_S_cons.
      rewrite (cexec_mono ce (Nat.max ce cw) _ lo [] _ (Nat.le_max_l ce cw) Hce).
      exact (cexec_mono cw (Nat.max ce cw) _ lo1 [] _ (Nat.le_max_r ce cw) Hcw).
Qed.

(* ── compiler correctness for statements (the #601 statement tail) ───────────
   A terminating statement's lowering runs to the same locals, leaving the value
   stack EMPTY (unit) — `cexec … = Some (ONormal lo' [])`. d = length lo: all
   slots are live, expressions are RLet-free (stmt_depth s = 0), no scratch. *)
Theorem compile_stmt_correct : forall fuel s lo lo',
  run_stmt fuel s lo = Some lo' ->
  stmt_depth s = 0 ->
  exists cf, cexec cf (compile_stmt (length lo) s) lo [] = Some (ONormal lo' []).
Proof.
  induction fuel as [| f IH]; intros s lo lo' Hrun Hsd; [discriminate Hrun|].
  destruct s as [| s1 s2 | slot e | cond body]; cbn [compile_stmt].
  - (* SSkip *)
    cbn [run_stmt] in Hrun; injection Hrun as <-. exists 1; reflexivity.
  - (* SSeq s1 s2 *)
    cbn [run_stmt] in Hrun.
    destruct (run_stmt f s1 lo) as [lo1|] eqn:H1; [| discriminate Hrun].
    cbn [stmt_depth] in Hsd.
    assert (Hsd1 : stmt_depth s1 = 0) by lia.
    assert (Hsd2 : stmt_depth s2 = 0) by lia.
    destruct (IH s1 lo lo1 H1 Hsd1) as [c1 Hc1].
    destruct (IH s2 lo1 lo' Hrun Hsd2) as [c2 Hc2].
    rewrite (run_stmt_length f s1 lo lo1 H1) in Hc2.
    exists (c1 + c2).
    apply cexec_app_normal with (lo1:=lo1) (st1:=@nil Z); [exact Hc1 | exact Hc2].
  - (* SSet slot e *)
    cbn [run_stmt] in Hrun. cbn [stmt_depth] in Hsd.
    destruct (eval lo e) as [v|] eqn:Hev; [| discriminate Hrun].
    destruct (Nat.ltb slot (length lo)) eqn:Hslt; [| discriminate Hrun].
    injection Hrun as <-.
    destruct (cexec_compile e lo (length lo) lo [] v Hev eq_refl
                (fun i _ => eq_refl) ltac:(rewrite Hsd; lia))
      as [ce [lc [Hwc [Hlenc Hlow]]]].
    assert (Hlc : lc = lo).
    { apply list_ext_Z; [congruence|]. intros i Hi. apply Hlow; congruence. }
    subst lc.
    exists (ce + S (S 0)).
    apply cexec_app_normal with (lo1:=lo) (st1:=v::[]); [exact Hwc|].
    rewrite cexec_S_cons; cbn [step1]. rewrite Hslt. reflexivity.
  - (* SWhile cond body *)
    cbn [stmt_depth] in Hsd.
    assert (Hcd : depth cond = 0) by lia.
    assert (Hbd : stmt_depth body = 0) by lia.
    assert (Hbody : forall n lo0 lo0', n < S f -> length lo0 = length lo ->
              run_stmt n body lo0 = Some lo0' ->
              exists cf, cexec cf (compile_stmt (length lo) body) lo0 [] = Some (ONormal lo0' [])).
    { intros n lo0 lo0' Hn Hlen0 Hrun0. rewrite <- Hlen0.
      apply (IH body lo0 lo0').
      - apply (run_stmt_mono n f body lo0 lo0'); [lia | exact Hrun0].
      - exact Hbd. }
    destruct (cexec_loop (S f) cond body lo lo' (length lo) eq_refl Hcd Hrun Hbody)
      as [cf Hcf].
    exists (S (S cf)). rewrite cexec_S_cons.
    rewrite (cexec_mono cf (S cf) _ lo [] _ (Nat.le_succ_diag_r cf) Hcf).
    reflexivity.
Qed.

(* concrete end-to-end: the lowered countdown WHILE runs to [0] with an EMPTY
   value stack — the unit/statement tail (#601 settled concretely). *)
Example compile_stmt_countdown_demo :
  exists cf, cexec cf (compile_stmt 1 s_countdown) [3%Z] [] = Some (ONormal [0%Z] []).
Proof. apply (compile_stmt_correct 100 s_countdown [3%Z] [0%Z] run_countdown_demo eq_refl). Qed.

Print Assumptions cexec_compile.
Print Assumptions compile_stmt_correct.
