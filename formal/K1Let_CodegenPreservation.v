(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   K1Let_CodegenPreservation.v
   ═══════════════════════════
   K-1, GROWN. Extends the minimal K1_CodegenPreservation fragment with the
   first real binder: de Bruijn **variables** and **`let`**, evaluated under an
   environment. The target machine gains a **locals** register alongside the
   operand stack; compilation balances `IBind`/`IUnbind` so locals scope
   correctly. The codegen-preservation theorem is re-proven for this richer
   fragment — still complete and axiom-free (no `Admitted`).

   This is a step of the K-1 obligation toward the real AST (binders →
   environments). `if`/control-flow is the next increment (it needs structured
   target control + a termination measure, deliberately out of this step).

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
   Check:  coqc -Q . ASFormal K1Let_CodegenPreservation.v
*)

Require Import List.
Import ListNotations.

(* ════════════════════════ Source (env-based, with let) ═══════════════════ *)

Inductive sval : Type := VNat (n : nat) | VBool (b : bool).

Inductive sexp : Type :=
| SNat (n : nat)
| SBool (b : bool)
| SVar (i : nat)             (* de Bruijn index into the environment *)
| SAdd (a b : sexp)
| SAnd (a b : sexp)
| SLet (e1 e2 : sexp).       (* let x = e1 in e2 ; x is index 0 in e2 *)

Definition senv := list sval.

Fixpoint seval (env : senv) (e : sexp) : option sval :=
  match e with
  | SNat n  => Some (VNat n)
  | SBool b => Some (VBool b)
  | SVar i  => nth_error env i
  | SAdd a b =>
      match seval env a, seval env b with
      | Some (VNat x), Some (VNat y) => Some (VNat (x + y))
      | _, _ => None
      end
  | SAnd a b =>
      match seval env a, seval env b with
      | Some (VBool x), Some (VBool y) => Some (VBool (andb x y))
      | _, _ => None
      end
  | SLet e1 e2 =>
      match seval env e1 with
      | Some v => seval (v :: env) e2
      | None   => None
      end
  end.

(* ═══════════════ Target: stack machine + a locals register ════════════════ *)

Inductive wval : Type := WNat (n : nat) | WBool (b : bool).

Definition obs (v : sval) : wval :=
  match v with VNat n => WNat n | VBool b => WBool b end.

Inductive instr : Type :=
| IPushN (n : nat)
| IPushB (b : bool)
| IGet   (i : nat)   (* push locals[i] onto the stack *)
| IAdd
| IAnd
| IBind              (* pop stack top, push it onto locals (new index 0) *)
| IUnbind.           (* drop locals[0]; operand stack unchanged *)

Definition code   := list instr.
Definition locals := list wval.
Definition stack  := list wval.

Definition wstep (i : instr) (l : locals) (s : stack) : option (locals * stack) :=
  match i, l, s with
  | IPushN n, l, s                   => Some (l, WNat n :: s)
  | IPushB b, l, s                   => Some (l, WBool b :: s)
  | IGet k, l, s                     =>
      match nth_error l k with
      | Some w => Some (l, w :: s)
      | None   => None
      end
  | IAdd, l, WNat b :: WNat a :: s'  => Some (l, WNat (a + b) :: s')
  | IAnd, l, WBool b :: WBool a :: s' => Some (l, WBool (andb a b) :: s')
  | IBind, l, w :: s'                => Some (w :: l, s')
  | IUnbind, _ :: l', s              => Some (l', s)
  | _, _, _                          => None
  end.

Fixpoint wexec (c : code) (l : locals) (s : stack) : option (locals * stack) :=
  match c with
  | []        => Some (l, s)
  | i :: rest =>
      match wstep i l s with
      | Some (l', s') => wexec rest l' s'
      | None          => None
      end
  end.

Fixpoint compile (e : sexp) : code :=
  match e with
  | SNat n     => [IPushN n]
  | SBool b    => [IPushB b]
  | SVar i     => [IGet i]
  | SAdd a b   => compile a ++ compile b ++ [IAdd]
  | SAnd a b   => compile a ++ compile b ++ [IAnd]
  | SLet e1 e2 => compile e1 ++ [IBind] ++ compile e2 ++ [IUnbind]
  end.

(* ═════════════════════════ preservation (no Admitted) ════════════════════ *)

(* Execution distributes over code concatenation — the workhorse that lets the
   `let` case step its `IBind`/`IUnbind` cleanly without continuation juggling. *)
Lemma wexec_app : forall c1 c2 l s,
  wexec (c1 ++ c2) l s =
  match wexec c1 l s with
  | Some (l', s') => wexec c2 l' s'
  | None          => None
  end.
Proof.
  induction c1 as [| i c1 IH]; intros c2 l s; simpl.
  - reflexivity.
  - destruct (wstep i l s) as [[l' s'] |]; [apply IH | reflexivity].
Qed.

(* Codegen preservation, generalized over the operand stack `s` and carrying
   the environment/locals correspondence `l = map obs env`. `IUnbind` restores
   locals, so the machine returns to the same `l` it started with. *)
Lemma compile_correct : forall e env v l s,
  seval env e = Some v ->
  l = map obs env ->
  wexec (compile e) l s = Some (l, obs v :: s).
Proof.
  induction e as [n | b | i | a IHa b IHb | a IHa b IHb | e1 IH1 e2 IH2];
    intros env v l s Hev Hl; simpl in Hev; subst l.
  - (* SNat *)  inversion Hev; subst; reflexivity.
  - (* SBool *) inversion Hev; subst; reflexivity.
  - (* SVar *)
    apply (map_nth_error obs) in Hev; simpl; rewrite Hev; reflexivity.
  - (* SAdd *)
    destruct (seval env a) as [[x | xb] |] eqn:Ha; try discriminate;
    destruct (seval env b) as [[y | yb] |] eqn:Hb; try discriminate;
    inversion Hev; subst; clear Hev; simpl compile;
    rewrite wexec_app, (IHa env (VNat x) (map obs env) s Ha eq_refl); simpl;
    rewrite wexec_app, (IHb env (VNat y) (map obs env) (WNat x :: s) Hb eq_refl); simpl;
    reflexivity.
  - (* SAnd *)
    destruct (seval env a) as [[xn | x] |] eqn:Ha; try discriminate;
    destruct (seval env b) as [[yn | y] |] eqn:Hb; try discriminate;
    inversion Hev; subst; clear Hev; simpl compile;
    rewrite wexec_app, (IHa env (VBool x) (map obs env) s Ha eq_refl); simpl;
    rewrite wexec_app, (IHb env (VBool y) (map obs env) (WBool x :: s) Hb eq_refl); simpl;
    reflexivity.
  - (* SLet *)
    destruct (seval env e1) as [v1 |] eqn:H1; try discriminate; simpl in Hev;
    simpl compile;
    rewrite wexec_app, (IH1 env v1 (map obs env) s H1 eq_refl); simpl;
    rewrite wexec_app, (IH2 (v1 :: env) v (obs v1 :: map obs env) s Hev eq_refl); simpl;
    reflexivity.
Qed.

(* For a closed program (empty env) the locals balance back to empty and the
   result is the single wasm value on the operand stack. *)
Theorem k1let_preservation_holds : forall e v,
  seval [] e = Some v ->
  wexec (compile e) [] [] = Some ([], [obs v]).
Proof.
  intros e v H.
  exact (compile_correct e [] v [] [] H eq_refl).
Qed.

Print Assumptions k1let_preservation_holds.
