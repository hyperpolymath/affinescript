(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   K1_CodegenPreservation.v
   ════════════════════════
   Wave-0 seed of obligation **K-1** from docs/PROOF-NEEDS.adoc:
   "codegen → typed-WASM semantic-preservation" — the keystone every face
   (via F-1) and every aLib conformer ultimately composes through.

   This file is a Coq/Rocq PROOF SCRIPT. It is NOT V-lang and NOT Verilog —
   the `.v` extension is shared between Coq, Verilog and V-lang. The estate
   language policy bans *V-lang* (→ Zig) and carves Coq proof scripts out of
   the `cicd_rules/vlang_detected` rule via `path_allow_prefixes`; this repo
   makes that explicit for `formal/` in `.hypatia-ignore`.

   SCOPE (honest). This discharges K-1 for a deliberately minimal fragment:
   a two-type (nat, bool) expression language with `add` and `and`, compiled
   to a little stack machine that stands in for typed-WASM. The full
   obligation — the real AffineScript AST and the real typed-WASM operational
   semantics — is future work that *expands* this core, exactly the way
   solo-core's Duet/Ensemble tracks expand the Solo fragment. What is proven
   here is the genuine article for the fragment: a complete, machine-checked
   compiler-correctness theorem with NO `Admitted`, NO `Axiom`, NO `postulate`
   (estate rule: load-bearing proofs must be constructive and complete).

   Check it:  coqc formal/K1_CodegenPreservation.v   (or: just -f formal/justfile check)
*)

Require Import List.
Import ListNotations.

(* ══════════════════ Source: a minimal AffineScript fragment ══════════════ *)

Inductive sval : Type :=
| VNat  (n : nat)
| VBool (b : bool).

Inductive sexp : Type :=
| SNat  (n : nat)
| SBool (b : bool)
| SAdd  (e1 e2 : sexp)
| SAnd  (e1 e2 : sexp).

(* Big-step source evaluator. Partial: `add` on bools (or `and` on nats) is
   stuck (None), so `seval e = Some v` is a real, non-trivial hypothesis. *)
Fixpoint seval (e : sexp) : option sval :=
  match e with
  | SNat n  => Some (VNat n)
  | SBool b => Some (VBool b)
  | SAdd e1 e2 =>
      match seval e1, seval e2 with
      | Some (VNat a), Some (VNat b) => Some (VNat (a + b))
      | _, _ => None
      end
  | SAnd e1 e2 =>
      match seval e1, seval e2 with
      | Some (VBool a), Some (VBool b) => Some (VBool (andb a b))
      | _, _ => None
      end
  end.

(* ══════════════ Target: a minimal typed-WASM-style stack machine ═════════ *)

Inductive wval : Type :=
| WNat  (n : nat)
| WBool (b : bool).

Inductive instr : Type :=
| IPushN (n : nat)
| IPushB (b : bool)
| IAdd
| IAnd.

Definition code  := list instr.
Definition stack := list wval.

(* One instruction over the stack. None = trap (ill-typed operand stack),
   the analogue of a typed-WASM validation failure. *)
Definition wstep (i : instr) (s : stack) : option stack :=
  match i, s with
  | IPushN n, s                        => Some (WNat n :: s)
  | IPushB b, s                        => Some (WBool b :: s)
  | IAdd, WNat b :: WNat a :: s'       => Some (WNat (a + b) :: s')
  | IAnd, WBool b :: WBool a :: s'     => Some (WBool (andb a b) :: s')
  | _, _                               => None
  end.

Fixpoint wexec (c : code) (s : stack) : option stack :=
  match c with
  | []        => Some s
  | i :: rest =>
      match wstep i s with
      | Some s' => wexec rest s'
      | None    => None
      end
  end.

(* ══════════════════════ Compiler: source → stack code ════════════════════ *)

Fixpoint compile (e : sexp) : code :=
  match e with
  | SNat n     => [IPushN n]
  | SBool b    => [IPushB b]
  | SAdd e1 e2 => compile e1 ++ compile e2 ++ [IAdd]
  | SAnd e1 e2 => compile e1 ++ compile e2 ++ [IAnd]
  end.

(* Value correspondence: a source value and the wasm value it denotes. *)
Definition obs (v : sval) : wval :=
  match v with
  | VNat n  => WNat n
  | VBool b => WBool b
  end.

(* ════════════════ The K-1 obligation, for this fragment ══════════════════ *)

(* Codegen preserves meaning: if the source evaluates to v, then running the
   compiled code on the empty operand stack yields exactly [obs v]. *)
Definition K1_preservation : Prop :=
  forall (e : sexp) (v : sval),
    seval e = Some v ->
    wexec (compile e) [] = Some [obs v].

(* ───────────────────────── discharge (no Admitted) ──────────────────────── *)

(* Correctness in continuation-passing form: running `compile e` followed by
   any continuation `k`, from any stack `s`, is the same as running `k` with
   the source value `v` pushed. Threading `k` is exactly the induction
   strength needed (and avoids reasoning about a residual `match`). *)
Lemma compile_correct : forall e v s k,
  seval e = Some v ->
  wexec (compile e ++ k) s = wexec k (obs v :: s).
Proof.
  induction e as [n | b | e1 IH1 e2 IH2 | e1 IH1 e2 IH2];
    intros v s k Hev; simpl in Hev.
  - (* SNat *)  inversion Hev; subst; reflexivity.
  - (* SBool *) inversion Hev; subst; reflexivity.
  - (* SAdd *)
    destruct (seval e1) as [[a | ab] |]; try discriminate;
    destruct (seval e2) as [[b | bb] |]; try discriminate;
    inversion Hev; subst; clear Hev; simpl; rewrite <- !app_assoc;
    rewrite (IH1 _ s _ eq_refl), (IH2 _ _ _ eq_refl); reflexivity.
  - (* SAnd *)
    destruct (seval e1) as [[na | a] |]; try discriminate;
    destruct (seval e2) as [[nb | b] |]; try discriminate;
    inversion Hev; subst; clear Hev; simpl; rewrite <- !app_assoc;
    rewrite (IH1 _ s _ eq_refl), (IH2 _ _ _ eq_refl); reflexivity.
Qed.

Theorem k1_preservation_holds : K1_preservation.
Proof.
  unfold K1_preservation; intros e v Hev.
  pose proof (compile_correct e v [] [] Hev) as H.
  rewrite app_nil_r in H; simpl in H; exact H.
Qed.

(* `Print Assumptions` must report "Closed under the global context" — i.e.
   the theorem rests on no axioms. CI greps for exactly that string. *)
Print Assumptions k1_preservation_holds.
