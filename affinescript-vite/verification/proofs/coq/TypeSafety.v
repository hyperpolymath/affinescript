(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) *)
(*
   Coq Proof Template: Type system soundness
   Replace with your project's type system proofs.
   All proofs must be complete — NO Admitted allowed.
*)

Require Import Coq.Lists.List.
Require Import Coq.Arith.Arith.
Require Import Coq.Bool.Bool.
Import ListNotations.

(** * Example: Simple expression language with type safety *)
(** Replace this entire section with your project's type system. *)

(** Types *)
Inductive ty : Type :=
  | TyNat  : ty
  | TyBool : ty.

(** Expressions *)
Inductive expr : Type :=
  | EConst : nat -> expr
  | ETrue  : expr
  | EFalse : expr
  | EPlus  : expr -> expr -> expr
  | EEq    : expr -> expr -> expr.

(** Values *)
Inductive value : Type :=
  | VNat  : nat -> value
  | VBool : bool -> value.

(** Typing relation *)
Inductive has_type : expr -> ty -> Prop :=
  | T_Const : forall n, has_type (EConst n) TyNat
  | T_True  : has_type ETrue TyBool
  | T_False : has_type EFalse TyBool
  | T_Plus  : forall e1 e2,
      has_type e1 TyNat -> has_type e2 TyNat ->
      has_type (EPlus e1 e2) TyNat
  | T_Eq    : forall e1 e2,
      has_type e1 TyNat -> has_type e2 TyNat ->
      has_type (EEq e1 e2) TyBool.

(** Evaluation *)
Inductive eval : expr -> value -> Prop :=
  | E_Const : forall n, eval (EConst n) (VNat n)
  | E_True  : eval ETrue (VBool true)
  | E_False : eval EFalse (VBool false)
  | E_Plus  : forall e1 e2 n1 n2,
      eval e1 (VNat n1) -> eval e2 (VNat n2) ->
      eval (EPlus e1 e2) (VNat (n1 + n2))
  | E_Eq    : forall e1 e2 n1 n2,
      eval e1 (VNat n1) -> eval e2 (VNat n2) ->
      eval (EEq e1 e2) (VBool (Nat.eqb n1 n2)).

(** Value typing *)
Definition value_has_type (v : value) (t : ty) : Prop :=
  match v, t with
  | VNat _, TyNat   => True
  | VBool _, TyBool => True
  | _, _            => False
  end.

(** Type soundness: well-typed expressions evaluate to well-typed values *)
Theorem type_soundness : forall e t v,
  has_type e t -> eval e v -> value_has_type v t.
Proof.
  intros e t v Htype Heval.
  induction Htype; inversion Heval; subst; simpl; auto.
Qed.
