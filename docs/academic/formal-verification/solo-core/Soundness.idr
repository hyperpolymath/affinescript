-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- Statements of the soundness theorems for the Solo core.
--
-- This file deliberately contains NO PROOFS. The week-1-2
-- deliverable for Track F1 is the *statement* of progress and
-- preservation; the actual derivations are weeks 3-12 work and
-- will be filled in case-by-case. All theorems are left as
-- explicit `?todo_...` holes so the file still typechecks as a
-- declaration module.

module Soundness

import Quantity
import Syntax
import Context
import ContextLemmas
import Typing
import Subst

%default total

------------------------------------------------------------
-- Values
------------------------------------------------------------

||| Solo values: canonical forms of closed terms.
public export
data Value : Tm -> Type where
  VUnit : Value UnitT
  VLam  : Value (Lam q a t)
  VPair : Value t1 -> Value t2 -> Value (Pair t1 t2)
  VInl  : Value t -> Value (Inl b t)
  VInr  : Value t -> Value (Inr a t)

------------------------------------------------------------
-- Small-step reduction (declaration only)
------------------------------------------------------------
--
-- We declare the relation `Step t t'` as a data family, but do
-- NOT enumerate its constructors yet. The constructors will
-- follow call-by-value, context-free beta/projection/case
-- reduction. They are introduced alongside the progress /
-- preservation proofs in weeks 3-6.

||| Call-by-value, left-to-right small-step reduction. The redex
||| rules (beta, projection, case, let) mirror `docs/spec.md` §4.3;
||| the congruence rules implement the evaluation contexts of
||| §4.2 (function before argument, left of a pair before right,
||| inside an injection, scrutinee before branch selection). We do
||| NOT reduce under a binder, so every `Lam` is a value.
public export
data Step : Tm -> Tm -> Type where

  ------------------------------------------------------------
  -- beta / redex rules
  ------------------------------------------------------------

  ||| (fn(q x:a) => body) v  →  body[0 ↦ v]
  SBeta    : Value v
          -> Step (App (Lam q a body) v) (subst0 body v)

  ||| fst (v1, v2)  →  v1
  SFstBeta : Value v1 -> Value v2
          -> Step (Fst (Pair v1 v2)) v1

  ||| snd (v1, v2)  →  v2
  SSndBeta : Value v1 -> Value v2
          -> Step (Snd (Pair v1 v2)) v2

  ||| case (inl v) of {l; r}  →  l[0 ↦ v]
  SCaseL   : Value v
          -> Step (Case (Inl b v) tL tR) (subst0 tL v)

  ||| case (inr v) of {l; r}  →  r[0 ↦ v]
  SCaseR   : Value v
          -> Step (Case (Inr a v) tL tR) (subst0 tR v)

  ||| let (q x) = v in body  →  body[0 ↦ v]
  SLetBeta : Value v
          -> Step (Let q v body) (subst0 body v)

  ------------------------------------------------------------
  -- congruence rules (evaluation contexts)
  ------------------------------------------------------------

  SApp1    : Step t1 t1' -> Step (App t1 t2) (App t1' t2)
  SApp2    : Value v1 -> Step t2 t2' -> Step (App v1 t2) (App v1 t2')
  SPair1   : Step t1 t1' -> Step (Pair t1 t2) (Pair t1' t2)
  SPair2   : Value v1 -> Step t2 t2' -> Step (Pair v1 t2) (Pair v1 t2')
  SFst     : Step t t' -> Step (Fst t) (Fst t')
  SSnd     : Step t t' -> Step (Snd t) (Snd t')
  SInl     : Step t t' -> Step (Inl b t) (Inl b t')
  SInr     : Step t t' -> Step (Inr a t) (Inr a t')
  SCase    : Step s s' -> Step (Case s tL tR) (Case s' tL tR)
  SLet1    : Step e e' -> Step (Let q e body) (Let q e' body)

------------------------------------------------------------
-- Existential wrapper (no dependent pair imports needed)
------------------------------------------------------------

||| Simple Sigma to avoid bringing in `Data.DPair` right now. The
||| reduct `t'` is erased (`0`): `StepsTo t` is the proposition
||| "there exists `t'` with `t` ⟶ `t'`", and the proofs never need
||| to compute the reduct as run-time data.
public export
data StepsTo : Tm -> Type where
  MkStepsTo : (0 t' : Tm) -> Step t t' -> StepsTo t

------------------------------------------------------------
-- Inversions used by progress
------------------------------------------------------------

||| The empty context binds no variables.
noEmptyVar : HasVar Empty n a -> Void
noEmptyVar (HVHere _ _)  impossible
noEmptyVar (HVThere _ _) impossible

||| Elimination forms are never values.
notValApp : Value (App x y) -> Void
notValApp VUnit       impossible
notValApp VLam        impossible
notValApp (VPair _ _) impossible
notValApp (VInl _)    impossible
notValApp (VInr _)    impossible

notValFst : Value (Fst x) -> Void
notValFst VUnit       impossible
notValFst VLam        impossible
notValFst (VPair _ _) impossible
notValFst (VInl _)    impossible
notValFst (VInr _)    impossible

notValSnd : Value (Snd x) -> Void
notValSnd VUnit       impossible
notValSnd VLam        impossible
notValSnd (VPair _ _) impossible
notValSnd (VInl _)    impossible
notValSnd (VInr _)    impossible

notValCase : Value (Case s l r) -> Void
notValCase VUnit       impossible
notValCase VLam        impossible
notValCase (VPair _ _) impossible
notValCase (VInl _)    impossible
notValCase (VInr _)    impossible

notValLet : Value (Let q e b) -> Void
notValLet VUnit       impossible
notValLet VLam        impossible
notValLet (VPair _ _) impossible
notValLet (VInl _)    impossible
notValLet (VInr _)    impossible

------------------------------------------------------------
-- Progress
------------------------------------------------------------

||| Progress: a closed, well-typed Solo term is either a value
||| or can take a step.
|||
||| "Closed" means typed in the empty context — `Empty` has no
||| `HasVar` inhabitants (`noEmptyVar`), so there are no free de
||| Bruijn indices. The proof is by induction on the typing
||| derivation; the splitting rules first invert their context
||| equation to learn that every sub-derivation is itself closed,
||| then recurse left-to-right per the call-by-value strategy.
public export
progress : Has Empty t a -> Either (Value t) (StepsTo t)

progress (THVar v) = absurd (noEmptyVar v)

progress (THUnit _) = Left VUnit

progress (THLam _) = Left VLam

progress (THInl d) = case progress d of
  Right (MkStepsTo _ st) => Right (MkStepsTo _ (SInl st))
  Left dval               => Left (VInl dval)

progress (THInr d) = case progress d of
  Right (MkStepsTo _ st) => Right (MkStepsTo _ (SInr st))
  Left dval               => Left (VInr dval)

progress (THApp g1 g2 q f x prf) =
  let (e1, esc) = addEmptyInv g1 (ctxScale q g2) prf
      e2        = scaleEmptyInv q g2 esc in
  case (e1, e2) of
    (Refl, Refl) => case progress f of
      Right (MkStepsTo _ st1) => Right (MkStepsTo _ (SApp1 st1))
      Left fval => case progress x of
        Right (MkStepsTo _ st2) => Right (MkStepsTo _ (SApp2 fval st2))
        Left xval => case f of
          THLam _              => Right (MkStepsTo _ (SBeta xval))
          THVar v              => absurd (noEmptyVar v)
          THApp _ _ _ _ _ _    => absurd (notValApp fval)
          THFst _              => absurd (notValFst fval)
          THSnd _              => absurd (notValSnd fval)
          THCase _ _ _ _ _ _   => absurd (notValCase fval)
          THLet _ _ _ _ _ _    => absurd (notValLet fval)

progress (THPair g1 g2 p1 p2 prf) =
  let (e1, e2) = addEmptyInv g1 g2 prf in
  case (e1, e2) of
    (Refl, Refl) => case progress p1 of
      Right (MkStepsTo _ st1) => Right (MkStepsTo _ (SPair1 st1))
      Left v1 => case progress p2 of
        Right (MkStepsTo _ st2) => Right (MkStepsTo _ (SPair2 v1 st2))
        Left v2 => Left (VPair v1 v2)

progress (THFst d) = case progress d of
  Right (MkStepsTo _ st) => Right (MkStepsTo _ (SFst st))
  Left dval => case d of
    THPair _ _ _ _ _   => case dval of
                            VPair v1 v2 => Right (MkStepsTo _ (SFstBeta v1 v2))
    THVar v            => absurd (noEmptyVar v)
    THApp _ _ _ _ _ _  => absurd (notValApp dval)
    THFst _            => absurd (notValFst dval)
    THSnd _            => absurd (notValSnd dval)
    THCase _ _ _ _ _ _ => absurd (notValCase dval)
    THLet _ _ _ _ _ _  => absurd (notValLet dval)

progress (THSnd d) = case progress d of
  Right (MkStepsTo _ st) => Right (MkStepsTo _ (SSnd st))
  Left dval => case d of
    THPair _ _ _ _ _   => case dval of
                            VPair v1 v2 => Right (MkStepsTo _ (SSndBeta v1 v2))
    THVar v            => absurd (noEmptyVar v)
    THApp _ _ _ _ _ _  => absurd (notValApp dval)
    THFst _            => absurd (notValFst dval)
    THSnd _            => absurd (notValSnd dval)
    THCase _ _ _ _ _ _ => absurd (notValCase dval)
    THLet _ _ _ _ _ _  => absurd (notValLet dval)

progress (THCase g1 g2 s l r prf) =
  let (e1, e2) = addEmptyInv g1 g2 prf in
  case (e1, e2) of
    (Refl, Refl) => case progress s of
      Right (MkStepsTo _ st) => Right (MkStepsTo _ (SCase st))
      Left sval => case s of
        THInl _            => case sval of VInl v => Right (MkStepsTo _ (SCaseL v))
        THInr _            => case sval of VInr v => Right (MkStepsTo _ (SCaseR v))
        THVar v            => absurd (noEmptyVar v)
        THApp _ _ _ _ _ _  => absurd (notValApp sval)
        THFst _            => absurd (notValFst sval)
        THSnd _            => absurd (notValSnd sval)
        THCase _ _ _ _ _ _ => absurd (notValCase sval)
        THLet _ _ _ _ _ _  => absurd (notValLet sval)

progress (THLet g1 g2 q d1 d2 prf) =
  let (esc, e2) = addEmptyInv (ctxScale q g1) g2 prf
      e1        = scaleEmptyInv q g1 esc in
  case (e1, e2) of
    (Refl, Refl) => case progress d1 of
      Right (MkStepsTo _ st1) => Right (MkStepsTo _ (SLet1 st1))
      Left v1                  => Right (MkStepsTo _ (SLetBeta v1))

------------------------------------------------------------
-- Preservation
------------------------------------------------------------

||| Preservation: reduction preserves typing in the same
||| context. The fact that the context is preserved (not merely
||| "there exists some g'") is the affine-accounting content of
||| the theorem — if `t` could run while duplicating a linear
||| variable, the reduct would require a *larger* context.
public export
preservation : Has g t a -> Step t t' -> Has g t' a
preservation _ _ = ?todo_preservation

------------------------------------------------------------
-- Affine preservation (corollary)
------------------------------------------------------------

||| Affine preservation: if a term is well-typed with every
||| binding at quantity `One` or `Zero` and it steps, the reduct
||| is still well-typed in the same context. For Solo this is a
||| direct corollary of `preservation` above (the preserved
||| context already carries the quantity accounting), and is
||| stated here only for documentation.
public export
affinePreservation : Has g t a -> Step t t' -> Has g t' a
affinePreservation = preservation
