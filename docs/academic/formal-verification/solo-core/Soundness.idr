-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- Soundness (progress + preservation) for the Solo core.
--
-- Progress and preservation are now fully proved (no holes, no
-- dangerous primitives, total). The crux is the QTT substitution
-- lemma `SubstLemma.substLemma0`; preservation's beta cases are
-- direct corollaries of it, and the congruence cases rebuild the
-- same typing rule around the recursively-preserved subderivation.
--
-- Under the SPLIT (multiplicative) product rule the conclusion of
-- preservation is now AFFINE: the reduct is typed in a `Weaker`
-- SUB-context, not necessarily the same context (projection of a
-- split pair discards one summand's resources).

module Soundness

import Quantity
import Syntax
import Context
import ContextLemmas
import Typing
import Subst
import SubstLemma

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
-- Small-step reduction (call-by-value, left-to-right)
------------------------------------------------------------

||| `Step t t'` — one step of the CBV operational semantics. The
||| constructors mirror the reference interpreter `lib/interp.ml`
||| (left-to-right evaluation; beta/projection/case fire only on
||| values).
public export
data Step : Tm -> Tm -> Type where
  -- application
  SApp1 : Step t1 t1' -> Step (App t1 t2) (App t1' t2)
  SApp2 : Value t1 -> Step t2 t2' -> Step (App t1 t2) (App t1 t2')
  SBeta : Value v -> Step (App (Lam q a body) v) (subst0 body v)
  -- pairs
  SPair1 : Step t1 t1' -> Step (Pair t1 t2) (Pair t1' t2)
  SPair2 : Value t1 -> Step t2 t2' -> Step (Pair t1 t2) (Pair t1 t2')
  -- projections
  SFst1 : Step t t' -> Step (Fst t) (Fst t')
  SFstV : Value v1 -> Value v2 -> Step (Fst (Pair v1 v2)) v1
  SSnd1 : Step t t' -> Step (Snd t) (Snd t')
  SSndV : Value v1 -> Value v2 -> Step (Snd (Pair v1 v2)) v2
  -- sums
  SInl : Step t t' -> Step (Inl b t) (Inl b t')
  SInr : Step t t' -> Step (Inr a t) (Inr a t')
  -- case
  SCase  : Step t t' -> Step (Case t tL tR) (Case t' tL tR)
  SCaseL : Value v -> Step (Case (Inl b v) tL tR) (subst0 tL v)
  SCaseR : Value v -> Step (Case (Inr a v) tL tR) (subst0 tR v)
  -- let
  SLet1    : Step t1 t1' -> Step (Let q t1 t2) (Let q t1' t2)
  SLetBeta : Value v -> Step (Let q v t2) (subst0 t2 v)

------------------------------------------------------------
-- Existential wrapper (no dependent pair imports needed)
------------------------------------------------------------

||| Simple Sigma to avoid bringing in `Data.DPair` right now.
|||
||| The reduct `t'` is an erased witness — `StepsTo t` is the
||| proposition "`t` can take a step". `progress` works over de
||| Bruijn terms that are erased typing-judgement indices, so the
||| reduct is recovered by unification with the `Step` proof rather
||| than carried at runtime.
public export
data StepsTo : Tm -> Type where
  MkStepsTo : {0 t' : Tm} -> Step t t' -> StepsTo t

------------------------------------------------------------
-- Empty context has no variables
------------------------------------------------------------

||| `HasVar Empty n a` is uninhabited: neither `HVHere` nor
||| `HVThere` can produce the `Empty` context.
noVarInEmpty : HasVar Empty n a -> Void
noVarInEmpty HVHere impossible
noVarInEmpty HVThere impossible

------------------------------------------------------------
-- Canonical-forms step builders
------------------------------------------------------------
--
-- Rather than returning the (erased) structure of the value, each
-- helper directly produces the reduction step, with the
-- non-canonical value shapes ruled out by the typing derivation.

||| A value function applied to a value beta-reduces. Matching the
||| `Value` exposes the lambda; the reduct is recovered (erased) from
||| the `SBeta` step. The non-lambda value shapes contradict the
||| arrow-typing derivation.
appStep : Value t1 -> Has Empty t1 (TArr q a b) -> Value t2 -> StepsTo (App t1 t2)
appStep VLam        _   vx = MkStepsTo (SBeta vx)
appStep VUnit       thd vx impossible
appStep (VPair _ _) thd vx impossible
appStep (VInl _)    thd vx impossible
appStep (VInr _)    thd vx impossible

||| `Fst` of a value of product type steps to its first component.
fstStep : Value t -> Has Empty t (TPair a b) -> StepsTo (Fst t)
fstStep (VPair vv1 vv2) _ = MkStepsTo (SFstV vv1 vv2)
fstStep VUnit       thd impossible
fstStep VLam        thd impossible
fstStep (VInl _)    thd impossible
fstStep (VInr _)    thd impossible

||| `Snd` of a value of product type steps to its second component.
sndStep : Value t -> Has Empty t (TPair a b) -> StepsTo (Snd t)
sndStep (VPair vv1 vv2) _ = MkStepsTo (SSndV vv1 vv2)
sndStep VUnit       thd impossible
sndStep VLam        thd impossible
sndStep (VInl _)    thd impossible
sndStep (VInr _)    thd impossible

||| `Case` on a value of sum type steps into the matching branch.
caseStep : Value t -> Has Empty t (TSum a b) -> StepsTo (Case t tL tR)
caseStep (VInl vu) _ = MkStepsTo (SCaseL vu)
caseStep (VInr vu) _ = MkStepsTo (SCaseR vu)
caseStep VUnit       thd impossible
caseStep VLam        thd impossible
caseStep (VPair _ _) thd impossible

------------------------------------------------------------
-- Progress
------------------------------------------------------------

||| Progress: a closed, well-typed Solo term is either a value
||| or can take a step.
|||
||| "Closed" means typed in the empty context — there are no
||| free de Bruijn indices because `Empty` has no `HVHere` /
||| `HVThere` inhabitants.
public export
progress : Has Empty t a -> Either (Value t) (StepsTo t)
progress (THVar hv) = absurd (noVarInEmpty hv)
progress (THUnit _) = Left VUnit
progress (THLam _ _ _) = Left VLam
progress (THApp {t1} {t2} {g1} {a} {b} q g2 fD xD ac) =
  -- the application is closed (typed in Empty), so the function
  -- context g1 and the scaled argument context are both Empty.
  let (eg1, escg2) = addEmptyInv ac
      eg2 : (g2 = Empty) := scaleEmptyInv g2 escg2
      fD' : (Has Empty t1 (TArr q a b)) := rewrite sym eg1 in fD
      xD' : (Has Empty t2 a)            := rewrite sym eg2 in xD
  in case progress fD' of
    Right (MkStepsTo st) => Right (MkStepsTo (SApp1 st))
    Left vf => case progress xD' of
      Right (MkStepsTo st) => Right (MkStepsTo (SApp2 vf st))
      Left vx => Right (appStep vf fD' vx)
progress (THPair {t1} {t2} {a=pa} {b=pb} g1 g2 t1D t2D ac) =
  -- the pair is closed (typed in Empty), so the SPLIT puts both
  -- component contexts at Empty.
  let (eg1, eg2) = addEmptyInv ac
      t1D' : (Has Empty t1 pa) := rewrite sym eg1 in t1D
      t2D' : (Has Empty t2 pb) := rewrite sym eg2 in t2D
  in case progress t1D' of
    Right (MkStepsTo st) => Right (MkStepsTo (SPair1 st))
    Left v1 => case progress t2D' of
      Right (MkStepsTo st) => Right (MkStepsTo (SPair2 v1 st))
      Left v2 => Left (VPair v1 v2)
progress (THFst pD) =
  case progress pD of
    Right (MkStepsTo st) => Right (MkStepsTo (SFst1 st))
    Left vp => Right (fstStep vp pD)
progress (THSnd pD) =
  case progress pD of
    Right (MkStepsTo st) => Right (MkStepsTo (SSnd1 st))
    Left vp => Right (sndStep vp pD)
progress (THInl pD) =
  case progress pD of
    Right (MkStepsTo st) => Right (MkStepsTo (SInl st))
    Left vp => Left (VInl vp)
progress (THInr pD) =
  case progress pD of
    Right (MkStepsTo st) => Right (MkStepsTo (SInr st))
    Left vp => Left (VInr vp)
progress (THCase {t} a b g1 sD lD rD ac) =
  let (eg1, _) = addEmptyInv ac
      sD' : (Has Empty t (TSum a b)) := rewrite sym eg1 in sD
  in case progress sD' of
    Right (MkStepsTo st) => Right (MkStepsTo (SCase st))
    Left vs => Right (caseStep vs sD')
progress (THLet {t1} q a g1 e1D e2D ac) =
  let (escg1, _) = addEmptyInv ac
      eg1 : (g1 = Empty) := scaleEmptyInv g1 escg1
      e1D' : (Has Empty t1 a) := rewrite sym eg1 in e1D
  in case progress e1D' of
    Right (MkStepsTo st) => Right (MkStepsTo (SLet1 st))
    Left v1 => Right (MkStepsTo (SLetBeta v1))

------------------------------------------------------------
-- Preservation
------------------------------------------------------------

||| Preservation (AFFINE reading): reduction preserves typing in a
||| `Weaker` SUB-context. Concretely, if `t : a` in `g` and `t` steps
||| to `tp`, there is a context `gp` with `Weaker gp g` (every
||| quantity `<=` the corresponding quantity of `g`) such that
||| `tp : a` in `gp`.
|||
||| The sub-context content is the affine-accounting heart of the
||| theorem under the SPLIT (multiplicative) product: a projection
||| `Fst (Pair v1 v2)` discards `v2` and so its reduct `v1` lives in
||| the LEFT summand `g1`, a proper sub-context of the whole `g`. A
||| same-context statement would be FALSE for this fragment; the
||| `Weaker` existential is exactly the right weakening.
|||
||| * Congruence rules recurse, then re-split the SAME rule around the
|||   shrunk summand via `addCtxMonoLeft` / `addCtxMonoRight` (with
|||   `scaleMono` for the scaled summands of `THApp` / `THLet`),
|||   concluding `Weaker` by quantity monotonicity. Single-context
|||   congruences (`Fst`/`Snd`/`Inl`/`Inr`) thread the `Weaker` through
|||   unchanged.
||| * Beta rules invoke the QTT substitution lemma `substLemma0`, which
|||   types the reduct in EXACTLY `g`; the conclusion is `Weaker g g`
|||   (recovered from the in-hand `AddCtx _ _ g` via `weakerReflFromAdd`).
||| * The projection betas return the relevant summand of the SPLIT
|||   pair: `g1` (with `addCtxLeftWeaker ac`) for `Fst`, `g2`
|||   (`addCtxRightWeaker ac`) for `Snd`.
public export
preservation : Has g t a -> Step t tp -> (gp : Ctx ** (Weaker gp g, Has gp tp a))
-- congruence: split-context rules re-split around the preserved subderivation
preservation (THApp {g1} q g2 fD xD ac) (SApp1 st) =
  let (g1p ** (wk, fD')) = preservation fD st
      (gOut ** (acOut, wkOut)) = addCtxMonoLeft wk ac
  in (gOut ** (wkOut, THApp q g2 fD' xD acOut))
preservation (THApp {g1} q g2 fD xD ac) (SApp2 _ st) =
  let (g2p ** (wk, xD')) = preservation xD st
      (gOut ** (acOut, wkOut)) = addCtxMonoRight (scaleMono q wk) ac
  in (gOut ** (wkOut, THApp q g2p fD xD' acOut))
preservation (THPair g1 g2 t1D t2D ac) (SPair1 st) =
  let (g1p ** (wk, t1D')) = preservation t1D st
      (gOut ** (acOut, wkOut)) = addCtxMonoLeft wk ac
  in (gOut ** (wkOut, THPair g1p g2 t1D' t2D acOut))
preservation (THPair g1 g2 t1D t2D ac) (SPair2 _ st) =
  let (g2p ** (wk, t2D')) = preservation t2D st
      (gOut ** (acOut, wkOut)) = addCtxMonoRight wk ac
  in (gOut ** (wkOut, THPair g1 g2p t1D t2D' acOut))
preservation (THCase a b g1 sD lD rD ac) (SCase st) =
  let (g1p ** (wk, sD')) = preservation sD st
      (gOut ** (acOut, wkOut)) = addCtxMonoLeft wk ac
  in (gOut ** (wkOut, THCase a b g1p sD' lD rD acOut))
preservation (THLet q at g1 e1D e2D ac) (SLet1 st) =
  let (g1p ** (wk, e1D')) = preservation e1D st
      (gOut ** (acOut, wkOut)) = addCtxMonoLeft (scaleMono q wk) ac
  in (gOut ** (wkOut, THLet q at g1p e1D' e2D acOut))
-- congruence: single-context rules thread Weaker directly
preservation (THFst pD) (SFst1 st) =
  let (gp ** (wk, pD')) = preservation pD st in (gp ** (wk, THFst pD'))
preservation (THSnd pD) (SSnd1 st) =
  let (gp ** (wk, pD')) = preservation pD st in (gp ** (wk, THSnd pD'))
preservation (THInl pD) (SInl st) =
  let (gp ** (wk, pD')) = preservation pD st in (gp ** (wk, THInl pD'))
preservation (THInr pD) (SInr st) =
  let (gp ** (wk, pD')) = preservation pD st in (gp ** (wk, THInr pD'))
-- projection beta on the SPLIT product: return the relevant summand
preservation (THFst pD) (SFstV _ _) =
  case pD of
    THPair g1 g2 t1D t2D ac => (g1 ** (addCtxLeftWeaker ac, t1D))
preservation (THSnd pD) (SSndV _ _) =
  case pD of
    THPair g1 g2 t1D t2D ac => (g2 ** (addCtxRightWeaker ac, t2D))
-- function beta: substLemma0 types the reduct in exactly g
preservation (THApp {g} q g2 fD xD ac) (SBeta _) =
  case fD of
    THLam q a fBodyD =>
      -- fBodyD : Has (Snoc g1 a q) body bt ; xD : Has g2 v a ;
      -- ac : AddCtx g1 (ctxScale q g2) g.
      let (gr ** geq) = acResult ac in
      (gr ** ( rewrite geq in weakerReflFromAdd ac
             , rewrite geq in substLemma0 g2 fBodyD xD ac ))
-- let beta
preservation (THLet {g} q at g1 e1D e2D ac) (SLetBeta _) =
  -- e2D : Has (Snoc g2 at q) body bt ; e1D : Has g1 v at ;
  -- ac : AddCtx (ctxScale q g1) g2 g.  Need AddCtx g2 (ctxScale q g1) g.
  let (gr ** geq) = acResult ac in
  (gr ** ( rewrite geq in weakerReflFromAdd ac
         , rewrite geq in substLemma0 g1 e2D e1D (acComm ac) ))
-- case beta
preservation (THCase {g} {g2} a b g1 sD lD rD ac) (SCaseL _) =
  case sD of
    THInl vD =>
      -- vD : Has g1 v a ; lD : Has (Snoc g2 a One) tL c ;
      -- ac : AddCtx g1 g2 g.  Need AddCtx g2 (ctxScale One g1) g.
      let acOut : (AddCtx g2 (ctxScale One g1) g)
                := rewrite scaleOne g1 in acComm ac
          (gr ** geq) = acResult ac
      in (gr ** ( rewrite geq in weakerReflFromAdd ac
                , rewrite geq in substLemma0 g1 lD vD acOut ))
preservation (THCase {g} {g2} a b g1 sD lD rD ac) (SCaseR _) =
  case sD of
    THInr vD =>
      let acOut : (AddCtx g2 (ctxScale One g1) g)
                := rewrite scaleOne g1 in acComm ac
          (gr ** geq) = acResult ac
      in (gr ** ( rewrite geq in weakerReflFromAdd ac
                , rewrite geq in substLemma0 g1 rD vD acOut ))

------------------------------------------------------------
-- Affine preservation (corollary)
------------------------------------------------------------

||| Affine preservation: identical to `preservation` — under the SPLIT
||| product the affine sub-context existential IS the preservation
||| statement (the `Weaker` witness carries the quantity accounting:
||| every reduct quantity is `<=` the original). Kept as a named
||| corollary for documentation and downstream reference.
public export
affinePreservation : Has g t a -> Step t tp -> (gp : Ctx ** (Weaker gp g, Has gp tp a))
affinePreservation = preservation
