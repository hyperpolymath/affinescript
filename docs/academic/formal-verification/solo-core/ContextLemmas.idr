-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- Context-level lemmas supporting the Solo-core soundness proofs.
--
-- Two design notes, both forced by mechanising (rather than merely
-- *stating*) the meta-theory in Idris2:
--
--   1. `IsZero g` replaces the bare index `ctxZero g0` that the
--      week-1-2 statement-only `Typing.idr` used for the all-zero
--      context in T-Var / T-Unit. A computed index such as
--      `ctxZero g0` is not invertible by unification, so a derivation
--      cannot be pattern-matched against a *concrete* context (e.g.
--      `Empty`). `IsZero` is the same information as an analysable,
--      inductive premise: `IsZero g` holds exactly when every entry
--      of `g` carries quantity `Zero`, i.e. iff `g = ctxZero g`.
--
--   2. The pointwise inversions below take their contexts as explicit
--      (runtime) arguments. Idris2 erases data-type indices, so the
--      split contexts threaded through T-App / T-Pair / T-Case /
--      T-Let must be available at run time to be analysed at all; the
--      reformulated `Typing.idr` therefore passes them explicitly.
--
-- None of this changes the typing *relation* — the set of derivable
-- `Has g t a` judgements is identical to the statement-only version.

module ContextLemmas

import Quantity
import Syntax
import Context

%default total

------------------------------------------------------------
-- The "every entry is Zero" predicate
------------------------------------------------------------

||| `IsZero g` witnesses that every binding in `g` has quantity
||| `Zero`. This is the analysable form of "`g` is an all-zero
||| context" (`g = ctxZero g`), used by T-Var and T-Unit.
public export
data IsZero : Ctx -> Type where
  IZEmpty : IsZero Empty
  IZSnoc  : IsZero g -> IsZero (Snoc g a Zero)

||| `ctxZero g` is always a zero context.
public export
ctxZeroIsZero : (g : Ctx) -> IsZero (ctxZero g)
ctxZeroIsZero Empty        = IZEmpty
ctxZeroIsZero (Snoc g a q) = IZSnoc (ctxZeroIsZero g)

------------------------------------------------------------
-- Inversions: when does a combination collapse to `Empty`?
------------------------------------------------------------

||| If two contexts add to `Empty`, both were `Empty`. Contexts are
||| explicit so the proof can case-split them (indices are erased).
public export
addEmptyInv : (x, y : Ctx) -> ctxAdd x y = Just Empty -> (x = Empty, y = Empty)
addEmptyInv Empty Empty _ = (Refl, Refl)
addEmptyInv Empty (Snoc _ _ _) Refl impossible
addEmptyInv (Snoc _ _ _) Empty Refl impossible
addEmptyInv (Snoc g1 a1 q1) (Snoc g2 a2 q2) prf with (ctxAdd g1 g2)
  addEmptyInv (Snoc g1 a1 q1) (Snoc g2 a2 q2) Refl     | Nothing impossible
  addEmptyInv (Snoc g1 a1 q1) (Snoc g2 a2 q2) Refl     | (Just _) impossible

||| Scaling a context to `Empty` means it was already `Empty`.
public export
scaleEmptyInv : (q : Q) -> (g : Ctx) -> ctxScale q g = Empty -> g = Empty
scaleEmptyInv q Empty _ = Refl
scaleEmptyInv q (Snoc _ _ _) Refl impossible
