-- SPDX-License-Identifier: PMPL-1.0-or-later
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
import Typing

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

public export
data Step : Tm -> Tm -> Type where
  -- Constructors intentionally omitted until week 3.
  -- This keeps the *statements* below well-typed without
  -- prematurely committing to a specific operational-semantics
  -- formulation. The constructors will mirror the reference
  -- interpreter in `lib/interp.ml` (call-by-value, left-to-right).

------------------------------------------------------------
-- Existential wrapper (no dependent pair imports needed)
------------------------------------------------------------

||| Simple Sigma to avoid bringing in `Data.DPair` right now.
public export
data StepsTo : Tm -> Type where
  MkStepsTo : (t' : Tm) -> Step t t' -> StepsTo t

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
progress _ = ?todo_progress

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
