-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- Syntax of the Solo core of AffineScript.
--
-- Solo = simply-typed lambda calculus + Unit + pairs + sums + let,
-- with every binder annotated by a QTT quantity q ∈ {0, 1, omega}.
-- We use intrinsically-untyped de Bruijn indices here; Typing.idr
-- then layers the QTT typing judgement on top.
--
-- Deliberately EXCLUDED from Solo (deferred to Duet / Ensemble):
--   * traits and effects
--   * row polymorphism and records
--   * refinement and dependent types
--   * ownership (own / ref / mut)
--   * modules, arrays, unsafe, handlers

module Syntax

import Quantity

%default total

------------------------------------------------------------
-- Types
------------------------------------------------------------

||| Solo types. The function arrow `TArr q a b` carries the
||| quantity with which its argument is consumed — matching the
||| `(q x : τ₁) → τ₂` shape in `docs/spec.md` §3.5 (with effects
||| stripped for Solo).
public export
data Ty : Type where
  TUnit : Ty
  TPair : Ty -> Ty -> Ty
  TSum  : Ty -> Ty -> Ty
  TArr  : Q -> Ty -> Ty -> Ty

%name Ty a, b, c

------------------------------------------------------------
-- Terms (de Bruijn)
------------------------------------------------------------

||| Solo terms, using de Bruijn indices represented as `Nat`.
|||
||| Note `Lam` carries the quantity annotation of the bound
||| variable and the domain type (following QTT). `Case` binds
||| one variable in each branch (the injected value).
public export
data Tm : Type where
  Var  : Nat -> Tm
  UnitT : Tm
  Lam  : Q -> Ty -> Tm -> Tm
  App  : Tm -> Tm -> Tm
  Pair : Tm -> Tm -> Tm
  Fst  : Tm -> Tm
  Snd  : Tm -> Tm
  Inl  : Ty -> Tm -> Tm  -- type annotation = the *other* summand
  Inr  : Ty -> Tm -> Tm
  Case : Tm -> Tm -> Tm -> Tm
         -- scrutinee, left branch (binds 1), right branch (binds 1)
  Let  : Q -> Tm -> Tm -> Tm
         -- let (q x) = e1 in e2

%name Tm t, t1, t2, u
