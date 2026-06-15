-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- QTT typing judgement for the Solo core.
--
-- The judgement `Has g t a` means: term `t` has type `a` in
-- context `g`, with `g`'s quantities exactly accounting for the
-- uses of each bound variable inside `t`.
--
-- These are the RULES, stated as data constructors. The meta-
-- theorems (progress, preservation, affine-preservation) are
-- stated in `Soundness.idr` and will be proved in weeks 3-12 of
-- Track F1.
--
-- Source of truth for the rule shapes:
--   * docs/spec.md §3.1, §3.6 (T-Var, T-Lam, T-App, T-Let)
--   * lib/quantity.ml — semiring operations and the "linear must
--     be used once" discipline
--
-- NOTE on one difference from docs/spec.md §3.6: the spec writes
-- the rules in a "shared Γ" style, which is *not* by itself
-- sufficient for affine preservation. The implementation in
-- `lib/quantity.ml` reconstructs the QTT accounting via a
-- post-hoc usage walk. Here we take the standard QTT
-- presentation with context splitting (ctxAdd) — this is the
-- formulation for which progress + preservation are provable,
-- and is equivalent to the implementation's usage-walk for the
-- Solo fragment. An explicit equivalence lemma is future work.

module Typing

import Quantity
import Syntax
import Context
import ContextLemmas

%default total

------------------------------------------------------------
-- Variable lookup: "x has type a with quantity 1 here, 0
-- elsewhere"
------------------------------------------------------------

||| `HasVar g n a` says the de Bruijn variable `n` has type `a`
||| in `g`, where `g` is a context that assigns quantity `One`
||| to position `n` and quantity `Zero` to every other position.
||| This is the T-Var rule in QTT: "using a variable once uses
||| it exactly once, and uses nothing else".
|||
||| `HVHere` carries an explicit `IsZero g` witness (rather than
||| pinning the tail to the literal `ctxZero g`) so the
||| substitution proof can analyse the surrounding erased context.
public export
data HasVar : Ctx -> Nat -> Ty -> Type where
  HVHere  : IsZero g -> HasVar (Snoc g a One) Z a
  HVThere : HasVar g n a
         -> HasVar (Snoc g b Zero) (S n) a

------------------------------------------------------------
-- The typing judgement
------------------------------------------------------------

||| `Has g t a` — in QTT context `g`, term `t` has type `a`.
||| Context splitting is explicit at T-App, T-Pair, T-Let, and
||| T-Case. T-Lam introduces a binder with a declared quantity.
public export
data Has : Ctx -> Tm -> Ty -> Type where

  ||| T-Var: variable use consumes quantity One at its position.
  THVar : HasVar g n a
       -> Has g (Var n) a

  ||| T-Unit: the unit term types in any all-zero context.
  THUnit : IsZero g -> Has g UnitT TUnit

  ||| T-Lam: introduce a binding with quantity `q` and type `a`,
  ||| type the body in the extended context. The resulting
  ||| function type `TArr q a b` records the quantity.
  |||
  ||| `q` and `a` are explicit RUNTIME arguments (not just erased
  ||| indices) so the metatheory proofs can analyse the binder when
  ||| they descend under it (Idris2 erases data-type implicit
  ||| indices, but the substitution/weakening proofs must rebuild
  ||| the crossed binder at runtime).
  THLam : (q : Q) -> (a : Ty)
       -> Has (Snoc g a q) t b
       -> Has g (Lam q a t) (TArr q a b)

  ||| T-App: the function is typed in `g1`, the argument in `g2`
  ||| scaled by the function's parameter quantity `q`, and the
  ||| whole application is typed in the pointwise sum
  ||| `g1 + q * g2`. `q` and the (UNscaled) argument context `g2` are
  ||| explicit runtime arguments: scaling is not invertible, so the
  ||| proofs need `g2` itself to split the argument's context.
  THApp : (q : Q) -> (g2 : Ctx)
       -> Has g1 t1 (TArr q a b)
       -> Has g2 t2 a
       -> AddCtx g1 (ctxScale q g2) g
       -> Has g (App t1 t2) b

  ||| T-Pair: MULTIPLICATIVE (tensor) product introduction. The two
  ||| components are typed in SEPARATE contexts `g1` and `g2`, and the
  ||| whole pair is typed in their pointwise sum `g1 + g2`, mirroring
  ||| `THApp`'s `AddCtx` split (with no scaling, since neither
  ||| component sits under a quantity annotation).
  |||
  ||| `g1` and `g2` are explicit RUNTIME arguments so the metatheory
  ||| proofs can rebuild the two summand contexts when they descend
  ||| into the components (the typed indices are erased).
  |||
  ||| Under this SPLIT rule preservation can no longer hold in the
  ||| SAME context for the projection-beta steps — `Fst (Pair v1 v2)`
  ||| steps to `v1`, which is typed only in the LEFT summand `g1`, a
  ||| sub-context of the whole `g`. This is exactly the affine reading:
  ||| preservation returns a reduct typed in a `Weaker` sub-context.
  THPair : (g1 : Ctx) -> (g2 : Ctx)
        -> Has g1 t1 a
        -> Has g2 t2 b
        -> AddCtx g1 g2 g
        -> Has g (Pair t1 t2) (TPair a b)

  ||| T-Fst: projection (single subterm, same context).
  THFst : Has g t (TPair a b)
       -> Has g (Fst t) a

  ||| T-Snd
  THSnd : Has g t (TPair a b)
       -> Has g (Snd t) b

  ||| T-Inl / T-Inr: sum introduction carries the other summand
  ||| as the annotation, matching `Syntax.idr`.
  THInl : Has g t a
       -> Has g (Inl b t) (TSum a b)
  THInr : Has g t b
       -> Has g (Inr a t) (TSum a b)

  ||| T-Case: the scrutinee is typed in `g1`; each branch binds
  ||| the injected value with quantity One and is typed in `g2`
  ||| extended with that binder. Branches must agree on `g2` and
  ||| on the result type. The whole `case` is typed in
  ||| `g1 + g2`.
  ||| `a` and `b` (the two summand types, hence the two branch
  ||| binder types) and the scrutinee context `g1` are explicit
  ||| runtime arguments so the proofs can descend under the branch
  ||| binders and run the substitution lemma at the case-beta step
  ||| (which substitutes the injected value, typed in `g1`).
  THCase : (a : Ty) -> (b : Ty) -> (g1 : Ctx)
        -> Has g1 t (TSum a b)
        -> Has (Snoc g2 a One) tL c
        -> Has (Snoc g2 b One) tR c
        -> AddCtx g1 g2 g
        -> Has g (Case t tL tR) c

  ||| T-Let: bind `t1` with declared quantity `q`. The RHS is
  ||| typed in `g1` and is then SCALED by `q` before being added
  ||| to `g2` (the body's context). This is the usual QTT let
  ||| rule and matches how `lib/typecheck.ml` threads quantities
  ||| through `let` bindings.
  ||| `q`, `a`, and the (UNscaled) RHS context `g1` are explicit
  ||| runtime arguments: the proofs need `g1` to split the RHS's
  ||| (scaled) context (scaling is not invertible). The body context
  ||| `g2` is recovered at runtime from the additive-split witness.
  THLet : (q : Q) -> (a : Ty) -> (g1 : Ctx)
       -> Has g1 t1 a
       -> Has (Snoc g2 a q) t2 b
       -> AddCtx (ctxScale q g1) g2 g
       -> Has g (Let q t1 t2) b
