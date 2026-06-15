-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- de Bruijn shifting and substitution for the Solo core.
--
-- Convention (matching Typing.idr's HasVar): `Var Z` is the
-- innermost (top-of-snoc) binding; descending under a binder
-- increments indices.
--
--   * `shift d c t`  — add `d` to every `Var` index >= cutoff `c`.
--   * `subst j s t`  — replace `Var j` by `s`, decrement `Var k`
--                      for `k > j`, leave `Var k` for `k < j`.
--                      Going under a binder bumps `j` and shifts
--                      `s` up by one.
--   * `subst0 t v`   — the top-level beta substitution `t[v/0]`.

module Subst

import Syntax

%default total

------------------------------------------------------------
-- Index shifting helpers
------------------------------------------------------------

||| `shiftVar d c n`: bump a single de Bruijn index `n` by `d` if it
||| is at or above the cutoff `c`.
|||
||| Defined by structural recursion on the cutoff and index together
||| (rather than via a `Bool` test) so that it reduces cleanly when
||| the cutoff is built up `Snoc`-by-`Snoc` in the proofs, and so
||| `shiftVar 1 c` computes a literal `S _` definitionally.
public export
shiftVar : Nat -> Nat -> Nat -> Nat
shiftVar d Z     n     = d + n
shiftVar d (S c) Z     = Z
shiftVar d (S c) (S n) = S (shiftVar d c n)

------------------------------------------------------------
-- Term shifting
------------------------------------------------------------

||| `shift d c t` — increment every free variable of `t` (index
||| >= `c`) by `d`. Binders raise the cutoff.
public export
shift : Nat -> Nat -> Tm -> Tm
shift d c (Var n)       = Var (shiftVar d c n)
shift d c UnitT         = UnitT
shift d c (Lam q a t)   = Lam q a (shift d (S c) t)
shift d c (App t1 t2)   = App (shift d c t1) (shift d c t2)
shift d c (Pair t1 t2)  = Pair (shift d c t1) (shift d c t2)
shift d c (Fst t)       = Fst (shift d c t)
shift d c (Snd t)       = Snd (shift d c t)
shift d c (Inl b t)     = Inl b (shift d c t)
shift d c (Inr a t)     = Inr a (shift d c t)
shift d c (Case t tL tR) =
  Case (shift d c t) (shift d (S c) tL) (shift d (S c) tR)
shift d c (Let q t1 t2) = Let q (shift d c t1) (shift d (S c) t2)

------------------------------------------------------------
-- Substitution at an index
------------------------------------------------------------

||| `substVar j s n` — the result of substituting `s` for the
||| variable `j` inside the single occurrence `Var n`.
|||
||| `n < j`  : untouched.
||| `n == j` : becomes `s`.
||| `n > j`  : decremented (the bound variable disappears).
public export
substVar : Nat -> Tm -> Nat -> Tm
substVar Z     s Z     = s
substVar Z     s (S k) = Var k
substVar (S j) s Z     = Var Z
substVar (S j) s (S k) = shift 1 0 (substVar j s k)

||| `subst j s t` — substitute `s` for the variable with index `j`
||| in `t`, lowering the indices above `j`.
|||
||| The substituted term `s` is NOT pre-shifted when descending
||| under a binder: instead `substVar` re-shifts the substituted
||| value by exactly the binder-depth at which it lands (each `S/S`
||| step in `substVar` applies one `shift 1 0`). This keeps the two
||| operations in lock-step and avoids double-counting.
public export
subst : Nat -> Tm -> Tm -> Tm
subst j s (Var n)        = substVar j s n
subst j s UnitT          = UnitT
subst j s (Lam q a t)    = Lam q a (subst (S j) s t)
subst j s (App t1 t2)    = App (subst j s t1) (subst j s t2)
subst j s (Pair t1 t2)   = Pair (subst j s t1) (subst j s t2)
subst j s (Fst t)        = Fst (subst j s t)
subst j s (Snd t)        = Snd (subst j s t)
subst j s (Inl b t)      = Inl b (subst j s t)
subst j s (Inr a t)      = Inr a (subst j s t)
subst j s (Case t tL tR) =
  Case (subst j s t) (subst (S j) s tL) (subst (S j) s tR)
subst j s (Let q t1 t2)  =
  Let q (subst j s t1) (subst (S j) s t2)

||| Top-level beta substitution `t[v/0]`.
public export
subst0 : Tm -> Tm -> Tm
subst0 t v = subst Z v t
