-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- De Bruijn shifting and single-variable substitution for the Solo
-- core. These are the term-level operations that the small-step
-- reduction relation (Soundness.idr) uses in its beta/let/case rules,
-- and that the substitution lemma (SubstLemma.idr) proves type-safe.
--
-- The formulation is the standard call-by-value de Bruijn one
-- (cf. TAPL ch. 6): `shift c t` lifts every free variable index >= c
-- by one; `subst j s t` replaces variable `j` by `s` inside `t`,
-- decrementing the free variables above `j` and lifting `s` past each
-- binder it crosses. `subst0` is the index-0 instance used by every
-- beta reduction. Everything is structurally recursive, so `%default
-- total` holds with no `assert_total`.

module Subst

import Syntax

%default total

------------------------------------------------------------
-- Variable shifting
------------------------------------------------------------

||| `shiftVar c k` lifts a single de Bruijn index: indices `< c` are
||| left alone (they are bound by binders crossed since the cutoff),
||| indices `>= c` are incremented (they are free past the cutoff).
public export
shiftVar : (c : Nat) -> (k : Nat) -> Nat
shiftVar Z      k     = S k
shiftVar (S c)  Z     = Z
shiftVar (S c)  (S k) = S (shiftVar c k)

||| `shift c t` lifts every free variable of `t` whose index is `>= c`.
||| The cutoff increases by one under each binder.
public export
shift : (c : Nat) -> Tm -> Tm
shift c (Var k)      = Var (shiftVar c k)
shift c UnitT        = UnitT
shift c (Lam q a t)  = Lam q a (shift (S c) t)
shift c (App t1 t2)  = App (shift c t1) (shift c t2)
shift c (Pair t1 t2) = Pair (shift c t1) (shift c t2)
shift c (Fst t)      = Fst (shift c t)
shift c (Snd t)      = Snd (shift c t)
shift c (Inl b t)    = Inl b (shift c t)
shift c (Inr a t)    = Inr a (shift c t)
shift c (Case s l r) = Case (shift c s) (shift (S c) l) (shift (S c) r)
shift c (Let q e b)  = Let q (shift c e) (shift (S c) b)

------------------------------------------------------------
-- Single-variable substitution
------------------------------------------------------------

||| `substVar j k s` is the variable case of substitution: replace
||| index `k` while substituting for index `j` with `s`.
|||
||| * `k < j` — a more recently bound variable, unchanged.
||| * `k = j` — the substituted variable; yields `s` lifted past the
|||   `j` binders crossed to reach this occurrence.
||| * `k > j` — a free variable above the hole, decremented by one
|||   (its binder is gone).
|||
||| The single `shift 0` in the `S j / S k` case threads the
||| past-binder lifting through the recursion, so callers of `subst`
||| do *not* pre-shift `s` when descending under a binder.
public export
substVar : (j : Nat) -> (k : Nat) -> (s : Tm) -> Tm
substVar Z      Z      s = s
substVar Z      (S k)  s = Var k
substVar (S j)  Z      s = Var Z
substVar (S j)  (S k)  s = shift 0 (substVar j k s)

||| `subst j s t` replaces the free variable `j` in `t` by `s`.
public export
subst : (j : Nat) -> (s : Tm) -> Tm -> Tm
subst j s (Var k)      = substVar j k s
subst j s UnitT        = UnitT
subst j s (Lam q a t)  = Lam q a (subst (S j) s t)
subst j s (App t1 t2)  = App (subst j s t1) (subst j s t2)
subst j s (Pair t1 t2) = Pair (subst j s t1) (subst j s t2)
subst j s (Fst t)      = Fst (subst j s t)
subst j s (Snd t)      = Snd (subst j s t)
subst j s (Inl b t)    = Inl b (subst j s t)
subst j s (Inr a t)    = Inr a (subst j s t)
subst j s (Case sc l r) = Case (subst j s sc) (subst (S j) s l) (subst (S j) s r)
subst j s (Let q e b)  = Let q (subst j s e) (subst (S j) s b)

||| Substitute for the most-recently-bound variable (index 0). This is
||| the operation performed by every beta-style reduction.
public export
subst0 : Tm -> Tm -> Tm
subst0 body v = subst Z v body
