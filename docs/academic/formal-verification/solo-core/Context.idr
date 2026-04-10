-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- QTT-style typing contexts for the Solo core.
--
-- A context `Ctx` is a snoc-list of `(Ty, Q)` entries: each bound
-- variable carries both its type and its current available
-- quantity. Context addition `ctxAdd` adds quantities pointwise;
-- context scaling `ctxScale q Γ` multiplies every entry's
-- quantity by `q`.
--
-- The typing judgement in Typing.idr uses these operations to
-- SPLIT the input context across subterms in T-App, T-Pair,
-- T-Let, T-Case — this is the standard QTT context discipline
-- and is what makes affine accounting work.
--
-- Relationship to lib/quantity.ml: the OCaml implementation does
-- NOT maintain a QTT context; it walks the term after type
-- checking and accumulates `usage` counts, then checks them
-- against the declared quantities at the end
-- (`check_function_quantities`). This module formalises the
-- static QTT view that a usage-walk ought to agree with. An
-- equivalence lemma between the two (static splitting vs.
-- post-hoc counting) is future work.

module Context

import Quantity
import Syntax

%default total

------------------------------------------------------------
-- Context representation
------------------------------------------------------------

||| A snoc-list context: `Empty` at the base, `Snoc g a q` pushes
||| a binding with type `a` and quantity `q` on top.
public export
data Ctx : Type where
  Empty : Ctx
  Snoc  : Ctx -> Ty -> Q -> Ctx

%name Ctx g, g1, g2, d

------------------------------------------------------------
-- Length (for well-scopedness conditions later)
------------------------------------------------------------

public export
ctxLen : Ctx -> Nat
ctxLen Empty        = 0
ctxLen (Snoc g _ _) = S (ctxLen g)

------------------------------------------------------------
-- Pointwise quantity addition on contexts
------------------------------------------------------------

||| Pointwise-add two contexts. Defined only when the two
||| contexts have the same shape (same length and same types);
||| on a mismatch we return `Nothing`. In a well-typed derivation
||| the contexts will always agree.
public export
ctxAdd : Ctx -> Ctx -> Maybe Ctx
ctxAdd Empty Empty = Just Empty
ctxAdd (Snoc g1 a1 q1) (Snoc g2 a2 q2) =
  case ctxAdd g1 g2 of
    Nothing => Nothing
    Just g  =>
      -- We DO NOT check type equality here; Typing.idr will
      -- only ever combine contexts whose shapes match by
      -- construction. A decidable equality on Ty would let us
      -- tighten this; deferred to a later pass.
      Just (Snoc g a1 (qAdd q1 q2))
ctxAdd _ _ = Nothing

------------------------------------------------------------
-- Scalar multiplication of contexts
------------------------------------------------------------

||| Multiply every quantity in the context by `q`. Used in the
||| T-App rule (when an argument is used under a function whose
||| parameter quantity is `q`) and in the zero-context rule for
||| T-Var.
public export
ctxScale : Q -> Ctx -> Ctx
ctxScale _ Empty            = Empty
ctxScale q (Snoc g a qEntry) = Snoc (ctxScale q g) a (qMul q qEntry)

------------------------------------------------------------
-- The all-zero context derived from a shape
------------------------------------------------------------

||| Given any context, produce the same-shape context with every
||| quantity replaced by `Zero`. This is the "nothing is used"
||| context that appears in T-Unit and as the non-consuming
||| siblings of T-Var.
public export
ctxZero : Ctx -> Ctx
ctxZero Empty        = Empty
ctxZero (Snoc g a _) = Snoc (ctxZero g) a Zero
