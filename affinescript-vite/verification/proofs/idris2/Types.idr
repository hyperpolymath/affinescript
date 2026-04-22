-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- Typing Proof: Core data type well-formedness
-- Template — replace with your project's core types.
-- All proofs MUST be constructive (no b_me, no a_total).

module Types

%default total

||| Example: A bounded natural number (0 to max).
||| Replace with your project's core types.
public export
record Bounded (max : Nat) where
  constructor MkBounded
  value : Nat
  {auto 0 inBounds : LTE value max}

||| Proof that a Bounded value is always <= max.
export
boundedLeMax : (b : Bounded max) -> LTE b.value max
boundedLeMax b = b.inBounds

||| Proof that zero is always a valid Bounded value.
export
zeroIsBounded : {max : Nat} -> Bounded (S max)
zeroIsBounded = MkBounded 0

||| Example: A non-empty list with a compile-time guarantee.
public export
data NonEmpty : List a -> Type where
  IsNonEmpty : NonEmpty (x :: xs)

||| Proof that cons always produces a non-empty list.
export
consIsNonEmpty : (x : a) -> (xs : List a) -> NonEmpty (x :: xs)
consIsNonEmpty _ _ = IsNonEmpty
