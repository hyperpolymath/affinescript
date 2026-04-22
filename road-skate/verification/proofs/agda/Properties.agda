-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- Agda Proof Template: Inductive and coinductive properties
-- Replace with your project's domain-specific proofs.
-- All proofs must be total (no postulate, no {-# TERMINATING #-}).

module Properties where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤_; z≤n; s≤s; _<_)
open import Data.Nat.Properties using (+-comm; +-assoc; ≤-refl; ≤-trans)
open import Data.List using (List; []; _∷_; length; _++_)
open import Data.List.Properties using (length-++ )
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; sym; trans)

-- Example: Proof that list append preserves total length
-- Replace with your project's domain proofs.

append-length : ∀ {A : Set} (xs ys : List A) →
  length (xs ++ ys) ≡ length xs + length ys
append-length xs ys = length-++ xs

-- Example: Monotonicity proof template
-- Use for state machines, confidence scores, trust levels
record Monotone {A : Set} (_≤A_ : A → A → Set) (f : A → A) : Set where
  field
    preserves : ∀ {x y} → x ≤A y → f x ≤A f y

-- Example: Idempotence proof template
-- Use for normalisation, deduplication, formatting
record Idempotent {A : Set} (_≡A_ : A → A → Set) (f : A → A) : Set where
  field
    idem : ∀ (x : A) → f (f x) ≡A f x

-- Example: Natural number successor is monotone
suc-monotone : Monotone _≤_ suc
suc-monotone = record { preserves = s≤s }
