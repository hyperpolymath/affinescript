# AffineScript Agda Formalization

**Status**: Stub / Planned

This directory will contain the mechanized Agda proof development for AffineScript's metatheory.

## Planned Structure

```
agda/
├── AffineScript.agda          -- Main module
├── Syntax/
│   ├── Types.agda             -- Type syntax
│   ├── Terms.agda             -- Term syntax
│   └── Contexts.agda          -- Context definitions
├── Typing/
│   ├── Rules.agda             -- Typing rules
│   ├── Quantities.agda        -- QTT
│   └── Decidable.agda         -- Decidable type checking
├── Semantics/
│   ├── Reduction.agda         -- Small-step semantics
│   ├── Values.agda            -- Value predicates
│   └── Evaluation.agda        -- Big-step evaluator
├── Metatheory/
│   ├── Substitution.agda      -- Substitution lemmas
│   ├── Progress.agda          -- Progress theorem
│   └── Preservation.agda      -- Preservation theorem
├── Effects/
│   ├── Signatures.agda        -- Effect signatures
│   ├── Handlers.agda          -- Handler typing
│   └── Safety.agda            -- Effect safety
└── Ownership/
    ├── Model.agda             -- Ownership model
    └── Borrowing.agda         -- Borrow checking
```

## Dependencies

- Agda 2.6.4+
- agda-stdlib 2.0+

## Building

```bash
# Check all modules
agda --safe AffineScript.agda

# Generate HTML documentation
agda --html AffineScript.agda

# Check with flags
agda --without-K --safe AffineScript.agda
```

## TODO

### Phase 1: Core Language

- [ ] Intrinsically-typed syntax
- [ ] Substitution via categories
- [ ] Progress and preservation

### Phase 2: Quantities

- [ ] Semiring structure
- [ ] Graded contexts
- [ ] Quantity erasure

### Phase 3: Effects

- [ ] Effect algebras
- [ ] Free monad interpretation
- [ ] Handler correctness

## Example Structure

```agda
-- Syntax/Types.agda
module Syntax.Types where

open import Data.Nat using (ℕ)
open import Data.Fin using (Fin)

-- Types indexed by number of free type variables
data Ty (n : ℕ) : Set where
  `Unit  : Ty n
  `Bool  : Ty n
  `Int   : Ty n
  `_⇒_   : Ty n → Ty n → Ty n
  `∀_    : Ty (suc n) → Ty n
  `Var   : Fin n → Ty n

-- Syntax/Terms.agda
module Syntax.Terms where

open import Syntax.Types

-- Well-scoped terms
data Term (n : ℕ) (Γ : Vec (Ty 0) n) : Ty 0 → Set where
  `var  : ∀ {τ} (x : Fin n) → lookup Γ x ≡ τ → Term n Γ τ
  `lam  : ∀ {τ₁ τ₂} → Term (suc n) (τ₁ ∷ Γ) τ₂ → Term n Γ (τ₁ `⇒ τ₂)
  `app  : ∀ {τ₁ τ₂} → Term n Γ (τ₁ `⇒ τ₂) → Term n Γ τ₁ → Term n Γ τ₂
  `unit : Term n Γ `Unit
  `true : Term n Γ `Bool
  `false : Term n Γ `Bool

-- Metatheory/Progress.agda
module Metatheory.Progress where

open import Syntax.Terms
open import Semantics.Reduction
open import Semantics.Values

data Progress {τ : Ty 0} (e : Term 0 [] τ) : Set where
  step : ∀ {e'} → e ⟶ e' → Progress e
  done : Value e → Progress e

progress : ∀ {τ} (e : Term 0 [] τ) → Progress e
progress (`var x p) with () ← x
progress (`lam e) = done V-lam
progress (`app e₁ e₂) with progress e₁
... | step s₁ = step (ξ-app-L s₁)
... | done V-lam with progress e₂
...   | step s₂ = step (ξ-app-R V-lam s₂)
...   | done v₂ = step (β-lam v₂)
progress `unit = done V-unit
progress `true = done V-true
progress `false = done V-false

-- Metatheory/Preservation.agda
module Metatheory.Preservation where

preservation : ∀ {τ} {e e' : Term 0 [] τ}
             → e ⟶ e'
             → Term 0 [] τ  -- e' has the same type (trivial with intrinsic typing)
preservation {e' = e'} _ = e'

-- With intrinsically-typed syntax, preservation is "free"!
```

## Intrinsic vs Extrinsic Typing

Agda is particularly well-suited for **intrinsically-typed** representations where:
- Terms are indexed by their types
- Ill-typed terms are not representable
- Preservation is automatic

This makes many proofs trivial but requires more effort upfront.

## Advantages of Agda

1. **Dependent types**: Natural for indexed syntax
2. **Pattern matching**: Clean proof style
3. **Mixfix operators**: Readable syntax
4. **Unicode support**: Mathematical notation
5. **Cubical Agda**: Univalence if needed

## References

1. Programming Language Foundations in Agda (Wadler et al.)
2. Agda standard library documentation
3. Type Theory and Formal Proof (Nederpelt & Geuvers)
