# AffineScript Lean 4 Formalization

**Status**: Stub / Planned

This directory will contain the mechanized Lean 4 proof development for AffineScript's metatheory.

## Planned Structure

```
lean/
├── AffineScript.lean      -- Main entry point
├── Syntax.lean            -- Abstract syntax definitions
├── Typing.lean            -- Typing rules
├── Quantities.lean        -- QTT semiring
├── Reduction.lean         -- Operational semantics
├── Progress.lean          -- Progress theorem
├── Preservation.lean      -- Preservation theorem
├── Effects.lean           -- Effect system
├── Ownership.lean         -- Ownership model
├── Rows.lean              -- Row polymorphism
└── lakefile.lean          -- Build configuration
```

## Dependencies

- Lean 4.x
- Mathlib4 (for mathematical structures)
- Std4 (standard library)

## Building

```bash
# Initialize lake project
lake init AffineScript

# Build
lake build

# Check proofs
lake env lean AffineScript.lean
```

## TODO

### Phase 1: Core Definitions

- [ ] Syntax with well-scoped indices
- [ ] Typing judgments as inductive families
- [ ] Decidable type checking

### Phase 2: Metatheory

- [ ] Substitution lemmas
- [ ] Progress and preservation
- [ ] Type safety corollary

### Phase 3: Advanced Features

- [ ] Quantitative type theory
- [ ] Effect typing
- [ ] Ownership verification

## Example Structure

```lean
-- Syntax.lean
inductive Ty : Type where
  | unit : Ty
  | bool : Ty
  | int : Ty
  | arrow : Ty → Ty → Ty
  | forall_ : Ty → Ty
  deriving Repr, DecidableEq

inductive Expr : Type where
  | var : Nat → Expr
  | lam : Ty → Expr → Expr
  | app : Expr → Expr → Expr
  | tLam : Expr → Expr
  | tApp : Expr → Ty → Expr
  deriving Repr

-- Typing.lean
inductive HasType : List Ty → Expr → Ty → Prop where
  | var : ∀ {Γ x τ},
      Γ.get? x = some τ →
      HasType Γ (.var x) τ
  | lam : ∀ {Γ τ₁ τ₂ e},
      HasType (τ₁ :: Γ) e τ₂ →
      HasType Γ (.lam τ₁ e) (.arrow τ₁ τ₂)
  | app : ∀ {Γ e₁ e₂ τ₁ τ₂},
      HasType Γ e₁ (.arrow τ₁ τ₂) →
      HasType Γ e₂ τ₁ →
      HasType Γ (.app e₁ e₂) τ₂

-- Progress.lean
inductive Value : Expr → Prop where
  | lam : Value (.lam τ e)
  | tLam : Value (.tLam e)

inductive Step : Expr → Expr → Prop where
  | beta : ∀ {τ e v},
      Value v →
      Step (.app (.lam τ e) v) (subst v e)
  | appL : ∀ {e₁ e₁' e₂},
      Step e₁ e₁' →
      Step (.app e₁ e₂) (.app e₁' e₂)
  | appR : ∀ {v e₂ e₂'},
      Value v →
      Step e₂ e₂' →
      Step (.app v e₂) (.app v e₂')

theorem progress {e τ} (h : HasType [] e τ) :
    Value e ∨ ∃ e', Step e e' := by
  induction h with
  | var h => simp at h
  | lam _ => left; constructor
  | app h₁ h₂ ih₁ ih₂ =>
    right
    cases ih₁ with
    | inl hv₁ =>
      cases hv₁ with
      | lam =>
        cases ih₂ with
        | inl hv₂ => exact ⟨_, .beta hv₂⟩
        | inr ⟨e₂', hs₂⟩ => exact ⟨_, .appR .lam hs₂⟩
    | inr ⟨e₁', hs₁⟩ => exact ⟨_, .appL hs₁⟩

-- Preservation.lean
theorem preservation {Γ e e' τ}
    (ht : HasType Γ e τ) (hs : Step e e') :
    HasType Γ e' τ := by
  induction hs generalizing τ with
  | beta hv =>
    cases ht with
    | app h₁ h₂ =>
      cases h₁ with
      | lam hbody => exact substitution_lemma hbody h₂
  | appL _ ih =>
    cases ht with
    | app h₁ h₂ => exact .app (ih h₁) h₂
  | appR _ _ ih =>
    cases ht with
    | app h₁ h₂ => exact .app h₁ (ih h₂)
```

## Advantages of Lean 4

1. **Decidable type checking**: Can compute types
2. **Metaprogramming**: Powerful tactic framework
3. **Performance**: Compiled tactics
4. **Interoperability**: Can call external tools

## References

1. Theorem Proving in Lean 4
2. Mathematics in Lean
3. Lean 4 Metaprogramming
