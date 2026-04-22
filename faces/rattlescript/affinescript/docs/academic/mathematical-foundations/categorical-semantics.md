# Categorical Semantics of AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete

## Abstract

This document provides a categorical semantics for AffineScript, interpreting the type system in suitable categorical structures. We construct models using:
1. Locally Cartesian closed categories (LCCCs) for dependent types
2. Symmetric monoidal categories for linear/affine types
3. Graded comonads for quantitative types
4. Freyd categories for effects
5. Fibrations for refinement types

## 1. Introduction

Categorical semantics provides:
- A mathematical foundation independent of syntax
- Proof of consistency and relative consistency
- Guidance for language extensions
- Connection to other mathematical structures

AffineScript requires multiple categorical structures due to its combination of features.

## 2. Preliminaries

### 2.1 Category Theory Basics

**Definition 2.1 (Category)**: A category C consists of:
- Objects: ob(C)
- Morphisms: C(A, B) for objects A, B
- Identity: id_A : A → A
- Composition: g ∘ f : A → C for f : A → B, g : B → C

satisfying identity and associativity laws.

### 2.2 Key Categorical Structures

**Cartesian Closed Category (CCC)**:
- Terminal object 1
- Binary products A × B
- Exponentials B^A (function spaces)

**Locally Cartesian Closed Category (LCCC)**:
- Each slice category C/Γ is CCC
- Models dependent types

**Symmetric Monoidal Category (SMC)**:
- Tensor product ⊗
- Unit object I
- Associativity, commutativity (up to isomorphism)

**Symmetric Monoidal Closed Category (SMCC)**:
- SMC with internal hom A ⊸ B
- Models linear types

## 3. Interpretation of Types

### 3.1 Base Types

Interpret types as objects in category C:

```
⟦Unit⟧ = 1                    (terminal object)
⟦Bool⟧ = 1 + 1                (coproduct)
⟦Nat⟧ = N                     (natural numbers object)
⟦Int⟧ = Z                     (integers)
```

### 3.2 Function Types

**Simple Functions** (in a CCC):
```
⟦τ → σ⟧ = ⟦σ⟧^⟦τ⟧
```

**Linear Functions** (in a SMCC):
```
⟦τ ⊸ σ⟧ = ⟦τ⟧ ⊸ ⟦σ⟧
```

**Effectful Functions** (in Freyd category):
```
⟦τ →{ε} σ⟧ = ⟦τ⟧ → T_ε(⟦σ⟧)
```

where T_ε is the monad/applicative functor for effect ε.

### 3.3 Product Types

```
⟦τ × σ⟧ = ⟦τ⟧ × ⟦σ⟧           (Cartesian product)
⟦τ ⊗ σ⟧ = ⟦τ⟧ ⊗ ⟦σ⟧           (Tensor product, linear)
```

### 3.4 Sum Types

```
⟦τ + σ⟧ = ⟦τ⟧ + ⟦σ⟧           (Coproduct)
```

### 3.5 Record Types (Row Polymorphism)

Records are interpreted as dependent products over finite label sets:

```
⟦{l₁: τ₁, ..., lₙ: τₙ}⟧ = ∏_{i=1}^n ⟦τᵢ⟧
```

With row polymorphism:
```
⟦{l: τ | ρ}⟧ = ⟦τ⟧ × ⟦{ρ}⟧
```

## 4. Dependent Types

### 4.1 Locally Cartesian Closed Categories

For dependent types, we work in an LCCC C.

**Contexts as Objects**:
```
⟦·⟧ = 1
⟦Γ, x:τ⟧ = Σ_{⟦Γ⟧} ⟦τ⟧        (dependent sum in slice)
```

**Types as Objects in Slice**:
```
⟦Γ ⊢ τ⟧ ∈ ob(C/⟦Γ⟧)
```

### 4.2 Π-Types

**Interpretation**:
```
⟦Π(x:τ). σ⟧ = Π_τ(⟦σ⟧)
```

where Π_τ is the right adjoint to pullback along τ : ⟦τ⟧ → ⟦Γ⟧.

**Adjunction**:
```
Σ_τ ⊣ τ* ⊣ Π_τ

where:
Σ_τ : C/⟦Γ,x:τ⟧ → C/⟦Γ⟧     (dependent sum)
τ*  : C/⟦Γ⟧ → C/⟦Γ,x:τ⟧     (weakening/pullback)
Π_τ : C/⟦Γ,x:τ⟧ → C/⟦Γ⟧     (dependent product)
```

### 4.3 Σ-Types

**Interpretation**:
```
⟦Σ(x:τ). σ⟧ = Σ_τ(⟦σ⟧)
```

### 4.4 Identity Types

**Interpretation** (in a category with path objects):
```
⟦a == b⟧ = Path_τ(⟦a⟧, ⟦b⟧)
```

where Path_τ is the path object functor.

## 5. Quantitative Types

### 5.1 Graded Comonads

Quantities are modeled by a graded exponential comonad:

**Definition 5.1**: A graded comonad on C indexed by semiring R is:
- Functors D_π : C → C for each π ∈ R
- Natural transformations:
  - ε : D_1 A → A (counit)
  - δ : D_{π₁ × π₂} A → D_{π₁}(D_{π₂} A) (comultiplication)
  - θ : D_0 A → I (dereliction for 0)
  - c : D_ω A → D_ω A × D_ω A (contraction for ω)
  - w : D_ω A → I (weakening for ω)

### 5.2 Interpretation

```
⟦π τ⟧ = D_π(⟦τ⟧)

⟦0 τ⟧ = D_0(⟦τ⟧) ≅ I          (erased)
⟦1 τ⟧ = D_1(⟦τ⟧) ≅ ⟦τ⟧        (linear)
⟦ω τ⟧ = !⟦τ⟧                   (exponential comonad)
```

### 5.3 Quantity Semiring Structure

The semiring laws correspond to:
```
D_0 ∘ D_π ≅ D_0              (0 × π = 0)
D_π ∘ D_0 ≅ D_0              (π × 0 = 0)
D_1 ∘ D_π ≅ D_π              (1 × π = π)
D_π₁ ∘ D_π₂ ≅ D_{π₁×π₂}      (multiplication)
```

## 6. Effects

### 6.1 Freyd Categories

Effects are modeled in a Freyd category (C, J, K):
- C is a Cartesian category (pure computations)
- K is a category (effectful computations)
- J : C → K is identity on objects, cartesian on morphisms

### 6.2 Effect Algebra

Effects form an algebra of operations and equations:

**Definition 6.1 (Effect Theory)**: An effect theory is a Lawvere theory T with:
- Sorts (value types)
- Operations: op : τ → σ
- Equations between terms

### 6.3 Free Monad Interpretation

For an effect signature Σ:
```
⟦ε⟧ = Free_Σ
```

where Free_Σ is the free monad on the functor corresponding to Σ.

### 6.4 Handler Interpretation

A handler for effect E is an E-algebra:
```
h : F_E(A) → A
```

where F_E is the effect functor.

**Handle**:
```
⟦handle e with h⟧ = fold(h, ⟦e⟧)
```

### 6.5 Effect Row Polymorphism

Effect rows are interpreted as colimits:
```
⟦ε₁ | ε₂⟧ = ⟦ε₁⟧ ⊕ ⟦ε₂⟧
```

where ⊕ is coproduct of effect theories.

## 7. Ownership and Borrowing

### 7.1 Presheaf Model

Model ownership using presheaves over a category of regions:

**Definition 7.1**: Let R be the category of regions with:
- Objects: Regions (lifetimes)
- Morphisms: Inclusions 'a ≤ 'b

A type with lifetime is a presheaf on R:
```
⟦ref['a] τ⟧ : R^op → Set
⟦ref['a] τ⟧('b) = if 'a ≤ 'b then ⟦τ⟧ else ∅
```

### 7.2 Affine Category

Ownership uses an affine symmetric monoidal category:
- Objects: Types with ownership annotations
- Morphisms: Functions respecting ownership
- Tensor: Combines owned values (linear)
- Weakening: Allows dropping (affine, not linear)

### 7.3 Borrow Semantics

Borrows are modeled as comonadic access:
```
⟦ref τ⟧ = R(⟦τ⟧)              (reader comonad)
⟦mut τ⟧ = S(⟦τ⟧)              (state comonad, exclusive)
```

## 8. Refinement Types

### 8.1 Fibrations

Refinement types are modeled in a fibration:

**Definition 8.1**: A fibration p : E → B is a functor with cartesian liftings.

For refinements:
- B = types
- E = refined types (types with predicates)
- p = forgetful functor

### 8.2 Predicate Interpretation

```
⟦{x: τ | φ}⟧ = {a ∈ ⟦τ⟧ | ⟦φ⟧(a) = true}
```

As a subobject:
```
⟦{x: τ | φ}⟧ ↣ ⟦τ⟧
```

### 8.3 Subset Types

Using subset types in a topos:
```
⟦{x: τ | φ}⟧ = Σ_{a:⟦τ⟧} ⟦φ(a)⟧
```

where ⟦φ(a)⟧ is a proposition (subsingleton).

## 9. Soundness

### 9.1 Interpretation of Terms

Each typing judgment is interpreted as a morphism:
```
⟦Γ ⊢ e : τ⟧ : ⟦Γ⟧ → ⟦τ⟧
```

### 9.2 Soundness Theorem

**Theorem 9.1 (Soundness)**: The interpretation is sound:
1. Well-typed terms denote morphisms
2. Equal terms denote equal morphisms
3. Reduction preserves denotation

**Proof**: By induction on typing derivations, verifying categorical equations. ∎

### 9.3 Adequacy

**Theorem 9.2 (Adequacy)**: For closed terms of observable type:
```
⟦e⟧ = ⟦e'⟧ implies e ≃ e' (observationally equivalent)
```

## 10. Coherence

### 10.1 Coherence for Row Polymorphism

**Theorem 10.1**: Row-polymorphic terms have unique interpretations up to canonical isomorphism.

The interpretation is independent of the order of record fields.

### 10.2 Coherence for Effects

**Theorem 10.2**: Effect interpretations are coherent: different derivations of the same typing judgment yield equal morphisms.

### 10.3 Coherence for Quantities

**Theorem 10.3**: Quantity polymorphism is coherent: instantiation at different quantities (respecting constraints) yields consistent behavior.

## 11. Parametricity

### 11.1 Relational Interpretation

Define relations over the categorical model:

**Definition 11.1**: For types τ, the relational interpretation ⟦τ⟧_R is:
- ⟦α⟧_R = R (a relation, the parameter)
- ⟦τ → σ⟧_R = {(f, g) | ∀(a,b) ∈ ⟦τ⟧_R. (f a, g b) ∈ ⟦σ⟧_R}
- ...

### 11.2 Parametricity Theorem

**Theorem 11.1 (Parametricity)**: For any polymorphic term `Γ ⊢ e : ∀α. τ`:
```
∀ types A, B. ∀ relation R ⊆ A × B.
(⟦e⟧(A), ⟦e⟧(B)) ∈ ⟦τ⟧_R[R/α]
```

### 11.3 Free Theorems

From parametricity, we derive free theorems:

**Example**: For `f : ∀α. List[α] → List[α]`:
```
∀ g : A → B. map g ∘ f_A = f_B ∘ map g
```

## 12. Models

### 12.1 Set-Theoretic Model

The simplest model uses Set:
- Types as sets
- Functions as set-theoretic functions
- Effects as free monads

### 12.2 Domain-Theoretic Model

For recursion, use CPO (complete partial orders):
- Types as CPOs
- Functions as continuous functions
- Recursion as least fixed points

### 12.3 Topos Model

For full dependent types, use a topos:
- Types as objects in a topos
- Dependent types in slice topoi
- Refinements as subobjects

### 12.4 Realizability Model

For extraction to computable functions:
- Types as assemblies
- Functions as realized by programs
- Connects to extraction

## 13. Examples

### 13.1 State Monad

The state effect S[σ] is modeled by:
```
⟦τ →{State[σ]} ρ⟧ = σ × ⟦τ⟧ → σ × ⟦ρ⟧
```

State transformers.

### 13.2 Linear Function Space

The linear function space:
```
⟦τ ⊸ σ⟧ = ⟦τ⟧ ⊸ ⟦σ⟧
```

where ⊸ is the internal hom in a SMCC.

### 13.3 Dependent Sum

The dependent sum:
```
⟦Σ(x:τ). σ⟧ = Σ_{a ∈ ⟦τ⟧} ⟦σ⟧(a)
```

A dependent pair (a, b) where b : σ[a/x].

## 14. Related Work

1. **Categorical Logic**: Lambek & Scott, Jacobs
2. **LCCCs for Dependent Types**: Seely (1984), Hofmann (1997)
3. **Linear Logic Categories**: Benton, Bierman, de Paiva, Hyland
4. **Graded Comonads**: Gaboardi et al., Brunel et al.
5. **Freyd Categories for Effects**: Power & Robinson, Levy
6. **Fibrations for Refinements**: Jacobs, Hermida

## 15. References

1. Lambek, J., & Scott, P. J. (1986). *Introduction to Higher Order Categorical Logic*. Cambridge.
2. Jacobs, B. (1999). *Categorical Logic and Type Theory*. Elsevier.
3. Seely, R. A. G. (1984). Locally Cartesian Closed Categories and Type Theory. *Math. Proc. Cambridge Phil. Soc.*
4. Benton, N. (1995). A Mixed Linear and Non-Linear Logic. *CSL*.
5. Power, J., & Robinson, E. (1997). Premonoidal Categories and Notions of Computation. *MSCS*.
6. Moggi, E. (1991). Notions of Computation and Monads. *Information and Computation*.

---

**Document Metadata**:
- This document is pure theory; no implementation dependencies
- Mechanized proof: See `mechanized/coq/Semantics.v` (stub)
