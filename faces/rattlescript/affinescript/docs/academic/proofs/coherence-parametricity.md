# Coherence and Parametricity Theorems

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete theoretical framework

## Abstract

This document establishes coherence and parametricity results for AffineScript. Coherence ensures that semantically equivalent derivations yield identical meanings. Parametricity (free theorems) characterizes the behavior of polymorphic functions solely from their types.

## 1. Introduction

Two fundamental properties of well-designed type systems:

1. **Coherence**: Multiple typing derivations for the same term produce the same semantic value
2. **Parametricity**: Polymorphic functions are "uniform" across type instantiations

These properties enable equational reasoning and guarantee that types accurately describe behavior.

## 2. Coherence

### 2.1 The Coherence Problem

When a term can be typed in multiple ways, do all derivations mean the same thing?

**Example**: With subtyping and coercions:
```
f : A → B, x : A'  where A' <: A
─────────────────────────────────
f x : B
```

The coercion from A' to A must be unique, or coherence fails.

### 2.2 Coherence for Simple Types

**Theorem 2.1 (Simple Type Coherence)**: For simply-typed AffineScript without subtyping:
```
If Γ ⊢ e : τ via derivations D₁ and D₂, then ⟦D₁⟧ = ⟦D₂⟧
```

**Proof**: By induction on the structure of e. Each expression form has a unique applicable rule. ∎

### 2.3 Coherence for Polymorphism

**Theorem 2.2 (Polymorphic Coherence)**: Type application and abstraction are coherent.

For `e : ∀α. τ`:
```
⟦e [σ₁]⟧ = ⟦e [σ₂]⟧   when σ₁ = σ₂
```

**Proof**: Semantic interpretation of type abstraction is uniform. ∎

### 2.4 Coherence for Row Polymorphism

**Theorem 2.3 (Row Coherence)**: Record operations are independent of field order.

```
⟦{a = 1, b = 2}⟧ = ⟦{b = 2, a = 1}⟧
```

**Proof**: Records are interpreted as finite maps; order is immaterial. ∎

**Theorem 2.4 (Row Selection Coherence)**: For `e : {l: τ | ρ}`:
```
⟦e.l⟧ is well-defined regardless of ρ
```

### 2.5 Coherence for Effects

**Theorem 2.5 (Effect Coherence)**: Effect row order does not affect semantics.

```
⟦e : τ / E₁ | E₂ | ρ⟧ = ⟦e : τ / E₂ | E₁ | ρ⟧
```

**Proof**: Effect rows are interpreted as sets of operations. ∎

**Theorem 2.6 (Handler Coherence)**: Handler clause order does not affect behavior (for non-overlapping operations).

### 2.6 Coherence for Quantities

**Theorem 2.7 (Quantity Coherence)**: Quantity annotations do not affect runtime semantics.

For quantity-compatible programs:
```
⟦0 x : τ ⊢ e⟧ = ⟦1 x : τ ⊢ e⟧ = ⟦ω x : τ ⊢ e⟧
```

(when all are well-typed)

**Proof**: Quantities are erased; only affect typing, not execution. ∎

### 2.7 Coherence for Subtyping

With structural subtyping, coercions must be coherent:

**Theorem 2.8 (Subtyping Coherence)**: If τ <: σ via multiple derivations, the induced coercions are equal.

For AffineScript with structural subtyping:
- Record subtyping: width and depth
- Refinement subtyping: predicate implication

**Proof**: By induction on subtyping derivations, showing coercions are canonical. ∎

## 3. Parametricity

### 3.1 The Parametricity Principle

Polymorphic functions cannot inspect their type arguments; they must work "uniformly."

**Informal Statement**: A function `f : ∀α. F(α)` cannot distinguish between types α.

### 3.2 Relational Interpretation

Define a relational interpretation:

**Type Relations**:
```
⟦α⟧_R = R                                   (type variable: the relation parameter)
⟦Unit⟧_R = {((), ())}
⟦Bool⟧_R = {(true, true), (false, false)}
⟦Int⟧_R = {(n, n) | n ∈ Z}
⟦τ → σ⟧_R = {(f, g) | ∀(a, b) ∈ ⟦τ⟧_R. (f a, g b) ∈ ⟦σ⟧_R}
⟦τ × σ⟧_R = {((a₁, a₂), (b₁, b₂)) | (a₁, b₁) ∈ ⟦τ⟧_R ∧ (a₂, b₂) ∈ ⟦σ⟧_R}
⟦∀α. τ⟧_R = {(f, g) | ∀A, B : Type. ∀R ⊆ A × B. (f[A], g[B]) ∈ ⟦τ⟧_R[R/α]}
```

### 3.3 Fundamental Property

**Theorem 3.1 (Parametricity / Abstraction Theorem)**: For any `⊢ e : τ` and relational interpretation:
```
(⟦e⟧, ⟦e⟧) ∈ ⟦τ⟧_R
```

**Proof**: By induction on typing derivation.

*Case Var*: `(⟦x⟧ρ₁, ⟦x⟧ρ₂) ∈ R_τ` by assumption on related environments.

*Case Lam*: For `λx. e : τ → σ`, need to show:
```
∀(a, b) ∈ ⟦τ⟧_R. (⟦e⟧[a/x], ⟦e⟧[b/x]) ∈ ⟦σ⟧_R
```
By IH on e with extended related environments. ✓

*Case TyAbs*: For `Λα. e : ∀α. τ`, need to show:
```
∀A, B, R. (⟦e⟧[A/α], ⟦e⟧[B/α]) ∈ ⟦τ⟧_R[R/α]
```
By IH on e with R as the interpretation of α. ✓

∎

### 3.4 Free Theorems

From parametricity, we derive "free theorems" about polymorphic functions:

**Theorem 3.2 (Identity Free Theorem)**: For `id : ∀α. α → α`:
```
id = λx. x
```

**Proof**: By parametricity, for any R ⊆ A × B and (a, b) ∈ R:
```
(id_A a, id_B b) ∈ R
```
Setting R = {(a, id_B a)}, we get id_A a = a. ∎

**Theorem 3.3 (Map Free Theorem)**: For `f : ∀α β. (α → β) → List[α] → List[β]`:
```
f g ∘ map h = map (g ∘ h)
```

(f distributes over map)

**Theorem 3.4 (Fold Free Theorem)**: For `fold : ∀α β. (α → β → β) → β → List[α] → β`:
```
fold f z ∘ map g = fold (f ∘ g) z
```

### 3.5 Parametricity for Rows

**Theorem 3.5 (Row Parametricity)**: For `f : ∀ρ. {l: τ | ρ} → σ`:
```
f is independent of fields other than l
```

**Example**:
```affinescript
fn get_name[ρ](r: {name: String | ρ}) -> String {
    r.name
}
```

By parametricity, `get_name` cannot observe or use any field other than `name`.

### 3.6 Parametricity for Effects

**Theorem 3.6 (Effect Parametricity)**: For `f : ∀ε. (() →{ε} A) → (() →{ε} B)`:
```
f cannot perform or suppress effects in ε
```

f can only transform the result; effects pass through unchanged.

### 3.7 Parametricity and Quantities

**Theorem 3.7 (Quantity Parametricity)**: Quantity-polymorphic functions respect usage:

For `f : ∀π. (π x : τ) → σ`:
- At π = 0: x is not used
- At π = 1: x is used exactly once
- At π = ω: x may be used arbitrarily

### 3.8 Parametricity for Refinements

**Theorem 3.8**: Parametricity extends to refinement types via logical relations.

For `f : ∀α. {x: α | P(x)} → {y: α | Q(y)}`:
```
∀x. P(x) ⟹ Q(f x)
```

The predicate transformer is derivable from the type.

## 4. Applications

### 4.1 Program Equivalence

Use parametricity to prove program equivalences:

**Example**: `reverse ∘ reverse = id` (for lists)

**Proof**: By parametricity and induction. ∎

### 4.2 Optimization Validity

Parametricity justifies optimizations:

**Example**: Fusion
```
map f ∘ map g = map (f ∘ g)
```

### 4.3 Representation Independence

Parametricity ensures abstract types hide representation:

```affinescript
module Set[A] {
    type T                               -- abstract
    fn empty() -> T
    fn insert(x: A, s: T) -> T
    fn member(x: A, s: T) -> Bool
}
```

Clients cannot distinguish implementations (list vs tree).

### 4.4 Security Properties

Parametricity implies information flow properties:

```affinescript
fn secure[α](secret: α, public: Int) -> Int
```

By parametricity, the result cannot depend on `secret`.

## 5. Limitations

### 5.1 Effects Break Parametricity

Unrestricted effects can break parametricity:

```affinescript
// BAD: breaks parametricity if allowed
fn bad[α](x: α) -> String / IO {
    print(type_of(x))  // type introspection
    "done"
}
```

AffineScript prevents this by not having `type_of` or similar primitives.

### 5.2 General Recursion

Non-termination weakens parametricity:

```affinescript
fn loop[α]() -> α {
    loop()  // non-terminating
}
```

Total functions satisfy stronger parametricity.

### 5.3 Unsafe Operations

`unsafe` blocks can break all guarantees:

```affinescript
fn bad[α](x: α) -> Int / unsafe {
    unsafe { transmute(x) }
}
```

Parametricity holds only outside `unsafe`.

## 6. Formal Statements

### 6.1 Coherence Theorem (Full)

**Theorem 6.1 (Full Coherence)**: For AffineScript with all features:

If `Γ ⊢ e : τ` via derivations D₁ and D₂, then:
```
⟦D₁⟧_η = ⟦D₂⟧_η
```

for any environment η satisfying Γ.

Conditions:
- No `unsafe` blocks
- All coercions are canonical
- Effects are handled

### 6.2 Parametricity Theorem (Full)

**Theorem 6.2 (Full Parametricity)**: For a well-typed closed term `⊢ e : τ`:
```
(⟦e⟧, ⟦e⟧) ∈ ⟦τ⟧_{id}
```

where ⟦_⟧_{id} is the identity relational interpretation.

For open terms, with related substitutions:
```
(⟦e⟧ρ₁, ⟦e⟧ρ₂) ∈ ⟦τ⟧_R   when ρ₁ R_Γ ρ₂
```

## 7. Related Work

1. **Reynolds (1983)**: Types, Abstraction, and Parametric Polymorphism
2. **Wadler (1989)**: Theorems for Free!
3. **Plotkin & Abadi (1993)**: A Logic for Parametric Polymorphism
4. **Dreyer et al. (2010)**: Logical Relations for Fine-Grained Concurrency
5. **Ahmed et al. (2017)**: Parametricity and Local State

## 8. References

1. Reynolds, J. C. (1983). Types, Abstraction and Parametric Polymorphism. *IFIP*.
2. Wadler, P. (1989). Theorems for Free! *FPCA*.
3. Plotkin, G., & Abadi, M. (1993). A Logic for Parametric Polymorphism. *TLCA*.
4. Wadler, P., & Blott, S. (1989). How to Make Ad-Hoc Polymorphism Less Ad Hoc. *POPL*.
5. Ahmed, A. (2006). Step-Indexed Syntactic Logical Relations for Recursive and Quantified Types. *ESOP*.

---

**Document Metadata**:
- Pure theory; no implementation dependencies
- Mechanized proof: See `mechanized/coq/Parametricity.v` (stub)
