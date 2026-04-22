# Logic Foundations of AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete theoretical framework

## Abstract

This document presents the logical foundations of AffineScript's type system through the lens of the Curry-Howard correspondence. We establish connections between AffineScript types and logical propositions, programs and proofs, type checking and proof checking.

## 1. Introduction

The Curry-Howard correspondence reveals deep connections:

| Type Theory | Logic |
|-------------|-------|
| Types | Propositions |
| Programs | Proofs |
| Type checking | Proof checking |
| Type inhabitation | Provability |
| Normalization | Cut elimination |

AffineScript extends this to:
- Quantitative types ↔ Linear logic
- Effects ↔ Modal logic / Computational interpretations
- Ownership ↔ Affine/linear logic

## 2. Propositional Logic

### 2.1 Intuitionistic Propositional Logic

The simply-typed fragment corresponds to intuitionistic propositional logic:

| Type | Proposition | Connective |
|------|-------------|------------|
| `τ → σ` | P ⟹ Q | Implication |
| `τ × σ` | P ∧ Q | Conjunction |
| `τ + σ` | P ∨ Q | Disjunction |
| `Unit` | ⊤ | Truth |
| `Void` | ⊥ | Falsity |

### 2.2 Introduction and Elimination Rules

**Implication**:
```
    Γ, P ⊢ Q               Γ ⊢ P ⟹ Q    Γ ⊢ P
    ─────────── ⟹I         ───────────────────── ⟹E
    Γ ⊢ P ⟹ Q                    Γ ⊢ Q
```

Corresponds to:
```
    Γ, x:τ ⊢ e : σ          Γ ⊢ f : τ → σ    Γ ⊢ a : τ
    ─────────────────        ────────────────────────────
    Γ ⊢ λx. e : τ → σ            Γ ⊢ f a : σ
```

**Conjunction**:
```
    Γ ⊢ P    Γ ⊢ Q          Γ ⊢ P ∧ Q         Γ ⊢ P ∧ Q
    ────────────── ∧I       ───────── ∧E₁     ───────── ∧E₂
    Γ ⊢ P ∧ Q                 Γ ⊢ P             Γ ⊢ Q
```

Corresponds to tuples:
```
    Γ ⊢ e₁ : τ    Γ ⊢ e₂ : σ    Γ ⊢ e : τ × σ    Γ ⊢ e : τ × σ
    ──────────────────────      ──────────────    ──────────────
    Γ ⊢ (e₁, e₂) : τ × σ        Γ ⊢ fst e : τ    Γ ⊢ snd e : σ
```

**Disjunction**:
```
    Γ ⊢ P           Γ ⊢ Q           Γ ⊢ P ∨ Q    Γ, P ⊢ R    Γ, Q ⊢ R
    ─────── ∨I₁     ─────── ∨I₂     ───────────────────────────────── ∨E
    Γ ⊢ P ∨ Q       Γ ⊢ P ∨ Q                     Γ ⊢ R
```

Corresponds to sum types:
```
    Γ ⊢ e : τ               Γ ⊢ e : σ               Γ ⊢ e : τ + σ    ...
    ───────────────         ───────────────         ───────────────────
    Γ ⊢ inl e : τ + σ       Γ ⊢ inr e : τ + σ       Γ ⊢ case e ... : ρ
```

### 2.3 Negation

Negation is encoded as implication to falsity:
```
¬P ≡ P → ⊥
```

In AffineScript:
```affinescript
type Not[P] = P -> Void

fn absurd[A](v: Void) -> A {
    case v { }  // empty case
}
```

## 3. Predicate Logic

### 3.1 Universal Quantification

Polymorphism corresponds to universal quantification:

```
∀α. P(α) ↔ ∀α:Type. τ(α)
```

**Introduction**:
```
    Γ ⊢ P(α)    α not free in Γ     Γ, α:Type ⊢ e : τ    α ∉ FTV(Γ)
    ─────────────────────────       ────────────────────────────────
    Γ ⊢ ∀α. P(α)                    Γ ⊢ Λα. e : ∀α. τ
```

**Elimination**:
```
    Γ ⊢ ∀α. P(α)    Γ ⊢ t : Type     Γ ⊢ e : ∀α. τ    Γ ⊢ σ : Type
    ────────────────────────────     ──────────────────────────────
    Γ ⊢ P(t)                         Γ ⊢ e [σ] : τ[σ/α]
```

### 3.2 Existential Quantification

Existential types:
```
∃α. P(α) ↔ ∃α:Type. τ(α)
```

Corresponds to:
```affinescript
type Exists[F] = (α: Type, F[α])   // existential package
```

Introduction (packing):
```
    Γ ⊢ e : τ[σ/α]
    ──────────────────────────
    Γ ⊢ pack[σ](e) : ∃α. τ
```

Elimination (unpacking):
```
    Γ ⊢ e₁ : ∃α. τ    Γ, α:Type, x:τ ⊢ e₂ : σ    α ∉ FTV(σ)
    ───────────────────────────────────────────────────────
    Γ ⊢ unpack e₁ as (α, x) in e₂ : σ
```

## 4. Linear Logic

### 4.1 Linear Connectives

AffineScript's quantitative types correspond to linear logic:

| Quantity | Linear Logic |
|----------|--------------|
| 0 | Erasure (?) |
| 1 | Linear (exact) |
| ω | Exponential (!) |

| Type | Linear Connective |
|------|-------------------|
| `τ ⊸ σ` | Linear implication |
| `τ ⊗ σ` | Multiplicative conjunction (tensor) |
| `τ & σ` | Additive conjunction (with) |
| `τ ⊕ σ` | Additive disjunction (plus) |
| `!τ` | Of course (exponential) |
| `?τ` | Why not (dual exponential) |

### 4.2 Linear Implication

```
    Γ, x:τ ⊢ e : σ    x used exactly once in e
    ──────────────────────────────────────────
    Γ ⊢ λx. e : τ ⊸ σ
```

### 4.3 Multiplicative Conjunction

Both components must be used:
```
    Γ ⊢ e₁ : τ    Δ ⊢ e₂ : σ
    ─────────────────────────
    Γ, Δ ⊢ (e₁, e₂) : τ ⊗ σ
```

### 4.4 Exponential Modality

The `!` modality allows unrestricted use:
```
    !τ ≡ ω τ    (omega quantity)
```

Rules:
```
    Γ ⊢ e : τ         !Γ ⊢ e : τ
    ─────────────     ─────────────
    !Γ ⊢ e : τ        Γ ⊢ e : !τ

    (contraction)     (promotion)
```

### 4.5 Affine Logic

AffineScript is actually affine (can drop, not required to use):
```
    Γ, x:τ ⊢ e : σ    x not used
    ────────────────────────────
    Γ, x:τ ⊢ e : σ    (weakening allowed)
```

## 5. Dependent Types as Logic

### 5.1 Dependent Product (Π-Types)

```
Π(x:A). B(x) ↔ ∀x:A. P(x)
```

Full predicate logic:
```
    Γ, x:A ⊢ P(x)    x ∉ FV(Γ)
    ──────────────────────────
    Γ ⊢ ∀x:A. P(x)
```

### 5.2 Dependent Sum (Σ-Types)

```
Σ(x:A). B(x) ↔ ∃x:A. P(x)
```

Existential quantification over terms.

### 5.3 Identity Types

Propositional equality:
```
a = b : A ↔ a == b : Type
```

The type `a == b` is inhabited iff a and b are definitionally equal.

### 5.4 Propositions as Types

**Theorem 5.1 (Curry-Howard for Dependent Types)**:
```
Γ ⊢ e : τ  iff  Γ ⊢ τ : Prop and e is a proof of τ
```

## 6. Modal Logic

### 6.1 Necessity and Possibility

Effects can be viewed through modal logic:

| Modal | Effect |
|-------|--------|
| □P (necessarily P) | Pure computation |
| ◇P (possibly P) | Effectful computation |

### 6.2 Computational Interpretations

Moggi's computational lambda calculus:
```
T(A) ≡ computation that may produce A
```

Effects as modalities:
```
⊢ e : A         (pure: e is of type A)
⊢ e : T(A)      (effectful: e computes to type A)
```

### 6.3 Graded Modalities

Quantities as graded modalities:
```
□_π A     (A used with quantity π)
```

## 7. Refinement Types as Subset Logic

### 7.1 Comprehension

Refinement types correspond to set comprehension:
```
{x: τ | φ} ↔ {x ∈ ⟦τ⟧ | φ(x)}
```

### 7.2 Subtyping as Implication

```
{x: τ | φ} <: {x: τ | ψ}  iff  ∀x:τ. φ(x) ⟹ ψ(x)
```

### 7.3 Verification Conditions

Refinement type checking generates logical formulas:
```
Γ ⊢ e : {x: τ | φ}  generates  ⟦Γ⟧ ⟹ φ[⟦e⟧/x]
```

## 8. Proof Irrelevance

### 8.1 Erased Proofs

Proofs at quantity 0 are erased:
```
(0 pf : P) → Q    -- proof pf is erased at runtime
```

### 8.2 Proof Irrelevance

For propositions (types with at most one inhabitant):
```
∀p₁, p₂ : P. p₁ = p₂
```

Proofs are unique, so they can be erased.

### 8.3 SProp (Strict Propositions)

Types that are proof-irrelevant:
```affinescript
SProp ⊂ Type    -- strict propositions
```

## 9. Classical vs Intuitionistic

### 9.1 Constructive Mathematics

AffineScript is constructive:
- Proofs are programs
- Existence proofs provide witnesses
- No excluded middle

### 9.2 Excluded Middle

The law of excluded middle is not provable:
```
-- This type is not inhabited in general:
type LEM[P] = Either[P, Not[P]]
```

### 9.3 Double Negation

Double negation elimination is not valid:
```
-- Cannot implement:
fn dne[P](nnp: Not[Not[P]]) -> P { ... }
```

But double negation translation is possible for classical reasoning.

### 9.4 Axiom of Choice

The axiom of choice:
```
-- Can be stated but not proven:
fn choice[A, B](
    f: (a: A) -> Exists[λb. R(a, b)]
) -> Exists[λg. (a: A) -> R(a, g(a))]
```

## 10. Proof-Carrying Code

### 10.1 Certified Programs

Programs carry proofs of their properties:
```affinescript
fn certified_sort(xs: List[Int])
    -> (ys: List[Int], pf: sorted(ys) ∧ permutation(xs, ys))
```

### 10.2 Proof Extraction

Proofs can be extracted as programs:
```
⊢ ∀x:Nat. ∃y:Nat. x < y
↓ (extract)
succ : Nat → Nat
```

### 10.3 Program Verification

Verified by construction:
```affinescript
fn verified_div(
    x: Int,
    y: {v: Int | v ≠ 0},
    0 _: y ≠ 0             -- proof (erased)
) -> Int
```

## 11. Consistency

### 11.1 Logical Consistency

**Theorem 11.1**: The type `Void` is uninhabited in AffineScript.

```
⊬ e : Void    for any closed e
```

**Proof**: By strong normalization and inspection of canonical forms. ∎

### 11.2 Relative Consistency

**Theorem 11.2**: AffineScript's type system is consistent relative to:
- Martin-Löf Type Theory (for dependent types)
- Linear Logic (for quantities)
- Classical set theory (for semantics)

### 11.3 Adding Axioms

Care must be taken with axioms:
```affinescript
-- DANGEROUS: Makes system inconsistent
axiom absurd : Void
```

Safe axioms include:
- Functional extensionality
- Propositional extensionality
- Univalence (with care)

## 12. Proof Automation

### 12.1 Tactics

Proof search strategies:
- `auto`: Automatic proof search
- `simp`: Simplification
- `induction`: Structural induction
- `smt`: SMT solver invocation

### 12.2 Decision Procedures

Automated for:
- Propositional logic (SAT)
- Linear arithmetic (LIA)
- Presburger arithmetic
- Equality reasoning (congruence closure)

### 12.3 Interactive Proofs

For complex proofs:
```affinescript
theorem example : ∀n:Nat. n + 0 = n := by
    intro n
    induction n with
    | zero => refl
    | succ n ih => simp [add_succ, ih]
```

`[IMPL-DEP: proof-assistant]` Interactive proof mode pending.

## 13. Related Work

1. **Curry-Howard**: Curry (1934), Howard (1980)
2. **Linear Logic**: Girard (1987)
3. **Martin-Löf Type Theory**: Martin-Löf (1984)
4. **Calculus of Constructions**: Coquand & Huet (1988)
5. **Propositions as Types**: Wadler (2015)
6. **Linear Type Theory**: Pfenning et al.

## 14. References

1. Howard, W. A. (1980). The Formulae-as-Types Notion of Construction. *Curry Festschrift*.
2. Girard, J.-Y. (1987). Linear Logic. *TCS*.
3. Martin-Löf, P. (1984). *Intuitionistic Type Theory*. Bibliopolis.
4. Wadler, P. (2015). Propositions as Types. *CACM*.
5. Pfenning, F., & Davies, R. (2001). A Judgmental Reconstruction of Modal Logic. *MSCS*.

---

**Document Metadata**:
- This document is pure logic theory
- Implementation: Type checker provides the proof checker
