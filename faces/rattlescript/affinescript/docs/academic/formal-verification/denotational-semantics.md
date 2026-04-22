# Denotational Semantics of AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete specification

## Abstract

This document provides a denotational semantics for AffineScript, interpreting programs as mathematical objects in suitable semantic domains. We use domain theory for handling recursion, monads for effects, and logical relations for establishing semantic properties.

## 1. Introduction

Denotational semantics provides:
- Compositional interpretation of programs
- Mathematical foundation for reasoning
- Basis for program equivalence
- Connection to logic and category theory

## 2. Semantic Domains

### 2.1 Basic Domains

We use complete partial orders (CPOs) with least elements (pointed CPOs):

**Definition 2.1 (Pointed CPO)**: A pointed CPO (D, ⊑, ⊥) consists of:
- A set D
- A partial order ⊑ on D
- A least element ⊥ ∈ D
- Every ω-chain has a least upper bound

### 2.2 Domain Constructors

**Lifted Domain**: D_⊥ = D ∪ {⊥}

**Product Domain**: D₁ × D₂ with pointwise ordering

**Sum Domain**: D₁ + D₂ = {inl(d) | d ∈ D₁} ∪ {inr(d) | d ∈ D₂}

**Function Domain**: [D₁ → D₂] = {f : D₁ → D₂ | f is continuous}

**Recursive Domains**: Solutions to domain equations using inverse limits

### 2.3 Type Denotations

```
⟦Unit⟧ = {*}
⟦Bool⟧ = {true, false}_⊥
⟦Int⟧ = Z_⊥
⟦Float⟧ = R_⊥
⟦String⟧ = List(Char)_⊥

⟦τ → σ⟧ = [⟦τ⟧ → ⟦σ⟧]
⟦τ × σ⟧ = ⟦τ⟧ × ⟦σ⟧
⟦τ + σ⟧ = ⟦τ⟧ + ⟦σ⟧
```

## 3. Environment and Interpretation

### 3.1 Environments

An environment maps variables to values:

```
Env = Var → Val

⟦Γ⟧ = {ρ : Var → Val | ∀(x:τ) ∈ Γ. ρ(x) ∈ ⟦τ⟧}
```

### 3.2 Expression Interpretation

```
⟦_⟧ : Expr → Env → Val
```

Compositionally defined for each expression form.

## 4. Core Language Interpretation

### 4.1 Literals

```
⟦()⟧ρ = *
⟦true⟧ρ = true
⟦false⟧ρ = false
⟦n⟧ρ = n
⟦f⟧ρ = f
⟦"s"⟧ρ = s
```

### 4.2 Variables

```
⟦x⟧ρ = ρ(x)
```

### 4.3 Functions

```
⟦λ(x:τ). e⟧ρ = λd. ⟦e⟧(ρ[x ↦ d])
⟦e₁ e₂⟧ρ = ⟦e₁⟧ρ (⟦e₂⟧ρ)
```

### 4.4 Let Binding

```
⟦let x = e₁ in e₂⟧ρ = ⟦e₂⟧(ρ[x ↦ ⟦e₁⟧ρ])
```

### 4.5 Recursion

Using the least fixed point operator:

```
⟦fix f. e⟧ρ = fix(λd. ⟦e⟧(ρ[f ↦ d]))
            = ⊔ₙ fⁿ(⊥)
```

where f = λd. ⟦e⟧(ρ[f ↦ d])

### 4.6 Conditionals

```
⟦if e₁ then e₂ else e₃⟧ρ =
    case ⟦e₁⟧ρ of
        true  → ⟦e₂⟧ρ
        false → ⟦e₃⟧ρ
        ⊥     → ⊥
```

### 4.7 Tuples

```
⟦(e₁, ..., eₙ)⟧ρ = (⟦e₁⟧ρ, ..., ⟦eₙ⟧ρ)
⟦e.i⟧ρ = πᵢ(⟦e⟧ρ)
```

### 4.8 Records

```
⟦{l₁ = e₁, ..., lₙ = eₙ}⟧ρ = {l₁ ↦ ⟦e₁⟧ρ, ..., lₙ ↦ ⟦eₙ⟧ρ}
⟦e.l⟧ρ = ⟦e⟧ρ(l)
⟦e₁ with {l = e₂}⟧ρ = ⟦e₁⟧ρ[l ↦ ⟦e₂⟧ρ]
```

### 4.9 Pattern Matching

```
⟦case e {p₁ → e₁ | ... | pₙ → eₙ}⟧ρ =
    let v = ⟦e⟧ρ in
    match v with
        | ⟦p₁⟧ → ⟦e₁⟧(ρ ⊕ bindings(p₁, v))
        | ...
        | ⟦pₙ⟧ → ⟦eₙ⟧(ρ ⊕ bindings(pₙ, v))
        | _ → ⊥
```

## 5. Type-Level Interpretation

### 5.1 Type Abstraction

```
⟦Λα:κ. e⟧ρ = λT. ⟦e⟧ρ
⟦e [τ]⟧ρ = ⟦e⟧ρ (⟦τ⟧)
```

### 5.2 Dependent Types

For dependent types, we use families of domains:

```
⟦Π(x:τ). σ⟧ = Π_{d ∈ ⟦τ⟧} ⟦σ⟧[d/x]
⟦Σ(x:τ). σ⟧ = Σ_{d ∈ ⟦τ⟧} ⟦σ⟧[d/x]
```

### 5.3 Refinement Types

```
⟦{x: τ | φ}⟧ = {d ∈ ⟦τ⟧ | ⟦φ⟧[d/x] = true}
```

### 5.4 Equality Types

```
⟦a == b⟧ = if ⟦a⟧ = ⟦b⟧ then {*} else ∅
```

## 6. Effects

### 6.1 Monad Transformers

Effects are interpreted using monad transformers:

**State Monad**:
```
State S A = S → (A × S)

⟦τ →{State[S]} σ⟧ = ⟦τ⟧ → State ⟦S⟧ ⟦σ⟧
```

**Exception Monad**:
```
Exn E A = A + E

⟦τ →{Exn[E]} σ⟧ = ⟦τ⟧ → Exn ⟦E⟧ ⟦σ⟧
```

**Reader Monad**:
```
Reader R A = R → A

⟦τ →{Reader[R]} σ⟧ = ⟦τ⟧ → Reader ⟦R⟧ ⟦σ⟧
```

### 6.2 Free Monad Interpretation

For general algebraic effects:

```
Free F A = Pure A | Op (F (Free F A))

⟦ε⟧ = Free (⟦Ops(ε)⟧)
```

where Ops(ε) is the functor for effect operations.

### 6.3 Handler Interpretation

```
⟦handle e with h⟧ρ = fold_h(⟦e⟧ρ)

where fold_h : Free F A → B is defined by:
    fold_h(Pure a) = ⟦e_ret⟧(ρ[x ↦ a])
    fold_h(Op op(v, k)) = ⟦e_op⟧(ρ[x ↦ v, k ↦ λy. fold_h(k(y))])
```

### 6.4 Effectful Function Interpretation

```
⟦perform op(e)⟧ρ = Op op(⟦e⟧ρ, Pure)
```

## 7. Ownership and References

### 7.1 Store Model

```
Store = Loc → Val
Conf = Expr × Store
```

### 7.2 Stateful Interpretation

```
⟦_⟧ : Expr → Env → Store → (Val × Store)

⟦ref e⟧ρσ =
    let (v, σ') = ⟦e⟧ρσ in
    let ℓ fresh in
    (ℓ, σ'[ℓ ↦ v])

⟦!e⟧ρσ =
    let (ℓ, σ') = ⟦e⟧ρσ in
    (σ'(ℓ), σ')

⟦e₁ := e₂⟧ρσ =
    let (ℓ, σ') = ⟦e₁⟧ρσ in
    let (v, σ'') = ⟦e₂⟧ρσ' in
    ((), σ''[ℓ ↦ v])
```

### 7.3 Ownership Erasure

At the semantic level, ownership annotations are erased:
```
⟦own τ⟧ = ⟦τ⟧
⟦ref τ⟧ = ⟦τ⟧
⟦mut τ⟧ = ⟦τ⟧
```

The ownership system ensures safety statically; runtime behavior is identical.

## 8. Quantitative Types

### 8.1 Graded Semantics

For quantities, use graded monads:

```
⟦0 τ⟧ = 1                   (erased, unit type)
⟦1 τ⟧ = ⟦τ⟧                 (linear)
⟦ω τ⟧ = !⟦τ⟧ = ⟦τ⟧         (in CPO, no distinction)
```

### 8.2 Usage Tracking

Alternatively, track usage in an effect:
```
Usage = Var → Nat

⟦e⟧ : Env → Usage → (Val × Usage)
```

## 9. Semantic Properties

### 9.1 Adequacy

**Theorem 9.1 (Adequacy)**: For closed terms e of ground type:
```
⟦e⟧{} = v  iff  e ⟶* v
```

**Proof**: By logical relations between syntax and semantics. ∎

### 9.2 Soundness

**Theorem 9.2 (Semantic Soundness)**: If `Γ ⊢ e : τ` then for all ρ ∈ ⟦Γ⟧:
```
⟦e⟧ρ ∈ ⟦τ⟧
```

**Proof**: By induction on typing derivation. ∎

### 9.3 Compositionality

**Theorem 9.3 (Compositionality)**: The semantics is compositional:
```
⟦E[e]⟧ρ = ⟦E⟧(ρ, ⟦e⟧ρ)
```

The meaning of a compound expression depends only on the meanings of its parts.

### 9.4 Full Abstraction

**Open Problem**: Is the semantics fully abstract?

Full abstraction: `⟦e₁⟧ = ⟦e₂⟧` iff e₁ ≃ e₂ (contextually equivalent)

This typically requires game semantics or more refined models.

## 10. Logical Relations

### 10.1 Definition

Define a family of relations R_τ ⊆ ⟦τ⟧ × ⟦τ⟧ indexed by types:

```
R_Unit = {(*, *)}
R_Bool = {(true, true), (false, false)}
R_Int = {(n, n) | n ∈ Z}

R_{τ→σ} = {(f, g) | ∀(d₁, d₂) ∈ R_τ. (f d₁, g d₂) ∈ R_σ}
R_{τ×σ} = {((d₁, d₂), (d₁', d₂')) | (d₁, d₁') ∈ R_τ ∧ (d₂, d₂') ∈ R_σ}
```

### 10.2 Fundamental Property

**Theorem 10.1 (Fundamental Property)**: For all `Γ ⊢ e : τ` and related environments ρ₁ R_Γ ρ₂:
```
(⟦e⟧ρ₁, ⟦e⟧ρ₂) ∈ R_τ
```

**Proof**: By induction on typing derivation. ∎

### 10.3 Applications

Logical relations prove:
- Parametricity (free theorems)
- Termination
- Observational equivalence

## 11. Domain Equations

### 11.1 Recursive Types

For recursive types, solve domain equations:

```
⟦μα. τ⟧ = fix(λD. ⟦τ⟧[D/α])
```

Using inverse limit construction for existence.

### 11.2 Example: Lists

```
⟦List[A]⟧ = fix(D. 1 + (⟦A⟧ × D))
          = 1 + (⟦A⟧ × (1 + (⟦A⟧ × ...)))
          ≅ ⟦A⟧*  (finite and infinite lists)
```

### 11.3 Example: Streams

```
⟦Stream[A]⟧ = fix(D. ⟦A⟧ × D)
            = ⟦A⟧ω  (infinite sequences)
```

## 12. Effect Semantics Details

### 12.1 State Effect

```
⟦State[S]⟧ = Free (StateF S)

where StateF S X = Get (S → X) | Put (S × X)

⟦perform get()⟧ρ = Op (Get Pure)
⟦perform put(e)⟧ρ = Op (Put (⟦e⟧ρ, Pure ()))
```

### 12.2 Exception Effect

```
⟦Exn[E]⟧ = Free (ExnF E)

where ExnF E X = Raise E

⟦perform raise(e)⟧ρ = Op (Raise (⟦e⟧ρ))
```

### 12.3 Nondeterminism

```
⟦Choice⟧ = Free ChoiceF

where ChoiceF X = Choose (X × X) | Fail

⟦perform choose()⟧ρ = Op (Choose (Pure true, Pure false))
⟦perform fail()⟧ρ = Op Fail
```

## 13. Continuations

### 13.1 CPS Semantics

Alternative: continuation-passing style interpretation:

```
⟦τ⟧_k = (⟦τ⟧ → R) → R

⟦e₁ e₂⟧_k ρ κ = ⟦e₁⟧_k ρ (λf. ⟦e₂⟧_k ρ (λa. f a κ))
```

### 13.2 Delimited Continuations

For effect handlers:
```
⟦handle e with h⟧_k ρ κ =
    reset(⟦e⟧_k ρ (λv. ⟦e_ret⟧(ρ[x ↦ v]) κ))
```

## 14. Examples

### 14.1 Factorial

```
⟦let rec fact = λn. if n == 0 then 1 else n * fact(n - 1) in fact 5⟧

= fix(λf. λn. if n = 0 then 1 else n × f(n - 1))(5)
= 120
```

### 14.2 State Handler

```
⟦handle { let x = get(); put(x + 1); get() } with run_state(0)⟧

= fold_{run_state(0)}(
    Op Get(λs. Op Put((s+1, λ(). Op Get(Pure))))
  )
= 1
```

## 15. References

1. Scott, D. S. (1976). Data Types as Lattices. *SIAM J. Computing*.
2. Stoy, J. E. (1977). *Denotational Semantics*. MIT Press.
3. Winskel, G. (1993). *The Formal Semantics of Programming Languages*. MIT Press.
4. Gunter, C. A. (1992). *Semantics of Programming Languages*. MIT Press.
5. Moggi, E. (1991). Notions of Computation and Monads. *I&C*.
6. Plotkin, G., & Power, J. (2002). Notions of Computation Determine Monads. *FOSSACS*.

---

**Document Metadata**:
- This document is pure theory; no implementation dependencies
- Mechanized proof: See `mechanized/coq/Denotational.v` (stub)
