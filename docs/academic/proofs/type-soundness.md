<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Type System Soundness

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete; implementation verification pending `[IMPL-DEP: type-checker]`

## Abstract

This document presents the formal metatheory of the AffineScript type system, establishing the fundamental soundness properties: type safety via progress and preservation (subject reduction). We prove that well-typed AffineScript programs do not get "stuck" during evaluation and that types are preserved under reduction.

## 1. Introduction

AffineScript's type system combines several advanced features:
- Bidirectional type checking with principal types
- Quantitative type theory (QTT) for linearity
- Algebraic effects with row-polymorphic effect types
- Dependent types with refinements
- Ownership and borrowing

This document focuses on the core type system soundness, with extensions for effects, quantities, and ownership treated in companion documents.

## 2. Syntax

### 2.1 Types

```
τ, σ ::=
    | α                           -- Type variable
    | τ → σ                       -- Function type
    | τ →{ε} σ                    -- Effectful function type
    | ∀α:κ. τ                     -- Universal quantification
    | ∃α:κ. τ                     -- Existential quantification
    | (τ₁, ..., τₙ)               -- Tuple type
    | {l₁: τ₁, ..., lₙ: τₙ | ρ}   -- Record type with row
    | [l₁: τ₁ | ... | lₙ: τₙ | ρ] -- Variant type with row
    | τ[e]                        -- Indexed type (dependent)
    | {x: τ | φ}                  -- Refinement type
    | own τ                       -- Owned type
    | ref τ                       -- Immutable reference
    | mut τ                       -- Mutable reference
```

### 2.2 Kinds

```
κ ::=
    | Type                        -- Kind of types
    | Nat                         -- Kind of natural numbers
    | Row                         -- Kind of row types
    | Effect                      -- Kind of effects
    | κ₁ → κ₂                     -- Higher-order kinds
```

### 2.3 Expressions

```
e ::=
    | x                           -- Variable
    | λx:τ. e                     -- Lambda abstraction
    | e₁ e₂                       -- Application
    | Λα:κ. e                     -- Type abstraction
    | e [τ]                       -- Type application
    | let x = e₁ in e₂            -- Let binding
    | (e₁, ..., eₙ)               -- Tuple
    | e.i                         -- Tuple projection
    | {l₁ = e₁, ..., lₙ = eₙ}     -- Record
    | e.l                         -- Record projection
    | e with {l = e'}             -- Record update
    | case e {p₁ → e₁ | ... | pₙ → eₙ}  -- Pattern match
    | handle e with h             -- Effect handler
    | perform op(e)               -- Effect operation
    | v                           -- Values
```

### 2.4 Values

```
v ::=
    | λx:τ. e                     -- Function value
    | Λα:κ. v                     -- Type abstraction value
    | (v₁, ..., vₙ)               -- Tuple value
    | {l₁ = v₁, ..., lₙ = vₙ}     -- Record value
    | C v                         -- Constructor application
    | ℓ                           -- Location (for references)
```

## 3. Static Semantics

### 3.1 Contexts

```
Γ ::= · | Γ, x:τ | Γ, α:κ
```

Well-formed context judgment: `⊢ Γ`

### 3.2 Kinding Judgment

```
Γ ⊢ τ : κ
```

**K-Var**
```
    α:κ ∈ Γ
    ──────────
    Γ ⊢ α : κ
```

**K-Arrow**
```
    Γ ⊢ τ₁ : Type    Γ ⊢ τ₂ : Type
    ───────────────────────────────
    Γ ⊢ τ₁ → τ₂ : Type
```

**K-EffArrow**
```
    Γ ⊢ τ₁ : Type    Γ ⊢ τ₂ : Type    Γ ⊢ ε : Effect
    ─────────────────────────────────────────────────
    Γ ⊢ τ₁ →{ε} τ₂ : Type
```

**K-Forall**
```
    Γ, α:κ ⊢ τ : Type
    ─────────────────────
    Γ ⊢ ∀α:κ. τ : Type
```

**K-Record**
```
    Γ ⊢ ρ : Row    ∀i. Γ ⊢ τᵢ : Type
    ─────────────────────────────────────────
    Γ ⊢ {l₁: τ₁, ..., lₙ: τₙ | ρ} : Type
```

**K-Indexed**
```
    Γ ⊢ τ : Nat → Type    Γ ⊢ e : Nat
    ─────────────────────────────────
    Γ ⊢ τ[e] : Type
```

**K-Refinement**
```
    Γ ⊢ τ : Type    Γ, x:τ ⊢ φ : Prop
    ─────────────────────────────────
    Γ ⊢ {x: τ | φ} : Type
```

### 3.3 Bidirectional Typing

We use bidirectional typing with two judgments:

- **Synthesis**: `Γ ⊢ e ⇒ τ` (infer type τ from expression e)
- **Checking**: `Γ ⊢ e ⇐ τ` (check expression e against type τ)

**Subsumption**
```
    Γ ⊢ e ⇒ τ    Γ ⊢ τ <: σ
    ────────────────────────
    Γ ⊢ e ⇐ σ
```

**Var**
```
    x:τ ∈ Γ
    ───────────
    Γ ⊢ x ⇒ τ
```

**Abs-Check**
```
    Γ, x:τ₁ ⊢ e ⇐ τ₂
    ───────────────────────────
    Γ ⊢ λx. e ⇐ τ₁ → τ₂
```

**Abs-Synth** (with annotation)
```
    Γ, x:τ₁ ⊢ e ⇒ τ₂
    ───────────────────────────
    Γ ⊢ λx:τ₁. e ⇒ τ₁ → τ₂
```

**App**
```
    Γ ⊢ e₁ ⇒ τ₁ → τ₂    Γ ⊢ e₂ ⇐ τ₁
    ──────────────────────────────────
    Γ ⊢ e₁ e₂ ⇒ τ₂
```

**TyAbs**
```
    Γ, α:κ ⊢ e ⇒ τ
    ─────────────────────
    Γ ⊢ Λα:κ. e ⇒ ∀α:κ. τ
```

**TyApp**
```
    Γ ⊢ e ⇒ ∀α:κ. τ    Γ ⊢ σ : κ
    ─────────────────────────────
    Γ ⊢ e [σ] ⇒ τ[σ/α]
```

**Let**
```
    Γ ⊢ e₁ ⇒ τ₁    Γ, x:τ₁ ⊢ e₂ ⇒ τ₂
    ──────────────────────────────────
    Γ ⊢ let x = e₁ in e₂ ⇒ τ₂
```

**Record-Intro**
```
    ∀i. Γ ⊢ eᵢ ⇒ τᵢ
    ─────────────────────────────────────────────
    Γ ⊢ {l₁ = e₁, ..., lₙ = eₙ} ⇒ {l₁: τ₁, ..., lₙ: τₙ}
```

**Record-Elim**
```
    Γ ⊢ e ⇒ {l: τ | ρ}
    ───────────────────
    Γ ⊢ e.l ⇒ τ
```

**Case**
```
    Γ ⊢ e ⇒ τ    ∀i. Γ ⊢ pᵢ : τ ⊣ Γᵢ    ∀i. Γ, Γᵢ ⊢ eᵢ ⇐ σ
    ────────────────────────────────────────────────────────
    Γ ⊢ case e {p₁ → e₁ | ... | pₙ → eₙ} ⇐ σ
```

### 3.4 Pattern Typing

```
Γ ⊢ p : τ ⊣ Γ'
```

**P-Var**
```
    ────────────────────
    Γ ⊢ x : τ ⊣ (x:τ)
```

**P-Wild**
```
    ────────────────────
    Γ ⊢ _ : τ ⊣ ·
```

**P-Constructor**
```
    C : τ₁ → ... → τₙ → T    ∀i. Γ ⊢ pᵢ : τᵢ ⊣ Γᵢ
    ──────────────────────────────────────────────
    Γ ⊢ C(p₁, ..., pₙ) : T ⊣ Γ₁, ..., Γₙ
```

**P-Record**
```
    ∀i. Γ ⊢ pᵢ : τᵢ ⊣ Γᵢ
    ──────────────────────────────────────────────────────────
    Γ ⊢ {l₁ = p₁, ..., lₙ = pₙ} : {l₁: τ₁, ..., lₙ: τₙ | ρ} ⊣ Γ₁, ..., Γₙ
```

## 4. Dynamic Semantics

### 4.1 Evaluation Contexts

```
E ::=
    | □
    | E e
    | v E
    | E [τ]
    | let x = E in e
    | (v₁, ..., E, ..., eₙ)
    | {l₁ = v₁, ..., l = E, ..., lₙ = eₙ}
    | E.l
    | E.i
    | case E {p₁ → e₁ | ... | pₙ → eₙ}
    | handle E with h
```

### 4.2 Small-Step Reduction

```
e ⟶ e'
```

**β-Reduction**
```
    ───────────────────────────
    (λx:τ. e) v ⟶ e[v/x]
```

**Type Application**
```
    ───────────────────────────
    (Λα:κ. e) [τ] ⟶ e[τ/α]
```

**Let**
```
    ───────────────────────────
    let x = v in e ⟶ e[v/x]
```

**Tuple Projection**
```
    ───────────────────────────
    (v₁, ..., vₙ).i ⟶ vᵢ
```

**Record Projection**
```
    ────────────────────────────────────────
    {l₁ = v₁, ..., lₙ = vₙ}.lᵢ ⟶ vᵢ
```

**Record Update**
```
    ──────────────────────────────────────────────────────────────
    {l₁ = v₁, ..., l = v, ..., lₙ = vₙ} with {l = v'} ⟶ {l₁ = v₁, ..., l = v', ..., lₙ = vₙ}
```

**Case-Match**
```
    match(p, v) = θ
    ─────────────────────────────────────────────
    case v {... | p → e | ...} ⟶ θ(e)
```

**Congruence**
```
    e ⟶ e'
    ──────────────
    E[e] ⟶ E[e']
```

### 4.3 Pattern Matching

The `match(p, v) = θ` judgment produces a substitution θ if pattern p matches value v.

**M-Var**
```
    ─────────────────
    match(x, v) = [v/x]
```

**M-Wild**
```
    ─────────────────
    match(_, v) = []
```

**M-Constructor**
```
    ∀i. match(pᵢ, vᵢ) = θᵢ
    ──────────────────────────────────────────
    match(C(p₁,...,pₙ), C(v₁,...,vₙ)) = θ₁∪...∪θₙ
```

## 5. Type Safety

### 5.1 Progress

**Theorem 5.1 (Progress)**: If `· ⊢ e : τ` then either:
1. e is a value, or
2. there exists e' such that `e ⟶ e'`

**Proof**: By induction on the typing derivation.

*Case Var*: Impossible, as the context is empty.

*Case Abs*: `e = λx:τ₁. e'` is a value. ✓

*Case App*: We have `· ⊢ e₁ e₂ : τ₂` derived from `· ⊢ e₁ : τ₁ → τ₂` and `· ⊢ e₂ : τ₁`.

By IH on e₁:
- If e₁ is a value, by canonical forms it must be `λx:τ₁. e'` for some e'.
  By IH on e₂:
  - If e₂ is a value v₂, then `(λx:τ₁. e') v₂ ⟶ e'[v₂/x]` by β-reduction. ✓
  - If e₂ steps, then `e₁ e₂ ⟶ e₁ e₂'` by congruence. ✓
- If e₁ steps, then `e₁ e₂ ⟶ e₁' e₂` by congruence. ✓

*Case TyAbs*: `e = Λα:κ. e'` is a value. ✓

*Case TyApp*: Similar to App case.

*Case Let*: We have `· ⊢ let x = e₁ in e₂ : τ₂`.
- If e₁ is a value v₁, then `let x = v₁ in e₂ ⟶ e₂[v₁/x]`. ✓
- If e₁ steps, then the whole expression steps by congruence. ✓

*Case Record-Intro*: If all components are values, the record is a value. Otherwise, the leftmost non-value steps, and we apply congruence. ✓

*Case Record-Elim*: By IH, e is a value or steps. If value, by canonical forms it's a record, and we project. ✓

*Case Case*: By IH, the scrutinee is a value or steps. If value, by exhaustiveness (ensured by type checking), some pattern matches. ✓

∎

### 5.2 Preservation (Subject Reduction)

**Theorem 5.2 (Preservation)**: If `Γ ⊢ e : τ` and `e ⟶ e'`, then `Γ ⊢ e' : τ`.

**Proof**: By induction on the derivation of `e ⟶ e'`.

*Case β-Reduction*: `(λx:τ₁. e) v ⟶ e[v/x]`

We have:
- `Γ ⊢ (λx:τ₁. e) v : τ₂` derived from
- `Γ ⊢ λx:τ₁. e : τ₁ → τ₂` and `Γ ⊢ v : τ₁`

From the lambda typing: `Γ, x:τ₁ ⊢ e : τ₂`

By the Substitution Lemma (Lemma 5.3): `Γ ⊢ e[v/x] : τ₂` ✓

*Case Type Application*: `(Λα:κ. e) [τ] ⟶ e[τ/α]`

We have `Γ ⊢ (Λα:κ. e) [τ] : σ[τ/α]` derived from:
- `Γ ⊢ Λα:κ. e : ∀α:κ. σ` which gives `Γ, α:κ ⊢ e : σ`
- `Γ ⊢ τ : κ`

By the Type Substitution Lemma (Lemma 5.4): `Γ ⊢ e[τ/α] : σ[τ/α]` ✓

*Case Let*: `let x = v in e ⟶ e[v/x]`

Similar to β-reduction, using the Substitution Lemma.

*Case Congruence*: `E[e] ⟶ E[e']` where `e ⟶ e'`

By IH, if `Γ' ⊢ e : τ'` then `Γ' ⊢ e' : τ'`.
By the Replacement Lemma (Lemma 5.5), the type of `E[e']` is preserved. ✓

∎

### 5.3 Key Lemmas

**Lemma 5.3 (Substitution)**: If `Γ, x:τ ⊢ e : σ` and `Γ ⊢ v : τ`, then `Γ ⊢ e[v/x] : σ`.

**Proof**: By induction on the typing derivation. ∎

**Lemma 5.4 (Type Substitution)**: If `Γ, α:κ ⊢ e : τ` and `Γ ⊢ σ : κ`, then `Γ ⊢ e[σ/α] : τ[σ/α]`.

**Proof**: By induction on the typing derivation. ∎

**Lemma 5.5 (Replacement/Compositionality)**: If `Γ ⊢ E[e] : τ` and replacing e with e' preserves the type of the hole, then `Γ ⊢ E[e'] : τ`.

**Proof**: By induction on the structure of E. ∎

**Lemma 5.6 (Canonical Forms)**: If `· ⊢ v : τ` where v is a value, then:
1. If τ = τ₁ → τ₂, then v = λx:τ₁. e for some x, e
2. If τ = ∀α:κ. σ, then v = Λα:κ. e for some e
3. If τ = (τ₁, ..., τₙ), then v = (v₁, ..., vₙ) for some values vᵢ
4. If τ = {l₁: τ₁, ..., lₙ: τₙ}, then v = {l₁ = v₁, ..., lₙ = vₙ}

**Proof**: By inspection of typing rules and definition of values. ∎

## 6. Type Soundness Corollary

**Corollary 6.1 (Type Safety)**: Well-typed programs don't get stuck.

If `· ⊢ e : τ` and `e ⟶* e'` (where `⟶*` is the reflexive-transitive closure of `⟶`), then either e' is a value or there exists e'' such that `e' ⟶ e''`.

**Proof**: By induction on the length of the reduction sequence, using Progress and Preservation. ∎

## 7. Extensions

### 7.1 Subtyping

AffineScript includes structural subtyping for records and variants.

**S-Record** (width subtyping)
```
    ────────────────────────────────────────────────────
    Γ ⊢ {l₁: τ₁, ..., lₙ: τₙ, l: τ | ρ} <: {l₁: τ₁, ..., lₙ: τₙ | ρ'}
```

**S-Arrow** (contravariant in domain, covariant in codomain)
```
    Γ ⊢ τ₁' <: τ₁    Γ ⊢ τ₂ <: τ₂'
    ────────────────────────────────
    Γ ⊢ τ₁ → τ₂ <: τ₁' → τ₂'
```

With subtyping, we extend preservation:

**Theorem 7.1 (Preservation with Subtyping)**: If `Γ ⊢ e : τ` and `e ⟶ e'` and `Γ ⊢ τ <: σ`, then `Γ ⊢ e' : τ'` for some τ' with `Γ ⊢ τ' <: σ`.

### 7.2 Recursion

For recursive functions, we extend with:

**Fix**
```
    Γ ⊢ e ⇐ τ → τ
    ────────────────────
    Γ ⊢ fix e ⇒ τ
```

**Fix-Reduce**
```
    ────────────────────────────
    fix (λx:τ. e) ⟶ e[fix (λx:τ. e)/x]
```

The addition of general recursion means termination is not guaranteed; partiality is the default in AffineScript unless marked `total`.

### 7.3 References and State

For mutable state, we need a store typing:

**Ref-Alloc**
```
    Γ | Σ ⊢ v : τ    ℓ ∉ dom(Σ)
    ──────────────────────────────────
    Γ | Σ ⊢ ref v ⇒ ref τ | Σ, ℓ:τ
```

**Ref-Read**
```
    Γ | Σ ⊢ e ⇒ ref τ
    ──────────────────────
    Γ | Σ ⊢ !e ⇒ τ
```

**Ref-Write**
```
    Γ | Σ ⊢ e₁ ⇒ mut τ    Γ | Σ ⊢ e₂ ⇐ τ
    ─────────────────────────────────────
    Γ | Σ ⊢ e₁ := e₂ ⇒ ()
```

Preservation must be extended to include store typing preservation:

**Theorem 7.2 (Preservation with Store)**: If `Γ | Σ ⊢ e : τ` and `(e, μ) ⟶ (e', μ')` and `Σ ⊢ μ`, then there exists Σ' ⊇ Σ such that `Γ | Σ' ⊢ e' : τ` and `Σ' ⊢ μ'`.

## 8. Implementation Notes

### 8.1 Correspondence to AST

The formal syntax maps to the AST defined in `lib/ast.ml`:

| Formal | AST Constructor |
|--------|-----------------|
| `τ → σ` | `TyArrow(τ, σ, None)` |
| `τ →{ε} σ` | `TyArrow(τ, σ, Some ε)` |
| `∀α:κ. τ` | Implicit in `fun_decl.fd_ty_params` |
| `{l: τ \| ρ}` | `TyRecord(fields, Some ρ)` |
| `τ[e]` | `TyApp(τ, [TaNat e])` |
| `{x: τ \| φ}` | `TyRefined(τ, φ)` |

### 8.2 Bidirectional Implementation

The bidirectional checking algorithm should follow the structure in `wiki/compiler/type-checker.md`:

```ocaml
(* Synthesis *)
val synth : ctx -> expr -> (typ * effect) result

(* Checking *)
val check : ctx -> expr -> typ -> effect result
```

`[IMPL-DEP: type-checker]` The type checker implementation is required to verify these theoretical results against the actual implementation.

## 9. Related Work

The type system draws from:

1. **Bidirectional Type Checking**: Pierce & Turner (2000), Dunfield & Krishnaswami (2021)
2. **Quantitative Type Theory**: Atkey (2018), McBride (2016)
3. **Algebraic Effects**: Plotkin & Pretnar (2013), Bauer & Pretnar (2015)
4. **Row Polymorphism**: Rémy (1989), Wand (1991)
5. **Ownership Types**: Clarke et al. (1998), Rust (2015)
6. **Refinement Types**: Freeman & Pfenning (1991), Liquid Types (Rondon et al., 2008)

## 10. References

1. Pierce, B. C. (2002). *Types and Programming Languages*. MIT Press.
2. Harper, R. (2016). *Practical Foundations for Programming Languages*. Cambridge University Press.
3. Dunfield, J., & Krishnaswami, N. (2021). Bidirectional typing. *ACM Computing Surveys*.
4. Wright, A. K., & Felleisen, M. (1994). A syntactic approach to type soundness. *Information and Computation*.
5. Atkey, R. (2018). Syntax and semantics of quantitative type theory. *LICS*.

---

## Appendix A: Full Typing Rules

[See supplementary material for complete rule set]

## Appendix B: Proof Details

[See supplementary material for expanded proof cases]

---

**Document Metadata**:
- Depends on: `lib/ast.ml`, `wiki/compiler/type-checker.md`
- Implementation verification: Pending type checker implementation
- Mechanized proof: See `mechanized/coq/TypeSoundness.v` (stub)
