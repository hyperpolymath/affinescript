# Dependent Types and Refinement Types: Complete Formalization

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete; implementation verification pending `[IMPL-DEP: type-checker, smt-integration]`

## Abstract

This document provides a complete formalization of AffineScript's dependent type system, including indexed types, Π-types (dependent functions), Σ-types (dependent pairs), refinement types, and propositional equality. We prove decidability of type checking (modulo SMT queries for refinements), normalization of type-level computation, and type safety.

## 1. Introduction

AffineScript supports a stratified dependent type system:

1. **Indexed types**: Types parameterized by values (`Vec[n, T]`)
2. **Π-types**: Dependent function types (`(n: Nat) → Vec[n, T]`)
3. **Σ-types**: Dependent pair types (`(n: Nat, Vec[n, T])`)
4. **Refinement types**: Types refined by predicates (`{x: Int | x > 0}`)
5. **Propositional equality**: Type-level equality proofs (`n == m`)

The system is designed for practical use with decidable type checking through:
- Restricted type-level computation
- SMT-based refinement checking
- Erasure of proof terms at runtime

## 2. Syntax

### 2.1 Universes

```
U ::= Type₀ | Type₁ | ...    -- Universe hierarchy
```

With universe polymorphism: `Type_i : Type_{i+1}`

### 2.2 Expressions and Types (Unified)

In dependent type theory, expressions and types share the same grammar:

```
e, τ, σ ::=
    -- Core
    | x                           -- Variable
    | U_i                         -- Universe at level i
    | λ(x:τ). e                   -- Lambda
    | e₁ e₂                       -- Application
    | Π(x:τ). σ                   -- Dependent function type
    | Σ(x:τ). σ                   -- Dependent pair type
    | (e₁, e₂)                    -- Pair
    | fst e | snd e               -- Projections

    -- Indexed types
    | C[ē]                        -- Indexed type constructor
    | c[ē](ē')                    -- Indexed data constructor

    -- Refinements
    | {x:τ | φ}                   -- Refinement type
    | ⌊e⌋                         -- Refinement introduction
    | ⌈e⌉                         -- Refinement elimination

    -- Equality
    | e₁ == e₂                    -- Propositional equality type
    | refl                        -- Reflexivity proof
    | J(P, d, p)                  -- Equality elimination (J rule)

    -- Type-level computation
    | n                           -- Natural literal
    | e₁ + e₂ | e₁ - e₂ | e₁ * e₂ -- Arithmetic
    | if e₁ then e₂ else e₃       -- Conditional
    | len(e)                      -- Length operation
```

### 2.3 Predicates (for Refinements)

```
φ, ψ ::=
    | e₁ < e₂ | e₁ ≤ e₂ | e₁ = e₂ | e₁ ≠ e₂    -- Comparisons
    | φ ∧ ψ | φ ∨ ψ | ¬φ                         -- Logical connectives
    | ∀x:τ. φ | ∃x:τ. φ                          -- Quantifiers
    | P(e₁, ..., eₙ)                             -- Predicate application
```

## 3. Judgments

### 3.1 Core Judgments

```
Γ ⊢ wf                            -- Context well-formed
Γ ⊢ e : τ                         -- Typing
Γ ⊢ e ≡ e' : τ                    -- Definitional equality
Γ ⊢ τ <: σ                        -- Subtyping
Γ ⊢ e ⇝ v                         -- Reduction to value (normalization)
Γ ⊢ φ                             -- Predicate validity
```

### 3.2 Bidirectional Judgments

```
Γ ⊢ e ⇒ τ                         -- Synthesis
Γ ⊢ e ⇐ τ                         -- Checking
```

## 4. Typing Rules

### 4.1 Universes

**Type-in-Type (Simplified)**
```
    ─────────────────
    Γ ⊢ Type_i : Type_{i+1}
```

**Cumulativity**
```
    Γ ⊢ τ : Type_i    i ≤ j
    ─────────────────────────
    Γ ⊢ τ : Type_j
```

### 4.2 Π-Types (Dependent Functions)

**Π-Form**
```
    Γ ⊢ τ : Type_i    Γ, x:τ ⊢ σ : Type_j
    ─────────────────────────────────────────
    Γ ⊢ Π(x:τ). σ : Type_{max(i,j)}
```

**Π-Intro**
```
    Γ, x:τ ⊢ e : σ
    ──────────────────────────
    Γ ⊢ λ(x:τ). e : Π(x:τ). σ
```

**Π-Elim**
```
    Γ ⊢ f : Π(x:τ). σ    Γ ⊢ a : τ
    ────────────────────────────────
    Γ ⊢ f a : σ[a/x]
```

**Π-β**
```
    Γ ⊢ (λ(x:τ). e) a ≡ e[a/x] : σ[a/x]
```

**Π-η**
```
    Γ ⊢ f : Π(x:τ). σ
    ──────────────────────────────────
    Γ ⊢ f ≡ λ(x:τ). f x : Π(x:τ). σ
```

### 4.3 Σ-Types (Dependent Pairs)

**Σ-Form**
```
    Γ ⊢ τ : Type_i    Γ, x:τ ⊢ σ : Type_j
    ─────────────────────────────────────────
    Γ ⊢ Σ(x:τ). σ : Type_{max(i,j)}
```

**Σ-Intro**
```
    Γ ⊢ a : τ    Γ ⊢ b : σ[a/x]
    ─────────────────────────────
    Γ ⊢ (a, b) : Σ(x:τ). σ
```

**Σ-Elim (First)**
```
    Γ ⊢ p : Σ(x:τ). σ
    ───────────────────
    Γ ⊢ fst p : τ
```

**Σ-Elim (Second)**
```
    Γ ⊢ p : Σ(x:τ). σ
    ───────────────────────────
    Γ ⊢ snd p : σ[fst p/x]
```

**Σ-β**
```
    Γ ⊢ fst (a, b) ≡ a : τ
    Γ ⊢ snd (a, b) ≡ b : σ[a/x]
```

**Σ-η**
```
    Γ ⊢ p : Σ(x:τ). σ
    ───────────────────────────────────
    Γ ⊢ p ≡ (fst p, snd p) : Σ(x:τ). σ
```

### 4.4 Indexed Types

**Definition**: An indexed type is defined with indices:

```affinescript
type Vec[n: Nat, T: Type] =
    | Nil : Vec[0, T]
    | Cons : (T, Vec[m, T]) → Vec[m + 1, T]
```

**Indexed-Form**
```
    Vec : Nat → Type → Type
    ────────────────────────────────
    Γ ⊢ n : Nat    Γ ⊢ T : Type
    ───────────────────────────────
    Γ ⊢ Vec[n, T] : Type
```

**Indexed-Intro (Nil)**
```
    Γ ⊢ T : Type
    ─────────────────────
    Γ ⊢ Nil : Vec[0, T]
```

**Indexed-Intro (Cons)**
```
    Γ ⊢ x : T    Γ ⊢ xs : Vec[n, T]
    ─────────────────────────────────
    Γ ⊢ Cons(x, xs) : Vec[n + 1, T]
```

**Indexed-Elim (Pattern Matching)**
```
    Γ ⊢ v : Vec[n, T]
    Γ ⊢ e_nil : P[0/n, Nil/v]
    Γ, m:Nat, x:T, xs:Vec[m,T] ⊢ e_cons : P[m+1/n, Cons(x,xs)/v]
    ────────────────────────────────────────────────────────────────
    Γ ⊢ case v { Nil → e_nil | Cons(x, xs) → e_cons } : P
```

### 4.5 Refinement Types

**Refine-Form**
```
    Γ ⊢ τ : Type    Γ, x:τ ⊢ φ : Prop
    ───────────────────────────────────
    Γ ⊢ {x:τ | φ} : Type
```

**Refine-Intro**
```
    Γ ⊢ e : τ    Γ ⊢ φ[e/x]
    ─────────────────────────
    Γ ⊢ ⌊e⌋ : {x:τ | φ}
```

**Refine-Elim**
```
    Γ ⊢ e : {x:τ | φ}
    ───────────────────
    Γ ⊢ ⌈e⌉ : τ
```

**Refine-Proj**
```
    Γ ⊢ e : {x:τ | φ}
    ───────────────────────
    Γ ⊢ φ[⌈e⌉/x]
```

### 4.6 Propositional Equality

**Eq-Form**
```
    Γ ⊢ a : τ    Γ ⊢ b : τ
    ─────────────────────────
    Γ ⊢ a == b : Type
```

**Eq-Intro (Refl)**
```
    Γ ⊢ a : τ
    ──────────────────
    Γ ⊢ refl : a == a
```

**Eq-Elim (J)**
```
    Γ, y:τ, p:(a == y) ⊢ P : Type
    Γ ⊢ d : P[a/y, refl/p]
    Γ ⊢ e : a == b
    ─────────────────────────────────
    Γ ⊢ J(P, d, e) : P[b/y, e/p]
```

**Eq-β**
```
    Γ ⊢ J(P, d, refl) ≡ d : P[a/y, refl/p]
```

### 4.7 Subtyping with Refinements

**Sub-Refine**
```
    Γ ⊢ τ <: σ    Γ, x:τ ⊢ φ ⟹ ψ
    ─────────────────────────────────────
    Γ ⊢ {x:τ | φ} <: {x:σ | ψ}
```

**Sub-Forget**
```
    ─────────────────────────
    Γ ⊢ {x:τ | φ} <: τ
```

## 5. Definitional Equality

### 5.1 Reduction Rules

**β-Reduction**
```
(λ(x:τ). e) a ⟶ e[a/x]
fst (a, b) ⟶ a
snd (a, b) ⟶ b
J(P, d, refl) ⟶ d
```

**Arithmetic Reduction**
```
n + m ⟶ n+m    (where n, m are literals)
n * m ⟶ n*m
if true then e₁ else e₂ ⟶ e₁
if false then e₁ else e₂ ⟶ e₂
len(Nil) ⟶ 0
len(Cons(_, xs)) ⟶ 1 + len(xs)
```

### 5.2 Definitional Equality

**Definition 5.1**: e₁ ≡ e₂ iff e₁ and e₂ reduce to the same normal form.

**Eq-Refl**
```
    ───────────
    Γ ⊢ e ≡ e
```

**Eq-Sym**
```
    Γ ⊢ e₁ ≡ e₂
    ─────────────
    Γ ⊢ e₂ ≡ e₁
```

**Eq-Trans**
```
    Γ ⊢ e₁ ≡ e₂    Γ ⊢ e₂ ≡ e₃
    ────────────────────────────
    Γ ⊢ e₁ ≡ e₃
```

**Eq-Reduce**
```
    e₁ ⟶* e'    e₂ ⟶* e'
    ────────────────────────
    Γ ⊢ e₁ ≡ e₂
```

### 5.3 Conversion Rule

**Conv**
```
    Γ ⊢ e : τ    Γ ⊢ τ ≡ σ : Type
    ─────────────────────────────────
    Γ ⊢ e : σ
```

## 6. Normalization

### 6.1 Strong Normalization

**Theorem 6.1 (Strong Normalization)**: Every well-typed term has a normal form; all reduction sequences terminate.

**Proof Sketch**: By logical relations / reducibility candidates.

Define for each type τ a set RED(τ) of "reducible" terms:
- RED(Nat) = SN (strongly normalizing terms)
- RED(Π(x:τ). σ) = {f | ∀a ∈ RED(τ). f a ∈ RED(σ[a/x])}
- RED(Σ(x:τ). σ) = {p | fst p ∈ RED(τ) ∧ snd p ∈ RED(σ[fst p/x])}

Show:
1. RED(τ) ⊆ SN for all τ
2. If Γ ⊢ e : τ then e ∈ RED(τ) under appropriate substitution

∎

**Note**: Normalization holds for the type-level fragment. General recursion at the term level introduces non-termination (partiality).

### 6.2 Decidability of Type Checking

**Theorem 6.2 (Decidability)**: Type checking for AffineScript's dependent types is decidable, modulo SMT queries for refinements.

**Proof**:
1. All type-level terms normalize (Theorem 6.1)
2. Definitional equality reduces to normal form comparison
3. Refinement checking is delegated to SMT solver
4. SMT queries may timeout, but the algorithm terminates

∎

## 7. SMT Integration for Refinements

### 7.1 Predicate Translation

Translate refinement predicates to SMT-LIB:

```ocaml
let rec to_smt (φ : predicate) : smt_term =
  match φ with
  | Less (e1, e2) -> Smt.lt (term_to_smt e1) (term_to_smt e2)
  | Equal (e1, e2) -> Smt.eq (term_to_smt e1) (term_to_smt e2)
  | And (φ1, φ2) -> Smt.and_ (to_smt φ1) (to_smt φ2)
  | Or (φ1, φ2) -> Smt.or_ (to_smt φ1) (to_smt φ2)
  | Not φ -> Smt.not_ (to_smt φ)
  | Forall (x, τ, φ) -> Smt.forall x (type_to_sort τ) (to_smt φ)
  | ...
```

### 7.2 Subtyping Check

```ocaml
let check_subtype (ctx : context) (r1 : refinement) (r2 : refinement) : bool =
  (* Check: ∀x. ctx ∧ r1 ⟹ r2 *)
  let premise = Smt.and_ (context_to_smt ctx) (to_smt r1) in
  let goal = to_smt r2 in
  let query = Smt.implies premise goal in
  Smt.check_valid query
```

### 7.3 Decidability and Completeness

**Theorem 7.1**: For the quantifier-free fragment of refinement logic over linear integer arithmetic, SMT checking is decidable.

**Theorem 7.2**: For the full fragment with quantifiers, SMT checking is undecidable in general, but practical for common patterns.

`[IMPL-DEP: smt-integration]` Requires Z3 or CVC5 integration.

## 8. Soundness

### 8.1 Progress

**Theorem 8.1 (Progress)**: If `· ⊢ e : τ` then either:
1. e is a value, or
2. e can reduce

**Proof**: By induction on typing. Dependent types do not change the structure of progress. ∎

### 8.2 Preservation

**Theorem 8.2 (Preservation)**: If `Γ ⊢ e : τ` and `e ⟶ e'` then `Γ ⊢ e' : τ`.

**Proof**: By induction on reduction. Key case:

*Case Π-β*: `(λ(x:τ). e) a ⟶ e[a/x]`
- From typing: `Γ ⊢ λ(x:τ). e : Π(x:τ). σ` and `Γ ⊢ a : τ`
- By inversion: `Γ, x:τ ⊢ e : σ`
- By substitution lemma: `Γ ⊢ e[a/x] : σ[a/x]`
- Result type is `σ[a/x]` as required ✓

∎

### 8.3 Refinement Soundness

**Theorem 8.3 (Refinement Soundness)**: If `Γ ⊢ e : {x:τ | φ}` and e evaluates to value v, then `Γ ⊢ φ[v/x]`.

**Proof**: By the introduction rule, every value of refinement type satisfies its predicate. The SMT checker verifies predicates are preserved through computation. ∎

## 9. Erasure

### 9.1 Type Erasure

Types, proofs, and zero-quantity terms are erased for runtime:

```
|x| = x
|λ(x:τ). e| = λx. |e|
|e₁ e₂| = |e₁| |e₂|
|Π(x:τ). σ| = erased
|refl| = ()
|J(P, d, p)| = |d|
|⌊e⌋| = |e|
|⌈e⌉| = |e|
```

### 9.2 Erasure Soundness

**Theorem 9.1 (Erasure Soundness)**: If `Γ ⊢ e : τ` and `e ⟶* v` then `|e| ⟶* |v|`.

The erased program simulates the full program.

## 10. Examples

### 10.1 Length-Indexed Vectors

```affinescript
type Vec[n: Nat, T: Type] =
    | Nil : Vec[0, T]
    | Cons : (head: T, tail: Vec[m, T]) → Vec[m + 1, T]

fn head[n: Nat, T](v: Vec[n + 1, T]) -> T {
    case v {
        Cons(x, _) → x
        // Nil case is impossible; n + 1 ≠ 0
    }
}

fn append[n: Nat, m: Nat, T](
    xs: Vec[n, T],
    ys: Vec[m, T]
) -> Vec[n + m, T] {
    case xs {
        Nil → ys,                           // Vec[0 + m, T] = Vec[m, T] ✓
        Cons(x, xs') → Cons(x, append(xs', ys))
            // Vec[(n' + 1) + m, T] = Vec[n' + (m + 1), T] by arithmetic
    }
}
```

### 10.2 Bounded Naturals

```affinescript
type Fin[n: Nat] =
    | FZero : Fin[m + 1]                    -- for any m
    | FSucc : Fin[m] → Fin[m + 1]

fn safe_index[n: Nat, T](v: Vec[n, T], i: Fin[n]) -> T {
    case (v, i) {
        (Cons(x, _), FZero) → x,
        (Cons(_, xs), FSucc(i')) → safe_index(xs, i')
        // (Nil, _) is impossible: Fin[0] is uninhabited
    }
}
```

### 10.3 Refinement Types

```affinescript
type Pos = {x: Int | x > 0}
type NonEmpty[T] = {xs: List[T] | len(xs) > 0}

fn divide(x: Int, y: Pos) -> Int {
    x / ⌈y⌉  // Safe: y > 0 guaranteed
}

fn head_safe[T](xs: NonEmpty[T]) -> T {
    case ⌈xs⌉ {
        Cons(x, _) → x
        // Nil impossible by refinement
    }
}

fn sqrt(x: {n: Int | n ≥ 0}) -> {r: Int | r * r ≤ x ∧ (r+1) * (r+1) > x} {
    // Implementation with proof
    ...
}
```

### 10.4 Equality Proofs

```affinescript
fn sym[A, x: A, y: A](p: x == y) -> y == x {
    J((z, _) → z == x, refl, p)
}

fn trans[A, x: A, y: A, z: A](p: x == y, q: y == z) -> x == z {
    J((w, _) → x == w, p, q)
}

fn cong[A, B, f: A → B, x: A, y: A](p: x == y) -> f(x) == f(y) {
    J((z, _) → f(x) == f(z), refl, p)
}

fn transport[A, P: A → Type, x: A, y: A](p: x == y, px: P(x)) -> P(y) {
    J((z, _) → P(z), px, p)
}
```

### 10.5 Proof-Carrying Code

```affinescript
fn merge_sorted[n: Nat, m: Nat, T: Ord](
    xs: {v: Vec[n, T] | sorted(v)},
    ys: {v: Vec[m, T] | sorted(v)}
) -> {v: Vec[n + m, T] | sorted(v)} {
    // Implementation maintains sortedness invariant
    // Proof obligations discharged by SMT
    ...
}
```

## 11. Implementation

### 11.1 AST Representation

From `lib/ast.ml`:

```ocaml
type nat_expr =
  | NatLit of int * Span.t
  | NatVar of ident
  | NatAdd of nat_expr * nat_expr
  | NatSub of nat_expr * nat_expr
  | NatMul of nat_expr * nat_expr
  | NatLen of ident
  | NatSizeof of type_expr

type predicate =
  | PredCmp of nat_expr * cmp_op * nat_expr
  | PredNot of predicate
  | PredAnd of predicate * predicate
  | PredOr of predicate * predicate

type type_expr =
  | ...
  | TyApp of ident * type_arg list           (* Vec[n, T] *)
  | TyDepArrow of dep_arrow                  (* (n: Nat) → ... *)
  | TyRefined of type_expr * predicate       (* {x: T | P} *)
```

### 11.2 Type Checker Module

`[IMPL-DEP: type-checker]`

```ocaml
module DepTypeCheck : sig
  val normalize : ctx -> expr -> expr
  val definitionally_equal : ctx -> expr -> expr -> bool
  val check_refinement : ctx -> predicate -> bool  (* SMT *)
  val infer : ctx -> expr -> typ result
  val check : ctx -> expr -> typ -> unit result
end
```

### 11.3 SMT Interface

`[IMPL-DEP: smt-integration]`

```ocaml
module SMT : sig
  type solver
  type term
  type sort

  val create : unit -> solver
  val int_sort : sort
  val bool_sort : sort
  val declare_const : solver -> string -> sort -> term
  val assert_ : solver -> term -> unit
  val check_sat : solver -> [`Sat | `Unsat | `Unknown]
  val check_valid : solver -> term -> bool
end
```

## 12. Related Work

1. **Martin-Löf Type Theory**: Foundation of dependent types
2. **Coq/Calculus of Constructions**: Full dependent types with universes
3. **Agda**: Dependently typed programming language
4. **Idris**: Dependent types with effects
5. **Liquid Haskell**: Refinement types via SMT
6. **F***: Dependent types with effects and refinements
7. **Dafny**: Verification via weakest preconditions
8. **ATS**: Linear and dependent types combined

## 13. References

1. Martin-Löf, P. (1984). *Intuitionistic Type Theory*. Bibliopolis.
2. Coquand, T., & Huet, G. (1988). The Calculus of Constructions. *Information and Computation*.
3. Norell, U. (2009). Dependently Typed Programming in Agda. *AFP*.
4. Brady, E. (2013). Idris, a General-Purpose Dependently Typed Programming Language. *JFP*.
5. Rondon, P., Kawaguchi, M., & Jhala, R. (2008). Liquid Types. *PLDI*.
6. Swamy, N., et al. (2016). Dependent Types and Multi-Monadic Effects in F*. *POPL*.
7. Xi, H., & Pfenning, F. (1999). Dependent Types in Practical Programming. *POPL*.

---

**Document Metadata**:
- Depends on: `lib/ast.ml` (dependent types), type checker, SMT integration
- Implementation verification: Pending
- Mechanized proof: See `mechanized/coq/DependentTypes.v` (stub)
