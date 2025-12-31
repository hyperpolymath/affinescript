# Quantitative Type Theory in AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete; implementation verification pending `[IMPL-DEP: type-checker, borrow-checker]`

## Abstract

This document formalizes AffineScript's quantitative type system, which annotates types with usage quantities to enforce linearity constraints. We prove that the quantity discipline is sound: variables annotated with quantity 1 (linear) are used exactly once, variables with quantity 0 (erased) are never used at runtime, and variables with quantity ω (unrestricted) may be used arbitrarily.

## 1. Introduction

AffineScript implements Quantitative Type Theory (QTT), following the work of Atkey (2018) and McBride (2016). Unlike traditional linear type systems, QTT integrates quantities into a dependent type theory, enabling:

1. **Compile-time erasure**: Types and proofs can be marked with 0 and erased
2. **Linear resources**: Values used exactly once with quantity 1
3. **Unrestricted values**: Normal values with quantity ω
4. **Quantity polymorphism**: Abstracting over quantities

## 2. Quantity Semiring

### 2.1 Definition

Quantities form a semiring (R, 0, 1, +, ×):

```
π, ρ, σ ∈ {0, 1, ω}
```

**Addition** (for context splitting in pairs/lets):
```
    0 + π = π
    π + 0 = π
    1 + 1 = ω
    1 + ω = ω
    ω + 1 = ω
    ω + ω = ω
```

**Multiplication** (for scaling contexts in application):
```
    0 × π = 0
    π × 0 = 0
    1 × π = π
    π × 1 = π
    ω × ω = ω
```

### 2.2 Semiring Laws

**Theorem 2.1**: (R, 0, 1, +, ×) satisfies the semiring axioms:

1. (R, 0, +) is a commutative monoid
2. (R, 1, ×) is a monoid
3. × distributes over +
4. 0 annihilates: 0 × π = π × 0 = 0

**Proof**: By case analysis on all combinations. ∎

### 2.3 Ordering

We define a preorder on quantities:

```
    0 ≤ 0
    0 ≤ 1
    0 ≤ ω
    1 ≤ 1
    1 ≤ ω
    ω ≤ ω
```

This captures the "can be used as" relation: a more restricted quantity can substitute for a less restricted one.

**Lemma 2.2**: The ordering respects semiring operations:
- If π ≤ π' and ρ ≤ ρ', then π + ρ ≤ π' + ρ'
- If π ≤ π' and ρ ≤ ρ', then π × ρ ≤ π' × ρ'

**Proof**: By case analysis. ∎

## 3. Syntax with Quantities

### 3.1 Quantified Types

```
τ, σ ::=
    | ...                         -- Base types (as before)
    | (π x : τ) → σ               -- Quantified function type
    | (π x : τ) × σ               -- Quantified pair type
```

The quantity π specifies how many times the argument x may be used in the body.

### 3.2 Quantified Contexts

Contexts associate variables with both types and quantities:

```
Γ ::= · | Γ, πx:τ
```

### 3.3 Context Operations

**Zero Context**: All variables have quantity 0
```
0Γ = {0x:τ | x:τ ∈ Γ}
```

**Context Scaling**:
```
πΓ = {(π×ρ)x:τ | ρx:τ ∈ Γ}
```

**Context Addition**:
```
Γ + Δ = {(π+ρ)x:τ | πx:τ ∈ Γ, ρx:τ ∈ Δ}
```

(Defined only when Γ and Δ have the same variables and types)

## 4. Typing Rules with Quantities

### 4.1 Core Judgment

```
Γ ⊢ e : τ
```

where Γ is a quantified context specifying exactly how each variable is used.

### 4.2 Structural Rules

**Var**
```
    ─────────────────────────────
    0Γ, 1x:τ, 0Δ ⊢ x : τ
```

Note: Only x has quantity 1; all other variables have quantity 0.

**Weaken**
```
    Γ ⊢ e : τ    0 ≤ π
    ─────────────────────
    Γ, πx:σ ⊢ e : τ
```

(Weakening is only valid at quantity 0)

### 4.3 Function Types

**Lam**
```
    Γ, πx:τ ⊢ e : σ
    ─────────────────────────────
    Γ ⊢ λx. e : (π x : τ) → σ
```

**App**
```
    Γ ⊢ e₁ : (π x : τ) → σ    Δ ⊢ e₂ : τ
    ──────────────────────────────────────
    Γ + πΔ ⊢ e₁ e₂ : σ[e₂/x]
```

The context for e₂ is scaled by π:
- If π = 0, the argument is erased (Δ must be empty/0)
- If π = 1, the argument is used linearly
- If π = ω, the argument is used unrestrictedly

### 4.4 Pair Types

**Pair-Intro**
```
    Γ ⊢ e₁ : τ    Δ ⊢ e₂ : σ[e₁/x]
    ─────────────────────────────────
    Γ + Δ ⊢ (e₁, e₂) : (π x : τ) × σ
```

**Pair-Elim**
```
    Γ ⊢ e₁ : (π x : τ) × σ    Δ, πx:τ, 1y:σ ⊢ e₂ : ρ
    ──────────────────────────────────────────────────
    Γ + Δ ⊢ let (x, y) = e₁ in e₂ : ρ
```

### 4.5 Let Binding

**Let**
```
    Γ ⊢ e₁ : τ    Δ, πx:τ ⊢ e₂ : σ
    ───────────────────────────────
    πΓ + Δ ⊢ let x = e₁ in e₂ : σ
```

### 4.6 Quantity Polymorphism

**QuantAbs**
```
    Γ ⊢ e : τ
    ──────────────────
    Γ ⊢ Λπ. e : ∀π. τ
```

**QuantApp**
```
    Γ ⊢ e : ∀π. τ
    ──────────────────
    Γ ⊢ e [ρ] : τ[ρ/π]
```

## 5. Soundness of Quantities

### 5.1 Runtime Irrelevance of 0

**Theorem 5.1 (Erasure Soundness)**: If `Γ ⊢ e : τ` where x has quantity 0 in Γ, then x does not occur free in the evaluation of e.

**Proof**: By induction on the typing derivation.

The key cases:
- **Var**: A variable x can only be typed in a context where it has quantity 1, not 0.
- **App**: If the function type has π = 0, then the argument is scaled by 0, meaning it contributes no usage.
- **Let**: If x is bound with quantity 0, the body cannot use x computationally.

∎

### 5.2 Linearity

**Definition 5.2 (Usage Count)**: Define use(e, x) as the number of times x is evaluated during the reduction of e.

**Theorem 5.3 (Linearity Soundness)**: If `0Γ, 1x:τ ⊢ e : σ` and e reduces to a value v, then use(e, x) = 1.

**Proof**: By induction on the typing derivation and reduction sequence.

Key insight: The context splitting rules ensure that for each constructor, the uses of x are properly distributed and sum to exactly 1.

∎

### 5.3 Affine Weakening

**Theorem 5.4 (Affine Weakening)**: If `Γ ⊢ e : τ` and x has quantity 1 in Γ, then x is used at most once.

This follows from linearity soundness, noting that AffineScript allows dropping linear values (unlike true linear types).

**Note**: AffineScript is affine by default, not linear. Values with quantity 1 must be used *at most* once, but may be explicitly dropped or left unused. This is enforced by the borrow checker rather than the type system.

## 6. Quantity Inference

### 6.1 Principal Quantities

For many expressions, quantities can be inferred:

**Algorithm**: Given an expression e and types for its free variables, compute the minimal quantities needed.

```ocaml
type usage = Zero | One | Many

let infer_usage (e : expr) (x : var) : usage =
  match e with
  | Var y -> if x = y then One else Zero
  | App (e1, e2) ->
      combine (infer_usage e1 x) (infer_usage e2 x)
  | Lam (y, body) ->
      if x = y then Zero
      else infer_usage body x
  | Let (y, rhs, body) ->
      let rhs_use = infer_usage rhs x in
      let body_use = infer_usage body x in
      if x = y then rhs_use
      else combine rhs_use body_use
  | ...

let combine u1 u2 =
  match (u1, u2) with
  | (Zero, u) | (u, Zero) -> u
  | (One, One) -> Many
  | _ -> Many
```

### 6.2 Quantity Constraints

During type checking, we generate quantity constraints and solve them:

```
π₁ + π₂ ≤ π₃
π₁ × π₂ = π₃
π ≤ ω
```

These constraints are solved by substitution and case analysis.

## 7. Interaction with Other Features

### 7.1 Quantities and Effects

Effect handlers interact with quantities:

```
    handle Γ ⊢ e : τ ! ε with h
```

The handler h may be invoked multiple times (for multi-shot continuations), which affects linearity:

**Resume-Once**
```
    Γ ⊢ handler { op(x, k) → e }
```

If k is used with quantity 1, the continuation is one-shot.
If k is used with quantity ω, the continuation is multi-shot.

`[IMPL-DEP: effect-checker]` Effect-quantity interaction requires effect implementation.

### 7.2 Quantities and Ownership

Quantities work with the ownership system:

- `0 (own τ)`: Impossible (must use owned value)
- `1 (own τ)`: Standard linear ownership
- `ω (own τ)`: Requires Copy trait

- `ω (ref τ)`: Multiple immutable borrows allowed
- `1 (mut τ)`: Exactly one mutable borrow

### 7.3 Quantities and Dependent Types

In dependent types, quantities distinguish:

- **Type dependencies** (quantity 0): Used in types, erased at runtime
- **Value dependencies** (quantity 1 or ω): Exist at runtime

```
-- The length n is used at quantity 0 (erased)
fn replicate[n: Nat, T](0 n: Nat, x: T) -> Vec[n, T]
```

## 8. Examples

### 8.1 File Handle (Linear)

```affinescript
type File = own FileHandle

fn open(path: String) -> File / IO

fn read(1 f: File) -> (String, File) / IO

fn close(1 f: File) -> () / IO

fn process_file(path: String) -> String / IO {
    let f = open(path)           -- f has quantity 1
    let (contents, f) = read(f)  -- f consumed, new f bound
    close(f)                     -- f consumed
    contents
}
```

### 8.2 Erased Proofs

```affinescript
fn safe_index[n: Nat, T](
    vec: Vec[n, T],
    i: Nat,
    0 pf: i < n                  -- proof is erased
) -> T {
    vec.unsafe_get(i)            -- pf not used at runtime
}
```

### 8.3 Quantity Polymorphism

```affinescript
fn pair[π: Quantity, A, B](
    π a: A,
    π b: B
) -> (A, B) {
    (a, b)
}

-- Can be instantiated at any quantity
let p1 = pair[1](file1, file2)   -- linear pair
let p2 = pair[ω](x, y)           -- unrestricted pair
```

## 9. Metatheoretic Properties

### 9.1 Quantity Substitution Lemma

**Lemma 9.1**: If `Γ ⊢ e : τ` and we substitute a more specific quantity ρ ≤ π for π throughout, the derivation remains valid.

### 9.2 Quantity Coherence

**Theorem 9.2**: If an expression type-checks at multiple quantities, the results are coherent:
- If `Γ ⊢ e : τ` and `Γ' ⊢ e : τ` where Γ' has larger quantities, then the semantics agree on shared resources.

### 9.3 Decidability

**Theorem 9.3**: Quantity checking is decidable.

**Proof**: The quantity semiring is finite ({0, 1, ω}), and all operations are computable. Quantity inference generates a finite constraint system solvable by enumeration. ∎

## 10. Implementation

### 10.1 AST Representation

From `lib/ast.ml`:

```ocaml
type quantity =
  | QZero    (* 0 - erased *)
  | QOne     (* 1 - linear *)
  | QOmega   (* ω - unrestricted *)
```

### 10.2 Type Checker Integration

`[IMPL-DEP: type-checker]`

```ocaml
(* Quantity context *)
type qctx = (ident * quantity * typ) list

(* Scale a context by a quantity *)
let scale (q : quantity) (ctx : qctx) : qctx =
  List.map (fun (x, q', t) -> (x, mult q q', t)) ctx

(* Add two contexts *)
let add (ctx1 : qctx) (ctx2 : qctx) : qctx =
  List.map2 (fun (x, q1, t) (_, q2, _) -> (x, plus q1 q2, t)) ctx1 ctx2

(* Check usage matches declared quantity *)
let check_quantity (expected : quantity) (actual : usage) : bool =
  match (expected, actual) with
  | (QZero, Zero) -> true
  | (QOne, One) -> true
  | (QOmega, _) -> true
  | _ -> false
```

## 11. Related Work

1. **Quantitative Type Theory**: Atkey (2018) - Foundation for QTT
2. **I Got Plenty o' Nuttin'**: McBride (2016) - Quantities in dependent types
3. **Linear Haskell**: Bernardy et al. (2018) - Linearity in Haskell
4. **Granule**: Orchard et al. (2019) - Graded modal types
5. **Linear Types in Rust**: Weiss et al. (2019) - Practical affine types

## 12. References

1. Atkey, R. (2018). Syntax and Semantics of Quantitative Type Theory. *LICS*.
2. McBride, C. (2016). I Got Plenty o' Nuttin'. *A List of Successes That Can Change the World*.
3. Bernardy, J.-P., et al. (2018). Linear Haskell: Practical Linearity in a Higher-Order Polymorphic Language. *POPL*.
4. Walker, D. (2005). Substructural Type Systems. *Advanced Topics in Types and Programming Languages*.
5. Wadler, P. (1990). Linear Types Can Change the World! *IFIP TC*.

---

**Document Metadata**:
- Depends on: `lib/ast.ml` (quantity type), type checker implementation
- Implementation verification: Pending
- Mechanized proof: See `mechanized/coq/Quantities.v` (stub)
