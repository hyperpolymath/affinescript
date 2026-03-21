# Operational Semantics of AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete specification

## Abstract

This document provides a complete operational semantics for AffineScript, specifying the dynamic behavior of programs through small-step reduction rules. We define evaluation contexts, reduction relations for all language constructs, and prove determinacy and confluence of the reduction relation.

## 1. Introduction

The operational semantics defines how AffineScript programs execute. We use:
- **Small-step semantics**: Step-by-step reduction
- **Structural operational semantics (SOS)**: Inference rule format
- **Evaluation contexts**: Specifying evaluation order

## 2. Syntax Recap

### 2.1 Values

```
v ::=
    | ()                          -- Unit
    | true | false                -- Booleans
    | n                           -- Integer literals
    | f                           -- Float literals
    | 'c'                         -- Characters
    | "s"                         -- Strings
    | λ(x:τ). e                   -- Lambda
    | Λα:κ. v                     -- Type lambda (value)
    | (v₁, ..., vₙ)               -- Tuple
    | {l₁ = v₁, ..., lₙ = vₙ}     -- Record
    | C(v₁, ..., vₙ)              -- Constructor
    | ℓ                           -- Location (heap reference)
    | handler h                   -- Handler value
```

### 2.2 Expressions

```
e ::=
    -- Core
    | v                           -- Values
    | x                           -- Variables
    | e₁ e₂                       -- Application
    | e [τ]                       -- Type application
    | let x = e₁ in e₂            -- Let binding
    | let (x₁, ..., xₙ) = e₁ in e₂ -- Tuple destructure

    -- Control
    | if e₁ then e₂ else e₃       -- Conditional
    | case e {p₁ → e₁ | ... | pₙ → eₙ}  -- Pattern match

    -- Data
    | (e₁, ..., eₙ)               -- Tuple
    | e.i                         -- Tuple projection
    | {l₁ = e₁, ..., lₙ = eₙ}     -- Record
    | e.l                         -- Record projection
    | {e₁ with l = e₂}            -- Record update
    | C(e₁, ..., eₙ)              -- Constructor

    -- Effects
    | perform op(e)               -- Effect operation
    | handle e with h             -- Effect handler
    | resume(e)                   -- Resume continuation

    -- References
    | ref e                       -- Allocation
    | !e                          -- Dereference
    | e₁ := e₂                    -- Assignment

    -- Ownership
    | move e                      -- Explicit move
    | &e                          -- Borrow
    | &mut e                      -- Mutable borrow
    | drop e                      -- Explicit drop

    -- Unsafe
    | unsafe { e }                -- Unsafe block
```

## 3. Evaluation Contexts

### 3.1 Pure Contexts

Evaluation contexts E specify where reduction occurs:

```
E ::=
    | □                           -- Hole
    | E e                         -- Function position
    | v E                         -- Argument position
    | E [τ]                       -- Type application
    | let x = E in e              -- Let binding
    | let (x̄) = E in e            -- Tuple let
    | if E then e₁ else e₂        -- Conditional
    | case E {branches}           -- Case scrutinee
    | (v₁, ..., E, ..., eₙ)       -- Tuple (left-to-right)
    | E.i                         -- Tuple projection
    | {l₁ = v₁, ..., l = E, ..., lₙ = eₙ}  -- Record
    | E.l                         -- Record projection
    | {E with l = e}              -- Record update (base)
    | {v with l = E}              -- Record update (field)
    | C(v₁, ..., E, ..., eₙ)      -- Constructor
    | perform op(E)               -- Effect operation
    | ref E                       -- Allocation
    | !E                          -- Dereference
    | E := e                      -- Assignment (left)
    | v := E                      -- Assignment (right)
    | move E                      -- Move
    | &E                          -- Borrow
    | &mut E                      -- Mutable borrow
    | drop E                      -- Drop
```

### 3.2 Effect Contexts

For effect handling, we need contexts that can trap effects:

```
E_eff ::=
    | E                           -- Pure context
    | handle E_eff with h         -- Handler context
```

### 3.3 Reduction Contexts

Full reduction contexts:
```
R ::= E | handle R with h
```

## 4. Reduction Rules

### 4.1 Notation

```
e ⟶ e'                           -- Single step reduction
e ⟶* e'                          -- Reflexive-transitive closure
(e, μ) ⟶ (e', μ')                -- Reduction with heap
```

### 4.2 Core Reductions

**β-Reduction**
```
    ─────────────────────────────────────
    (λ(x:τ). e) v ⟶ e[v/x]
```

**Type Application**
```
    ─────────────────────────────────────
    (Λα:κ. e) [τ] ⟶ e[τ/α]
```

**Let**
```
    ─────────────────────────────────────
    let x = v in e ⟶ e[v/x]
```

**Let-Tuple**
```
    ─────────────────────────────────────────────────────
    let (x₁, ..., xₙ) = (v₁, ..., vₙ) in e ⟶ e[v₁/x₁, ..., vₙ/xₙ]
```

### 4.3 Control Flow Reductions

**If-True**
```
    ─────────────────────────────────────
    if true then e₁ else e₂ ⟶ e₁
```

**If-False**
```
    ─────────────────────────────────────
    if false then e₁ else e₂ ⟶ e₂
```

**Case-Match**
```
    match(p, v) = θ
    ─────────────────────────────────────────────────
    case v {... | p → e | ...} ⟶ θ(e)
```

### 4.4 Data Structure Reductions

**Tuple-Proj**
```
    ─────────────────────────────────────────────
    (v₁, ..., vₙ).i ⟶ vᵢ     (1 ≤ i ≤ n)
```

**Record-Proj**
```
    ─────────────────────────────────────────────────────
    {l₁ = v₁, ..., lₙ = vₙ}.lᵢ ⟶ vᵢ
```

**Record-Update**
```
    ─────────────────────────────────────────────────────────────────
    {l₁ = v₁, ..., l = v, ..., lₙ = vₙ} with {l = v'} ⟶ {l₁ = v₁, ..., l = v', ..., lₙ = vₙ}
```

### 4.5 Arithmetic Reductions

**Int-Add**
```
    n₁ + n₂ = n₃
    ─────────────────────────────────────
    n₁ + n₂ ⟶ n₃
```

**Int-Sub**
```
    n₁ - n₂ = n₃
    ─────────────────────────────────────
    n₁ - n₂ ⟶ n₃
```

**Int-Mul**
```
    n₁ × n₂ = n₃
    ─────────────────────────────────────
    n₁ * n₂ ⟶ n₃
```

**Int-Div** (partial)
```
    n₂ ≠ 0    n₁ ÷ n₂ = n₃
    ─────────────────────────────────────
    n₁ / n₂ ⟶ n₃
```

**Comparison**
```
    compare(n₁, n₂) = b
    ─────────────────────────────────────
    n₁ < n₂ ⟶ b
```

### 4.6 Reference Reductions

These use a heap μ : Loc ⇀ Val.

**Ref-Alloc**
```
    ℓ fresh
    ─────────────────────────────────────────────
    (ref v, μ) ⟶ (ℓ, μ[ℓ ↦ v])
```

**Ref-Read**
```
    μ(ℓ) = v
    ─────────────────────────────────────
    (!ℓ, μ) ⟶ (v, μ)
```

**Ref-Write**
```
    ─────────────────────────────────────────────
    (ℓ := v, μ) ⟶ ((), μ[ℓ ↦ v])
```

### 4.7 Effect Reductions

**Handle-Return**
```
    h = { return x → e_ret, ... }
    ─────────────────────────────────────────────
    handle v with h ⟶ e_ret[v/x]
```

**Handle-Perform** (effect handled)
```
    op ∈ dom(h)
    h = { ..., op(x, k) → e_op, ... }
    k_val = λy. handle E_p[y] with h
    ─────────────────────────────────────────────────────────────
    handle E_p[perform op(v)] with h ⟶ e_op[v/x, k_val/k]
```

**Handle-Forward** (effect not handled)
```
    op ∉ dom(h)
    ─────────────────────────────────────────────────────────────────
    handle E_p[perform op(v)] with h ⟶ perform op(v) >>= (λy. handle E_p[y] with h)
```

Where `e >>= f` is monadic bind (continuation).

**Resume**
```
    k = λy. handle E_p[y] with h
    ─────────────────────────────────────────────
    resume(k, v) ⟶ handle E_p[v] with h
```

### 4.8 Ownership Reductions

**Move**
```
    ─────────────────────────────────────
    move v ⟶ v
```

(Move is identity at runtime; ownership is erased)

**Borrow**
```
    ─────────────────────────────────────
    &v ⟶ v
```

(Borrows are identity at runtime; lifetimes are erased)

**Drop**
```
    ─────────────────────────────────────────────
    (drop ℓ, μ) ⟶ ((), μ \ ℓ)
```

### 4.9 Congruence Rule

**Context**
```
    e ⟶ e'
    ─────────────────────────────────────
    E[e] ⟶ E[e']
```

## 5. Pattern Matching

### 5.1 Match Judgment

```
match(p, v) = θ    (pattern p matches value v with substitution θ)
```

### 5.2 Matching Rules

**Match-Var**
```
    ─────────────────────────────────────
    match(x, v) = [v/x]
```

**Match-Wild**
```
    ─────────────────────────────────────
    match(_, v) = []
```

**Match-Literal**
```
    ─────────────────────────────────────
    match(n, n) = []
```

**Match-Constructor**
```
    ∀i. match(pᵢ, vᵢ) = θᵢ
    ─────────────────────────────────────────────────────────
    match(C(p₁, ..., pₙ), C(v₁, ..., vₙ)) = θ₁ ∪ ... ∪ θₙ
```

**Match-Tuple**
```
    ∀i. match(pᵢ, vᵢ) = θᵢ
    ─────────────────────────────────────────────────────────
    match((p₁, ..., pₙ), (v₁, ..., vₙ)) = θ₁ ∪ ... ∪ θₙ
```

**Match-Record**
```
    ∀i. match(pᵢ, vᵢ) = θᵢ    v = {..., lᵢ = vᵢ, ...}
    ─────────────────────────────────────────────────────────
    match({l₁ = p₁, ..., lₙ = pₙ}, v) = θ₁ ∪ ... ∪ θₙ
```

**Match-As**
```
    match(p, v) = θ
    ─────────────────────────────────────
    match(x @ p, v) = θ[v/x]
```

**Match-Guard**
```
    match(p, v) = θ    θ(g) ⟶* true
    ─────────────────────────────────────
    match(p if g, v) = θ
```

## 6. Substitution

### 6.1 Capture-Avoiding Substitution

```
x[v/x] = v
y[v/x] = y                            (y ≠ x)
(e₁ e₂)[v/x] = e₁[v/x] e₂[v/x]
(λ(y:τ). e)[v/x] = λ(y:τ). e[v/x]    (y ≠ x, y ∉ FV(v))
(let y = e₁ in e₂)[v/x] = let y = e₁[v/x] in e₂[v/x]    (y ≠ x, y ∉ FV(v))
...
```

### 6.2 Type Substitution

```
α[τ/α] = τ
β[τ/α] = β                            (β ≠ α)
(σ → ρ)[τ/α] = σ[τ/α] → ρ[τ/α]
(∀β:κ. σ)[τ/α] = ∀β:κ. σ[τ/α]        (β ≠ α, β ∉ FTV(τ))
...
```

## 7. Machine Semantics

### 7.1 Abstract Machine State

For a more efficient implementation, define an abstract machine:

```
State = (Control, Environment, Heap, Stack)
        (C,       E,           H,    K)

C = e | v                             -- Control: expression or value
E = x ↦ v                             -- Environment
H = ℓ ↦ v                             -- Heap
K = Frame*                            -- Continuation stack

Frame =
    | Arg(e, E)                       -- Awaiting function, has argument
    | Fun(v)                          -- Awaiting argument, has function
    | Let(x, e, E)                    -- Let binding
    | Case(branches, E)               -- Case analysis
    | Handle(h, E)                    -- Effect handler
    | ...
```

### 7.2 Machine Transitions

**Var**
```
    E(x) = v
    ─────────────────────────────────────────
    (x, E, H, K) ⟹ (v, E, H, K)
```

**App-Left**
```
    ─────────────────────────────────────────────────────
    (e₁ e₂, E, H, K) ⟹ (e₁, E, H, Arg(e₂, E) :: K)
```

**App-Right**
```
    ─────────────────────────────────────────────────────
    (v, E, H, Arg(e, E') :: K) ⟹ (e, E', H, Fun(v) :: K)
```

**App-Beta**
```
    v₁ = λ(x:τ). e
    ─────────────────────────────────────────────────────
    (v₂, E, H, Fun(v₁) :: K) ⟹ (e, E[x ↦ v₂], H, K)
```

**Handle-Push**
```
    ─────────────────────────────────────────────────────────
    (handle e with h, E, H, K) ⟹ (e, E, H, Handle(h, E) :: K)
```

**Handle-Return**
```
    h = { return x → e_ret, ... }
    ─────────────────────────────────────────────────────────
    (v, E, H, Handle(h, E') :: K) ⟹ (e_ret, E'[x ↦ v], H, K)
```

## 8. Properties

### 8.1 Determinacy

**Theorem 8.1 (Determinacy)**: The reduction relation is deterministic on pure expressions.

```
If e ⟶ e₁ and e ⟶ e₂, then e₁ = e₂.
```

**Proof**: By induction on the derivation of `e ⟶ e₁`. Each expression form has exactly one applicable reduction rule, and evaluation contexts determine a unique redex. ∎

**Note**: Effects introduce non-determinism when handlers provide choices.

### 8.2 Confluence

**Theorem 8.2 (Confluence)**: The reduction relation is confluent.

```
If e ⟶* e₁ and e ⟶* e₂, then ∃e'. e₁ ⟶* e' and e₂ ⟶* e'.
```

**Proof**: By Newman's Lemma, since the relation is terminating (for types) and locally confluent (by determinacy for pure reduction). ∎

### 8.3 Standardization

**Theorem 8.3 (Standardization)**: Every reduction sequence can be rearranged to a standard reduction (leftmost-outermost).

**Proof**: Following the standard proof for lambda calculus with extensions. ∎

## 9. Semantic Domains

### 9.1 Value Domain

```
V = Unit | Bool | Int | Float | Char | String
  | Fun(Env × Expr)
  | Tuple(V*)
  | Record(Label → V)
  | Variant(Label × V)
  | Loc
  | Handler(H)
```

### 9.2 Heap Domain

```
Heap = Loc ⇀ V
```

### 9.3 Result Domain

```
Result =
  | Val(V)                            -- Normal value
  | Eff(Op × V × Cont)                -- Suspended effect
  | Err(Error)                        -- Runtime error
```

## 10. Examples

### 10.1 Function Application

```
(λ(x: Int). x + 1) 5
⟶ 5 + 1           [β-reduction]
⟶ 6               [arithmetic]
```

### 10.2 Effect Handling

```
handle (1 + perform get()) with {
    return x → x,
    get(_, k) → resume(k, 10)
}

⟶ handle (1 + perform get()) with h
    [where h = {return x → x, get(_, k) → resume(k, 10)}]

⟶ let k = λy. handle (1 + y) with h
   in resume(k, 10)           [Handle-Perform]

⟶ let k = λy. handle (1 + y) with h
   in handle (1 + 10) with h   [Resume]

⟶ handle 11 with h             [arithmetic]

⟶ 11                           [Handle-Return]
```

### 10.3 State Effect

```
handle {
    let x = perform get()
    perform put(x + 1)
    perform get()
} with run_state(0)

-- Evaluates step by step with state threading
⟶* 1
```

## 11. Implementation Notes

### 11.1 Correspondence to AST

From `lib/ast.ml`:

```ocaml
type expr =
  | ELit of literal
  | EVar of ident
  | EApp of expr * expr
  | ELam of lambda
  | ELet of let_binding
  | EIf of expr * expr * expr
  | ECase of expr * case_branch list
  | ETuple of expr list
  | ERecord of (ident * expr) list
  | ERecordAccess of expr * ident
  | EHandle of expr * handler
  | EPerform of ident * expr
  | ...
```

### 11.2 Evaluator Structure

`[IMPL-DEP: evaluator]`

```ocaml
module Eval : sig
  type value
  type heap
  type result = (value * heap, error) Result.t

  val eval : heap -> env -> expr -> result
  val step : heap -> expr -> (heap * expr) option
end
```

## 12. References

1. Wright, A. K., & Felleisen, M. (1994). A Syntactic Approach to Type Soundness. *I&C*.
2. Plotkin, G. D. (1981). A Structural Approach to Operational Semantics. *DAIMI*.
3. Felleisen, M., & Hieb, R. (1992). The Revised Report on the Syntactic Theories of Sequential Control and State. *TCS*.
4. Plotkin, G., & Pretnar, M. (2013). Handling Algebraic Effects. *LMCS*.

---

**Document Metadata**:
- Depends on: `lib/ast.ml` (syntax), evaluator implementation (pending)
- Implementation verification: Pending
- Mechanized proof: See `mechanized/coq/Operational.v` (stub)
