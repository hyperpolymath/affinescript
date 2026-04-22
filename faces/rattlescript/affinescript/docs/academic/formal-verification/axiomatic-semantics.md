# Axiomatic Semantics: Program Logic for AffineScript

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete specification

## Abstract

This document presents an axiomatic semantics for AffineScript based on Hoare logic and separation logic. We define program logics for reasoning about:
1. Partial and total correctness
2. Heap-manipulating programs (separation logic)
3. Effectful computations
4. Ownership and borrowing
5. Refinement type verification

## 1. Introduction

Axiomatic semantics provides:
- Proof rules for program properties
- Compositional verification
- Foundation for automated verification tools
- Connection to refinement type checking

## 2. Hoare Logic

### 2.1 Hoare Triples

**Partial Correctness**:
```
{P} e {Q}
```

If precondition P holds and e terminates, then postcondition Q holds.

**Total Correctness**:
```
[P] e [Q]
```

If P holds, then e terminates and Q holds.

### 2.2 Basic Rules

**Skip**
```
    ─────────────────
    {P} () {P}
```

**Sequence** (via let)
```
    {P} e₁ {Q}    {Q} e₂ {R}
    ──────────────────────────────
    {P} let _ = e₁ in e₂ {R}
```

**Assignment** (for mutable variables)
```
    ─────────────────────────────────
    {P[e/x]} x := e {P}
```

**Conditional**
```
    {P ∧ e₁} e₂ {Q}    {P ∧ ¬e₁} e₃ {Q}
    ─────────────────────────────────────
    {P} if e₁ then e₂ else e₃ {Q}
```

**Consequence**
```
    P' ⟹ P    {P} e {Q}    Q ⟹ Q'
    ────────────────────────────────
    {P'} e {Q'}
```

### 2.3 Loop Rule (for while)

```
    {I ∧ b} e {I}
    ─────────────────────────
    {I} while b do e {I ∧ ¬b}
```

where I is the loop invariant.

### 2.4 Function Call

```
    {P} f {Q}    (specification of f)
    ────────────────────────────────────
    {P[a/x]} f(a) {Q[a/x]}
```

## 3. Separation Logic

### 3.1 Spatial Assertions

For heap-manipulating programs, extend assertions with spatial operators:

```
P, Q ::=
    | emp                         -- Empty heap
    | e₁ ↦ e₂                     -- Singleton heap
    | P * Q                       -- Separating conjunction
    | P -* Q                      -- Magic wand (separating implication)
    | P ∧ Q                       -- Conjunction
    | P ∨ Q                       -- Disjunction
    | ∃x. P                       -- Existential
    | ∀x. P                       -- Universal
```

### 3.2 Semantics of Spatial Operators

**Empty Heap**:
```
h ⊨ emp  iff  dom(h) = ∅
```

**Points-To**:
```
h ⊨ e₁ ↦ e₂  iff  h = {⟦e₁⟧ ↦ ⟦e₂⟧}
```

**Separating Conjunction**:
```
h ⊨ P * Q  iff  ∃h₁, h₂. h = h₁ ⊎ h₂ ∧ h₁ ⊨ P ∧ h₂ ⊨ Q
```

**Magic Wand**:
```
h ⊨ P -* Q  iff  ∀h'. h' ⊨ P ∧ h # h' ⟹ h ⊎ h' ⊨ Q
```

### 3.3 Frame Rule

The key rule enabling local reasoning:

```
    {P} e {Q}
    ─────────────────────────
    {P * R} e {Q * R}
```

where FV(R) ∩ mod(e) = ∅.

### 3.4 Heap Rules

**Allocation**
```
    ─────────────────────────────────
    {emp} ref(e) {∃ℓ. ret ↦ ℓ * ℓ ↦ e}
```

**Read**
```
    ────────────────────────────────
    {ℓ ↦ v} !ℓ {ret = v * ℓ ↦ v}
```

**Write**
```
    ────────────────────────────────
    {ℓ ↦ _} ℓ := e {ℓ ↦ e}
```

**Deallocation**
```
    ────────────────────────────────
    {ℓ ↦ _} free(ℓ) {emp}
```

## 4. Ownership Logic

### 4.1 Ownership Assertions

Extend assertions for ownership:

```
P ::= ...
    | own(e, τ)                   -- Ownership of e at type τ
    | borrow(e, τ, 'a)            -- Borrow of e with lifetime 'a
    | mut_borrow(e, τ, 'a)        -- Mutable borrow
    | 'a ⊑ 'b                     -- Lifetime ordering
```

### 4.2 Ownership Rules

**Move**
```
    ─────────────────────────────────────────
    {own(x, τ)} let y = move x {own(y, τ)}
```

**Borrow**
```
    'a ⊑ lifetime(x)
    ────────────────────────────────────────────────────────
    {own(x, τ)} let y = &x {own(x, τ) * borrow(y, τ, 'a)}
```

**Mutable Borrow** (exclusive)
```
    'a ⊑ lifetime(x)
    ───────────────────────────────────────────────────────────
    {own(x, τ)} let y = &mut x {suspended(x) * mut_borrow(y, τ, 'a)}
```

**Borrow End**
```
    ───────────────────────────────────────────────
    {suspended(x) * mut_borrow(y, τ, 'a) * end('a)}
    e
    {own(x, τ')}
```

### 4.3 Affine Rule

```
    {P * own(x, τ)} e {Q}
    ────────────────────────────
    {P * own(x, τ)} e; drop(x) {Q}
```

Owned resources may be dropped (affine, not linear).

## 5. Effect Logic

### 5.1 Effect Assertions

For reasoning about effects:

```
P ::= ...
    | performs(E)                 -- May perform effect E
    | pure                        -- Performs no effects
    | handled(E)                  -- Effect E is handled
```

### 5.2 Effect Rules

**Pure**
```
    e is pure (no perform)
    ───────────────────────────
    {P * pure} e {Q * pure}
```

**Perform**
```
    op : τ → σ ∈ E
    ─────────────────────────────────────────────
    {P} perform op(e) {Q * performs(E)}
```

**Handle**
```
    {P * performs(E)} e {Q}
    {Q[v/ret]} e_ret {R}
    ∀op ∈ E. {Q' * k : σ → R} e_op {R}
    ───────────────────────────────────────────────────────
    {P} handle e with h {R * handled(E)}
```

### 5.3 Effect Frame Rule

```
    {P} e {Q * performs(ε₁)}    ε₁ ⊆ ε₂
    ───────────────────────────────────────
    {P} e {Q * performs(ε₂)}
```

## 6. Refinement Logic

### 6.1 Connection to Refinement Types

Refinement types `{x: τ | φ}` correspond to Hoare preconditions:

```
If Γ ⊢ e : {x: τ | φ} then {φ[e/x]} use(e) {...}
```

### 6.2 Verification Conditions

Generate verification conditions from refined function signatures:

```affinescript
fn divide(x: Int, y: {v: Int | v ≠ 0}) -> Int
```

generates VC:
```
∀x, y. y ≠ 0 ⟹ divide(x, y) is defined
```

### 6.3 Subtyping as Implication

```
{x: τ | φ} <: {x: τ | ψ}
```

iff

```
∀x: τ. φ ⟹ ψ
```

## 7. Total Correctness

### 7.1 Termination

For total correctness, add a variant (decreasing measure):

**While-Total**
```
    {I ∧ b ∧ V = n} e {I ∧ V < n}    V ≥ 0
    ───────────────────────────────────────────
    [I] while b do e [I ∧ ¬b]
```

### 7.2 Well-Founded Recursion

For recursive functions:

```
    ∀x. [P(x) ∧ ∀y. (y, x) ∈ R ⟹ Q(y)] f(x) [Q(x)]
    R is well-founded
    ────────────────────────────────────────────────
    [P(x)] f(x) [Q(x)]
```

## 8. Concurrent Separation Logic

### 8.1 Concurrent Rules

For concurrent AffineScript (async effects):

**Parallel**
```
    {P₁} e₁ {Q₁}    {P₂} e₂ {Q₂}
    ─────────────────────────────────────
    {P₁ * P₂} e₁ || e₂ {Q₁ * Q₂}
```

### 8.2 Lock Invariants

```
    {P * I} e {Q * I}
    ─────────────────────────────────────────────────
    {P * locked(l, I)} with_lock(l) { e } {Q * locked(l, I)}
```

### 8.3 Rely-Guarantee

For interference:
```
{P} e {Q}
R (rely): invariant maintained by environment
G (guarantee): invariant we maintain
```

## 9. Derived Proof Rules

### 9.1 Array Access

```
    {a ↦ [v₀, ..., vₙ₋₁] * 0 ≤ i < n}
    a[i]
    {ret = vᵢ * a ↦ [v₀, ..., vₙ₋₁]}
```

### 9.2 List Predicates

```
list(x, []) ≡ x = null
list(x, v::vs) ≡ ∃y. x ↦ (v, y) * list(y, vs)
```

### 9.3 Tree Predicates

```
tree(x, Leaf) ≡ x = null
tree(x, Node(v, l, r)) ≡ ∃y, z. x ↦ (v, y, z) * tree(y, l) * tree(z, r)
```

## 10. Soundness

### 10.1 Interpretation

Define satisfaction: s, h ⊨ P (state s and heap h satisfy P)

### 10.2 Soundness Theorem

**Theorem 10.1 (Soundness)**: If `{P} e {Q}` is derivable, then:
```
∀s, h. s, h ⊨ P ⟹ ∀s', h'. (e, s, h) ⟶* (v, s', h') ⟹ s', h' ⊨ Q[v/ret]
```

**Proof**: By induction on the derivation, using the operational semantics. ∎

### 10.3 Relative Completeness

**Theorem 10.2 (Relative Completeness)**: If `⊨ {P} e {Q}` holds semantically, then `{P} e {Q}` is derivable (relative to the assertion logic).

## 11. Examples

### 11.1 Swap

```affinescript
fn swap(x: mut Int, y: mut Int) {
    let t = *x
    *x = *y
    *y = t
}
```

Specification:
```
{x ↦ a * y ↦ b}
swap(x, y)
{x ↦ b * y ↦ a}
```

Proof:
```
{x ↦ a * y ↦ b}
let t = *x
{x ↦ a * y ↦ b * t = a}
*x = *y
{x ↦ b * y ↦ b * t = a}
*y = t
{x ↦ b * y ↦ a}
```

### 11.2 List Append

```affinescript
fn append(xs: own List[T], ys: own List[T]) -> own List[T] {
    case xs {
        Nil → ys,
        Cons(x, xs') → Cons(x, append(xs', ys))
    }
}
```

Specification:
```
{list(xs, as) * list(ys, bs)}
append(xs, ys)
{∃r. list(r, as ++ bs) * ret = r}
```

### 11.3 Binary Search

```affinescript
fn binary_search(arr: ref [Int], target: Int) -> Option[Nat] {
    let mut lo = 0
    let mut hi = arr.len()
    while lo < hi {
        let mid = lo + (hi - lo) / 2
        if arr[mid] == target {
            return Some(mid)
        } else if arr[mid] < target {
            lo = mid + 1
        } else {
            hi = mid
        }
    }
    None
}
```

Specification:
```
{sorted(arr) * len(arr) = n}
binary_search(arr, target)
{ret = Some(i) ⟹ arr[i] = target ∧ 0 ≤ i < n}
{ret = None ⟹ ∀i. 0 ≤ i < n ⟹ arr[i] ≠ target}
```

Loop invariant:
```
I ≡ 0 ≤ lo ≤ hi ≤ n ∧
    (∀j. 0 ≤ j < lo ⟹ arr[j] < target) ∧
    (∀j. hi ≤ j < n ⟹ arr[j] > target)
```

## 12. Automation

### 12.1 Verification Condition Generation

Weakest precondition:
```
wp(skip, Q) = Q
wp(x := e, Q) = Q[e/x]
wp(e₁; e₂, Q) = wp(e₁, wp(e₂, Q))
wp(if b then e₁ else e₂, Q) = (b ⟹ wp(e₁, Q)) ∧ (¬b ⟹ wp(e₂, Q))
```

### 12.2 SMT Integration

Verification conditions are discharged using SMT solvers:

```ocaml
let verify (spec : spec) (e : expr) : bool =
  let vc = wp e spec.post in
  let query = implies spec.pre vc in
  Smt.check_valid query
```

### 12.3 Symbolic Execution

For path-sensitive reasoning:
```
symbolic_exec : expr → path_condition → (path_condition × symbolic_state) list
```

## 13. Related Work

1. **Hoare Logic**: Hoare (1969)
2. **Separation Logic**: Reynolds (2002), O'Hearn (2019)
3. **Iris**: Jung et al. (2015) - Higher-order concurrent separation logic
4. **RustBelt**: Jung et al. (2017) - Semantic foundations for Rust
5. **Viper**: Müller et al. (2016) - Verification infrastructure
6. **Dafny**: Leino (2010) - Verification-aware programming

## 14. References

1. Hoare, C. A. R. (1969). An Axiomatic Basis for Computer Programming. *CACM*.
2. Reynolds, J. C. (2002). Separation Logic: A Logic for Shared Mutable Data Structures. *LICS*.
3. O'Hearn, P. W. (2019). Separation Logic. *CACM*.
4. Jung, R., et al. (2017). RustBelt: Securing the Foundations of the Rust Programming Language. *POPL*.
5. Leino, K. R. M. (2010). Dafny: An Automatic Program Verifier. *LPAR*.

---

**Document Metadata**:
- Depends on: Type system, operational semantics, SMT integration
- Implementation verification: Pending `[IMPL-DEP: verifier]`
- Mechanized proof: See `mechanized/coq/Hoare.v` (stub)
