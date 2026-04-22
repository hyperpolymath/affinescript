# Ownership System: Formal Verification

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete; implementation verification pending `[IMPL-DEP: borrow-checker]`

## Abstract

This document presents the formal semantics and soundness proofs for AffineScript's ownership system. We prove that well-typed programs are memory-safe: no use-after-free, no double-free, no data races, and no dangling references. The system combines affine types with a borrow-checking discipline inspired by Rust but adapted for AffineScript's dependent and effect-typed setting.

## 1. Introduction

AffineScript's ownership system provides compile-time memory safety guarantees through:

1. **Ownership**: Each value has exactly one owner
2. **Move semantics**: Ownership is transferred on assignment
3. **Borrowing**: Temporary access without ownership transfer
4. **Lifetimes**: Scoped validity of references
5. **Affine types**: Values must be used at most once (or explicitly dropped)

These features integrate with AffineScript's quantity annotations, providing a unified treatment of linearity and ownership.

## 2. Syntax

### 2.1 Ownership Modifiers

```
own τ     -- Owned value of type τ
ref τ     -- Immutable borrow of τ
mut τ     -- Mutable borrow of τ
```

### 2.2 Expressions with Ownership

```
e ::=
    | ...
    | move e                      -- Explicit move
    | &e                          -- Immutable borrow
    | &mut e                      -- Mutable borrow
    | *e                          -- Dereference
    | drop e                      -- Explicit drop
```

### 2.3 Lifetimes

```
'a, 'b, 'c, ...                   -- Lifetime variables
'static                           -- Static lifetime (lives forever)

ref['a] τ                         -- Reference with explicit lifetime
mut['a] τ                         -- Mutable reference with lifetime
```

## 3. Ownership Model

### 3.1 Ownership Tree

At any point in execution, values form an ownership tree:

```
root
├── x: own String
│   └── (owns heap allocation)
├── y: own Vec[Int]
│   ├── (owns buffer)
│   └── (owns elements)
└── z: ref String
    └── (borrows from x)
```

### 3.2 Ownership Invariants

**Invariant 1 (Unique Ownership)**: Each owned value has exactly one owning binding.

**Invariant 2 (Borrow Validity)**: All borrows are valid (not dangling).

**Invariant 3 (Borrow Exclusivity)**: At any point:
- Multiple immutable borrows (`ref τ`) may coexist, OR
- Exactly one mutable borrow (`mut τ`) exists
- But not both simultaneously

**Invariant 4 (Lifetime Containment)**: A borrow's lifetime is contained within the owner's lifetime.

## 4. Static Semantics

### 4.1 Contexts with Ownership

```
Γ ::= · | Γ, x: own τ | Γ, x: ref['a] τ | Γ, x: mut['a] τ
```

Additionally, we track:
- **Live set** L: Variables currently in scope and valid
- **Borrow set** B: Active borrows and their origins
- **Move set** M: Variables that have been moved

### 4.2 Well-Formedness

**Γ ⊢ wf** (context well-formed)

```
    ──────
    · ⊢ wf

    Γ ⊢ wf    x ∉ dom(Γ)    Γ ⊢ τ : Type
    ────────────────────────────────────
    Γ, x: own τ ⊢ wf

    Γ ⊢ wf    'a ∈ Γ    x ∉ dom(Γ)    Γ ⊢ τ : Type
    ────────────────────────────────────────────────
    Γ, x: ref['a] τ ⊢ wf
```

### 4.3 Typing Rules

**Own-Intro**
```
    Γ ⊢ e : τ
    ──────────────────
    Γ ⊢ e : own τ
```

**Own-Elim (Move)**
```
    Γ, x: own τ ⊢ x : own τ    x ∉ M
    ─────────────────────────────────
    Γ ⊢ move x : own τ
    (adds x to M)
```

**Borrow-Imm**
```
    Γ ⊢ e : own τ    e is a place
    'a = lifetime(e)    no mut borrows of e active
    ──────────────────────────────────────────────
    Γ ⊢ &e : ref['a] τ
    (adds borrow to B)
```

**Borrow-Mut**
```
    Γ ⊢ e : own τ    e is a place
    'a = lifetime(e)    no borrows of e active
    ─────────────────────────────────────────
    Γ ⊢ &mut e : mut['a] τ
    (adds exclusive borrow to B)
```

**Deref-Imm**
```
    Γ ⊢ e : ref['a] τ
    ──────────────────
    Γ ⊢ *e : τ
```

**Deref-Mut**
```
    Γ ⊢ e : mut['a] τ
    ──────────────────
    Γ ⊢ *e : τ          -- for reading
    Γ ⊢ *e := v : ()    -- for writing
```

**Drop**
```
    Γ ⊢ x : own τ    x ∉ M    no borrows from x active
    ──────────────────────────────────────────────────
    Γ ⊢ drop x : ()
    (adds x to M, calls destructor)
```

### 4.4 Lifetime Rules

**Lifetime Inclusion**
```
    'a ⊆ 'b    (lifetime 'a outlives 'b)
```

Rules:
```
    ──────────────
    'static ⊆ 'a

    'a ⊆ 'a

    'a ⊆ 'b    'b ⊆ 'c
    ────────────────────
    'a ⊆ 'c
```

**Reference Covariance**
```
    'a ⊆ 'b    τ = σ
    ────────────────────────
    ref['a] τ <: ref['b] σ
```

**Reference Invariance (Mutable)**
```
    'a = 'b    τ = σ
    ────────────────────────
    mut['a] τ = mut['b] σ
```

(Mutable references are invariant in both lifetime and type)

### 4.5 Non-Lexical Lifetimes

Lifetimes are computed based on actual usage, not lexical scope:

```affinescript
fn example() {
    let mut x = 5
    let y = &x        -- borrow starts here
    println(y)        -- last use of y
    // borrow ends here (not at end of scope)
    x = 10            -- mutation OK, borrow ended
}
```

**NLL Judgment**: `Γ ⊢ e : τ @ ['a₁, 'a₂]`

Where `['a₁, 'a₂]` is the lifetime interval of the expression.

## 5. Borrow Checking Algorithm

### 5.1 Places

A **place** is an l-value that can be borrowed:

```
place ::=
    | x                           -- Variable
    | place.field                 -- Field access
    | place[i]                    -- Index
    | *place                      -- Deref
```

### 5.2 Borrow Tracking

```ocaml
type borrow = {
  place : place;
  kind : Shared | Exclusive;
  lifetime : lifetime;
  origin : location;  (* source code location *)
}

type borrow_state = {
  active : borrow list;
  moved : place set;
}
```

### 5.3 Conflict Detection

```ocaml
let conflicts (b1 : borrow) (b2 : borrow) : bool =
  overlaps b1.place b2.place &&
  (b1.kind = Exclusive || b2.kind = Exclusive)

let check_borrow (state : borrow_state) (new_borrow : borrow) : result =
  if is_moved state new_borrow.place then
    Error "use after move"
  else if List.exists (conflicts new_borrow) state.active then
    Error "conflicting borrow"
  else
    Ok { state with active = new_borrow :: state.active }
```

### 5.4 Lifetime Inference

```ocaml
(* Compute minimal lifetime for a borrow *)
let infer_lifetime (uses : location list) (scope : scope) : lifetime =
  let last_use = List.fold_left max (List.hd uses) uses in
  Lifetime.from_span (List.hd uses) last_use scope
```

## 6. Soundness Theorems

### 6.1 Memory Safety

**Theorem 6.1 (No Use After Free)**: If `Γ ⊢ e : τ` and e reduces without error, then e never accesses freed memory.

**Proof Sketch**:
1. Owned values are freed when dropped or when owner goes out of scope
2. Borrows must have lifetimes contained in owner's lifetime
3. The borrow checker ensures no access after owner is freed

By induction on the reduction sequence, maintaining the invariant that all accessed memory is either owned or validly borrowed. ∎

### 6.2 No Double Free

**Theorem 6.2 (No Double Free)**: If `Γ ⊢ e : τ`, then no value is freed twice.

**Proof Sketch**:
1. Each value has exactly one owner (Invariant 1)
2. Move semantics transfers ownership, invalidating the source
3. The move set M tracks moved values, preventing re-drop

∎

### 6.3 No Data Races

**Theorem 6.3 (Data Race Freedom)**: If `Γ ⊢ e : τ` and e is executed concurrently, there are no data races.

**Definition (Data Race)**: Two accesses to the same memory location form a data race if:
1. At least one is a write
2. They are not synchronized
3. They happen concurrently

**Proof Sketch**:
1. By Invariant 3, mutable borrows are exclusive
2. Shared borrows are immutable (no writes)
3. Owned values cannot be accessed from other threads without transfer
4. Therefore, no unsynchronized concurrent write+access

∎

### 6.4 Borrow Validity

**Theorem 6.4 (No Dangling References)**: If `Γ ⊢ e : ref['a] τ`, then dereferencing e never accesses invalid memory.

**Proof**:
By Invariant 4, the borrow lifetime 'a is contained in the owner's lifetime.
The borrow checker ensures 'a does not exceed the owner's scope.
Therefore, when the borrow is used, the owner is still valid.
∎

## 7. Integration with QTT

### 7.1 Quantities and Ownership

The ownership modifiers interact with quantities:

| Quantity | Owned | Borrowed |
|----------|-------|----------|
| 0 | Type-level only | Type-level only |
| 1 | Must use once | Must use once |
| ω | Requires Copy | Multiple uses OK |

### 7.2 Copy Trait

```affinescript
trait Copy {
    fn copy(self: ref Self) -> Self
}
```

Only types implementing `Copy` can have unrestricted quantity with ownership:

**Copy-Omega**
```
    Γ ⊢ e : own τ    τ : Copy    π = ω
    ────────────────────────────────────
    Γ, πx:own τ ⊢ ...
```

### 7.3 Affine vs Linear

AffineScript is **affine** by default:
- Values with quantity 1 may be used *at most* once
- They may also be explicitly dropped
- This differs from true linear types where values *must* be used exactly once

**Affine-Drop**
```
    Γ, 1x:own τ ⊢ drop x : ()
    ─────────────────────────────
    (x is consumed, not used in computation)
```

For resources that must be explicitly handled (like file handles), use:

```affinescript
-- MustUse marker prevents implicit drop
type MustUse[T] = own T where must_use

fn use_file(1 f: MustUse[File]) -> () / IO {
    // Cannot drop f implicitly; must close or return
    close(f)
}
```

## 8. Ownership and Effects

### 8.1 Effect Operations with Ownership

```affinescript
effect Resource[R] {
    acquire : () → own R
    release : own R → ()
}
```

### 8.2 RAII via Handlers

```affinescript
fn with_resource[R, A](
    comp: (own R) →{ε} A
) -> A / Resource[R] | ε {
    let r = perform acquire()
    let result = comp(r)
    // r has been moved into comp
    result
}
```

### 8.3 Ownership Transfer in Continuations

When a handler captures a continuation, ownership must be preserved:

```affinescript
effect Transfer {
    give : own T → ()
    take : () → own T
}

fn transfer_handler[T, A](
    comp: () →{Transfer} A
) -> A {
    handle comp() with {
        return x → x,
        give(t, k) → {
            // t is owned here, must transfer to take
            handle resume(k, ()) with {
                take(_, k2) → resume(k2, t)
            }
        }
    }
}
```

## 9. Ownership and Dependent Types

### 9.1 Indexed Owned Types

```affinescript
type OwnedVec[n: Nat, T] = own { data: Ptr[T], len: n }
```

### 9.2 Refinements on Ownership

```affinescript
fn split[n: Nat, m: Nat, T](
    vec: own Vec[n + m, T]
) -> (own Vec[n, T], own Vec[m, T]) {
    // Ownership of vec is split into two parts
    ...
}
```

### 9.3 Proof-Carrying Ownership

```affinescript
fn take_ownership[T](
    x: ref T,
    0 proof: can_take_ownership(x)  -- proof is erased
) -> own T {
    unsafe_take_ownership(x)
}
```

## 10. Dynamic Semantics with Ownership

### 10.1 Runtime Representation

At runtime, ownership is erased but the invariants are guaranteed:

```
Heap H ::= {ℓ₁ ↦ v₁, ..., ℓₙ ↦ vₙ}
Stack S ::= · | S, x ↦ ℓ | S, x ↦ ref(ℓ)
```

### 10.2 Reduction with Heap

```
(e, H) ⟶ (e', H')
```

**Alloc**
```
    ℓ fresh
    ─────────────────────────────────
    (alloc(v), H) ⟶ (ℓ, H[ℓ ↦ v])
```

**Drop**
```
    x ↦ ℓ ∈ S
    ───────────────────────────────────────
    (drop x, S, H) ⟶ ((), S \ x, H \ ℓ)
```

**Move**
```
    x ↦ ℓ ∈ S
    ──────────────────────────────────────────────
    (let y = move x in e, S, H) ⟶ (e[y/x], (S \ x)[y ↦ ℓ], H)
```

### 10.3 Type Safety with Heap

**Theorem 10.1 (Heap Type Safety)**: If `Γ | Σ ⊢ e : τ` and `Σ ⊢ H` and `(e, H) ⟶* (e', H')`, then either:
1. e' is a value, or
2. (e', H') can step

And there exists Σ' ⊇ Σ such that `Γ | Σ' ⊢ e' : τ` and `Σ' ⊢ H'`.

## 11. Examples

### 11.1 File Handling

```affinescript
fn process_file(path: String) -> Result[String, IOError] / IO {
    let file = File::open(path)?          -- file: own File
    let contents = file.read_to_string()  -- moves file
    // file no longer accessible
    Ok(contents)
}
```

### 11.2 Container with Borrows

```affinescript
fn find_max['a, T: Ord](slice: ref['a] [T]) -> Option[ref['a] T] {
    if slice.is_empty() {
        None
    } else {
        let mut max = &slice[0]
        for i in 1..slice.len() {
            if slice[i] > *max {
                max = &slice[i]
            }
        }
        Some(max)
    }
}
```

### 11.3 Self-Referential Structures

```affinescript
-- Using explicit lifetime annotation
struct Parser['a] {
    input: ref['a] String,
    position: Nat,
}

fn parse['a](input: ref['a] String) -> Parser['a] {
    Parser { input: input, position: 0 }
}
```

## 12. Implementation

### 12.1 AST Representation

From `lib/ast.ml`:

```ocaml
type ownership =
  | Own      (* Owned value *)
  | Ref      (* Immutable borrow *)
  | Mut      (* Mutable borrow *)

type type_expr =
  | ...
  | TyOwn of type_expr
  | TyRef of type_expr   (* with implicit lifetime *)
  | TyMut of type_expr
```

### 12.2 Borrow Checker Module

`[IMPL-DEP: borrow-checker]`

```ocaml
module BorrowChecker : sig
  type place
  type borrow
  type state

  val check_expr : state -> expr -> (state * typ) result
  val check_borrow : state -> place -> borrow_kind -> lifetime result
  val check_move : state -> place -> state result
  val end_lifetime : state -> lifetime -> state
  val report_conflicts : state -> diagnostic list
end
```

### 12.3 Lifetime Inference Module

`[IMPL-DEP: lifetime-inference]`

```ocaml
module LifetimeInference : sig
  type constraint =
    | Outlives of lifetime * lifetime
    | Equals of lifetime * lifetime

  val gather_constraints : expr -> constraint list
  val solve : constraint list -> substitution result
  val infer_lifetimes : expr -> expr  (* annotated with lifetimes *)
end
```

## 13. Related Work

1. **Rust Ownership**: Matsakis & Klock (2014), RustBelt (Jung et al., 2017)
2. **Cyclone**: Jim et al. (2002) - Region-based memory management
3. **Linear Haskell**: Bernardy et al. (2018) - Linearity in Haskell
4. **Mezzo**: Pottier & Protzenko (2013) - Permissions and ownership
5. **ATS**: Xi (2004) - Linear types with dependent types
6. **Vault**: DeLine & Fähndrich (2001) - Adoption and focus

## 14. References

1. Jung, R., et al. (2017). RustBelt: Securing the Foundations of the Rust Programming Language. *POPL*.
2. Matsakis, N., & Klock, F. (2014). The Rust Language. *HILT*.
3. Weiss, A., et al. (2019). Oxide: The Essence of Rust. *arXiv*.
4. Jim, T., et al. (2002). Cyclone: A Safe Dialect of C. *USENIX*.
5. Pottier, F., & Protzenko, J. (2013). Programming with Permissions in Mezzo. *ICFP*.

---

**Document Metadata**:
- Depends on: `lib/ast.ml` (ownership type), borrow checker implementation
- Implementation verification: Pending
- Mechanized proof: See `mechanized/coq/Ownership.v` (stub)
