# AffineScript: A Quantitative Dependently-Typed Language with Algebraic Effects

**Technical Report / White Paper**
**Version**: 1.0
**Date**: 2024

## Abstract

We present AffineScript, a new programming language that unifies several advanced type system features into a coherent design: quantitative type theory for linearity, dependent types with refinements, algebraic effects with handlers, and an ownership system for memory safety. This paper describes the language design, its theoretical foundations, key design decisions, and comparison with related work. We demonstrate how these features interact harmoniously to enable both high-level reasoning and low-level control.

**Keywords**: type theory, linear types, dependent types, algebraic effects, ownership, refinement types, quantitative type theory

## 1. Introduction

Modern programming demands languages that are simultaneously:
- **Safe**: Preventing common errors at compile time
- **Expressive**: Supporting precise specifications
- **Efficient**: Enabling low-level control without runtime overhead
- **Composable**: Allowing modular reasoning

Existing languages make various trade-offs. Rust provides memory safety through ownership but lacks dependent types. Idris offers dependent types and effects but without resource tracking. Haskell has algebraic effects but not native linear types.

AffineScript synthesizes these features into a unified design where:
- **Quantities** track resource usage (linear, affine, unrestricted)
- **Dependent types** enable precise specifications
- **Refinement types** allow SMT-checked invariants
- **Algebraic effects** structure side effects compositionally
- **Ownership** ensures memory safety without garbage collection

### 1.1 Contributions

This paper makes the following contributions:

1. **Design of AffineScript**: A novel combination of QTT, effects, and ownership
2. **Integration of quantities and ownership**: Unified treatment of linearity
3. **Row-polymorphic effect system**: First-class effect handlers
4. **Refinement types with dependent indexing**: Practical dependent programming
5. **Formal metatheory**: Soundness proofs for all features

### 1.2 Paper Outline

- Section 2: Language overview and examples
- Section 3: Type system design
- Section 4: Effect system
- Section 5: Ownership model
- Section 6: Implementation strategy
- Section 7: Related work
- Section 8: Conclusion

## 2. Language Overview

### 2.1 Basic Syntax

AffineScript uses a clean, familiar syntax:

```affinescript
// Function definition
fn greet(name: String) -> String {
    "Hello, " ++ name ++ "!"
}

// Generic function
fn identity[T](x: T) -> T { x }

// Pattern matching
fn length[T](xs: List[T]) -> Nat {
    case xs {
        Nil → 0,
        Cons(_, tail) → 1 + length(tail)
    }
}
```

### 2.2 Quantities and Linearity

Variables can be annotated with quantities:

```affinescript
// Linear function: file must be used exactly once
fn read_and_close(1 file: File) -> String / IO {
    let contents = file.read()
    file.close()  // file consumed
    contents
}

// Erased parameter: only used in types
fn replicate[T](0 n: Nat, x: T) -> Vec[n, T] {
    // n is not available at runtime, only for type-level computation
    ...
}

// Unrestricted (default)
fn use_freely(ω x: Int) -> (Int, Int) {
    (x, x)  // x can be used multiple times
}
```

### 2.3 Dependent Types

Types can depend on values:

```affinescript
// Length-indexed vectors
type Vec[n: Nat, T: Type] =
    | Nil : Vec[0, T]
    | Cons : (T, Vec[m, T]) → Vec[m + 1, T]

// Safe head: only accepts non-empty vectors
fn head[n: Nat, T](v: Vec[n + 1, T]) -> T {
    case v { Cons(x, _) → x }
}

// Append with precise length
fn append[n: Nat, m: Nat, T](
    xs: Vec[n, T],
    ys: Vec[m, T]
) -> Vec[n + m, T]
```

### 2.4 Refinement Types

Types can be refined with predicates:

```affinescript
type Positive = {x: Int | x > 0}
type NonEmpty[T] = {xs: List[T] | length(xs) > 0}

fn divide(x: Int, y: Positive) -> Int {
    x / y  // Safe: y > 0 guaranteed
}

fn safe_head[T](xs: NonEmpty[T]) -> T {
    xs[0]  // Safe: xs not empty
}
```

### 2.5 Algebraic Effects

Effects are first-class:

```affinescript
effect State[S] {
    get : () → S
    put : S → ()
}

fn increment() -> () / State[Int] {
    let x = perform get()
    perform put(x + 1)
}

// Handle the effect
fn run_state[S, A](init: S, comp: () →{State[S]} A) -> (A, S) {
    let mut state = init
    handle comp() with {
        return x → (x, state),
        get(_, k) → resume(k, state),
        put(s, k) → { state = s; resume(k, ()) }
    }
}
```

### 2.6 Ownership

Memory safety through ownership:

```affinescript
fn process(file: own File) -> String / IO {
    let contents = file.read()  // file moved
    // file.close()  // Error: file was moved
    contents
}

fn borrow_example(data: ref [Int]) -> Int {
    data[0]  // data is borrowed, not consumed
}
```

## 3. Type System Design

### 3.1 Design Principles

The type system follows these principles:

1. **Stratification**: Clear separation of universes, types, and terms
2. **Bidirectional**: Efficient type checking with minimal annotations
3. **Principal types**: Type inference computes most general types
4. **Soundness**: Well-typed programs don't get stuck

### 3.2 Judgment Forms

The core judgments are:

```
Γ ⊢ e ⇒ τ    (synthesis)
Γ ⊢ e ⇐ τ    (checking)
Γ ⊢ τ : κ    (kinding)
Γ ⊢ τ <: σ   (subtyping)
```

### 3.3 Quantitative Type Theory

We integrate Atkey's QTT framework:

- Contexts track quantities: `Γ = x₁:^{π₁}τ₁, ..., xₙ:^{πₙ}τₙ`
- Context operations: scaling (`πΓ`) and addition (`Γ + Δ`)
- The semiring `{0, 1, ω}` with standard operations

Key typing rule for application:
```
    Γ ⊢ f : (π x : τ) → σ    Δ ⊢ a : τ
    ─────────────────────────────────────
    Γ + πΔ ⊢ f a : σ[a/x]
```

### 3.4 Dependent Types

We support:
- Π-types: `(x: A) → B(x)`
- Σ-types: `(x: A, B(x))`
- Indexed families: `Vec[n, T]`
- Propositional equality: `a == b`

Type-level computation is restricted to ensure decidability.

### 3.5 Refinement Types

Integration with SMT:
- Refinements: `{x: τ | φ}` where φ is decidable
- Subtyping: checked via SMT validity
- Automatic strengthening from control flow

### 3.6 Row Polymorphism

Records and effects use row polymorphism:

```
{l₁: τ₁, ..., lₙ: τₙ | ρ}    -- extensible record
⟨l₁: τ₁ | ... | lₙ: τₙ | ρ⟩  -- extensible variant
ε₁ | ε₂ | ... | ρ            -- effect row
```

## 4. Effect System

### 4.1 Effect Design

Effects in AffineScript are:
- **Declared**: User-defined effect signatures
- **Polymorphic**: Row-polymorphic effect types
- **Handled**: First-class handlers with typed continuations
- **Inferred**: Effect types inferred where possible

### 4.2 Effect Signatures

```affinescript
effect E {
    op₁ : τ₁ → σ₁
    op₂ : τ₂ → σ₂
    ...
}
```

### 4.3 Handler Typing

Handlers transform computations:
```
handle e with {
    return x → e_ret,
    op(x, k) → e_op,
    ...
}
```

The continuation `k` can be:
- Linear (one-shot): `1 k : σ → A`
- Unrestricted (multi-shot): `ω k : σ → A`

### 4.4 Effect Polymorphism

Functions are polymorphic over effects:
```affinescript
fn map[A, B, ε](f: A →{ε} B, xs: List[A]) -> List[B] / ε
```

## 5. Ownership Model

### 5.1 Ownership Principles

1. **Unique ownership**: Each value has one owner
2. **Move semantics**: Assignment transfers ownership
3. **Borrowing**: Temporary access without transfer
4. **Lifetimes**: Scoped validity of references

### 5.2 Integration with Quantities

Ownership and quantities interact:
- `1 (own τ)`: Linear owned value
- `ω (ref τ)`: Multiple immutable borrows
- `1 (mut τ)`: Exclusive mutable borrow

### 5.3 Borrow Checking

The borrow checker ensures:
- No use after move
- No conflicting borrows
- Borrows don't outlive owners

### 5.4 Non-Lexical Lifetimes

Lifetimes end at last use, not scope end:
```affinescript
fn example() {
    let x = alloc(5)
    let y = &x          // borrow starts
    use(y)              // last use of y
    mutate(x)           // OK: borrow ended
}
```

## 6. Implementation

### 6.1 Compiler Architecture

```
Source → Lexer → Parser → Type Checker → Borrow Checker → Codegen → WASM
```

### 6.2 Type Checking Algorithm

1. Parse to untyped AST
2. Elaborate to typed AST with bidirectional inference
3. Solve unification constraints
4. Check quantities
5. Infer effects
6. Verify refinements via SMT
7. Check borrows

### 6.3 Code Generation

Target: WebAssembly
- Types erased (except for dynamic dispatch)
- Quantities erased
- Ownership erased (safety guaranteed statically)
- Proofs erased (zero-cost abstraction)

### 6.4 Performance Considerations

- No garbage collection: ownership-based memory management
- Zero-cost abstractions: types and proofs erased
- Efficient effects: CPS or evidence-passing compilation
- SMT caching: memoize refinement checks

## 7. Related Work

### 7.1 Quantitative Type Theory

**Atkey (2018)**: Syntax and Semantics of QTT
**McBride (2016)**: Quantitative type theory origins

AffineScript adopts the {0, 1, ω} semiring for practical programming.

### 7.2 Dependent Types

**Idris (Brady)**: Practical dependent types
**Agda (Norell)**: Pure dependent types
**F* (Swamy et al.)**: Effects and refinements

AffineScript combines dependent types with ownership, distinguishing it from these systems.

### 7.3 Algebraic Effects

**Eff (Bauer & Pretnar)**: Original effect handlers
**Koka (Leijen)**: Row-polymorphic effects
**Frank (Lindley et al.)**: Direct-style effects

AffineScript integrates effects with quantities, allowing linear continuations.

### 7.4 Ownership and Borrowing

**Rust**: Practical ownership system
**Cyclone (Jim et al.)**: Region-based memory
**Mezzo (Pottier & Protzenko)**: Permissions

AffineScript formalizes ownership via QTT rather than ad-hoc rules.

### 7.5 Refinement Types

**Liquid Types (Rondon et al.)**: SMT-based refinements
**Dependent ML (Xi)**: Practical dependent types
**F* (Swamy et al.)**: Refinements with effects

AffineScript combines refinements with dependent indexing.

## 8. Discussion

### 8.1 Design Trade-offs

| Feature | Benefit | Cost |
|---------|---------|------|
| Dependent types | Precise specifications | Learning curve |
| Effects | Modular side effects | Additional annotations |
| Ownership | Memory safety | Borrow checker complexity |
| Quantities | Resource control | Quantity annotations |

### 8.2 Usability Considerations

- Extensive type inference reduces annotation burden
- Error messages guide users to fixes
- Gradual adoption: start with simple types, add precision
- IDE integration provides immediate feedback

### 8.3 Future Work

1. **Proof assistant mode**: Interactive theorem proving
2. **Totality checking**: Verify termination
3. **Parallelism**: Safe concurrent effects
4. **Compilation optimizations**: Exploit type information
5. **Mechanized metatheory**: Coq/Lean formalization

## 9. Conclusion

AffineScript demonstrates that advanced type system features—quantitative types, dependent types, algebraic effects, and ownership—can be integrated into a coherent, practical language. The key insight is that quantities provide a unifying framework for both linearity and ownership, while dependent types and refinements enable precise specifications verified at compile time.

The result is a language where:
- Programs are correct by construction
- Memory is safe without garbage collection
- Effects are tracked and handled modularly
- Resources are managed precisely

We believe this combination points toward a future of programming where powerful type systems make strong guarantees practical and accessible.

## Acknowledgments

We thank the academic community for foundational work on type theory, linear logic, and effect systems that made this design possible.

## References

1. Atkey, R. (2018). Syntax and Semantics of Quantitative Type Theory. *LICS*.
2. McBride, C. (2016). I Got Plenty o' Nuttin'. *Curry Festschrift*.
3. Brady, E. (2013). Idris, a General-Purpose Dependently Typed Programming Language. *JFP*.
4. Plotkin, G., & Pretnar, M. (2013). Handling Algebraic Effects. *LMCS*.
5. Leijen, D. (2017). Type Directed Compilation of Row-Typed Algebraic Effects. *POPL*.
6. Jung, R., et al. (2017). RustBelt: Securing the Foundations of the Rust Programming Language. *POPL*.
7. Rondon, P., Kawaguchi, M., & Jhala, R. (2008). Liquid Types. *PLDI*.
8. Swamy, N., et al. (2016). Dependent Types and Multi-Monadic Effects in F*. *POPL*.
9. Rémy, D. (1989). Type Checking Records and Variants in a Natural Extension of ML. *POPL*.
10. Wadler, P. (1990). Linear Types Can Change the World! *IFIP TC*.

---

**Appendix A**: Full syntax grammar (see SPEC.md)

**Appendix B**: Typing rules (see proofs/type-soundness.md)

**Appendix C**: Effect semantics (see proofs/effect-soundness.md)
