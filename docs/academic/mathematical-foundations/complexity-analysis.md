# Complexity and Decidability Analysis

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete theoretical analysis

## Abstract

This document analyzes the computational complexity and decidability properties of AffineScript's type system and related decision procedures. We establish complexity bounds for type checking, type inference, effect inference, and SMT-based refinement checking.

## 1. Introduction

Understanding complexity is crucial for:
- Practical compiler implementation
- Theoretical foundations
- Identifying expensive features
- Guiding language design decisions

## 2. Type Checking Complexity

### 2.1 Simple Type Checking

**Theorem 2.1**: Type checking for the simply-typed fragment of AffineScript is decidable in O(n) time, where n is the size of the program.

**Proof**: Each expression is visited once, with constant-time type operations. ∎

### 2.2 Polymorphic Type Checking

**Theorem 2.2**: Type checking for the ML-style polymorphic fragment is decidable in O(n) time (given type annotations).

With full type inference, the complexity increases.

### 2.3 Type Inference Complexity

**Theorem 2.3**: Hindley-Milner type inference is decidable in:
- O(n) time for programs without let-polymorphism
- O(n × α(n)) time in practice (quasi-linear, using union-find)
- DEXPTIME-complete in the worst case

The exponential worst case occurs with nested let-expressions creating exponentially large types.

**Example of exponential blowup**:
```
let x₁ = λx. (x, x) in
let x₂ = λx. x₁(x₁(x)) in
let x₃ = λx. x₂(x₂(x)) in
...
```

### 2.4 Row Polymorphism

**Theorem 2.4**: Row unification is decidable in O(n²) time in the worst case, O(n) in typical cases.

**Proof**: Row rewriting may require examining all labels, but the number of labels is bounded by program size. ∎

## 3. Dependent Type Checking

### 3.1 Type-Level Computation

**Theorem 3.1**: For the restricted type-level language (natural arithmetic, no general recursion), normalization is decidable.

**Proof**: The type-level language is strongly normalizing (no fix at the type level). ∎

### 3.2 Definitional Equality

**Theorem 3.2**: Checking definitional equality τ₁ ≡ τ₂ is decidable when:
1. Both types normalize
2. No undecidable type-level operations

**Complexity**: O(n) for normalized types, where n is the size of the normal form.

### 3.3 Undecidable Extensions

**Theorem 3.3**: With unrestricted type-level recursion, type checking becomes undecidable.

Adding `fix` at the type level allows encoding the halting problem.

## 4. Refinement Type Checking

### 4.1 SMT Fragment Decidability

**Theorem 4.1**: For the quantifier-free fragment of linear integer arithmetic (QF_LIA), SMT checking is decidable.

**Complexity**: NP-complete for satisfiability.

### 4.2 Presburger Arithmetic

**Theorem 4.2**: Presburger arithmetic (linear arithmetic with quantifiers) is decidable.

**Complexity**: At least doubly exponential in the worst case.

### 4.3 Nonlinear Arithmetic

**Theorem 4.3**: Nonlinear integer arithmetic is undecidable.

**Corollary**: Refinements involving multiplication of variables require approximations.

### 4.4 Practical Refinement Checking

In practice, refinement checking is:
- Fast for common patterns (linear constraints)
- Expensive for quantified formulas
- Undecidable for nonlinear integer arithmetic
- Approximated using timeouts

**Implementation**: Use SMT solver with timeout; report "unknown" on timeout.

## 5. Effect System Complexity

### 5.1 Effect Checking

**Theorem 5.1**: Effect checking (given effect annotations) is decidable in O(n) time.

**Proof**: Effect operations are checked locally; effect rows are compared by set equality. ∎

### 5.2 Effect Inference

**Theorem 5.2**: Effect inference with row polymorphism is decidable in O(n²) time.

Similar to row polymorphism for records, but with simpler constraints (no lack constraints typically needed).

### 5.3 Handler Coverage

**Theorem 5.3**: Checking handler completeness (all operations handled) is decidable in O(|ops|) time.

## 6. Ownership Checking

### 6.1 Borrow Checking

**Theorem 6.1**: Borrow checking for AffineScript is decidable in O(n²) time.

**Proof Sketch**:
1. Build dataflow graph: O(n)
2. Compute lifetimes: O(n)
3. Check conflicts: O(n²) pairwise

In practice, linear with good data structures.

### 6.2 Lifetime Inference

**Theorem 6.2**: Lifetime inference is decidable and produces principal lifetimes.

**Complexity**: O(n) for building constraints, O(n × α(n)) for solving (union-find).

### 6.3 Non-Lexical Lifetimes

**Theorem 6.3**: NLL-style lifetime inference is decidable via dataflow analysis.

**Complexity**: O(n × k) where k is the iteration count (bounded by program size).

## 7. Subtyping

### 7.1 Structural Subtyping

**Theorem 7.1**: Structural subtyping for records and variants is decidable in O(n) time.

**Proof**: Width subtyping requires checking field presence; depth subtyping is recursive but bounded by type depth. ∎

### 7.2 Subtyping with Refinements

**Theorem 7.2**: Subtyping with refinements reduces to SMT validity checking.

```
{x: τ | φ} <: {x: τ | ψ}  iff  ∀x. φ ⟹ ψ
```

Complexity determined by SMT fragment.

### 7.3 Higher-Rank Polymorphism

**Theorem 7.3**: Type checking with predicative higher-rank polymorphism is decidable.

**Theorem 7.4**: Type checking with impredicative higher-rank polymorphism is undecidable.

AffineScript uses predicative polymorphism.

## 8. Decidability Summary

| Feature | Decidable | Complexity |
|---------|-----------|------------|
| Simple types | ✓ | O(n) |
| HM inference | ✓ | O(n) typical, DEXPTIME worst |
| Row polymorphism | ✓ | O(n²) |
| Dependent types (restricted) | ✓ | O(n) after normalization |
| Refinements (QF_LIA) | ✓ | NP-complete |
| Refinements (nonlinear) | ✗ | Undecidable |
| Effect checking | ✓ | O(n) |
| Borrow checking | ✓ | O(n²) |
| Subtyping (structural) | ✓ | O(n) |
| Higher-rank (predicative) | ✓ | O(n) |
| Higher-rank (impredicative) | ✗ | Undecidable |

## 9. Termination Analysis

### 9.1 Total Functions

**Theorem 9.1**: Verifying totality (termination for all inputs) is undecidable in general.

**Corollary**: The `total` annotation cannot be automatically verified for all functions.

### 9.2 Structural Recursion

**Theorem 9.2**: Structural recursion on well-founded data types terminates.

AffineScript can verify totality for:
- Primitive recursion on naturals
- Structural recursion on algebraic data types
- Recursion with explicit well-founded measures

### 9.3 Totality Checker

`[IMPL-DEP: termination-checker]`

Approach:
1. Check for structural recursion
2. Verify decreasing arguments
3. For complex recursion, require explicit measure

## 10. Space Complexity

### 10.1 Type Representation

**Theorem 10.1**: Types may grow exponentially with let-polymorphism.

**Mitigation**: Use sharing (DAG representation) for O(n) space.

### 10.2 Context Representation

**Theorem 10.2**: Contexts require O(n) space for n bindings.

### 10.3 Proof Terms

For dependent types with proof terms, space is proportional to proof size.

**Mitigation**: Erase proofs at runtime (zero-quantity).

## 11. Parallelization

### 11.1 Type Checking Parallelization

**Theorem 11.1**: Type checking is parallelizable at module boundaries.

**Speedup**: O(n/p) with p processors for n modules.

### 11.2 Effect Inference Parallelization

Effect constraints can be gathered in parallel, solved centrally.

### 11.3 SMT Query Parallelization

Independent refinement checks can run in parallel.

## 12. Algorithmic Optimizations

### 12.1 Incremental Type Checking

On program modification:
- Re-check only affected parts
- Cache type inference results
- Complexity: O(Δ) where Δ is change size

### 12.2 Lazy Normalization

Normalize type-level terms on demand:
- Cache normal forms
- Share subterms

### 12.3 Constraint Solving Strategies

For type inference:
- Use union-find for equality constraints
- Solve in dependency order
- Fail fast on conflicts

## 13. Worst-Case Examples

### 13.1 Type Inference Blowup

```affinescript
let f1 = λx. (x, x)
let f2 = λx. f1(f1(x))
let f3 = λx. f2(f2(x))
// Type of f3 has size 2^8 = 256
```

### 13.2 Row Unification Explosion

```affinescript
fn f[α, β, γ, δ, ε](
    r: {a: α, b: β, c: γ, d: δ, e: ε, ..ρ}
) -> ...
```

Many constraints from one signature.

### 13.3 Refinement Timeout

```affinescript
fn complex(
    x: {v: Int | is_prime(v) ∧ v > 10^100}
) -> ...
```

SMT may timeout on complex predicates.

## 14. Implementation Recommendations

1. **Use sharing** for type representation
2. **Implement incremental checking** for IDE support
3. **Set SMT timeouts** for refinements
4. **Cache results** aggressively
5. **Parallelize** across modules
6. **Stratify** type-level computation
7. **Provide escape hatches** (unsafe blocks, assertions)

## 15. Open Problems

1. **Optimal type inference** with row polymorphism
2. **Practical nonlinear arithmetic** for refinements
3. **Automatic termination analysis** for complex recursion
4. **Principal types** for dependent types
5. **Optimal borrow checking** algorithm

## 16. References

1. Henglein, F. (1993). Type Inference with Polymorphic Recursion. *TOPLAS*.
2. Kfoury, A. J., Tiuryn, J., & Urzyczyn, P. (1993). The Undecidability of the Semi-unification Problem. *I&C*.
3. Pottier, F. (2014). Hindley-Milner Elaboration in Applicative Style. *ICFP*.
4. Rondon, P., Kawaguchi, M., & Jhala, R. (2008). Liquid Types. *PLDI*.
5. De Moura, L., & Bjørner, N. (2008). Z3: An Efficient SMT Solver. *TACAS*.
6. Weiss, A., et al. (2019). Oxide: The Essence of Rust. *arXiv*.

---

**Document Metadata**:
- This document is theoretical analysis
- Implementation guidance: See `[IMPL-DEP]` markers
