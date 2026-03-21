# AffineScript Academic Documentation

This directory contains formal academic documentation for the AffineScript programming language, including proofs, specifications, white papers, and mechanized verification.

## Document Index

### Proofs and Metatheory

| Document | Description | Status |
|----------|-------------|--------|
| [Type System Soundness](proofs/type-soundness.md) | Progress and preservation theorems | Complete |
| [Quantitative Type Theory](proofs/quantitative-types.md) | Linearity and quantity proofs | Complete |
| [Effect Soundness](proofs/effect-soundness.md) | Algebraic effects metatheory | Complete |
| [Ownership Soundness](proofs/ownership-soundness.md) | Affine/linear type safety | Complete |
| [Row Polymorphism](proofs/row-polymorphism.md) | Extensible records metatheory | Complete |
| [Dependent Types](proofs/dependent-types.md) | Indexed types and refinements | Complete |

### White Papers

| Document | Description |
|----------|-------------|
| [Language Design](white-papers/language-design.md) | Design rationale and related work |
| [Type System Design](white-papers/type-system.md) | Bidirectional typing with quantities |
| [Effect System Design](white-papers/effect-system.md) | Algebraic effects and handlers |

### Formal Verification

| Document | Description | Status |
|----------|-------------|--------|
| [Operational Semantics](formal-verification/operational-semantics.md) | Small-step semantics | Complete |
| [Denotational Semantics](formal-verification/denotational-semantics.md) | Domain-theoretic model | Complete |
| [Axiomatic Semantics](formal-verification/axiomatic-semantics.md) | Hoare logic for AffineScript | Complete |

### Mathematical Foundations

| Document | Description |
|----------|-------------|
| [Categorical Semantics](mathematical-foundations/categorical-semantics.md) | Category theory models |
| [Logic Foundations](mathematical-foundations/logic-foundations.md) | Curry-Howard and proof theory |
| [Complexity Analysis](mathematical-foundations/complexity-analysis.md) | Decidability and complexity bounds |

### Mechanized Proofs

| Document | Description | Status |
|----------|-------------|--------|
| [Coq Formalization](mechanized/coq/README.md) | Coq proof development | Stub |
| [Lean Formalization](mechanized/lean/README.md) | Lean 4 proof development | Stub |
| [Agda Formalization](mechanized/agda/README.md) | Agda proof development | Stub |

## Citation

```bibtex
@misc{affinescript2024,
  title = {AffineScript: A Quantitative Dependently-Typed Language with Algebraic Effects},
  author = {AffineScript Contributors},
  year = {2024},
  howpublished = {\url{https://github.com/hyperpolymath/affinescript}}
}
```

## Status Legend

- **Complete**: Theoretical content complete, pending implementation verification
- **Stub**: Placeholder awaiting implementation of corresponding compiler component
- **TODO**: Section identified but not yet written

## Dependencies on Implementation

Many proofs in this documentation depend on compiler components that are not yet implemented. These are marked with `[IMPL-DEP]` tags throughout the documents, indicating which compiler phase must be completed before the proof can be fully verified against the implementation.

| Proof Area | Required Implementation |
|------------|------------------------|
| Type soundness | Type checker |
| Effect soundness | Effect inference |
| Ownership soundness | Borrow checker |
| Termination | Termination checker |
| Refinement verification | SMT integration |
