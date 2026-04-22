# Proof Requirements — {{PROJECT}}
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Template: rsr-template-repo/PROOF-NEEDS.md -->
<!-- Authoritative master list: ~/Desktop/PROOF-REQUIREMENTS-MASTER.md -->

## Proof Tier

<!-- Assign one: T1 (Critical), T2 (High), T3 (Standard), T4 (Light), T5 (Exempt) -->
**Tier**: T3 — Standard

## Proof Categories

| Code | Meaning | Applies? |
|------|---------|----------|
| **TP** | Typing Proofs (type soundness, type safety) | Yes |
| **INV** | Invariant Proofs (state machines, monotonicity, bounds) | |
| **SEC** | Security Proofs (crypto, injection freedom, access control) | |
| **CONC** | Concurrency Proofs (linearizability, deadlock freedom) | |
| **ALG** | Algorithm Proofs (termination, correctness, bounds) | |
| **ABI** | ABI/FFI Proofs (memory layout, pointer safety, platform compat) | Yes |
| **DOM** | Domain-Specific Proofs (bespoke to this project) | |

## Mandatory Proofs (All RSR Repos)

These proofs come from the rsr-template-repo and MUST be present in every repo:

### ABI/FFI Boundary Proofs (Idris2)

| # | Proof | Status | File |
|---|-------|--------|------|
| ABI-1 | Non-null pointer proofs (`So (ptr /= 0)`) | Needed | `verification/proofs/idris2/ABI/Pointers.idr` |
| ABI-2 | Memory layout correctness (`HasSize`, `HasAlignment`) | Needed | `verification/proofs/idris2/ABI/Layout.idr` |
| ABI-3 | Platform type size proofs (per platform) | Needed | `verification/proofs/idris2/ABI/Platform.idr` |
| ABI-4 | FFI function return type proofs | Needed | `verification/proofs/idris2/ABI/Foreign.idr` |
| ABI-5 | C ABI compliance (`CABICompliant`, `FieldsAligned`) | Needed | `verification/proofs/idris2/ABI/Compliance.idr` |

### Typing Proofs (Prover Varies)

| # | Proof | Status | File |
|---|-------|--------|------|
| TP-1 | Core data type well-formedness | Needed | `verification/proofs/idris2/Types.idr` |
| TP-2 | Public API type safety (exported functions) | Needed | `verification/proofs/lean4/ApiTypes.lean` |

## Project-Specific Proofs

<!-- Fill in proofs specific to this project. Copy from PROOF-REQUIREMENTS-MASTER.md -->
<!-- Delete this section for T4/T5 repos -->

| # | Proof Needed | Category | Prover | Priority | File(s) |
|---|-------------|----------|--------|----------|---------|
| | | | | | |

## Dangerous Patterns (BANNED)

The following MUST NOT appear anywhere in proof files:

| Pattern | Language | Meaning |
|---------|----------|---------|
| `believe_me` | Idris2 | Unsafe cast / trust-me |
| `assert_total` | Idris2 | Skip totality check |
| `postulate` | Idris2/Agda | Unproven axiom |
| `sorry` | Lean4 | Incomplete proof |
| `Admitted` | Coq | Incomplete proof |
| `unsafeCoerce` | Haskell | Unsafe type cast |
| `Obj.magic` | OCaml/ReScript | Unsafe type cast |
| `unsafe` (unaudited) | Rust | Unsafe block without safety comment |

CI will reject any PR introducing these patterns (enforced by `panic-attack assail`).

## Prover Selection Guide

| Use Case | Recommended Prover | Why |
|----------|-------------------|-----|
| ABI/FFI boundaries | **Idris2** | Dependent types model layouts precisely |
| Type system proofs | **Coq** or **Lean4** | Mature proof assistants for metatheory |
| Algebraic properties | **Lean4** | Good mathlib support |
| Inductive/coinductive | **Agda** | Native support for (co)induction |
| Distributed systems | **TLA+** | Model checking for protocols |
| Numerical properties | **Isabelle** | Strong real analysis library |

## Proof File Locations

```
verification/proofs/
├── idris2/          # Idris2 proofs (ABI, dependent types)
│   ├── ABI/         # ABI-specific proofs
│   └── *.idr        # Project-specific Idris2 proofs
├── lean4/           # Lean4 proofs (algebra, lattices)
│   └── *.lean
├── agda/            # Agda proofs (induction, metatheory)
│   └── *.agda
├── coq/             # Coq proofs (type systems, compilation)
│   └── *.v
└── tlaplus/         # TLA+ specs (distributed protocols)
    └── *.tla
```

## References

- Master list: `~/Desktop/PROOF-REQUIREMENTS-MASTER.md`
- Proof status tracking: `PROOF-STATUS.md` (this repo)
- Proven library: `proven` repo (Idris2 verified foundations)
- Template: `rsr-template-repo/PROOF-NEEDS.md`
