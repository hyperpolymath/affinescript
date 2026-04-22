# Proof Status — {{PROJECT}}
<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Template: rsr-template-repo/PROOF-STATUS.md -->
<!-- Tracks proof completion. Requirements defined in PROOF-NEEDS.md -->

## Summary

| Category | Total | Done | In Progress | Blocked | Remaining |
|----------|-------|------|-------------|---------|-----------|
| ABI/FFI (ABI) | 5 | 0 | 0 | 0 | 5 |
| Typing (TP) | 2 | 0 | 0 | 0 | 2 |
| Invariant (INV) | 0 | 0 | 0 | 0 | 0 |
| Security (SEC) | 0 | 0 | 0 | 0 | 0 |
| Concurrency (CONC) | 0 | 0 | 0 | 0 | 0 |
| Algorithm (ALG) | 0 | 0 | 0 | 0 | 0 |
| Domain (DOM) | 0 | 0 | 0 | 0 | 0 |
| **Total** | **7** | **0** | **0** | **0** | **7** |

**Overall**: 0% proven

## Proofs Done

<!-- Format: -->
<!-- | ID | Proof | Prover | File | Date | Verified By | -->
<!-- | ABI-1 | Non-null pointer proofs | Idris2 | verification/proofs/idris2/ABI/Pointers.idr | 2026-XX-XX | idris2 --check | -->

| ID | Proof | Prover | File | Date | Verified By |
|----|-------|--------|------|------|-------------|
| — | No proofs completed yet | — | — | — | — |

## Proofs In Progress

| ID | Proof | Prover | Assignee | Started | Blocker |
|----|-------|--------|----------|---------|---------|
| — | — | — | — | — | — |

## Proofs Blocked

| ID | Proof | Blocked By | Notes |
|----|-------|------------|-------|
| — | — | — | — |

## Proofs Remaining

| ID | Proof | Category | Prover | Priority | Est. Effort |
|----|-------|----------|--------|----------|-------------|
| ABI-1 | Non-null pointer proofs | ABI | Idris2 | P1 | 2h |
| ABI-2 | Memory layout correctness | ABI | Idris2 | P1 | 4h |
| ABI-3 | Platform type size proofs | ABI | Idris2 | P1 | 2h |
| ABI-4 | FFI function return type proofs | ABI | Idris2 | P1 | 2h |
| ABI-5 | C ABI compliance | ABI | Idris2 | P1 | 4h |
| TP-1 | Core data type well-formedness | TP | Idris2 | P1 | 4h |
| TP-2 | Public API type safety | TP | Lean4 | P2 | 4h |

## Verification Commands

```bash
# Check all Idris2 proofs
just proof-check-idris2

# Check all Lean4 proofs
just proof-check-lean4

# Check all Agda proofs
just proof-check-agda

# Check all Coq proofs
just proof-check-coq

# Run all proof checks
just proof-check-all

# Scan for dangerous patterns
panic-attack assail --proofs-only
```

## Changelog

| Date | Change | By |
|------|--------|-----|
| 2026-04-04 | Initial proof status tracking | Template |
