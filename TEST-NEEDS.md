<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

# TEST-NEEDS: affinescript

Per-repo instance of the estate CRG taxonomy
(`standards/testing-and-benchmarking/TESTING-TAXONOMY.adoc`). Categories +
aspects + the bench model are the canonical ones; the full mapping +
risk/interop ledgers live in `docs/TESTING-AND-BENCH-MATRIX.adoc`. This file is
the **blitz ledger**: measured status + numbers, honestly marked.

**Blitz date:** 2026-06-16. **Self-assessed CRG grade: D, approaching C** — the
C-tier E2E/REG/PRF/CTR(partial) are present; the gaps to C are REF + the B-tier
PBT/FUZ/MUT and baselined benches (below).

## Scale

| | |
|---|---|
| Compiler source | **36,178** LOC OCaml (`lib/`) |
| Compiler binary | 10.4 MB |
| Backends wired (suffix dispatch) | **48** |
| Alcotest gate | **477 tests, 0 fail** (`dune runtest`) |
| Conformance fixtures | 24 · e2e fixtures 102 |

## Test categories (16) — measured status

| Category | Status | Count / where |
|---|---|---|
| **UT** Unit | PASS | within the 477 alcotest (`test/test_<module>.ml`): lexer, effect_sites, qualified_paths, module_mut, **solo_cesk (19, VM M1)**, … |
| **P2P** Point-to-point | PASS (1 seam) | `typed-wasm-validate` — AffineScript producer ↔ Rust `tw-verify`, bit-exact (2/2). GAP: parser↔typecheck, typecheck↔codegen as named P2P |
| **E2E** End-to-end | PASS | `test_e2e` + `wasm-validate` (19+2), `native-run`, `riscv-run-validate` (4), `coprocessor-validate` (12) |
| **BLD** Build | PASS | `just build` / `dune build` (CI) |
| **EXE** Execution/runtime | PASS | interp + native exec + qemu-riscv64 + VM M1 CESK execution |
| **REF** Reflexive | **GAP** | no `just doctor`/self-check (candidate: a `selfcheck` chaining the gates) |
| **LCY** Lifecycle | partial | compile-time via borrow checker; runtime via VM M1 affine enforcement |
| **SMK** Smoke | PASS | `run_codegen_wasm/deno_tests.sh`, deno-test, vscode host |
| **PBT** Property-based | **GAP (priority)** | only a deterministic seed in test_solo_cesk; need qcheck 1000+ (semiring laws, lex→parse→pp round-trip, codegen determinism) |
| **MUT** Mutation | **GAP** | no `cargo-mutants` equivalent for OCaml |
| **FUZ** Fuzz | **GAP (priority)** | none — lexer/parser + codegen-emission are the boundaries to fuzz (crowbar/AFL). No placeholders. |
| **CTR** Contract/invariant | partial | `just guard` (doc-truthing), `proof-check-all` (the proofs are invariants), VM affine enforcement |
| **REG** Regression | PASS | `test/e2e/fixtures/` + the deferred-regression discipline (STATE.a2ml) |
| **CHS** Chaos | N-A | compiler, not a service (parser error-recovery is the nearest analog) |
| **CMP** Compatibility | partial | typed-wasm v1 carrier pinned. GAP: version-matrix |
| **PRF** Proof regression | PASS | `proof-check-all`: Idris2 Solo + Lean tropical + Agda echo, **green**; dangerous-primitive scan |

## Aspects (14) — covered: DEP, IOP, SAF, FUN, PRT, SEC(partial), PER(partial). GAP: ACC, MNT(partial), OBS(partial). N-A: PRI. (Detail in the matrix.)

## Performance (blitz, best-of-N, this host)

| Measure | Number |
|---|---|
| Compile hello → wasm / .ll / js / c / julia | **2–3 ms** each (312 / 1806 / 1272 / 1445 / 192 B) |
| Compile comprehensive_test (36 ln) → wasm | 3 ms (463 B) |
| Native exec (x86, hello) | **1 ms** |
| Native exec (riscv64 under qemu) | 11 ms (emulation) |
| Proof check — Idris2 Solo | 365 ms |
| Proof check — Lean tropical | 185 ms |
| Proof check — **Agda echo** | **47.8 s** (≈all of `proof-check-all`'s 47 s — cubical + 22 boundary certs) |
| Gates (each) | wasm 138 ms · coprocessor 47 ms · android 46 ms · typed-wasm 15 ms · riscv-run 102 ms |

**Benches: GAP.** `bench/bench_{lex,parse,typecheck,codegen}.ml` run (4 cases) but
are **visibility-only — emit no ns/op metrics**, are not baselined, and the
`just bench` recipe's second command is broken (`@bench` mismatch). No Six-Sigma
classification, no per-backend runtime bench, no VM step-rate bench (the VM has
deterministic cost-metering but no wall-clock harness). The test corpus maxes at
114 lines, so **large-input/scaling performance is unmeasured**.

## Remaining gaps (priority order)

1. **Benches → metric-emitting + baselined** (fix the recipe; emit ns/op; Six-Sigma baseline; per-backend runtime; VM step-rate; large-input fixtures).
2. **PBT** (qcheck): semiring laws, round-trip, codegen determinism — 1000+ cases.
3. **FUZ** (crowbar/AFL on lexer/parser + codegen boundary; cargo-fuzz on the runtime).
4. **Symbol-audit** per backend (`nm -D`/`wasm-tools`) — the proven interop guard.
5. **REF** (`just selfcheck`) + **ACC** (error-message clarity / CLI a11y).
6. Port proven's `tests/e2e.sh` 5-section proof-chain harness (folds the gates into one).
