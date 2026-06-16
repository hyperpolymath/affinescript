<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

# Compile-time scaling is super-linear (≈O(n²)) — quadratic blow-up past ~1000 functions

**Surfaced by:** the scaling bench (`bench/bench_scaling.ml`, `just bench`), 2026-06-16.
**Severity:** Performance. Invisible on the in-repo corpus (max ~114 lines); real for any large program.

## Measurement (parse + resolve + wasm-codegen, generated `fn fI(x:Int)->Int { x+I }` × N)

| N functions | total | µs/func |
|---|---|---|
| 10 | 0.06 ms | 5.6 |
| 100 | 0.44 ms | 4.4 |
| 1000 | 12.5 ms | 12.5 |
| **5000** | **401 ms** | **80.3** |

µs/function should be ~flat for a linear pipeline. Instead it climbs ~18× between
n=100 and n=5000, and the 1000→5000 step (5× input) costs ~32× the time —
empirically ≈ O(n²) (log₅32 ≈ 2.15).

## Where to look

The per-function work that grows with total program size is almost certainly a
**full-program scan repeated per item**. Prime suspects, in order:

1. **Name resolution** (`lib/resolve.ml`) — if the symbol/module table is an
   assoc-list scanned per reference, or each function re-walks the whole
   top-level, that is the classic O(n²). (Resolve is the most likely culprit.)
2. **Codegen** (`lib/codegen.ml`) — a per-function pass that walks all
   declarations (e.g. building a function-index table by `List.assoc`/`List.nth`
   rather than a `Hashtbl`).
3. Parser/AST construction is usually linear; rule it out by phase-splitting the
   bench timing (parse vs resolve vs codegen separately) to localise the curve.

## Next step

Phase-split `bench_scaling` (time parse / resolve / codegen independently at each
N) to pin the quadratic phase, then replace the offending `List.assoc`/`List.nth`
/ per-item full scan with a `Hashtbl`. Target: flat µs/func to n≥50 000. Promote
the bench to a baselined Six-Sigma gate once linear (`docs/TESTING-AND-BENCH-MATRIX.adoc`).

This is the first defect found by closing the "large-input scaling unmeasured"
gap — the bench paid for itself on its first run.

## Update (2026-06-16) — phase-split localisation + partial fix (6.5×)

Phase-split timing (`bench_scaling.ml` now times parse/resolve/codegen
separately) shows **parse and resolve are flat (linear)** — the quadratic is
entirely in **`lib/codegen.ml`** (not resolve, as first guessed). Two confirmed
O(n²) sources found and fixed:

1. **`@`-append accumulation per function** — `gen_decl` appended to `funcs`,
   `func_indices`, `ownership_annots` with `xs @ [x]` (O(len) each) → O(n²). Fixed:
   cons (O(1)) + `List.rev` once at emission (`all_funcs`, `build_ownership_section`).
   Indices preserved (they come from `List.length`, order-independent).
2. **`List.length ctx.funcs` per function** (index assignment, codegen.ml:3204) →
   O(n²). Fixed: an O(1) `num_funcs` counter field on the context.

Result (codegen, n=5000): **453 ms → 70 ms (~6.5×)**; 477 tests + `wasm-validate`
green (byte-identical indices). **Residual:** codegen is still mildly
super-linear (~1→14 µs/func, 100→5000) — a third, smaller source remains. Ruled
out by inspection: `intern_func_type` (dedups identical signatures), `exports`
(empty for non-pub), `Effect_sites` (Hashtbl, O(n)). Localising the residual
needs a profiler (`ocaml-landmarks` / `perf`), not static reading — deferred.
The ADR-0026 F1 (Isabelle "resolve/codegen is O(n)") proof is the durable guard.
