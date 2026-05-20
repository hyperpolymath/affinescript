<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2026 hyperpolymath -->

# Stdlib AOT triage — #135 punch list

> **CLOSED 2026-05-18.** This punch list is complete. All 19 `stdlib/*.affine`
> files now compile through `resolve → typecheck → codegen`; epic #128 and
> sub-issues **#131–#138 are all merged and closed**. The "Current sweep" and
> "Remaining slices" sections below are preserved **as historical record** of
> the in-flight state on 2026-05-17 r2 — they no longer describe the tree.
> Live status is now machine-enforced by the CI gate `test/test_stdlib_aot.ml`
> (STAGE-A AOT smoke, 19/19 + the #137 multi-module integration test), so the
> AOT path cannot silently rot again. See the **Closure** section for the
> final state. Verified locally 2026-05-18: `dune runtest` = 253 tests green.

**Refreshed 2026-05-17 (r2)** after merging #135 slices 1,2,3,5,6,6b,7,8.

## Done (merged to main)

| Slice | What | PR |
|---|---|---|
| #131/#134/#132/#133 | front (keystone/soundness/ADR-011/dedup) | #149/#150/#151/#152 |
| sl.1 | `fn(x)=>e` anon-function expressions | #153 |
| sl.2 | slice/range index `e[a:b]` via `slice` builtin | #154 |
| sl.3 | bare `effect E;` + ADR-008 `-> T / E` row | #155 |
| sl.6 | `try`→`attempt`, `ref`→`make_ref` | #156 |
| sl.6b | `as`→`xs` (collections) | #158 |
| sl.5 | trait default bodies (left-factored; conflicts ↓) | #159 |
| **sl.7** | **let-polymorphism**: generic fn `<T>` instantiation + prelude `mut` | #163 |
| sl.8 | module visibility/imports per ADR-011 | #164 |

## Current sweep

| File | Stage | Slice / root cause |
|---|---|---|
| `prelude` | ✅ OK | compiles end-to-end (sl.7 + sl.8) |
| `string` `Core` + 7 module files | ✅ OK | — |
| `result` | TYPECHECK | post-sl.8 deeper typecheck (imports now resolve) — **slice 12** |
| `collections` | RESOLVE UndefinedVariable | `binary_search`→`binary_search_helper` — **slice 11** (resolver forward-ref) |
| `io` | RESOLVE UndefinedVariable | `split` lives in `string.affine`; needs cross-module import — **slice 8-tail** |
| `option` | PARSE 320 | `&mut Option<T>` ref param (`take`/`get_or_insert`) — **slice 9** |
| `testing` | PARSE 302 | record-literal `{f:v}` as final block expr after stmts — **slice 4** |
| `math` | PARSE 354 | `if`(no else) as a statement between stmts — **slice 4** |
| `traits` | PARSE 124 | `while let` / `Vec::new()` / `let mut` mid-block — **slice 4** |
| `effects` | TYPECHECK | "Too many arguments for kind" — generic-extern kind-check — **slice 10** |

~10/19 compile end-to-end (was 9 at session start; the *highest-leverage*
compiler fix — let-polymorphism, sl.7 — is the session's key win: it
unblocked the entire typecheck wall).

## Remaining slices — all correctness-/grammar-critical (rigorous, not auto)

- **slice 4** — block/statement LR ambiguity (testing/math/traits). The
  grammar's pre-existing `list(stmt)` vs `expr_record_body` r/r:
  `if`/`while`/`for` as statements vs trailing-expr; record-literal vs
  block `{`; `while let`. High regression risk; careful checkpointed
  block-grammar restructure + full conflict re-verify.
- **slice 9** — `&mut T` reference parameters with reassignment
  (`option` take/get_or_insert). Affine/borrow lowering;
  ownership-soundness-critical.
- **slice 10** — generic-extern kind-checking ("Too many arguments for
  kind"); `effects` `extern fn make_ref<T>`.
- **slice 11** (NEW, discovered in sl.8) — the resolver is single-pass
  with no top-level pre-registration, so **forward references between
  top-level functions fail** (`fn a(){ b() } fn b(){}` errors even
  *without* `module`). Pre-existing, affects collections and likely
  many files; resolver two-pass fix — resolver-critical.
- **slice 8-tail** — `io` needs `split` from `string.affine`
  cross-module; requires `module string; pub fn split` + `use
  string::{split}`, then re-verify every string.affine consumer
  (string currently compiles — must not regress).
- **slice 12** — `result` post-sl.8 deeper typecheck (separate from
  sl.7; surfaced once imports resolved).

## Closure

**Reached 2026-05-18 — epic #128 fully delivered.**

The criterion above was met: all 19 `stdlib/*.affine` files compile
`resolve → typecheck → codegen`. Final disposition of the slices:

- **slice 4** (block/statement LR ambiguity — testing/math/traits),
  **slice 9** (`&mut T` reference params), **slice 10** (generic-extern
  kind-check), **slice 11** (resolver two-pass / forward-ref), **slice
  8-tail** (`io` ← `string` cross-module), **slice 12** (`result`
  deeper typecheck) — all resolved under #135 (#172–#192).
- **#135** — every `stdlib/*.affine` compiles end-to-end. Closed.
- **#138** — `b895374` Some/None/Ok/Err seed band-aid removed;
  resolution now goes through `use prelude::{…}` (PR #193). Closed.
- **#136** — stdlib-wide AOT compile-smoke gate added as
  `test/test_stdlib_aot.ml`, run under `dune runtest` / CI (PR #194).
  Closed.
- **#137** — multi-module integration test (`prelude` + `string` +
  `option` + `collections` together) in the same file (PR #194).
  Closed.

Sub-issues **#131–#138 are all closed**; epic **#128**'s only remaining
scope is the consumer-side echidna trackers (#61–64), which close with
that migration, not this epic. The rigorous-over-partial-hack discipline
held throughout — no unattended changes to the block grammar, borrow
checker, kind checker, or resolver two-pass. This document is now a
historical record; the live invariant is the CI gate.
