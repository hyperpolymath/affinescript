<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- SPDX-FileCopyrightText: 2026 hyperpolymath -->

# Stdlib AOT triage — #135 punch list

**Refreshed 2026-05-17** after merging slices 1, 2, 3, 6. Live per-file
state of driving every `stdlib/*.affine` through
`resolve → typecheck → codegen` (Deno-ESM).

## Done (merged to main)

| Slice | What | PR |
|---|---|---|
| #131 | `>>` nested-generic close (keystone) | #149 |
| #134 | prelude unwrap/unwrap_result soundness | #150 |
| #132 | namespace ADR-011 | #151 |
| #133 | single-ownership dedup | #152 |
| #135 sl.1 | `fn(x) => e` anon-function expressions | #153 |
| #135 sl.2 | slice/range index `e[a:b]` via `slice` builtin | #154 |
| #135 sl.3 | bare `effect E;` + ADR-008 `-> T / E` row | #155 |
| #135 sl.6 | `try`→`attempt`, `ref`→`make_ref` (keyword-as-ident) | #156 |

## Current sweep

| File | Stage | Site | Slice / root cause |
|---|---|---|---|
| `string` | ✅ OK | — | full pipeline |
| `Core` | ✅ OK | — | full pipeline |
| `Ajv`/`Crypto`/`Grammy`/`Network`/`Sqlite`/`Vscode`/`VscodeLanguageClient` | ✅ OK | — | full pipeline |
| `traits` | PARSE | `12:43` | **Slice 5** — trait method *default body* + `ref self` (`pub fn ne(ref self, ...) { ... }`) |
| `collections` | PARSE | `40:13` | **Slice 6b** — `as` used as a parameter name (`fn zip<A,B>(as: [A], ...)`); `as` is the AS keyword. Same class as slice 6 → rename (`elems`) |
| `math` | PARSE | `354:3` | **Slice 4** — `if cond { ... }` (no else) used as a statement between other statements; the parser commits to if-as-trailing-expr |
| `testing` | PARSE | `302:3` | **Slice 4** — record literal `{ f: v }` as the block's final expression after statements (the pre-existing `list(stmt)` vs `expr_record_body` r/r) |
| `option` | PARSE | `320:15` | **Slice 9** — `&mut Option<T>` reference parameter type (`fn take<T>(opt: &mut Option<T>)`); flagged in #128 itself (take/get_or_insert + affine/borrow lowering) |
| `result` | RESOLVE | — | **Slice 8** — module/`use prelude::{...}` resolution (post-#133 model) |
| `io` | RESOLVE | — | **Slice 8** — builtin/extern + namespace resolution; model-coupled |
| `prelude` | TYPECHECK | `Unify (T, Int)` | **Slice 7 (root cause refined)** — *not* a simple empty-array literal (that pattern compiles in isolation). Deeper type-instantiation: `fold`'s `(U,T)->U` HOF instantiated monomorphically by `sum`/`product` (`fold(arr, 0, …)` forces `0:Int`). Needs careful typecheck investigation |
| `effects` | TYPECHECK | "Not implemented: Too many arguments for kind" | **Slice 10** — kind-checking of generic externs (`extern fn make_ref<T>(x: T) -> Ref<T> / state`); distinct codegen/kind defect |

9/19 compile end-to-end. Parse walls + deeper defects regroup into the
slices below.

## Remaining slices — difficulty & autonomy

**Safe to do autonomously (bounded, low risk):**
- **Slice 6b** — `as` param-name in `collections.affine` → rename
  (`elems`/`xs`). Pure stdlib edit, zero grammar risk. ~15 min.
- **Slice 5** — trait default bodies + `ref self`. `trait_item`
  already has `TraitFnDefault f` (line ~537); the gap is the `ref self`
  receiver in trait-method position. Grammar-bounded, medium; verify
  conflict counts.

**Needs care — rigorous, not auto-rushed:**
- **Slice 4** (testing, math) — block/statement ambiguity. The hardest:
  `if`/`while`/`for` as statements vs trailing-expr, and record-literal
  vs block `{`. This is the grammar's pre-existing `list(stmt)` /
  `expr_record_body` reduce/reduce. High regression risk; needs a
  careful block-grammar restructure with full conflict + suite
  re-verification. Multi-step.
- **Slice 9** (option) — `&mut T` reference parameters with
  reassignment. Touches affine/borrow lowering (the #128-noted hard
  case). Correctness-critical (ownership) — must be rigorous.
- **Slice 7** (prelude) — type-checker instantiation of HOF `fold`
  under monomorphic `sum`/`product`. Correctness-critical typecheck
  code; investigate, don't guess.
- **Slice 8** (result, io) — module/extern resolution against the
  post-#133 model (ADR-011). Model-coupled.
- **Slice 10** (effects) — generic-extern kind-checking
  ("Too many arguments for kind"). Distinct kind/codegen defect.

## Closure

#135 closes when all files compile resolve→typecheck→codegen; then
#138 (remove b895374 band-aid), #136 (CI AOT smoke gate), #137
(multi-module integration test). The safe slices (6b, 5) are ~1 short
session; slices 4/7/8/9/10 are correctness-/grammar-critical and are
each their own focused, rigorous unit (the "rigorous over partial-hack"
discipline applies — no guessed changes to typecheck/borrow/block
grammar).
