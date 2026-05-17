<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- SPDX-FileCopyrightText: 2026 hyperpolymath -->

# Stdlib AOT triage — #135 punch list

**Generated 2026-05-17**, against the compiler at `feat/135a-lambda-expr`
(origin/main + #131/#134 merged, #133/PR #152 auto-merging, + #135 slice 1
`fn(params) => expr`). This is the live per-file state of driving every
`stdlib/*.affine` through `resolve → typecheck → codegen` (Deno-ESM).

Methodology: `affinescript compile stdlib/<f>.affine -o /tmp/x.js
--deno-esm`, first error per file, classified by pipeline stage.

## Status snapshot

| File | Stage | Site | Root cause / slice |
|---|---|---|---|
| `string` | ✅ OK | — | full pipeline |
| `Core` | ✅ OK | — | full pipeline |
| `Ajv` `Crypto` `Grammy` `Network` `Sqlite` `Vscode` `VscodeLanguageClient` | ✅ OK | — | full pipeline (already `module`-declared) |
| `prelude` | TYPECHECK | `Unify.TypeMismatch (T, Int)` | **Slice 7** — empty-array literal `let result = []` then `result ++ [..]` not generalised |
| `option` | PARSE | `238:15` `Some(list[1:])` | **Slice 2** — slice/range index `list[1:]` |
| `collections` | PARSE | `24:35` `take(n-1, list[1:])` | **Slice 2** — same slice syntax |
| `result` | PARSE | `221:1` `fn try<T>(...)` | **Slice 6** — `try` is a reserved keyword (TRY); used as a fn name |
| `effects` | PARSE | `5:8` `effect io;` | **Slice 3** — bare `effect <name>;` declaration form |
| `testing` | PARSE | `302:3` `let total = ...;` then `{` | **Slice 4** — statement sequencing: `let;` followed by a block expr |
| `math` | PARSE | `354:3` `let total = 0.0;` then `for` | **Slice 4** — same: `let;` followed by `for` statement |
| `traits` | PARSE | `12:43` `pub fn ne(ref self, ...) -> Bool {` | **Slice 5** — trait method *default bodies* / `ref self` receiver |
| `io` | RESOLVE | `Resolve.UndefinedVariable` | **Slice 8** — builtin/extern + namespace wiring; interacts with #132/#133 module model |

9/19 already compile end-to-end. The remaining 10 reduce to **7 distinct
feature slices**, none of which is the #131 angle-bracket or #135-slice-1
lambda defect (both fixed).

## Slices (proposed order, each its own PR)

Ordering rationale: do the two-for-one parser slices first (max files
unblocked per change), keep the model-coupled one (Slice 8) until after
#133/PR #152 lands, and the typecheck/keyword ones are independent.

### Slice 2 — slice/range index `list[1:]`  *(unblocks `option`, `collections`)*
Postfix index currently accepts `e[i]` only. Add range-index
`e[a:b]` / `e[a:]` / `e[:b]` / `e[:]` to the postfix-expr rule, lowering
to the existing slice/`Array` op (check `lib/parser.mly` postfix rule +
how `typecheck.ml`/codegen model slices — `string` already slices, so a
lowering target likely exists). Two files, one feature. Independent.

### Slice 3 — `effect <name>;` declaration  *(unblocks `effects`)*
Grammar has `effect_decl` for `effect E { ops }`; `effects.affine` uses
the bare forward-declaration form `effect io;`. Add the bare form to
`effect_decl` (empty-op effect) or a dedicated production. Independent;
small.

### Slice 4 — statement sequencing: `let …;` then block/`for`  *(unblocks `testing`, `math`)*
Both fail where a `let …;` statement is immediately followed by another
statement that is a block expr (`{ … }`) or a `for`. Root cause is in
the block/statement-list rule (the `list(stmt)` vs `expr_record_body`
reduce/reduce noted in the grammar). Needs the statement-sequence rule
disambiguated so a trailing-semicolon `let` composes with a following
block/`for`. Independent; medium (touches the known r/r conflict — do
rigorously, re-check conflict counts before/after).

### Slice 5 — trait method default bodies + `ref self`  *(unblocks `traits`)*
`trait_decl` accepts method *signatures*; `traits.affine` provides
*default bodies* (`pub fn ne(ref self, ref other: Self) -> Bool { … }`).
Add optional `fn_body` to trait method items and accept the `ref self`
receiver form. Independent; medium.

### Slice 6 — `try` as a value identifier  *(unblocks `result`)*
`try` is the TRY keyword (try/catch). `result.affine` defines
`fn try<T>(…)`. Decide: (a) make `try` a contextual keyword (allow as
fn name / ident), or (b) rename the stdlib function (e.g. `attempt`).
(a) is the resolve-at-source fix but wider; (b) is local and safe.
Recommend (b) unless `try`-as-ident is wanted broadly. Independent; small.

### Slice 7 — empty-array literal generalisation  *(unblocks `prelude`)*
`map`/`filter` do `let result = []; for x in arr { result = result ++ [f(x)] }`.
`[]` is inferred at a concrete element type (`Int`) and then conflicts
with `[U]`. Needs the empty-array literal to take a fresh element tyvar
unified by later use (or an annotation path). Typecheck/inference, not
parsing. Independent; medium — touch `typecheck.ml` array-literal rule
with care + regression test.

### Slice 8 — `io` resolution / builtin + namespace  *(unblocks `io`)*  — **do after #133/PR #152**
`io.affine` references runtime builtins (`print`, `read_file`, …) the
static resolver doesn't know. This couples to the module/extern model
(ADR-011 / #132) and the #133 ownership changes. Sequence it after
PR #152 (#133) lands so it is solved against the real module model, not
the interpreter-era flat namespace. Medium; model-coupled.

## After all slices

Then #135 closes; unblocks #136 (CI AOT smoke gate), #137 (multi-module
integration test), #138 (remove b895374 band-aid). Estimated 3–5 further
focused sessions for slices 2–8 (each: feature + tests + conflict
re-verification), per the "rigorous over partial-hack" discipline.

## Done so far (this epic)

| Issue | PR | State |
|---|---|---|
| #131 `>>` nested-generic | #149 | merged |
| #134 prelude unwrap soundness | #150 | merged |
| #132 namespace ADR-011 | #151 | merged |
| #133 single-ownership dedup | #152 | auto-merging |
| #135 slice 1 (`fn(x)=>e`) | this PR | open |
