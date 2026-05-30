<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell -->

# `res-to-affine` — ReScript-to-AffineScript migration assistant

A small OCaml CLI that reads a `.res` file and emits a `.affine` skeleton
with **migration markers** — comments that name each anti-pattern the
scanner found, point at the source line, and propose the AffineScript
answer the human migrator should consider before porting.

Tracks: [`affinescript#57`](https://github.com/hyperpolymath/affinescript/issues/57)
(parser + metaparser).
Consumed by: [`hyperpolymath/gitbot-fleet#148`](https://github.com/hyperpolymath/gitbot-fleet/issues/148)
and the broader `idaptik` migration.

## Usage

```sh
# print skeleton to stdout (default: tree-sitter AST walker, Phase 2c)
dune exec tools/res-to-affine/main.exe -- path/to/Foo.res

# or write to a file
dune exec tools/res-to-affine/main.exe -- path/to/Foo.res -o Foo.affine

# Phase 3 (slice 1): also translate fully-structural type declarations
# (primitive aliases + simple sum types) into compilable AffineScript
dune exec tools/res-to-affine/main.exe -- --translate path/to/Foo.res

# opt back into the Phase-1 line-regex scanner (no grammar required)
dune exec tools/res-to-affine/main.exe -- --engine=scanner path/to/Foo.res
```

The output is **not compilable**. It is a starting point for the human:
a quoted copy of the original sits at the bottom; the top carries a
migration-considerations block; the middle is a `module` stub with
`TODO`s. The human picks the decomposition; the tool surfaces what
needs re-decomposing.

### Detection engines

| `--engine` | Implementation | When to use |
|---|---|---|
| `walker` (default) | Shells out to the vendored `tree-sitter` CLI, walks the AST (`walker.ml`). | Default since Phase 2c — covers all six anti-patterns including the two that the scanner cannot see (inline callback records, oversized functions) and eliminates the `let _ = chained.call()` / line-anchored false-positive classes. |
| `scanner` | Line-anchored regex over the raw source (`scanner.ml`). | Fallback when the vendored grammar is unavailable (no `tree-sitter` CLI, missing `tools/vendor/tree-sitter-rescript/`). Detects four of the six anti-patterns only. |

The walker requires the vendored `tree-sitter-rescript` grammar to be
built first:

```sh
just install-grammar
# or: ./editors/tree-sitter-rescript/scripts/install.sh
```

If the grammar isn't built or the `tree-sitter` CLI isn't on PATH, the
walker auto-falls-back to the scanner and prints the reason to stderr.

## What gets flagged

The six anti-patterns surfaced in the
[idaptik Wave 3 pilot](https://github.com/hyperpolymath/idaptik/blob/main/migration/main/LESSONS.md):

| Tag | Detection (walker, default) | AffineScript answer |
|---|---|---|
| `side-effect-import` | `let _ = Mod.foo` at module top level (structural — not nested inside a function body) | Explicit registration call |
| `raw-js` | `extension_expression` node — any `%name(...)` or `[%bs.name ...]` | Typed extern (`ABI-FFI-README.md`) |
| `untyped-exception` | `try_expression`, `raise(...)` call, `Js.Exn.*` reference, `Promise.catch` member access | `Result[E, A]` / `Validation[E, A]` |
| `mutable-global` | Top-level `let x = ref(...)` (call-of-`ref` body) OR top-level `mutation_expression` (`x := y`) | Affine record threaded through |
| `inline-callback-record` | ≥ 3 inline `function` values in one `record` literal OR one call's `arguments` list (via `labeled_argument` or direct) | Row-polymorphic handler record (LESSONS.md §callback-record) |
| `oversized-function` | `function` node whose row span exceeds 50 source lines | Re-decompose before porting; do not transliterate |

### Walker vs scanner coverage

| Anti-pattern | Scanner (regex) | Walker (AST) |
|---|---|---|
| `side-effect-import`  | ✓ | ✓ (since Phase 2b, #322) |
| `raw-js`              | ✓ | ✓ (since Phase 2c) |
| `untyped-exception`   | ✓ | ✓ (since Phase 2c) |
| `mutable-global`      | ✓ | ✓ (since Phase 2c) |
| `inline-callback-record` | — | ✓ (since Phase 2c, walker-only by construction) |
| `oversized-function`  | — | ✓ (since Phase 2c, walker-only by construction) |

The walker improves on the regex by being structural: it reports
`side-effect-import` only when `let _ = Mod.value` sits at module top
level, distinguishes a `try { ... }` expression from the identifier
`try`, only flags `Mutable_global` for module-scoped state (not local
refs inside a function body), and dedupes structurally-overlapping
findings on the same line.

## Why a skeleton and not a transliteration

The Frontier Programming Guides' standing rule is **re-decompose, not
transliterate**. A line-for-line port preserves the source's anti-patterns
into the target language and produces `.affine` files that are technically
parseable but architecturally still ReScript. The migration assistant's
job is to *make the re-decomposition tractable*, not to skip it. So:

- The skeleton is **honest about being incomplete** — it does not
  compile, on purpose.
- The original source is **quoted at the bottom** so the migrator
  doesn't tab between files while writing the port.
- Each marker links a source line to the AffineScript pattern that
  replaces it, so the migrator's next action is clear.

## Phase plan

### Phase 1 — text-scan emitter (this PR)

- OCaml binary builds with the repo's existing `dune` toolchain.
- `Scanner` walks lines with `str` regexes; cheap and dependency-free.
- `Emitter` writes the migration-considerations block, a `module` stub,
  and the quoted source.
- Snapshot tests under `test/` ensure stable output.

This phase is **deliberately small**. It is useful immediately — runs
against any `.res` file, surfaces 4 of 6 anti-patterns, gives the
migrator a starting document — and it gates the architectural commitment
to tree-sitter in Phase 2 behind something that already pays its way.

### Phase 2 — tree-sitter AST walker

Vendoring of the pinned grammar
(`rescript-lang/tree-sitter-rescript@990214a`) lives in
`editors/tree-sitter-rescript/`; `install.sh` materialises the
parser into `tools/vendor/tree-sitter-rescript/`.

- **Phase 2a (#321)** — `just install-grammar`, the
  `migration-assistant` CI job that runs it, dual install path
  (`cargo install tree-sitter-cli` or `npm install -g
  tree-sitter-cli`).
- **Phase 2b (#322)** — the walker itself: subprocess to the
  `tree-sitter` CLI, hand-rolled s-expression parser over the
  default `[row, col]`-annotated output, AST-based detection of
  `side-effect-import` only.
- **Phase 2c (this revision)** — walker covers all six anti-patterns
  including the two that the scanner cannot see; `--engine=walker`
  becomes the CLI default. The `Emitter` interface does not change;
  the marker schema is the same.

Walker output is deduplicated by `(kind, line)` so structurally-
overlapping AST matches don't inflate the bullet count above what
the line-based scanner would produce on the same file.

### Phase 3 — partial translation

Once the AST walker exists, the emitter can do more than mark — it can
**translate** the pure-structural parts (type aliases, sum decls,
simple `let` bindings, switch-to-match) and leave only effect-laden,
exception-bearing, or globally-mutating regions as TODO. The skeleton
becomes a working port of ~60–80% of the input, with TODO islands
where re-decomposition is genuinely required.

Phase 3 is when the tool earns its keep on idaptik's 542 files.

**Phase 3 (`--translate`, landed).** The translation path renders the
fully-structural type declarations into compilable AffineScript. Every
generated form below is verified by the compiler itself (`main.exe check`
→ *Type checking passed*).

| ReScript | AffineScript | Slice |
|---|---|---|
| `type userId = int` | `type UserId = Int` | 1 |
| `type color = Red \| Green \| Blue` | `type Color =`<br>`  \| Red`<br>`  \| Green`<br>`  \| Blue` | 1 |
| `type shape = Circle(float) \| Rect(int, int)` | `type Shape =`<br>`  \| Circle(Float)`<br>`  \| Rect(Int, Int)` | 1 |
| `type point = {x: int, y: int}` | `struct Point {`<br>`  x: Int,`<br>`  y: Int`<br>`}` | 2 |
| `type box<'a> = {value: 'a}` | `struct Box[A] {`<br>`  value: A`<br>`}` | 2 |
| `type option<'a> = None \| Some('a)` | `type Option[A] =`<br>`  \| None`<br>`  \| Some(A)` | 2 |
| `type id<'a> = 'a` | `type Id[A] = A` | 2 |

It is **conservative by construction**: a declaration is translated only
when every part is representable — a qualified-path reference
(`Belt.Map.t`), a non-primitive/opaque reference, a nested generic
(`array<int>`), a GADT return, a variant spread, an object type, or a
record with a `mutable` or optional-`?` field causes the whole decl to be
*skipped* (it stays in the marker block + quoted original, never
mis-translated). Two normalisations make the output referenceable:
lower-case ReScript type names are capitalised (`color` → `Color`) and
type variables are mapped (`'a` → `A`), because `lib/parser.mly` reads a
lower-case name in type position as a type *variable*, not a constructor.
Translation is walker-only (it needs the AST); with `--engine=scanner`
the flag is a no-op.

Deliberately **deferred to later Phase-3 slices**: `let`-to-`const` for
literal bindings, the `switch`→`match` expression rewrite (needs body
translation), and **module-qualified references** — these now *parse*
(the [#228](https://github.com/hyperpolymath/affinescript/issues/228)
grammar gap closed), but a faithful `Belt.Map.t` → `Belt::Map::T` would
not *resolve* against a target module that doesn't exist yet, so emitting
it would break the "every translated form type-checks" guarantee. It
waits for a module-mapping story.

## Corpus run

[`CORPUS-RUN.md`](CORPUS-RUN.md) records the first end-to-end run
against the estate's 491 deduplicated `.res` files. It documents the
false-positive sources that the corpus surfaced (and the regex fixes
that landed alongside it) plus the Phase-2 follow-ups it identified.
A machine-readable sidecar lives at [`CORPUS-RUN.json`](CORPUS-RUN.json).

## Testing

```sh
dune test tools/res-to-affine/
```

To regenerate snapshots after an intentional emitter change:

```sh
cd tools/res-to-affine/test
../../../_build/default/tools/res-to-affine/main.exe \
    fixtures/sample.res > expected/sample.affine
```

The fixture under `test/fixtures/sample.res` is synthetic and exercises
every Phase-1 anti-pattern; `test/fixtures/phase2c.res` exercises the
two anti-patterns that are walker-only by construction
(`inline-callback-record`, `oversized-function`); `test/fixtures/phase3.res`
and `test/fixtures/phase3b.res` exercise the Phase-3 `--translate` path
(aliases / sums / generics / records → compilable AffineScript, plus the
qualified / mutable / optional / non-type forms it must skip).
Real `.res` files
from the estate (e.g. `gitbot-fleet/bots/sustainabot/bot-integration/
src/*.res`) can be run ad hoc through the CLI without changes to the
test suite.

## Non-goals

- **Not a ReScript compiler.** The scanner does not parse ReScript;
  even Phase 2 only walks the tree-sitter CST, not the ReScript
  type-checker's AST. If a `.res` file is syntactically invalid the
  tool may still emit a (less useful) skeleton.
- **Not a build-time dependency on ReScript.** The pinned grammar is a
  parser, not the ReScript compiler. The estate's language policy
  (CLAUDE.md) bans new ReScript code; this tool exists to **help retire
  the existing ReScript surface**, not to bring more in.
- **Not for editor integration.** Editor tree-sitter bindings for
  AffineScript live at `editors/tree-sitter-affinescript/`; this tool's
  vendored grammar is for the migration pipeline only.
