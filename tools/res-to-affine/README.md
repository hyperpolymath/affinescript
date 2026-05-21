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
# print skeleton to stdout
dune exec tools/res-to-affine/main.exe -- path/to/Foo.res

# or write to a file
dune exec tools/res-to-affine/main.exe -- path/to/Foo.res -o Foo.affine
```

The output is **not compilable**. It is a starting point for the human:
a quoted copy of the original sits at the bottom; the top carries a
migration-considerations block; the middle is a `module` stub with
`TODO`s. The human picks the decomposition; the tool surfaces what
needs re-decomposing.

## What gets flagged (Phase 1)

The six anti-patterns surfaced in the
[idaptik Wave 3 pilot](https://github.com/hyperpolymath/idaptik/blob/main/migration/main/LESSONS.md),
of which the line-based scanner reliably detects four:

| Tag | Detection | AffineScript answer |
|---|---|---|
| `side-effect-import` | `let _ = Mod.foo` at top level | Explicit registration call |
| `raw-js` | `%raw(...)` or `[%bs.raw ...]` | Typed extern (`ABI-FFI-README.md`) |
| `untyped-exception` | `Promise.catch`, `Js.Exn`, `raise`, `try` | `Result[E, A]` / `Validation[E, A]` |
| `mutable-global` | `:=` operator | Affine record threaded through |

Deferred to Phase 2 (need real AST):

- **inline lambda callback record** — N ≥ 3 `~handler: (...) =>` lambdas
  inside one record literal (collapse to a row-polymorphic record).
- **oversized function** — function body > ~50 LOC (decompose).

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

- Install the pinned grammar from
  `editors/tree-sitter-rescript/` (manifest-only vendoring of
  `rescript-lang/tree-sitter-rescript@990214a`).
- Replace `Scanner` with a walker over the s-expression output of
  `tree-sitter parse --quiet`, parsed by the existing `sexplib0`
  dependency.
- Adds the two deferred patterns (callback records, oversized
  functions) and unlocks **structural** translation of trivial forms
  (e.g. `option<X>` → `Option[X]`, `result<X, Y>` → `Result[Y, X]`,
  `switch x { | A => ... }` → `match x { A => ... }`).
- The `Emitter` interface does not change: same skeleton shape, same
  marker schema, richer body.

### Phase 3 — partial translation

Once the AST walker exists, the emitter can do more than mark — it can
**translate** the pure-structural parts (type aliases, sum decls,
simple `let` bindings, switch-to-match) and leave only effect-laden,
exception-bearing, or globally-mutating regions as TODO. The skeleton
becomes a working port of ~60–80% of the input, with TODO islands
where re-decomposition is genuinely required.

Phase 3 is when the tool earns its keep on idaptik's 542 files.

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
every Phase-1 anti-pattern. Real `.res` files from the estate (e.g.
`gitbot-fleet/bots/sustainabot/bot-integration/src/*.res`) can be run
ad hoc through the CLI without changes to the test suite.

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
