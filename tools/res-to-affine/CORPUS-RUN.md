<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell -->

# `res-to-affine` — Phase-1 corpus run (2026-05-21)

First end-to-end exercise of the Phase-1 scanner against the estate's
real ReScript surface. Run after [#314] (Phase-1 skeleton merge) on
behalf of [#57]. Surfaced two high-impact false-positive sources in the
top-level regexes and one false-negative; this run records the fixes
and the new baseline.

[#57]: https://github.com/hyperpolymath/affinescript/issues/57
[#314]: https://github.com/hyperpolymath/affinescript/pull/314

## Corpus

| Repo | Dedup'd `.res` files | Notes |
|---|---:|---|
| `idaptik` | 475 | excludes `lib/bs/**`, `lib/ocaml/**` (copies of `src/**`) |
| `gitbot-fleet` | 16 | sustainabot + 3 SafeDOM examples; `lib/ocaml/**` excluded |
| **Total** | **491** | run via `dune exec tools/res-to-affine/main.exe -- <path>` |

`_wt-lic1-idaptik/` is a git worktree of `idaptik` and was skipped.

## Findings

### Before the regex fix

| Kind | Hits |
|---|---:|
| `side-effect-import` | 1,181 |
| `mutable-global` | 653 |
| `raw-js` | 198 |
| `untyped-exception` | 114 |
| **Total** | **2,146** across 216 files |

Spot-check of the top file (`idaptik/src/app/devices/LaptopGUI.res`, 105
markers) showed the 63 `side-effect-import` hits there were all
**indented** `let _ = Container.addChild(parent, child)` — i.e. ReScript's
normal "discard a chained call's return value" idiom inside a function
body, not LESSONS.md's "module-load side effect" anti-pattern.

The same problem applied to `mutable-global`: line 496 of
`NetworkDesktop.res` is `      currentY := currentY.contents +. ...`,
local `ref` mutation inside a function, not a top-level module-scoped
mutable.

### Fixes (this PR)

`scanner.ml`:

- `re_side_effect_import` — drop the leading `[ \t]*`; anchor at column
  0. Module-load side effects only fire at top level; in-function
  `let _ = X.f(...)` is a normal ReScript idiom.
- `re_mutable_global` — replace bare `:=` with
  `^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*:=`. Same logic: top-level assignment
  to a module-scoped ref is the anti-pattern; intra-function
  `counter := ...` is local mutation.
- `re_untyped_exn` — replace `[^a-zA-Z_]raise[ (]` /
  `[^a-zA-Z_]try[ {]` with `\(^\|[^a-zA-Z_]\)…`. Previous form
  required at least one character before `raise` / `try`, missing
  column-0 occurrences.

The trade-off is that we no longer flag module-load side effects or
top-level mutable globals nested inside a `module X = { ... }` block.
Those are the Phase 2 (AST) walker's job. The benefit is a clean
signal that the migrator can trust.

### After the regex fix

| Kind | Hits | Δ |
|---|---:|---|
| `raw-js` | 198 | unchanged |
| `untyped-exception` | 114 | unchanged |
| `side-effect-import` | 36 | −1,145 |
| `mutable-global` | 0 | −653 |
| **Total** | **348** across 94 files | **−84%** |

The 397 files (81%) now reporting zero markers do **not** mean those
files are clean — Phase-1 only sees 4 of 6 anti-patterns, and the
column-0 anchoring trades some recall for sharply improved precision.
A "no findings" skeleton already calls this out:

> A clean `.res` surface does not mean the port is mechanical —
> re-decomposition still applies (see PILOT.md upstream).

### Top remaining hot-spots

| Markers | File | Dominant kind |
|---:|---|---|
| 31 | `.affinescript-src/packages/affine-res/src/AffineScriptValue.res` | `raw-js` |
| 29 | `idaptik-ums/src/App.res` | `raw-js` |
| 24 | `src/Main.res` | `side-effect-import` (real, column-0 `let _ = X.constructor`) |
| 15 | `src/app/screens/training/TrainingBase.res` | `raw-js` |
| 13 | `src/app/screens/WorldBuilder.res` | mixed |

`AffineScriptValue.res` and `App.res` are heavy `%raw` users — the
expected shape for a value-encoding interop layer. `Main.res`
top-level `let _ = X.constructor` is the canonical
"explicit-registration" candidate.

## Validation

- `dune test tools/res-to-affine/` — 3/3 OK (synthetic fixture
  unchanged; the snapshot covers column-0 cases only, so the regex
  tightening leaves the snapshot byte-identical).
- Spot-check confirmed each `side-effect-import` hit is genuinely at
  column 0 (e.g. `idaptik/src/Main.res:5:let _ = PixiSound.sound`).
- Spot-check confirmed each `raw-js` and `untyped-exception` hit
  corresponds to a real `%raw(…)` block or `try`/`Js.Exn`/
  `Promise.catch` occurrence.

## Follow-ups (deferred to Phase 2 / separate issues)

These are noise sources the AST walker can fix that the line-regex
scanner cannot, plus a few small Phase-1 robustness items:

1. **Block-comment awareness** — `is_codeish` filters `//` but not
   `/* … */`. A `%raw(…)` reference inside a block comment would
   currently flag.
2. **Top-level ref declarations** — Phase 1 flags top-level `x := …`
   assignments but does not flag the top-level declaration
   `let x = ref(…)`. Phase 2 should surface both, paired.
3. **Nested-module side effects** — `module Foo = { let _ = X.bar }`
   is a module-load side effect inside a sub-module; needs AST.
4. **Callback-record + oversized-function** — already scoped to
   Phase 2 in the ADR; surfacing here for completeness.
5. **String-literal hits** — a `:=` or `%raw` inside a string literal
   would currently flag. Not seen in the corpus; flagged for awareness.

## Reproducing

```sh
# from a clone of affinescript at the commit landing this PR:
dune build tools/res-to-affine
BIN=$PWD/_build/default/tools/res-to-affine/main.exe

# from a directory containing idaptik/ and gitbot-fleet/ clones:
find idaptik gitbot-fleet -name '*.res' \
  -not -path '*/node_modules/*' \
  -not -path '*/_build/*' \
  -not -path '*/lib/ocaml/*' \
  -not -path '*/lib/bs/*' > corpus.txt

mkdir corpus-out
while IFS= read -r f; do
  safe=$(echo "$f" | tr '/' '_')
  $BIN "$f" > "corpus-out/$safe.affine"
done < corpus.txt

# tally
cat corpus-out/*.affine \
  | grep -oE '\[(side-effect-import|raw-js|untyped-exception|mutable-global)\]' \
  | sort | uniq -c | sort -rn
```
