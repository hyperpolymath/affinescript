<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
<!-- SPDX-FileCopyrightText: 2026 hyperpolymath -->

# Session handoff (b) — #128/#135 stdlib AOT, continued

Continues `SESSION-HANDOFF-2026-05-18.md`. Single session, sole driver
(the parallel session was stopped). Read the triage doc + this.

## State

stdlib **13/19 compile** `resolve→typecheck→borrow` on `main`
(was 11). 7 PRs merged this run (#167, #170, #172, #173, #174, #184,
plus the earlier slice-10/trunc), each gated on full suite **233/233**,
zero regression.

The import/builtin **infrastructure is now solid** (this was the bulk
of the work — io.affine surfaced a 5-layer systemic stack, all fixed):

- **#172** imported modules now get builtin-seeded resolve + typecheck
  contexts (`seed_builtins` shared; `register_builtins` called in the
  import path); ADR-011 `use module::{...}` works end to end.
- **#173** `register_builtins` reconciled with the resolver seed list —
  file-I/O / env / time / float-math family + `read_line` → `Result`.
- **#174** imported type-schemes threaded into `check_program`
  (`~import_types`), so cross-module *values* type-check.
- **#184** `math.affine` green (`to_float` → `float(n)`; 8 `let mut`).

ADR-011 module model is **ruled**: explicit `use module::{...}`,
`pub` required (not flat-namespace, not prelude-hub).

## Remaining to close STAGE A

6 files, each a span-less typecheck/parse hunt — one PR each:

| file | error | note |
|---|---|---|
| collections | `Unify (Array, (_ -> Bool))` | HOF/filter scheme |
| io | `Unify (String, Array[String])` | `split` `[String]` at use-site |
| option | parse `320:15` | slice 9 `&mut Option<T>`; hardest, do last |
| result | `Unify ((_->_), T)` | generic instantiation |
| testing | `Unify ((Unit->TestResult), TestResult)` | fn ref vs call |
| traits | `Unify (ref _, Int)` | ref/borrow scheme |

Then closers: **#138** (remove the `typecheck.ml` "If without else
returns Never" debug `eprintf`) → **#136** (CI stdlib-wide AOT smoke
gate) → **#137** (multi-module integration test).

## Build

```
cd /home/hyperpolymath/dev/affinescript
export PATH="/usr/bin:$PATH"
eval $(opam env --switch=/home/hyperpolymath/dev/affinescript --set-switch)
dune build && dune test            # must stay 233/233
./_build/default/bin/main.exe check stdlib/<f>.affine
```

When sweeping, filter the stray `If without else returns Never`
stderr line (a debug `eprintf`, not a failure — closer #138 removes it).

## Method (kept)

One file per PR; reproduce with `check`; errors carry no span so read
and reason; most failures are simple **stdlib** bugs (`let`→`let mut`,
`n+0.0`→`float(n)`, missing `use`/`pub`) not compiler bugs — prefer the
stdlib fix. Full sweep + `dune test` before every commit; zero
regression mandatory. `Refs #128`, squash-merge, mirror status on the
#128 thread. Rigorous triage over a partial hack; don't guess at
typecheck/borrow internals.
