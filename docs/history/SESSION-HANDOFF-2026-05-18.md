<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2026 hyperpolymath -->

# Session handoff — #135 stdlib AOT (note for the next Claude)

**Date:** 2026-05-17 → 2026-05-18. **Read `docs/history/STDLIB-AOT-TRIAGE.md`
first** — it is the authoritative live punch list.

## What this work was

Epic **#128 / #135**: make every `stdlib/*.affine` compile through
`resolve → typecheck → codegen` (Deno-ESM). This was the estate's
ReScript-port bottleneck.

## What landed (merged to `main`, ~20 PRs, #149–#169)

Front: #131 (`>>` keystone), #134 (unwrap soundness), #132 (ADR-011
namespace), #133 (single-ownership dedup).

#135 slices merged: **1** `fn(x)=>e` lambdas · **2** slice index
`e[a:b]` · **3** bare `effect E;` + ADR-008 `-> T / E` · **4** `total`
keyword-rename + `Iterator::collect` Vec→`[]` · **5** trait default
bodies · **6/6b** `try`/`ref`/`as` keyword-as-ident renames · **7**
let-polymorphism (generic fns were silently monomorphic — high-leverage
typecheck fix) · **8** module visibility/imports · **11** resolver
two-pass (forward refs + mutual recursion). Slice **10** (effects
generic-extern kinds) landed via a parallel session.

Result: **~11/19 stdlib files compile end-to-end** (prelude, string,
effects, Core + 7 module files). The two hardest *enabling* fixes
(sl.7 let-poly, sl.11 resolver two-pass) are done — nothing is blocked
on the resolver or the typecheck wall any more.

## What remains (see triage doc for precise per-file root causes)

- **slice 9** — `option` `&mut Option<T>` ref params (`take`/
  `get_or_insert`); affine/borrow lowering, ownership-soundness-critical.
- **slice 12** — deeper typecheck on `result`/`collections`/`testing`/
  `traits` (now past resolution).
- **io / math** — cross-module `split` (in `string.affine`) / `trunc`;
  see open PRs below.

## ⚠️ Concurrency note (important)

This repo was worked by **two Claude sessions in parallel** in the same
clone, under the shared `hyperpolymath` git identity. Open PRs **#167**
(slice 10 `Ref<T>` kind) and **#170** (`trunc` builtin) belong to the
*other* session — do not assume they are yours. Before resuming:
re-verify `origin/main`, base any branch on it (never on a stray
checked-out branch), and coordinate on `io`/`math`/resolver/`string`
which the other session was actively editing. Mechanics + hazards are
in memory `affinescript-spine-rederived`, `split-merge-failure-mode`,
`stray-branch-base-misdetection`, `git-checkout-discards-uncommitted-edits`.

## Build

Canonical clone is `/home/hyperpolymath/dev/affinescript` (has the
`_opam` local switch; the `repos/affinescript` clone has no toolchain).
Prefix every command:

```
export PATH="/usr/bin:$PATH"
eval $(opam env --switch=/home/hyperpolymath/dev/affinescript --set-switch)
dune build && dune test     # full suite was green at 233 tests
```

Discipline that paid off this session: isolate genuine root causes with
minimal repros before changing the compiler; commit a green change
*before* any conflict-check/file-swap; for grammar edits, diff menhir
conflict counts before/after; one scoped slice per PR; rigorous triage
over a damaging partial pass.
