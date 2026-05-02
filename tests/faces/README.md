# Face transformer regression tests

This directory holds **canonical-text snapshots** for each non-canonical face's
output, plus a tiny harness for confirming they keep parsing.

## What's tested

For every example under `examples/faces/`:

1. **Snapshot diff** — the script runs the corresponding `preview-*` subcommand
   and diffs its stdout against a committed `*.expected.txt` here. If the
   transformer's output for the same source ever changes, the diff fails.
2. **Round-trip parse** — the example file is parsed via the normal pipeline
   (which auto-detects face from the pragma). If a transformer change ever
   produces canonical text the parser rejects, this catches it.
3. **Canonical baseline** — `examples/faces/hello-canonical.affine` is parsed
   directly to confirm the reference shape stays valid.

## Files

| File | Source | Captured by |
|---|---|---|
| `hello-rattle.expected.txt` | `examples/faces/hello-rattle.affine` | `affinescript preview-python` |
| `hello-jaffa.expected.txt`  | `examples/faces/hello-jaffa.affine`  | `affinescript preview-js` |
| `hello-pseudo.expected.txt` | `examples/faces/hello-pseudo.affine` | `affinescript preview-pseudocode` |
| `hello-lucid.expected.txt`  | `examples/faces/hello-lucid.affine`  | `affinescript preview-lucid` |
| `hello-cafe.expected.txt`   | `examples/faces/hello-cafe.affine`   | `affinescript preview-cafe` |

## Workflow

### First-time setup (snapshots not yet captured)

```bash
just build
just test-faces-record   # captures any missing snapshot, then diffs
git diff tests/faces/    # review what was captured
git add tests/faces/*.expected.txt
git commit
```

### Routine CI / local check

```bash
just test-faces           # diffs against committed snapshots; fails on drift
```

### Intentional transformer change

Edit a face transformer (e.g. `lib/python_face.ml`), then:

```bash
just test-faces-update    # overwrites the affected snapshot
git diff tests/faces/     # review the lowering change
git add tests/faces/*.expected.txt
git commit
```

The diff in the PR shows reviewers exactly how the canonical lowering changed,
which is more useful than just "transformer modified".

## Why snapshot-test the transformers

The transformers are pure text-to-text. Bugs typically show up as drifted
output rather than crashes — a missing comma, a wrong keyword swap, a broken
indent rule. Snapshot diffs catch those instantly. Combined with the
round-trip parse, this gives a regression net that:

- runs in seconds (no codegen, no wasm),
- has zero false positives (output is deterministic),
- doubles as a side-by-side reference for "different faces, same cube".
