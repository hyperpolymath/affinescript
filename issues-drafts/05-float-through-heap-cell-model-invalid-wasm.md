<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

# Core-wasm: any `Float` that transits the heap is mismodeled (invalid/truncated wasm)

**Surfaced by:** WASM coverage sweep + coprocessor smoke test (2026-06-16)
**Affected version:** `affinescript` compiler at HEAD (branch `feat/solo-core-metatheory-proofs`)
**Severity:** Correctness + **security** — silent 32-of-64-bit truncation on store; invalid module on load. Caught for the first time by the new `just wasm-validate` gate (`tools/wasm-validate-gate.sh`); previously hidden because `test/test_e2e.ml:601` only checks "codegen did not raise", never `wasm-tools validate`.

## Reproducers

```affinescript
// (a) load: INVALID module — wasm-tools: "expected f64, found i32"
fn rd(i: Int, a: Array[Float]) -> Float { a[i] }

// (b) projection: INVALID module (same cause, via tuple)
fn proj() -> Float { let t: (Float, Float) = (1.0, 2.0); t.0 }

// (c) store: VALIDATES but is SEMANTICALLY WRONG — copies 32 of 64 bits
fn k(i: Int, mut o: Array[Float], a: Array[Float]) -> Unit { o[i] = a[i]; }
```

```
$ affinescript compile rd.affine -o rd.wasm && wasm-tools validate rd.wasm
error: func 1 failed to validate
  0: type mismatch: expected f64, found i32
```

Scalars are fine (`fn dbl(x: Float) -> Float { x * 2.0 }` validates) — the bug is strictly the heap path. `Int` through the heap is fine.

## Root cause

The core-wasm backend (`lib/codegen.ml`) uses a **uniform 4-byte (i32) heap-cell
model** everywhere:

- array alloc `size = 4 + (num_elements * 4)`, element offset `4 + (idx * 4)`
  (`lib/codegen.ml:1926`, `:1949`, `:2047-2053`)
- tuple/record fields at `index * 4`, `I32Load`/`I32Store` with a `* 4` stride
  (`:1964`, `:2030`, `:2519-2548`)

A `Float` is `f64` (8 bytes) in locals/arithmetic, but the moment it is stored
into or loaded from an array/tuple/record cell the layout assumes 4 bytes and
the access is emitted as `i32.{load,store}`. Hence: load-then-use-as-f64 →
invalid module; store → 32-bit truncation of a 64-bit value.

## Fix options

1. **Type-directed heap layout (the real fix).** Make cell size and the
   load/store opcode a function of the static element/field type: `f64` cells
   are 8 bytes with `f64.{load,store}` and an 8-byte stride; mixed records/
   tuples compute per-field offsets from field types. Touches arrays, tuples,
   records, and closures — a substantial, careful codegen change. **High
   revert-cost → land behind its own PR with the `wasm-validate` gate
   extended to cover Float-in-heap fixtures.**
2. **Honest loud-fail (interim, secure).** Until (1) lands, have the backend
   raise `Codegen.UnsupportedFeature` when a `Float` would transit a heap cell,
   with a message routing to the interpreter (`-i`), the Julia backend
   (`-julia`), or the GPU kernel backends (WGSL/CUDA/Metal/OpenCL **already
   lower `f64` array buffers correctly** — verified 2026-06-16). This converts
   silent-wrong into honest-reject, matching the #555/#556 loud-fail policy.

Recommended: ship (2) now for safety, then (1) as the durable fix. Both are
gated by `just wasm-validate`.

## Related

Not the same as the deliberate carve-outs #555 (effect handlers) / #556 (async
CPS) — those loud-fail already. This one is a *silent* defect in the value
representation, newly made visible by the validate gate.
