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

**Status (2026-06-16): interim secure fix (2) LANDED.** `lib/codegen.ml` now
raises `UnsupportedFeature` when a `Float` would transit a heap cell — guarded at
function param/return types (`guard_fn_no_heap_float`) and at Array/tuple/record
*literals* (`guard_no_float_elems`). Silent corruption / invalid emission is gone;
scalar `Float` and `Int` aggregates are untouched. `just wasm-validate` now pins
the loud-fail (two `rej` cases). **The real fix (1) — type-directed heap layout —
remains open as task #8.**

## Update (2026-06-16, cont.) — durable fix (1) for ARRAYS landed via the Float wall

The durable fix is being delivered type-directed and *complete-by-construction*
through the existing **Float-wall elaboration** (the same mechanism that makes
scalar `Float` arithmetic work): `synth` (the real typechecker) records the heap
nodes whose *cell* type is `Float`, and `elaborate_string_concat` rewrites those
exact nodes into specialized AST constructors that codegen lowers with f64 ops.
Because `synth` sees *every* node's checked type, recording is total — every
`Float` construction and every `Float`-yielding access is caught no matter how the
array flowed there — so codegen never guesses a cell width (the gap that made a
codegen-local fix unsafe). New constructors (`Ast.ExprFloatArray`,
`Ast.ExprFloatIndex`) lay out a 4-byte length header + **8-byte f64 cells**
(`f64.load`/`f64.store`, 8-byte stride, alignment hint 3); recorded in
`Typecheck.float_heap_sites`.

**Arrays DONE** (`Array[Float]`, incl. nested `Array[Array[Float]]`): construct,
read `a[i]`, and write `a[i] = e` all validate *and* round-trip the f64 correctly
on wasmtime (`FARR_OK` / `WRITE_OK`). `guard_no_heap_float`'s `Array` case is
lifted accordingly.

**Tuples DONE — all-`Float` AND mixed** (reproducer (b)): a tuple with any
scalar `Float` field uses a **uniform 8-byte cell** layout (`Ast.ExprCellTuple` /
`Ast.ExprCellTupleIndex`): field `i` at offset `i*8` regardless of the field-type
mix, per-cell op (`f64` for a `Float` field, `i32` — low 4 bytes — otherwise).
Uniform-8 sidesteps type-dependent offset accumulation, so `(Int, Float)` and
`(Float, Int)` both round-trip (`MIX_OK` / `FI_OK`), as do all-`Float` (`FTUP_OK`)
and **`Array[(Float, Float)]`** (`AFT_OK`). `synth` records per-field cell kinds
(construct) and, for *every* access to a float-bearing tuple, the accessed
field's kind. `guard`'s `TyTuple` case fully lifted.

`just wasm-validate` pins **12 positive** Float-in-heap checks (incl. 7 wasmtime
round-trips) + the loud-fails. 477 tests green.

**Closed `Float` records DONE.** A *closed* float-bearing record uses the
uniform-8 layout with fields ordered **by name** (`Ast.ExprCellRecord` /
`Ast.ExprCellField`), so construction and by-name access derive identical
offsets independent of literal-vs-type order — verified by the
`#{b:2.0, a:1.0}` → `REC_ORDER_OK` round-trip and mixed `REC_MIX_OK`. The
unification subtlety is handled: a field access only takes the cell path when
`repr obj_ty` is a **closed** `TRecord` (open/polymorphic rows and record
literals with a spread keep loud-failing; `guard`'s `TyRecord` case lifts only
when the row var is `None`).

Also fixed: `find_free_vars` (codegen, runs on the post-elaborate tree) now
traverses `ExprFloatBinary` and all the new cell nodes, so a variable captured
only inside a float expression is no longer missed.

**Still loud-failing (task #8) — now CLEAN `UnsupportedFeature` rejects:**

* **`Float` in closures** (captured `Float`, `Float` parameter, or `Float`
  result). This is a *calling-convention* gap, not a cell-layout one: the
  closure ABI uses uniform 4-byte env/parameter cells and i32 lambda
  param/result/local types. Full support needs an f64-aware closure ABI (env
  cells, lambda signature, and the matching `CallIndirect` type) — larger than
  the aggregate-cell work here. Now loud-fails cleanly (was `UnboundVariable`).
* Compound assignment (`a[i] += x`) to a float element (rare; rewrite as
  `a[i] = a[i] + x`).
* Open/polymorphic float records and float-record spreads.

**Summary:** every `Float` that transits a heap **aggregate** (array, tuple
— all/mixed, record — closed) now lowers correctly and is wasmtime-verified;
the residual rejects are the closure calling convention + two narrow cases,
all honest loud-fails (no silent corruption).

## Related

Not the same as the deliberate carve-outs #555 (effect handlers) / #556 (async
CPS) — those loud-fail already. This one is a *silent* defect in the value
representation, newly made visible by the validate gate.
