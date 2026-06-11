// SPDX-License-Identifier: MPL-2.0
// #500 — Node-ESM harness for examples/wasm-exports-demo.affine.
//
// Instantiates a hand-built 120-byte wasm module exporting four
// scalar-typed functions (one per WasmValue kind), then verifies each
// AS-side driver round-trips through the `wasm_export_call` boundary.

import assert from "node:assert/strict";
import {
  add_i32_via_wasm,
  add_i64_via_wasm,
  mul_f32_via_wasm,
  mul_f64_via_wasm,
} from "./wasm_exports_demo.deno.js";

// Hand-built minimal wasm module:
//   (module
//     (func (export "add_i32") (param i32 i32) (result i32)
//       local.get 0  local.get 1  i32.add)
//     (func (export "add_i64") (param i64 i64) (result i64)
//       local.get 0  local.get 1  i64.add)
//     (func (export "mul_f32") (param f32 f32) (result f32)
//       local.get 0  local.get 1  f32.mul)
//     (func (export "mul_f64") (param f64 f64) (result f64)
//       local.get 0  local.get 1  f64.mul))
//
// Layout (each section is `id` `size` `count` `entries…`):
//   - magic + version: 8 bytes
//   - type    section (0x01): 4 func-types        — (TT)->T for T∈{i32,i64,f32,f64}
//   - func    section (0x03): 4 funcs idx -> type — 0->0, 1->1, 2->2, 3->3
//   - export  section (0x07): 4 exports           — "add_i32"/"add_i64"/"mul_f32"/"mul_f64"
//   - code    section (0x0a): 4 bodies            — local.get 0, local.get 1, <op>, end
const wasmBytes = new Uint8Array([
  // Magic + version
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,

  // Type section (0x01), size 25, count 4
  0x01, 0x19, 0x04,
  // (i32,i32)->i32
  0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f,
  // (i64,i64)->i64
  0x60, 0x02, 0x7e, 0x7e, 0x01, 0x7e,
  // (f32,f32)->f32
  0x60, 0x02, 0x7d, 0x7d, 0x01, 0x7d,
  // (f64,f64)->f64
  0x60, 0x02, 0x7c, 0x7c, 0x01, 0x7c,

  // Function section (0x03), size 5, count 4
  0x03, 0x05, 0x04, 0x00, 0x01, 0x02, 0x03,

  // Export section (0x07), size 41, count 4
  0x07, 0x29, 0x04,
  // "add_i32" -> func 0
  0x07, 0x61, 0x64, 0x64, 0x5f, 0x69, 0x33, 0x32, 0x00, 0x00,
  // "add_i64" -> func 1
  0x07, 0x61, 0x64, 0x64, 0x5f, 0x69, 0x36, 0x34, 0x00, 0x01,
  // "mul_f32" -> func 2
  0x07, 0x6d, 0x75, 0x6c, 0x5f, 0x66, 0x33, 0x32, 0x00, 0x02,
  // "mul_f64" -> func 3
  0x07, 0x6d, 0x75, 0x6c, 0x5f, 0x66, 0x36, 0x34, 0x00, 0x03,

  // Code section (0x0a), size 33, count 4
  0x0a, 0x21, 0x04,
  // add_i32 body: local.get 0  local.get 1  i32.add  end
  0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b,
  // add_i64 body: local.get 0  local.get 1  i64.add  end
  0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x7c, 0x0b,
  // mul_f32 body: local.get 0  local.get 1  f32.mul  end
  0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x94, 0x0b,
  // mul_f64 body: local.get 0  local.get 1  f64.mul  end
  0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0xa2, 0x0b,
]);

const instance = new WebAssembly.Instance(new WebAssembly.Module(wasmBytes));
const exports = instance.exports;

// Sanity-check the hand-built module before we trust the AS path.
assert.equal(exports.add_i32(2, 3), 5, "module add_i32(2,3) == 5");
assert.equal(exports.add_i64(2n, 3n), 5n, "module add_i64(2,3) == 5n");
assert.ok(Math.abs(exports.mul_f32(2.5, 4.0) - 10) < 1e-5, "module mul_f32(2.5,4) ~= 10");
assert.ok(Math.abs(exports.mul_f64(2.5, 4.0) - 10) < 1e-12, "module mul_f64(2.5,4) ~= 10");

// ── i32 round-trip ──────────────────────────────────────────────
assert.equal(add_i32_via_wasm(exports, 2, 3),       5,   "add_i32(2,3) == 5");
assert.equal(add_i32_via_wasm(exports, -1, 1),      0,   "add_i32(-1,1) == 0");
assert.equal(add_i32_via_wasm(exports, 100, 200),   300, "add_i32(100,200) == 300");

// ── i64 round-trip — large values past i32 range ────────────────
assert.equal(add_i64_via_wasm(exports, 4000000000, 1000000000),
             5000000000, "add_i64 sums values beyond i32 range");

// ── f32 round-trip — assert at f32 precision (~1e-6) ────────────
{
  const got = mul_f32_via_wasm(exports, 2.5, 4.0);
  assert.ok(Math.abs(got - 10.0) < 1e-5, `mul_f32(2.5, 4) ~= 10 (got ${got})`);
}
{
  const got = mul_f32_via_wasm(exports, 0.1, 0.2);
  assert.ok(Math.abs(got - 0.02) < 1e-5, `mul_f32(0.1, 0.2) ~= 0.02 (got ${got})`);
}

// ── f64 round-trip ──────────────────────────────────────────────
{
  const got = mul_f64_via_wasm(exports, 2.5, 4.0);
  assert.ok(Math.abs(got - 10.0) < 1e-12, `mul_f64(2.5, 4) ~= 10 (got ${got})`);
}
{
  const got = mul_f64_via_wasm(exports, Math.PI, Math.E);
  assert.ok(Math.abs(got - Math.PI * Math.E) < 1e-12, "mul_f64 π × e at f64 precision");
}

console.log("wasm_exports_demo.harness.mjs OK");
