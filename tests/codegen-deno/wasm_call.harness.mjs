// SPDX-License-Identifier: MPL-2.0
// bindings #5 (closes #414) — Node ESM harness for the wasmCall extern.
//
// Instantiates a 41-byte inline wasm module exporting
// `add(i32, i32) -> i32` and asserts that addViaWasm, which lowers to
// __as_wasmCall(exports, "add", [a, b]), round-trips correctly.

import assert from "node:assert/strict";
import { addViaWasm } from "./wasm_call.deno.js";

// Hand-built minimal wasm module:
//   (module
//     (func (export "add") (param i32 i32) (result i32)
//       local.get 0  local.get 1  i32.add))
const wasmBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // magic + version
  0x01, 0x07, 0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, // type: (i32,i32)->i32
  0x03, 0x02, 0x01, 0x00,                               // func 0 of type 0
  0x07, 0x07, 0x01, 0x03, 0x61, 0x64, 0x64, 0x00, 0x00, // export "add" func 0
  0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b, // body
]);

const instance = new WebAssembly.Instance(new WebAssembly.Module(wasmBytes));
const exports = instance.exports;

assert.equal(addViaWasm(exports, 2, 3), 5, "addViaWasm(2,3) == 5");
assert.equal(addViaWasm(exports, -1, 1), 0, "addViaWasm(-1,1) == 0");
assert.equal(addViaWasm(exports, 0, 0), 0, "addViaWasm(0,0) == 0");
assert.equal(addViaWasm(exports, 100, 200), 300, "addViaWasm(100,200) == 300");

console.log("wasm_call.harness.mjs OK");
