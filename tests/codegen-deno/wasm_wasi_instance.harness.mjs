// SPDX-License-Identifier: MPL-2.0
// Node-ESM harness for the WASI-stub regression (wasm_wasi_instance.affine).
//
// Instantiates an 86-byte wasm module that IMPORTS
// `wasi_snapshot_preview1.fd_write` (declared, never called) and exports
// `add(i32,i32)->i32` — the same import shape every affinescript-compiled wasm
// carries. `addViaWasiWasm` lowers to `__as_wasmInstance(bytes)` followed by
// `__as_wasmCall(...)`; the call succeeds only when `__as_wasmInstance` supplies
// the `{ wasi_snapshot_preview1: { fd_write: () => 0 } }` import object. Before
// the fix this threw "Imports argument must be present and must be an object".

import assert from "node:assert/strict";
import { addViaWasiWasm } from "./wasm_wasi_instance.deno.js";

// (module
//   (import "wasi_snapshot_preview1" "fd_write"
//     (func (param i32 i32 i32 i32) (result i32)))   ;; imported, never called
//   (func (export "add") (param i32 i32) (result i32)
//     local.get 0  local.get 1  i32.add))
const wasmBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,                   // magic + version
  0x01, 0x0f, 0x02,                                                 // type section: 2 types
  0x60, 0x04, 0x7f, 0x7f, 0x7f, 0x7f, 0x01, 0x7f,                   //   (i32,i32,i32,i32)->i32  (fd_write)
  0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f,                               //   (i32,i32)->i32          (add)
  0x02, 0x23, 0x01,                                                 // import section: 1 import
  0x16, 0x77, 0x61, 0x73, 0x69, 0x5f, 0x73, 0x6e, 0x61, 0x70,       //   "wasi_snap...
  0x73, 0x68, 0x6f, 0x74, 0x5f, 0x70, 0x72, 0x65, 0x76, 0x69,       //   ...shot_previ...
  0x65, 0x77, 0x31,                                                 //   ...ew1"
  0x08, 0x66, 0x64, 0x5f, 0x77, 0x72, 0x69, 0x74, 0x65,             //   "fd_write"
  0x00, 0x00,                                                       //   kind=func, type 0
  0x03, 0x02, 0x01, 0x01,                                           // function section: func 1 of type 1
  0x07, 0x07, 0x01, 0x03, 0x61, 0x64, 0x64, 0x00, 0x01,             // export "add" -> func index 1
  0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b, // code: local.get 0/1, i32.add
]);

assert.equal(addViaWasiWasm(wasmBytes, 2, 3), 5, "addViaWasiWasm(2,3) == 5 (WASI-importing module instantiates)");
assert.equal(addViaWasiWasm(wasmBytes, -1, 1), 0, "addViaWasiWasm(-1,1) == 0");
assert.equal(addViaWasiWasm(wasmBytes, 100, 200), 300, "addViaWasiWasm(100,200) == 300");

console.log("wasm_wasi_instance.harness.mjs OK");
