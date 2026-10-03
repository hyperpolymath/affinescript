// SPDX-License-Identifier: MPL-2.0
// #56-A — wasm instantiate of the Dom convenience constructors.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/dom_pilot_surface.wasm');
// BufferSource overload -> { module, instance }. (Passing a WebAssembly.Module
// instead resolves to the Instance itself; `.instance` would be undefined.
// See tools/check-wasm-harness-idioms.mjs.)
const { instance: inst } = await WebAssembly.instantiate(buf, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
});
assert.equal(inst.exports.main(), 0, 'dom pilot surface compiled and ran');
console.log('test_dom_pilot_surface.mjs OK');
