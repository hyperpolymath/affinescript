// SPDX-License-Identifier: MPL-2.0
// #56-A — wasm instantiate of the Dom convenience constructors.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/dom_pilot_surface.wasm');
const mod = new WebAssembly.Module(buf);
const inst = (await WebAssembly.instantiate(mod, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
})).instance;
assert.equal(inst.exports.main(), 0, 'dom pilot surface compiled and ran');
console.log('test_dom_pilot_surface.mjs OK');
