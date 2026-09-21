// SPDX-License-Identifier: MPL-2.0
// #56 — wasm instantiate of the aspirational StartupError shape.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/dom_pilot_startup_error.wasm');
const mod = new WebAssembly.Module(buf);
const inst = (await WebAssembly.instantiate(mod, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
})).instance;
assert.equal(inst.exports.main(), 0, 'dom pilot startupError compiled and ran');
console.log('test_dom_pilot_startup_error.mjs OK');
