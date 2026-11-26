// SPDX-License-Identifier: MPL-2.0
//
// #560 — wasm parity check for `slice` on String and `show` on Int / Bool.
// string_slice.affine sets one bit per case that matches the interpreter
// oracle (lib/interp.ml `slice` / `show`); all five bits set is 31.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_slice.wasm');
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };
const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.equal(result, 31, `string_slice parity: wasm main()=${result}, want 31 (bits: plain=1 neg=2 empty=4 bothneg=8 show=16)`);
console.log('test_string_slice.mjs OK');
