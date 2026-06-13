// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 6 — executable wasm-vs-interp parity check for
// string_find (substring search; read-side nested scan, no allocation).
//
// The fixture string_find.affine returns one packed Int derived from the
// interp oracle (lib/interp.ml string_find; empty-needle crash fixed in the
// same change so an empty needle returns 0):
//
//   a = string_find("hello","ll")      = 2    (found mid)
//   b = string_find("hello","o")       = 4    (found at end)
//   c = string_find("hello","xyz")     = -1   (not found)
//   d = string_find("abcabc","bc")     = 1    (FIRST occurrence)
//   e = string_find("hello","")        = 0    (empty needle)
//   f = string_find("hello","hellox")  = -1   (needle longer than haystack)
//
// Each result is in [-1,5]; (result+1) is packed as a 4-bit nibble:
//   (2+1) + (4+1)*16 + 0*256 + (1+1)*4096 + (0+1)*65536 + 0*1048576 = 73811
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 6).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_find.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const a = 2, b = 4, c = -1, d = 1, e = 0, f = -1;
const expected =
  (a + 1) + (b + 1) * 16 + (c + 1) * 256 + (d + 1) * 4096
  + (e + 1) * 65536 + (f + 1) * 1048576; // 73811

assert.equal(
  result,
  expected,
  `string_find parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_find.mjs OK');
