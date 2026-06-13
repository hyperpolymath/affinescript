// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 8b — executable wasm-vs-interp parity check for
// string `++` (the type-directed byte-concat lowering).
//
// The fixture string_concat.affine concatenates strings (literal, var-var,
// chained, empty) and reads them back through the slice-1 reader, returning
// one packed Int derived from the interp oracle:
//
//   c     = scca("ab" ++ "cd", 2)         = 99  ('c'; was 2 under the buggy
//                                                 list-concat lowering)
//   len4  = len("ab" ++ "cd")             = 4
//   vbyte = scca(a ++ b, 3)  (a,b vars)   = 98  ('b'; the var-var case the
//                                                 slice-8a guard could not catch)
//   vlen  = len(a ++ b)                   = 6
//   ch    = scca("a"++"bb"++"ccc", 3)     = 99  ('c'; chained)
//   chlen = len("a"++"bb"++"ccc")         = 6
//   el    = len("" ++ "x")                = 1
//   er    = len("x" ++ "")                = 1
//
//   packed = c + vbyte*256 + ch*65536     = 6513251
//   total  = packed + 4 + 6 + 6 + 1 + 1   = 6513269
//
// Equality of the wasm result with this oracle value is the parity gate; the
// same string semantics are pinned interp-side in test/test_e2e.ml. The byte-2
// check is the direct regression for the silent miscompile the guard caught.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_concat.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const c = 99, vbyte = 98, ch = 99;
const len4 = 4, vlen = 6, chlen = 6, el = 1, er = 1;
const expected = c + vbyte * 256 + ch * 65536 + len4 + vlen + chlen + el + er; // 6513269

assert.equal(
  result,
  expected,
  `string ++ parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_concat.mjs OK');
