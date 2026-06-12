// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 3 — executable wasm-vs-interp parity check for
// string_sub (the runtime-length copy op: runtime-sized allocation + byte
// copy loop).
//
// The fixture string_sub.affine builds substrings of "hello" and reads them
// back through the slice-1 reader, returning one packed Int derived from the
// interp oracle (lib/interp.ml string_sub clamps):
//
//   e        = scca(sub("hello",1,3), 0) = 101  ('e'; "ell")
//   l        = scca(sub("hello",1,3), 2) = 108  ('l')
//   h        = scca(sub("hello",-1,2),0) = 104  ('h'; negative start -> 0)
//   lenfull  = len(sub("hello",0,5))     = 5    (whole string)
//   lenclamp = len(sub("hello",2,100))   = 3    (length clamped to slen-start')
//   lenstart = len(sub("hello",10,3))    = 0    (start clamped to slen)
//   lenzero  = len(sub("hello",1,0))     = 0    (zero length)
//
//   packed = e + l*256 + h*65536       = 6843493
//   total  = packed + 5 + 3 + 0 + 0    = 6843501
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 3).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_sub.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const e = 101, l = 108, h = 104;
const lenfull = 5, lenclamp = 3, lenstart = 0, lenzero = 0;
const expected = e + l * 256 + h * 65536 + lenfull + lenclamp + lenstart + lenzero; // 6843501

assert.equal(
  result,
  expected,
  `string_sub parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_sub.mjs OK');
