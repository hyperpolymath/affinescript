// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 5 — executable wasm-vs-interp parity check for
// trim (scan-for-bounds then copy).
//
// The fixture string_trim.affine trims strings and reads them back through the
// slice-1 reader, returning one packed Int derived from the interp oracle
// (lib/interp.ml String.trim; whitespace = space/tab/newline/form-feed/CR):
//
//   h             = scca(trim("  hi  "),0)   = 104  ('h')
//   i             = scca(trim("  hi  "),1)   = 105  ('i')
//   lenBoth       = len(trim("  hi  "))      = 2    (both ends stripped)
//   lenWs         = len(trim("   "))         = 0    (all whitespace -> empty)
//   lenInternal   = len(trim(" a b "))       = 3    (internal space kept)
//   internalSpace = scca(trim(" a b "),1)    = 32   (the kept internal space)
//   lenTab        = len(trim("\thi\n"))      = 2    (tab + newline stripped)
//
//   packed = h + i*256                                = 26984
//   total  = packed + 2 + 0 + 3 + 32 + 2              = 27023
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 5).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_trim.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const h = 104, i = 105, lenBoth = 2, lenWs = 0, lenInternal = 3, internalSpace = 32, lenTab = 2;
const expected = h + i * 256 + lenBoth + lenWs + lenInternal + internalSpace + lenTab; // 27023

assert.equal(
  result,
  expected,
  `trim parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_trim.mjs OK');
