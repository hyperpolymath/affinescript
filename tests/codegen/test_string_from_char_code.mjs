// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 2 — executable wasm-vs-interp parity check for
// string_from_char_code (the write-side of the [len][utf8] ABI).
//
// The fixture string_from_char_code.affine builds one-byte strings and reads
// them back through the slice-1 reader, returning one packed Int derived from
// the interp oracle (lib/interp.ml: String.make 1 (Char.chr (n land 0xff))):
//
//   a      = scca(sfcc(65), 0)  = 65    ('A')
//   z      = scca(sfcc(90), 0)  = 90    ('Z')
//   masked = scca(sfcc(321),0)  = 65    (321 & 0xff)
//   neg    = scca(sfcc(-1), 0)  = 255   (low byte of -1; I32Store8 truncates)
//   len    = string_length(sfcc(0)) = 1 (NUL byte; length unaffected)
//   oob    = scca(sfcc(65), 1)  = -1    (1-byte string, index 1 out of range)
//
//   packed = a + z*256 + masked*65536  = 4282945
//   total  = packed + neg + len - oob  = 4283202
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 2).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_from_char_code.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const a = 65, z = 90, masked = 65, neg = 255, len = 1, oob = -1;
const expected = a + z * 256 + masked * 65536 + neg + len - oob; // 4283202

assert.equal(
  result,
  expected,
  `string_from_char_code parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_from_char_code.mjs OK');
