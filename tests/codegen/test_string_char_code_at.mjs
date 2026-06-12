// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 1 — executable wasm-vs-interp parity check for
// string indexing (`string_char_code_at`) + `char_to_int`.
//
// The fixture `string_char_code_at.affine` returns one packed Int derived
// from the interp oracle (lib/interp.ml):
//
//   "ABC" bytes        : 'A'=65, 'B'=66, 'C'=67  (read-side [len][utf8] ABI)
//   out-of-bounds      : index -1, 9, and "" at 0  -> -1 sentinel each
//   char_to_int('Z')   : 90
//
//   inbounds = ((65*256 + 66)*256 + 67) = 0x414243 = 4276803
//   total    = inbounds - (neg + past + empty) + z
//            = 4276803 - (-3) + 90            = 4276896
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 1). The positional base-256 packing makes the
// total sensitive to a wrong byte at any index, a wrong OOB sentinel, or a
// wrong char_to_int.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_char_code_at.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const A = 65, B = 66, C = 67;            // in-bounds bytes of "ABC"
const inbounds = (A * 256 + B) * 256 + C; // positional pack
const oobSum = -1 + -1 + -1;              // three -1 sentinels
const z = 90;                             // char_to_int('Z')
const expected = inbounds - oobSum + z;   // 4276896

assert.equal(
  result,
  expected,
  `string indexing parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_char_code_at.mjs OK');
