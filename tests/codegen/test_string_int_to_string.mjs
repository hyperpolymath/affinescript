// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 7 — executable wasm-vs-interp parity check for
// int_to_string (decimal rendering of an i32).
//
// The fixture string_int_to_string.affine renders integers and reads the
// bytes back through the slice-1 reader, returning one packed Int derived from
// the interp oracle (lib/interp.ml: string_of_int):
//
//   z       = scca(int_to_string(0),0)            = 48  ('0')
//   neg     = scca(int_to_string(-5),0)           = 45  ('-')
//   d5      = scca(int_to_string(-5),1)           = 53  ('5')
//   minHead = scca(int_to_string(-2147483648),1)  = 50  ('2' of "-2147483648")
//   minTail = scca(int_to_string(-2147483648),10) = 56  ('8' of "-2147483648")
//   lmin    = string_length(int_to_string(-2147483648)) = 11
//   lmax    = string_length(int_to_string(2147483647))  = 10
//
//   packed = 48 + 45*256 + 53*65536                   = 3484976
//   total  = packed + 50 + 56 + 11 + 10               = 3485103
//
// The INT_MIN case exercises the negative-space digit extraction (the
// magnitude of INT_MIN is not a representable positive i32). Equality of the
// wasm result with this oracle value is the parity gate; full-string
// INT_MIN/INT_MAX content is pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 7).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_int_to_string.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const z = 48, neg = 45, d5 = 53, minHead = 50, minTail = 56, lmin = 11, lmax = 10;
const expected = z + neg * 256 + d5 * 65536 + minHead + minTail + lmin + lmax; // 3485103

assert.equal(
  result,
  expected,
  `int_to_string parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_int_to_string.mjs OK');
