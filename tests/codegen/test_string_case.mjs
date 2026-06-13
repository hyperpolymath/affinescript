// SPDX-License-Identifier: MPL-2.0
//
// PHASE-F string-wall slice 4 — executable wasm-vs-interp parity check for
// to_lowercase / to_uppercase (ASCII case-folding: copy-with-transform over
// the runtime-length idiom).
//
// The fixture string_case.affine folds case and reads the bytes back through
// the slice-1 reader, returning one packed Int derived from the interp oracle
// (lib/interp.ml String.{lowercase,uppercase}_ascii):
//
//   la    = scca(to_lowercase("ABC"),0) = 97  ('A'->'a')
//   lc    = scca(to_lowercase("ABC"),2) = 99  ('C'->'c')
//   uA    = scca(to_uppercase("abc"),0) = 65  ('a'->'A')
//   digit = scca(to_lowercase("aB3"),2) = 51  ('3' unchanged)
//   below = scca(to_lowercase("@"),0)   = 64  (just below 'A', unchanged)
//   above = scca(to_lowercase("["),0)   = 91  (just above 'Z', unchanged)
//   len   = string_length(to_uppercase("Hello")) = 5
//
//   packed = la + lc*256 + uA*65536            = 4285281
//   total  = packed + digit + below + above+len = 4285492
//
// Equality of the wasm result with this oracle value is the parity gate;
// the same constants are pinned interp-side in test/test_e2e.ml
// (E2E String-wall slice 4). The @ / [ boundary probes prove the case-shift
// range brackets 'A'..'Z' exactly.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/string_case.wasm');

// The fixture performs no I/O; fd_write is declared defensively and never
// called, so a no-op stub suffices for instantiation.
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };

const inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

const la = 97, lc = 99, uA = 65, digit = 51, below = 64, above = 91, len = 5;
const expected = la + lc * 256 + uA * 65536 + digit + below + above + len; // 4285492

assert.equal(
  result,
  expected,
  `case-fold parity: wasm main()=${result}, oracle=${expected}`
);
console.log('test_string_case.mjs OK');
