// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// ADR-015 S4a (#180) — assert the `clock_now_ms` builtin wires its
// `wasi_snapshot_preview1.clock_time_get` import correctly and
// computes `ns / 1_000_000` (wrapped to i32). Host-stubbed: no real
// clock involved (the real-host path is the component smoke,
// tests/componentize/smoke.sh).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/clock_now_ms.wasm');
let inst = null;
let observed = null; // (clock_id, precision, time_ptr) the guest asked for

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    // Modern V8 passes wasm i64 to JS as BigInt (not two i32s):
    // (clock_id: i32, precision: i64-as-BigInt, time_ptr: i32) -> i32.
    clock_time_get: (clock_id, _precision, time_ptr) => {
      // 5_000_000_000 ns = 5000 ms. Store i64 little-endian at time_ptr.
      observed = { clock_id, time_ptr };
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setBigUint64(time_ptr, 5_000_000_000n, true);
      return 0; // errno OK
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called clock_time_get');
assert.equal(observed.clock_id, 1, 'CLOCK_MONOTONIC threaded through');
assert.equal(result, 5000, 'ns/1_000_000: 5_000_000_000 → 5000 ms');
console.log('test_clock_now_ms.mjs OK');
