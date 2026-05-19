// SPDX-License-Identifier: PMPL-1.0-or-later
// ADR-015 S4b regression: all three optional WASI imports used in
// one unit. Each lookup in ctx.wasi_func_indices must return THIS
// import's idx (no collision, no off-by-one). Stubbed values are
// chosen so the sum uniquely identifies any indexing mistake.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/wasi_combo.wasm');
let inst = null;
const called = [];

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    clock_time_get: (clock_id, _precision, time_ptr) => {
      called.push('clock');
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setBigUint64(time_ptr, 100_000_000n, true); // -> 100 ms
      return 0;
    },
    environ_sizes_get: (envc_ptr, envbuf_ptr) => {
      called.push('env');
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(envc_ptr, 20, true);
      dv.setUint32(envbuf_ptr, 64, true);
      return 0;
    },
    args_sizes_get: (argc_ptr, argv_buf_ptr) => {
      called.push('arg');
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(argc_ptr, 3, true);
      dv.setUint32(argv_buf_ptr, 64, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();
// 100 + 20 + 3 = 123 — only correct if all three indices resolved
// to distinct, right imports.
assert.deepEqual(
  called.sort(),
  ['arg', 'clock', 'env'],
  'all three WASI imports dispatched (no collision)',
);
assert.equal(result, 123, 'each builtin returned its host-stub value');
console.log('test_wasi_combo.mjs OK');
