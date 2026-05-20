// SPDX-License-Identifier: MPL-2.0
// ADR-015 S4b (#180) — arg_count via WASI preview1 args_sizes_get.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/arg_count.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    args_sizes_get: (argc_ptr, argv_buf_ptr) => {
      observed = { argc_ptr, argv_buf_ptr };
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(argc_ptr, 3, true);
      dv.setUint32(argv_buf_ptr, 32, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called args_sizes_get');
assert.equal(result, 3, 'arg_count returns the count');
console.log('test_arg_count.mjs OK');
