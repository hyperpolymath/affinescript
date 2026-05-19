// SPDX-License-Identifier: PMPL-1.0-or-later
// ADR-015 S4b (#180) — env_count via WASI preview1 environ_sizes_get.
// Host stubs the import with a known count + buf_size; asserts the
// guest returns that count.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/env_count.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    environ_sizes_get: (envc_ptr, envbuf_ptr) => {
      observed = { envc_ptr, envbuf_ptr };
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(envc_ptr, 7, true);        // 7 env vars
      dv.setUint32(envbuf_ptr, 256, true);    // unused by env_count
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called environ_sizes_get');
assert.equal(result, 7, 'env_count returns the count the host wrote');
console.log('test_env_count.mjs OK');
