// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 dedup regression — a unit that uses both `env_count`
// and `env_at`. Both lower to `environ_sizes_get`; the dedup pass
// in codegen's optional_wasi table must register that import
// EXACTLY ONCE. A passing instantiation proves the import set is
// well-formed; the matching numeric result proves both builtins
// resolved to the deduped index correctly.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/env_count_and_at.wasm');
let inst = null;
let sizesCalls = 0;

const entries = ['HOME=/root', 'PATH=/usr/bin'];
const bufSize = entries.reduce((n, s) => n + s.length + 1, 0);

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    environ_sizes_get: (envc_ptr, envbuf_ptr) => {
      sizesCalls++;
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(envc_ptr, entries.length, true);
      dv.setUint32(envbuf_ptr, bufSize, true);
      return 0;
    },
    environ_get: (ptrvec_ptr, buf_ptr) => {
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      let writePtr = buf_ptr;
      entries.forEach((s, i) => {
        dv.setUint32(ptrvec_ptr + i * 4, writePtr, true);
        for (let k = 0; k < s.length; k++) mem[writePtr + k] = s.charCodeAt(k);
        mem[writePtr + s.length] = 0;
        writePtr += s.length + 1;
      });
      return 0;
    },
  },
};

// Will throw at instantiation if environ_sizes_get is imported twice.
inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

// env_count(()) returns 2; string_length(env_at(0)) returns len("HOME=/root") = 10.
assert.equal(result, entries.length + entries[0].length, 'sum across both builtins');
// env_count() makes one call; env_at() makes another. Both come through the
// same import slot — proving the dedup didn't accidentally drop the second
// call site's wiring.
assert.equal(sizesCalls, 2, 'each builtin invocation hit environ_sizes_get');
console.log('test_env_count_and_at.mjs OK');
