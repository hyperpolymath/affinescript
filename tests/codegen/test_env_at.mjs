// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 (#180) — env_at via WASI preview1 environ_get.
// Host writes two known env-var strings into the guest's buffer;
// the guest scans for the null terminator on entry 0 and returns
// its length. A passing run proves the byte-scan loop terminates
// at the right byte (i.e. I32Load8U / the new byte-level wasm IR
// is wired through end-to-end).
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/env_at.wasm');
let inst = null;
const called = [];

// Two env vars: "FOO=bar\0" (8 bytes incl. NUL), "PI=3.14\0" (8 bytes).
const entries = ['FOO=bar', 'PI=3.14'];
const totalBufSize = entries.reduce((n, s) => n + s.length + 1, 0);

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    environ_sizes_get: (envc_ptr, envbuf_ptr) => {
      called.push('sizes');
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(envc_ptr, entries.length, true);
      dv.setUint32(envbuf_ptr, totalBufSize, true);
      return 0;
    },
    environ_get: (ptrvec_ptr, buf_ptr) => {
      called.push('get');
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      let writePtr = buf_ptr;
      entries.forEach((s, i) => {
        dv.setUint32(ptrvec_ptr + i * 4, writePtr, true);
        for (let k = 0; k < s.length; k++) mem[writePtr + k] = s.charCodeAt(k);
        mem[writePtr + s.length] = 0; // NUL terminator
        writePtr += s.length + 1;
      });
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.deepEqual(
  called.sort(),
  ['get', 'sizes'],
  'guest called both environ_sizes_get and environ_get',
);
assert.equal(
  result,
  entries[0].length,
  `env_at(0) length should be ${entries[0].length} ("${entries[0]}")`,
);
console.log('test_env_at.mjs OK');
