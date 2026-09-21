// SPDX-License-Identifier: MPL-2.0
// #487 — net_recv lowers to sock_recv and copies host bytes into an AS string.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/net_recv.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    sock_recv: (fd, iovs, iovs_len, ri_flags, ro_datalen, _ro_flags) => {
      observed = { fd, iovs_len, ri_flags };
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      const bufPtr = dv.getUint32(iovs, true);
      mem[bufPtr] = 104; // 'h'
      mem[bufPtr + 1] = 105; // 'i'
      dv.setUint32(ro_datalen, 2, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called sock_recv');
assert.equal(observed.fd, 4, 'fd forwarded');
assert.equal(result, 2, 'net_recv copied 2 bytes');
console.log('test_net_recv.mjs OK');
