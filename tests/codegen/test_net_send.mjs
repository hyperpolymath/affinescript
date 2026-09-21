// SPDX-License-Identifier: MPL-2.0
// #487 — net_send lowers to sock_send over the AS string bytes.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/net_send.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    sock_send: (fd, iovs, _iovs_len, si_flags, so_datalen) => {
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      const bufPtr = dv.getUint32(iovs, true);
      const bufLen = dv.getUint32(iovs + 4, true);
      const data = String.fromCharCode(...mem.slice(bufPtr, bufPtr + bufLen));
      observed = { fd, data, si_flags };
      dv.setUint32(so_datalen, bufLen, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called sock_send');
assert.equal(observed.fd, 4, 'fd forwarded');
assert.equal(observed.data, 'ping', 'payload bytes forwarded');
assert.equal(result, 0, 'errno 0');
console.log('test_net_send.mjs OK');
