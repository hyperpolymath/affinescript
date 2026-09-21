// SPDX-License-Identifier: MPL-2.0
// #487 — net_accept lowers to sock_accept and returns the new fd.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/net_accept.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    sock_accept: (fd, flags, fd_ptr) => {
      observed = { fd, flags };
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(fd_ptr, 9, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called sock_accept');
assert.equal(observed.fd, 3, 'listen fd forwarded');
assert.equal(observed.flags, 0, 'flags default 0');
assert.equal(result, 9, 'net_accept returns the host-written fd');
console.log('test_net_accept.mjs OK');
