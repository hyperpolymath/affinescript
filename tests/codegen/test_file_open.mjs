// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 (#485) — assert file_open wires path_open correctly.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/file_open.wasm');
let inst = null;
let observed = null;

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    path_open: (dirfd, dirflags, path_ptr, path_len, oflags, _rb, _ri, _fdflags, opened_fd_ptr) => {
      const mem = new Uint8Array(inst.exports.memory.buffer);
      const path = String.fromCharCode(...mem.slice(path_ptr, path_ptr + path_len));
      observed = { dirfd, dirflags, path, oflags };
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(opened_fd_ptr, 7, true);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(observed, 'guest called path_open');
assert.equal(observed.dirfd, 3, 'dirfd is the first WASI preopen');
assert.equal(observed.path, 'hello.txt', 'path bytes forwarded');
assert.equal(observed.oflags, 1, 'CREAT oflag forwarded');
assert.equal(result, 7, 'file_open returns the host-written fd');
console.log('test_file_open.mjs OK');
