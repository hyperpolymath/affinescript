// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 (#485) — open/write/close/open/read against stubbed WASI.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/file_roundtrip.wasm');
let inst = null;
const store = new Map(); // path -> bytes
let nextFd = 10;
const called = [];

function readPath(path_ptr, path_len) {
  const mem = new Uint8Array(inst.exports.memory.buffer);
  return String.fromCharCode(...mem.slice(path_ptr, path_ptr + path_len));
}

const fds = new Map(); // fd -> path

const imports = {
  wasi_snapshot_preview1: {
    fd_write: (fd, iovs, _iovs_len, nwritten_ptr) => {
      called.push('fd_write');
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      const bufPtr = dv.getUint32(iovs, true);
      const bufLen = dv.getUint32(iovs + 4, true);
      const bytes = mem.slice(bufPtr, bufPtr + bufLen);
      const path = fds.get(fd);
      store.set(path, Buffer.from(bytes));
      dv.setUint32(nwritten_ptr, bufLen, true);
      return 0;
    },
    path_open: (_dirfd, _dirflags, path_ptr, path_len, _oflags, _rb, _ri, _fdflags, opened_fd_ptr) => {
      called.push('path_open');
      const path = readPath(path_ptr, path_len);
      const fd = nextFd++;
      fds.set(fd, path);
      if (!store.has(path)) store.set(path, Buffer.alloc(0));
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(opened_fd_ptr, fd, true);
      return 0;
    },
    fd_read: (fd, iovs, _iovs_len, nread_ptr) => {
      called.push('fd_read');
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      const bufPtr = dv.getUint32(iovs, true);
      const bufLen = dv.getUint32(iovs + 4, true);
      const path = fds.get(fd);
      const data = store.get(path) || Buffer.alloc(0);
      const n = Math.min(bufLen, data.length);
      mem.set(data.subarray(0, n), bufPtr);
      dv.setUint32(nread_ptr, n, true);
      return 0;
    },
    fd_close: (fd) => {
      called.push('fd_close');
      fds.delete(fd);
      return 0;
    },
  },
};

inst = (await WebAssembly.instantiate(buf, imports)).instance;
const result = inst.exports.main();

assert.ok(called.includes('path_open'), 'path_open used');
assert.ok(called.includes('fd_write'), 'fd_write used');
assert.ok(called.includes('fd_read'), 'fd_read used');
assert.ok(called.filter((c) => c === 'fd_close').length >= 2, 'fd_close used twice');
assert.equal(result, 2, 'round-trip string_length("ok") == 2');
console.log('test_file_roundtrip.mjs OK');
