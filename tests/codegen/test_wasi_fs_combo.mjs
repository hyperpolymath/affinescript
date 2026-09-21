// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 (#485) — canonical-order regression for filesystem +
// clock + env + sock_shutdown in one module.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/wasi_fs_combo.wasm');
const mod = new WebAssembly.Module(buf);
const names = WebAssembly.Module.imports(mod)
  .filter((i) => i.module === 'wasi_snapshot_preview1')
  .map((i) => i.name);

const expected = [
  'fd_write',
  'clock_time_get',
  'environ_sizes_get',
  'sock_shutdown',
  'path_open',
  'fd_read',
  'fd_close',
];
assert.deepEqual(names, expected, `canonical order drifted: ${names.join(' ')}`);

let inst = null;
const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    clock_time_get: (_id, _p, time_ptr) => {
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setBigUint64(time_ptr, 0n, true);
      return 0;
    },
    environ_sizes_get: (envc_ptr, envbuf_ptr) => {
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(envc_ptr, 0, true);
      dv.setUint32(envbuf_ptr, 0, true);
      return 0;
    },
    sock_shutdown: () => 0,
    path_open: (_d, _f, _p, _l, _o, _rb, _ri, _ff, opened_fd_ptr) => {
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(opened_fd_ptr, 4, true);
      return 0;
    },
    fd_read: (_fd, _iovs, _n, nread_ptr) => {
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(nread_ptr, 0, true);
      return 0;
    },
    fd_close: () => 0,
  },
};

inst = (await WebAssembly.instantiate(mod, imports)).instance;
const result = inst.exports.main();
assert.equal(typeof result, 'number', 'combo ran without trap');
console.log('test_wasi_fs_combo.mjs OK');
