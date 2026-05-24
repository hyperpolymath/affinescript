// SPDX-License-Identifier: MPL-2.0
// ADR-015 S5 (#180) — arg_at via WASI preview1 args_get.
// Index 1 (the SECOND argv entry) — verifies the `ptrvec[n]` lookup
// at `n*4` doesn't silently always return the first pointer.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/arg_at.wasm');
let inst = null;
const called = [];

// argv: ["./prog", "--flag=value"]; arg_at(1) -> "--flag=value" (len 12).
const argv = ['./prog', '--flag=value'];
const bufSize = argv.reduce((n, s) => n + s.length + 1, 0);

const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
    args_sizes_get: (argc_ptr, argv_buf_ptr) => {
      called.push('sizes');
      const dv = new DataView(inst.exports.memory.buffer);
      dv.setUint32(argc_ptr, argv.length, true);
      dv.setUint32(argv_buf_ptr, bufSize, true);
      return 0;
    },
    args_get: (ptrvec_ptr, buf_ptr) => {
      called.push('get');
      const dv = new DataView(inst.exports.memory.buffer);
      const mem = new Uint8Array(inst.exports.memory.buffer);
      let writePtr = buf_ptr;
      argv.forEach((s, i) => {
        dv.setUint32(ptrvec_ptr + i * 4, writePtr, true);
        for (let k = 0; k < s.length; k++) mem[writePtr + k] = s.charCodeAt(k);
        mem[writePtr + s.length] = 0;
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
  'guest called both args_sizes_get and args_get',
);
assert.equal(
  result,
  argv[1].length,
  `arg_at(1) length should be ${argv[1].length} ("${argv[1]}")`,
);
console.log('test_arg_at.mjs OK');
