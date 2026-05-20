// SPDX-License-Identifier: MPL-2.0
// issue #235 — proves the #199 [fnId@0,envPtr@4] closure ABI through
// the exported __indirect_function_table in a real wasm engine, with
// NO async/CPS transform involved (no thenableThen import). This is
// the negative-control the http_cps_* tests cannot isolate: it
// validates the table export + captured-env marshalling for ANY
// closure user (rsr-certifier, my-lang, …), independent of #205.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

let inst = null;
let cbFired = 0;

// Identical dispatch to wrapHandler in packages/affine-vscode/mod.js:
// closure = heap [i32 fnId @+0][i32 envPtr @+4]; look fnId up in the
// exported table, call with envPtr first, zero-pad to arity.
function wrapHandler(closurePtr) {
  return () => {
    const tbl = inst.exports.__indirect_function_table;
    const dv = new DataView(inst.exports.memory.buffer);
    const fnId = dv.getInt32(closurePtr, true);
    const envPtr = dv.getInt32(closurePtr + 4, true);
    const fn = tbl.get(fnId);
    const args = [envPtr];
    while (args.length < fn.length) args.push(0);
    return fn(...args);
  };
}

const imports = {
  wasi_snapshot_preview1: { fd_write: () => 0 },
  env: {
    // Plain synchronous extern: dispatch the closure immediately —
    // no Promise/Thenable. Proves the captured local (7) survives
    // table dispatch via envPtr.
    invokeCallback: (closurePtr) => {
      cbFired += 1;
      return wrapHandler(closurePtr)();
    },
  },
};

const buf = await readFile('./tests/codegen/closure_indirect_dispatch.wasm');
inst = (await WebAssembly.instantiate(buf, imports)).instance;

assert.ok(
  inst.exports.__indirect_function_table,
  '__indirect_function_table is exported for a closure-bearing, non-async unit',
);
const r = inst.exports.launch();
assert.equal(cbFired, 1, 'host dispatched the closure exactly once');
assert.equal(
  r,
  42,
  'captured local (7) reached the closure via envPtr; 7 + 35 = 42',
);
console.log('test_closure_indirect_dispatch.mjs OK');
