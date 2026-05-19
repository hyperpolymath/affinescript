// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #234 S3 — proves the effect-threaded async-boundary detection
// end-to-end: the source uses `fetchThing` (a user-defined `/ {Async}`
// extern, NOT `http_request_thenable`, NOT in codegen's hardcoded
// `async_primitives`). The compiler still synthesised the #205/#199
// continuation purely from the typecheck effect side-table (ADR-016).
// Same host scaffold as test_http_cps_base.mjs.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

let inst = null;
const _handles = new Map();
const _results = new Map();
let _next = 1;
let contFired = 0;
let contReturn = null;
let savedCb = null;

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
    fetchThing: (_urlPtr) => {
      const h = _next++;
      _handles.set(h, Promise.resolve({ status: 200 }));
      return h;
    },
    thingStatus: (tHandle) => {
      const v = _results.get(tHandle);
      return v && typeof v.status === 'number' ? v.status : -1;
    },
  },
  Http: {
    thenableThen: (tHandle, onSettlePtr) => {
      const cb = wrapHandler(onSettlePtr);
      savedCb = cb;
      Promise.resolve(_handles.get(tHandle)).then((v) => {
        _results.set(tHandle, v);
        contFired += 1;
        contReturn = cb();
      });
      return 1;
    },
  },
};

const buf = await readFile('./tests/codegen/effect_async_boundary.wasm');
inst = (await WebAssembly.instantiate(buf, imports)).instance;

const disposable = inst.exports.launch();
assert.ok(Number.isInteger(disposable), 'launch() returns synchronously');
assert.equal(contFired, 0, 'continuation deferred until settlement');

await new Promise((r) => setTimeout(r, 0));
await Promise.resolve();

assert.equal(contFired, 1, 'continuation fired exactly once');
assert.equal(
  contReturn,
  200,
  'effect-detected boundary: continuation read the settled status',
);
assert.throws(
  () => savedCb(),
  (e) => e instanceof WebAssembly.RuntimeError,
  'second continuation entry traps (ADR-013 once-resumption)',
);
console.log('test_effect_async_boundary.mjs OK');
