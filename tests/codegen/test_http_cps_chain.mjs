// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #225 PR 3c — wasm e2e for Async->Async chaining.
//
// Two sequential async boundaries. Proves the recursive transform:
// launch() returns synchronously; the host settles request A, which
// re-enters the OUTER continuation; that continuation itself issues
// request B (a second Thenable) and registers the INNER continuation;
// the host settles B and the inner continuation combines a value
// derived from A's response (s1) with B's. Thenables compose up the
// chain — same #199 wrapHandler dispatch + #205 convention as the
// PR2/PR3a tests, just twice.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// /a -> 200, /b -> 201 (distinct so the combine proves ordering).
globalThis.fetch = async (url, init) => ({
  status: url.includes('/b') ? 201 : 200,
  text: async () => `ok:${init && init.method}`,
});

let inst = null;
const _handles = new Map();
const _results = new Map();
let _next = 1;
const reqUrls = [];
let combineCalls = [];
let combineReturn = null;

function readString(ptr) {
  const dv = new DataView(inst.exports.memory.buffer);
  const len = dv.getUint32(ptr, true);
  const bytes = new Uint8Array(inst.exports.memory.buffer, ptr + 4, len);
  return new TextDecoder('utf-8').decode(bytes);
}

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
    httpThenableStatus: (tHandle) => {
      const v = _results.get(tHandle);
      return v && typeof v.status === 'number' ? v.status : -1;
    },
    combine: (a, b) => {
      combineCalls.push([a, b]);
      return a * 1000 + b;
    },
  },
  Http: {
    http_request_thenable: (urlPtr, methodPtr, bodyPtr) => {
      const url = readString(urlPtr);
      const method = readString(methodPtr);
      readString(bodyPtr);
      reqUrls.push(url);
      const h = _next++;
      const p = globalThis
        .fetch(url, { method })
        .then(async (r) => ({ status: r.status, body: await r.text() }))
        .catch((e) => ({ __error: String(e) }));
      _handles.set(h, p);
      return h;
    },
    thenableThen: (tHandle, onSettlePtr) => {
      const cb = wrapHandler(onSettlePtr);
      Promise.resolve(_handles.get(tHandle)).then((v) => {
        _results.set(tHandle, v);
        combineReturn = cb();
      });
      return 1;
    },
  },
};

const buf = await readFile('./tests/codegen/http_cps_chain.wasm');
const m = await WebAssembly.instantiate(buf, imports);
inst = m.instance;

const disposable = inst.exports.launch();
assert.ok(Number.isInteger(disposable), 'launch() returns synchronously');
assert.deepEqual(reqUrls, ['https://example.test/a'],
  'only request A issued synchronously; B is deferred to the continuation');
assert.equal(combineCalls.length, 0, 'final continuation has not run yet');

// Flush enough microtask/timer rounds for BOTH settlement hops.
for (let i = 0; i < 6; i++) {
  await new Promise((res) => setTimeout(res, 0));
  await Promise.resolve();
}

assert.deepEqual(reqUrls,
  ['https://example.test/a', 'https://example.test/b'],
  'request B was issued by the outer continuation (chaining)');
assert.deepEqual(combineCalls, [[200, 201]],
  'inner continuation combined A-derived s1 (200) with B status (201)');
assert.equal(combineReturn, 200201,
  'chained result threaded back: combine(200,201) = 200201');

console.log('test_http_cps_chain.mjs OK');
