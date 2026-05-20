// SPDX-License-Identifier: MPL-2.0
// issue #225 PR 3a — wasm e2e for multi-var live-local capture.
//
// Proves the continuation, when the host re-enters it after settlement,
// sees BOTH the captured prelude local `tag` (=7) and the settled HTTP
// status (=200): addTag(7, 200) -> 7200. Same #199 wrapHandler dispatch
// + #205 Thenable convention as the PR2 base-case test.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

globalThis.fetch = async (url, init) => ({
  status: url.includes('/missing') ? 404 : 200,
  headers: { forEach: (cb) => cb('text/plain', 'content-type') },
  text: async () => `ok:${init && init.method}`,
});

let inst = null;
const _handles = new Map();
const _results = new Map();
let _next = 1;
let contFired = 0;
let contReturn = null;
let addTagCalls = [];
let savedCb = null;

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
    // Proves the continuation received the captured prelude local AND
    // the settled value: distinct, order-sensitive encoding.
    addTag: (base, status) => {
      addTagCalls.push([base, status]);
      return base * 1000 + status;
    },
  },
  Http: {
    http_request_thenable: (urlPtr, methodPtr, bodyPtr) => {
      const url = readString(urlPtr);
      const method = readString(methodPtr);
      const body = readString(bodyPtr);
      const h = _next++;
      const p = globalThis
        .fetch(url, { method, body: body || undefined })
        .then(async (r) => ({ status: r.status, body: await r.text() }))
        .catch((e) => ({ __error: String(e) }));
      _handles.set(h, p);
      return h;
    },
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

const buf = await readFile('./tests/codegen/http_cps_capture.wasm');
const m = await WebAssembly.instantiate(buf, imports);
inst = m.instance;

const disposable = inst.exports.launch();
assert.ok(Number.isInteger(disposable), 'launch() returns synchronously');
assert.equal(contFired, 0, 'continuation deferred until settlement');

await new Promise((res) => setTimeout(res, 0));
await Promise.resolve();

assert.equal(contFired, 1, 'continuation fired exactly once');
assert.deepEqual(addTagCalls, [[7, 200]],
  'continuation saw captured prelude local tag=7 AND settled status=200');
assert.equal(contReturn, 7200, 'addTag(7,200) result returned by the continuation');

assert.throws(() => savedCb(),
  (e) => e instanceof WebAssembly.RuntimeError,
  'second continuation entry traps (once-resumption guard)');
assert.equal(contFired, 1, 'trapped re-entry did not re-run continuation');

console.log('test_http_cps_capture.mjs OK');
