// SPDX-License-Identifier: MPL-2.0
// issue #225 PR3d — wasm e2e host for http_response_reader.affine.
//
// Same #205 Thenable + #199 closure scaffold as test_http_cps_base.mjs
// (mirrors packages/affine-vscode/mod.js). The continuation here is the
// typed-Response reconstruction over the scalar/string primitives; the
// harness implements those host imports off the settled record and
// asserts the folded result proves status + structured `headers`
// decode + is_ok — i.e. http_fetch parity (cf. the Deno-ESM
// tests/codegen-deno/http_fetch.* field access) on the wasm path.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// Stubbed host fetch: 1 response header (proves the [(String,String)]
// decode jsonField cannot do), status 200.
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
let savedCb = null;
const reads = []; // names of response* primitives the reader invoked

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

const settled = (tHandle) => _results.get(tHandle);

const imports = {
  wasi_snapshot_preview1: { fd_write: () => 0 },
  Http: {
    http_request_thenable: (urlPtr, methodPtr, bodyPtr) => {
      const url = readString(urlPtr);
      const method = readString(methodPtr);
      const body = readString(bodyPtr);
      const h = _next++;
      const p = globalThis
        .fetch(url, { method, body: body || undefined })
        .then(async (r) => {
          const headers = [];
          r.headers.forEach((v, k) => headers.push([k, v]));
          return { status: r.status, headers, body: await r.text() };
        })
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
    // Typed-reader scalar/string primitives (ADR-013 minimal reader;
    // the proven jsonField-style convention — no record crosses the
    // i32 boundary). String returns are opaque host handles the guest
    // only stores; the fold asserts status + header count + is_ok.
    responseStatus: (t) => {
      reads.push('status');
      const v = settled(t);
      return v && typeof v.status === 'number' ? v.status : -1;
    },
    responseHeaderCount: (t) => {
      reads.push('count');
      const v = settled(t);
      return v && Array.isArray(v.headers) ? v.headers.length : -1;
    },
    responseHeaderName: (t, i) => {
      reads.push(`name${i}`);
      return 0x4000 + i; // opaque String handle (guest never decodes)
    },
    responseHeaderValue: (t, i) => {
      reads.push(`value${i}`);
      return 0x8000 + i;
    },
  },
};

const buf = await readFile('./tests/codegen/http_response_reader.wasm');
inst = (await WebAssembly.instantiate(buf, imports)).instance;

// 1. launch() returns synchronously (async deferred via the transform).
const disposable = inst.exports.launch();
assert.ok(Number.isInteger(disposable), 'launch() returns synchronously');
assert.equal(contFired, 0, 'continuation deferred until settlement');

// 2. settle.
await new Promise((r) => setTimeout(r, 0));
await Promise.resolve();

assert.equal(contFired, 1, 'continuation fired exactly once');
// status 200 + 10*headerCount(1) + 1000 (200 is 2xx) = 1210. Only a
// correct status + structured-header decode + is_ok yields this.
assert.equal(
  contReturn,
  1210,
  'typed Response reconstructed: status=200, headers decoded (count=1), is_ok=true',
);
assert.ok(
  reads.includes('status') &&
    reads.includes('count') &&
    reads.includes('name0') &&
    reads.includes('value0'),
  'continuation invoked the typed-reader primitives (incl. per-header decode)',
);

// 3. ADR-013 obligation 1: a forced second resumption traps.
assert.throws(
  () => savedCb(),
  (e) => e instanceof WebAssembly.RuntimeError,
  'second continuation entry traps (once-resumption guard)',
);
assert.equal(contFired, 1, 'trapped re-entry did not re-run the continuation');

console.log('test_http_response_reader.mjs OK');
