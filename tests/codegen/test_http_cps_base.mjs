// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #225 PR 2 — wasm e2e for the CPS transform base case (ADR-013).
//
// Proves the transparent async transform end-to-end on the WasmGC
// backend: the source (http_cps_base.affine) has NO thenableThen /
// closure plumbing — the compiler synthesised it. This harness is the
// host: it implements the #205 Thenable convention + the #199
// closure-pointer dispatch (wrapHandler, mirroring
// packages/affine-vscode/mod.js) and asserts:
//   1. launch() returns synchronously (a disposable i32) — it did NOT
//      block on the network; the continuation was deferred.
//   2. after the host settles the Thenable it re-enters the guest
//      continuation, which reads the settled scalar via the minimal
//      accessor and returns the correct status (200).
//   3. the continuation fires exactly once.
//   4. ADR-013 obligation 1: forcing a second continuation entry traps
//      (the guest-side once-resumption guard), i.e. double-resumption
//      is impossible.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// ── stubbed host fetch (no network) ──────────────────────────────────
globalThis.fetch = async (url, init) => ({
  status: url.includes('/missing') ? 404 : 200,
  headers: { forEach: (cb) => cb('text/plain', 'content-type') },
  text: async () => `ok:${init && init.method}`,
});

let inst = null;
const _handles = new Map();   // Thenable handle -> settling Promise
const _results = new Map();   // Thenable handle -> settled value
let _next = 1;

let contFired = 0;            // times the continuation actually ran
let contReturn = null;        // value the continuation returned
let statusReads = [];         // (handle) args the continuation passed
let savedCb = null;           // captured continuation thunk (re-entry test)

function readString(ptr) {
  const dv = new DataView(inst.exports.memory.buffer);
  const len = dv.getUint32(ptr, true);
  const bytes = new Uint8Array(inst.exports.memory.buffer, ptr + 4, len);
  return new TextDecoder('utf-8').decode(bytes);
}

// #199 closure-pointer dispatch: closure is [i32 fnId @0][i32 envPtr @4];
// invoke via the exported __indirect_function_table, env first, zero-pad
// to arity. Identical to wrapHandler in packages/affine-vscode/mod.js.
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
  // Top-level `extern fn` in the test unit imports under "env".
  env: {
    httpThenableStatus: (tHandle) => {
      statusReads.push(tHandle);
      const v = _results.get(tHandle);
      return v && typeof v.status === 'number' ? v.status : -1;
    },
  },
  // `pub extern fn` from `module Http` imports under "Http".
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
    // #205 thenableThen: register the guest continuation; on settle,
    // record the value FIRST (so the accessor sees it) then fire the
    // continuation exactly once. Returns an opaque disposable i32.
    thenableThen: (tHandle, onSettlePtr) => {
      const cb = wrapHandler(onSettlePtr);
      savedCb = cb;
      const p = _handles.get(tHandle);
      Promise.resolve(p).then((v) => {
        _results.set(tHandle, v);
        contFired += 1;
        contReturn = cb();
      });
      return 1; // disposable token
    },
  },
};

const buf = await readFile('./tests/codegen/http_cps_base.wasm');
const m = await WebAssembly.instantiate(buf, imports);
inst = m.instance;

// 1. launch() returns synchronously without awaiting the network.
const disposable = inst.exports.launch();
assert.ok(
  Number.isInteger(disposable),
  'launch() returns synchronously (an i32 disposable) — async deferred',
);
assert.equal(contFired, 0, 'continuation has NOT run before settlement');

// 2/3. Let the stubbed fetch + thenableThen chain settle.
await new Promise((res) => setTimeout(res, 0));
await Promise.resolve();

assert.equal(contFired, 1, 'continuation fired exactly once');
assert.equal(contReturn, 200, 'continuation read settled status via the minimal accessor');
assert.deepEqual(statusReads, [1], 'continuation called the minimal accessor once, with the Thenable handle');

// 4. ADR-013 obligation 1: a forced second resumption must trap.
assert.throws(
  () => savedCb(),
  (e) => e instanceof WebAssembly.RuntimeError,
  'second continuation entry traps (once-resumption guard)',
);
assert.equal(contFired, 1, 'trapped re-entry did not re-run continuation logic');

console.log('test_http_cps_base.mjs OK');
