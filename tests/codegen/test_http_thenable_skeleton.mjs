// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #225 PR 1 — wasm e2e for the typed-wasm Http skeleton.
//
// Inlines the generic Http host adapter (the reusable package is
// deferred until the Ephapax convergence-ABI review, typed-wasm#31).
// Implements the #205 protocol: http_request_thenable registers a
// Promise keyed by an i32 handle; the harness (the host) resolves it
// and asserts the round-trip. `globalThis.fetch` is stubbed — no
// network. Mirrors the tests/codegen/*.mjs inline-imports style.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

// ── stubbed host fetch (no network) ──────────────────────────────────
globalThis.fetch = async (url, init) => ({
  status: url.includes('/missing') ? 404 : 200,
  headers: { forEach: (cb) => cb('text/plain', 'content-type') },
  text: async () => `ok:${init && init.method}`,
});

let inst = null;
const _handles = new Map();
let _next = 1;
const _results = new Map();

function readString(ptr) {
  const dv = new DataView(inst.exports.memory.buffer);
  const len = dv.getUint32(ptr, true);
  const bytes = new Uint8Array(inst.exports.memory.buffer, ptr + 4, len);
  return new TextDecoder('utf-8').decode(bytes);
}

// The generic Http convergence adapter (host import surface).
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
      p.then((v) => _results.set(h, v));
      return h;
    },
  },
};

const buf = await readFile('./tests/codegen/http_thenable_skeleton.wasm');
const m = await WebAssembly.instantiate(buf, imports);
inst = m.instance;

// Pure pass-through: launch() returns the Thenable handle.
const handle = inst.exports.launch();
assert.ok(Number.isInteger(handle) && handle > 0, 'launch returns an i32 Thenable handle');

// Host resolves it via the protocol (guest-side resolution is PR 2/3).
const settled = await _handles.get(handle);
assert.equal(settled.status, 200, 'response status round-trips');
assert.equal(settled.body, 'ok:GET', 'method + body round-trip via readString');
assert.deepEqual(settled.headers, [['content-type', 'text/plain']], 'headers assoc list');
assert.equal(_results.get(handle).status, 200, 'thenableResultJson-equivalent payload stored');

console.log('test_http_thenable_skeleton.mjs OK');
