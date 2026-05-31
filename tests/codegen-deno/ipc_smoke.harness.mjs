// SPDX-License-Identifier: MPL-2.0
// bindings #9 — Node ESM harness for the Ipc binding.
//
// MessageChannel + structuredClone are standard web-platform globals
// in Node 16+, so no host injection is required — we just import the
// compiled module and assert side-effects on real port instances.

import assert from "node:assert/strict";

const { smokeChannelFlow, smokeCloseFlow, smokeTargetFlow, smokeStructuredClone } = await import("./ipc_smoke.deno.js");

// Channel flow: assert handlers fire on the peer side, postMessage
// round-trips identical payload, and close()s don't throw.
const receivedOn1 = [];
const receivedOn2 = [];
const handler1 = (event) => { receivedOn1.push(event.data); };
const handler2 = (event) => { receivedOn2.push(event.data); };
const payload = { kind: "level.load", levelId: 42, params: { difficulty: "hard" } };
// Keep the returned channel alive across the drain — both ports are
// GC-eligible on Node the moment their last reference drops.
const channelHandle = smokeChannelFlow(payload, handler1, handler2);
assert.ok(channelHandle, "smokeChannelFlow returns a non-null channel handle");

// MessagePort delivery is async — on Node the worker_threads-backed
// MessageChannel batches across at least one real `setTimeout`
// (microtask + a single setImmediate aren't enough). 50 ms is
// well-above the empirical ~few-ms delivery window.
await new Promise((r) => setTimeout(r, 50));
assert.equal(receivedOn1.length, 1, "port1 handler received one message");
assert.equal(receivedOn2.length, 1, "port2 handler received one message");
assert.deepEqual(receivedOn2[0], payload, "port2 receives the payload posted from port1");
assert.deepEqual(receivedOn1[0], payload, "port1 receives the payload posted from port2");

// Close flow: should not throw, and a subsequent post on the closed
// port should land quietly (this is a pure host-side smoke of the
// close lifecycle path).
assert.equal(smokeCloseFlow(), 0, "smokeCloseFlow returns 0");

// Target flow: a custom target object satisfying the
// `.postMessage(msg)` shape should observe the call.
const targetCalls = [];
const target = { postMessage: (msg) => targetCalls.push(msg) };
assert.equal(smokeTargetFlow(target, { ack: 1 }), 0, "smokeTargetFlow returns 0");
assert.deepEqual(targetCalls, [{ ack: 1 }], "target.postMessage observed with payload");

// Structured clone: the result must equal the source by value but
// not by reference (the whole point of the helper).
const source = { a: 1, nested: { b: [1, 2, { c: 3 }] } };
const clone = smokeStructuredClone(source);
assert.deepEqual(clone, source, "clone deep-equals source");
assert.notStrictEqual(clone, source, "clone is a distinct object");
assert.notStrictEqual(clone.nested, source.nested, "clone.nested is a distinct object");
assert.notStrictEqual(clone.nested.b[2], source.nested.b[2], "deep clone reaches nested array elements");
source.a = 999;
assert.equal(clone.a, 1, "mutating source does not affect clone");

// Close the channel's ports so the active listeners don't keep the
// event loop alive after the assertions complete.
channelHandle.port1.close();
channelHandle.port2.close();

console.log("ipc_smoke.harness.mjs OK");
