// SPDX-License-Identifier: MPL-2.0
// Thunk typing + numeric builtin lowering on Bun-ESM.
import assert from "node:assert/strict";

const notes = [];
const later = [];
globalThis.host_note = (s) => notes.push(s);
globalThis.host_later = (f) => later.push(f);
const m = await import("./thunk_math.bun.js");

assert.equal(m.inline_thunk(), 42, "inline thunk passed to () -> Int");
m.named_thunk();
assert.equal(notes.length, 0, "thunk not run eagerly");
later.forEach((f) => f());
assert.deepEqual(notes, ["ran"], "named thunk runs when called");
assert.equal(m.geometry(3, 4), 5 + 2 + 1 + 0, "sqrt/floor/round/atan2/float lower to Math");

console.log("thunk_math.harness.mjs OK");
