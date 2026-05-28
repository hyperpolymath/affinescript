// SPDX-License-Identifier: MPL-2.0
// bindings #4 — Node ESM harness for the motion library binding.
//
// Installs a globalThis.__as_motion mock before importing the
// generated module, drives smokeAnimate / smokeCancel, and asserts
// the arguments + cancel side-effect were observed.

import assert from "node:assert/strict";

let lastAnimateCall = null;
let cancelCount = 0;

globalThis.__as_motion = {
  animate(target, keyframes, options) {
    lastAnimateCall = { target, keyframes, options };
    return {
      then(cb) { if (cb) cb(); return this; },
      cancel() { cancelCount += 1; },
    };
  },
};

const { smokeAnimate, smokeCancel } = await import("./motion_smoke.deno.js");

const controls = smokeAnimate("#player", { x: 100, opacity: 0.5 }, { duration: 1.0 });
assert.equal(lastAnimateCall.target, "#player", "target reaches host");
assert.deepEqual(lastAnimateCall.keyframes, { x: 100, opacity: 0.5 }, "keyframes reach host");
assert.deepEqual(lastAnimateCall.options, { duration: 1.0 }, "options reach host");

assert.equal(smokeCancel(controls), 0, "cancel returns 0");
assert.equal(cancelCount, 1, "cancel invoked exactly once");

// Cancel a null-controls value is a no-op (mock controls without .cancel)
assert.equal(smokeCancel({}), 0, "cancel on bare object returns 0");
assert.equal(cancelCount, 1, "cancel count unchanged for bare object");

console.log("motion_smoke.harness.mjs OK");
