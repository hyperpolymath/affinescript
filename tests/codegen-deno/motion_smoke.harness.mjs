// SPDX-License-Identifier: MPL-2.0
// bindings #4 — Node ESM harness for the motion library binding.
//
// Installs a globalThis.__as_motion mock before importing the
// generated module, then drives every wrapper (animate / cancel +
// animateMini / tween / spring / ease) and asserts the arguments +
// return values land as expected.

import assert from "node:assert/strict";

let lastAnimateCall = null;
let lastAnimateMiniCall = null;
let lastTweenCall = null;
let lastSpringCall = null;
let lastEaseName = null;
let cancelCount = 0;

const makeControls = (label) => ({
  __label: label,
  then(cb) { if (cb) cb(); return this; },
  cancel() { cancelCount += 1; },
});

globalThis.__as_motion = {
  animate(target, keyframes, options) {
    lastAnimateCall = { target, keyframes, options };
    return makeControls("animate");
  },
  animateMini(target, keyframes, options) {
    lastAnimateMiniCall = { target, keyframes, options };
    return makeControls("animateMini");
  },
  tween(target, from, to, options) {
    lastTweenCall = { target, from, to, options };
    return makeControls("tween");
  },
  spring(target, keyframes, springConfig) {
    lastSpringCall = { target, keyframes, springConfig };
    return makeControls("spring");
  },
  ease(name) {
    lastEaseName = name;
    // Return a sentinel function so consumers can verify the value
    // round-trips through an options record unchanged.
    const fn = (t) => t;
    fn.__easingName = name;
    return fn;
  },
};

const {
  smokeAnimate,
  smokeCancel,
  smokeAnimateMini,
  smokeTween,
  smokeSpring,
  smokeEase,
} = await import("./motion_smoke.deno.js");

// ---- animate + cancel (original surface) ----
const controls = smokeAnimate("#player", { x: 100, opacity: 0.5 }, { duration: 1.0 });
assert.equal(lastAnimateCall.target, "#player", "animate target reaches host");
assert.deepEqual(lastAnimateCall.keyframes, { x: 100, opacity: 0.5 }, "animate keyframes reach host");
assert.deepEqual(lastAnimateCall.options, { duration: 1.0 }, "animate options reach host");

assert.equal(smokeCancel(controls), 0, "cancel returns 0");
assert.equal(cancelCount, 1, "cancel invoked exactly once");

assert.equal(smokeCancel({}), 0, "cancel on bare object returns 0");
assert.equal(cancelCount, 1, "cancel count unchanged for bare object");

// ---- animateMini ----
const miniControls = smokeAnimateMini("#hud", { y: 50 }, { duration: 0.25 });
assert.equal(lastAnimateMiniCall.target, "#hud", "animateMini target reaches host");
assert.deepEqual(lastAnimateMiniCall.keyframes, { y: 50 }, "animateMini keyframes reach host");
assert.deepEqual(lastAnimateMiniCall.options, { duration: 0.25 }, "animateMini options reach host");
assert.equal(miniControls.__label, "animateMini", "animateMini returns its controls handle");

// ---- tween ----
const tweenControls = smokeTween("#enemy", { x: 0 }, { x: 200 }, { duration: 0.6 });
assert.equal(lastTweenCall.target, "#enemy", "tween target reaches host");
assert.deepEqual(lastTweenCall.from, { x: 0 }, "tween from reaches host");
assert.deepEqual(lastTweenCall.to, { x: 200 }, "tween to reaches host");
assert.deepEqual(lastTweenCall.options, { duration: 0.6 }, "tween options reach host");
assert.equal(tweenControls.__label, "tween", "tween returns its controls handle");

// ---- spring ----
const springControls = smokeSpring("#bubble", { scale: 1.4 }, { stiffness: 240, damping: 18, mass: 1 });
assert.equal(lastSpringCall.target, "#bubble", "spring target reaches host");
assert.deepEqual(lastSpringCall.keyframes, { scale: 1.4 }, "spring keyframes reach host");
assert.deepEqual(lastSpringCall.springConfig, { stiffness: 240, damping: 18, mass: 1 }, "spring config reaches host");
assert.equal(springControls.__label, "spring", "spring returns its controls handle");

// ---- ease ----
const easing = smokeEase("backOut");
assert.equal(lastEaseName, "backOut", "ease name reaches host");
assert.equal(typeof easing, "function", "ease returns an opaque easing-function value");
assert.equal(easing.__easingName, "backOut", "easing value carries its name through the boundary");

console.log("motion_smoke.harness.mjs OK");
