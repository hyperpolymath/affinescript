// SPDX-License-Identifier: MPL-2.0
// bindings #8 — Node ESM harness for the Canvas 2D binding.
//
// Mocks an HTMLCanvasElement + CanvasRenderingContext2D, runs the
// generated smoke functions, and asserts every shipped extern's
// side-effect was observed on the mock context.

import assert from "node:assert/strict";

class MockCtx2D {
  constructor() {
    this.fillStyle = "#000";
    this.strokeStyle = "#000";
    this.lineWidth = 1;
    this.globalAlpha = 1;
    this.font = "10px sans-serif";
    this.textAlign = "start";
    this.textBaseline = "alphabetic";
    this.ops = [];
  }
  fillRect(x, y, w, h) { this.ops.push(["fillRect", x, y, w, h]); }
  strokeRect(x, y, w, h) { this.ops.push(["strokeRect", x, y, w, h]); }
  clearRect(x, y, w, h) { this.ops.push(["clearRect", x, y, w, h]); }
  beginPath() { this.ops.push(["beginPath"]); }
  closePath() { this.ops.push(["closePath"]); }
  moveTo(x, y) { this.ops.push(["moveTo", x, y]); }
  lineTo(x, y) { this.ops.push(["lineTo", x, y]); }
  arc(x, y, r, s, e) { this.ops.push(["arc", x, y, r, s, e]); }
  fill() { this.ops.push(["fill"]); }
  stroke() { this.ops.push(["stroke"]); }
  save() { this.ops.push(["save"]); }
  restore() { this.ops.push(["restore"]); }
  translate(x, y) { this.ops.push(["translate", x, y]); }
  rotate(rad) { this.ops.push(["rotate", rad]); }
  scale(x, y) { this.ops.push(["scale", x, y]); }
  fillText(text, x, y) { this.ops.push(["fillText", text, x, y]); }
  strokeText(text, x, y) { this.ops.push(["strokeText", text, x, y]); }
  measureText(text) { this.ops.push(["measureText", text]); return { width: text.length * 7.5, fontBoundingBoxAscent: 12, fontBoundingBoxDescent: 4 }; }
  drawImage(img, ...rest) { this.ops.push(["drawImage", img, ...rest]); }
}

const ctx = new MockCtx2D();
const canvas = { getContext: (kind) => { assert.equal(kind, "2d", "getContext called with '2d'"); return ctx; } };

const { smokeRects, smokePath, smokeTransform, smokeText, smokeImages } = await import("./canvas_smoke.deno.js");

// Rects: assert each style + each of the three rect ops + clear.
const ctxBack = smokeRects(canvas);
assert.equal(ctxBack, ctx, "smokeRects returns the same ctx instance");
assert.equal(ctx.fillStyle, "#ff0000");
assert.equal(ctx.strokeStyle, "rgba(0, 0, 0, 0.5)");
assert.equal(ctx.lineWidth, 2.5);
assert.equal(ctx.globalAlpha, 0.75);
assert.deepEqual(ctx.ops[0], ["clearRect", 0, 0, 100, 100]);
assert.deepEqual(ctx.ops[1], ["fillRect", 10, 20, 30, 40]);
assert.deepEqual(ctx.ops[2], ["strokeRect", 50, 60, 20, 20]);

// Path: 7 ops in order (beginPath, moveTo, lineTo, arc, closePath, fill, stroke).
ctx.ops = [];
assert.equal(smokePath(ctx), 0);
assert.deepEqual(ctx.ops, [
  ["beginPath"],
  ["moveTo", 10, 10],
  ["lineTo", 20, 30],
  ["arc", 50, 50, 10, 0, 6.283],
  ["closePath"],
  ["fill"],
  ["stroke"],
]);

// Transform: save → translate → rotate → scale → fillRect → restore.
ctx.ops = [];
assert.equal(smokeTransform(ctx), 0);
assert.deepEqual(ctx.ops, [
  ["save"],
  ["translate", 100, 100],
  ["rotate", 1.5708],
  ["scale", 2, 3],
  ["fillRect", 0, 0, 5, 5],
  ["restore"],
]);

// Text: font/align/baseline are set; fillText + strokeText recorded;
// measureText returns the typed metrics object.
ctx.ops = [];
const metrics = smokeText(ctx);
assert.equal(ctx.font, "16px sans-serif");
assert.equal(ctx.textAlign, "center");
assert.equal(ctx.textBaseline, "middle");
assert.deepEqual(ctx.ops[0], ["fillText", "Hello", 50, 50]);
assert.deepEqual(ctx.ops[1], ["strokeText", "Outlined", 50, 70]);
assert.deepEqual(ctx.ops[2], ["measureText", "Measurement"]);
assert.equal(metrics.width, "Measurement".length * 7.5);
assert.equal(metrics.fontBoundingBoxAscent, 12);

// Images: drawImage natural-size and scaled variants.
ctx.ops = [];
const img = { __mockImage: true, naturalWidth: 200, naturalHeight: 100 };
assert.equal(smokeImages(ctx, img), 0);
assert.deepEqual(ctx.ops[0], ["drawImage", img, 10, 20]);
assert.deepEqual(ctx.ops[1], ["drawImage", img, 30, 40, 100, 50]);

console.log("canvas_smoke.harness.mjs OK");
