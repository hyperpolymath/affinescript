// SPDX-License-Identifier: MPL-2.0
// #56 PR 4 — Node ESM harness for Window / Document / utility bindings.

import assert from "node:assert/strict";

const log = [];
const kids = [];
const body = {
  get firstChild() { return kids[0] || null; },
  get innerHTML() { return this._html; },
  set innerHTML(v) { this._html = String(v); log.push(["innerHTML", v]); },
  _html: "",
  appendChild(n) { kids.push(n); log.push(["append", n.tag]); return n; },
  removeChild(n) {
    const i = kids.indexOf(n);
    if (i >= 0) kids.splice(i, 1);
    log.push(["remove", n.tag]);
    return n;
  },
};
const elements = { root: { tag: "div", id: "root" } };
const document = {
  body,
  createElement(tag) { const el = { tag }; log.push(["create", tag]); return el; },
  getElementById(id) { log.push(["byId", id]); return elements[id] || null; },
};
const windowMock = {
  innerWidth: 1280,
  innerHeight: 720,
  devicePixelRatio: 2,
  document,
  addEventListener(event, handler) { log.push(["on", event, handler]); },
  removeEventListener(event, handler) { log.push(["off", event, handler]); },
  matchMedia(q) { log.push(["mq", q]); return { media: q, matches: true }; },
  open(url, target) { log.push(["open", url, target]); return null; },
  setTimeout(handler, ms) { log.push(["timeout", ms]); return 11; },
  setInterval(handler, ms) { log.push(["interval", ms]); return 22; },
  clearTimeout(id) { log.push(["clearTimeout", id]); },
  clearInterval(id) { log.push(["clearInterval", id]); },
};
globalThis.window = windowMock;
globalThis.document = document;

const {
  smokeWindow, smokeTimers, smokeDocument, smokeUtil,
} = await import("./dom_window_smoke.bun.js");

const handler = () => 0;
assert.equal(smokeWindow(handler), 0);
assert.deepEqual(log.filter((x) => x[0] === "on")[0].slice(0, 2), ["on", "resize"]);
assert.equal(log.find((x) => x[0] === "mq")[1], "(min-width: 600px)");
assert.deepEqual(log.find((x) => x[0] === "open"), ["open", "https://example.com", "_blank"]);

log.length = 0;
assert.equal(smokeTimers(handler), 0);
assert.deepEqual(log.find((x) => x[0] === "timeout"), ["timeout", 16]);
assert.deepEqual(log.find((x) => x[0] === "interval"), ["interval", 1000]);
assert.deepEqual(log.find((x) => x[0] === "clearTimeout"), ["clearTimeout", 11]);
assert.deepEqual(log.find((x) => x[0] === "clearInterval"), ["clearInterval", 22]);

log.length = 0;
const html = smokeDocument();
assert.equal(html, "<p>hi</p>");
assert.equal(log.find((x) => x[0] === "create")[1], "div");
assert.equal(log.find((x) => x[0] === "byId")[1], "root");
assert.equal(log.find((x) => x[0] === "innerHTML")[1], "<p>hi</p>");

const encoded = smokeUtil(1.5, "hi");
assert.equal(encoded, "aGk=");
assert.equal(Buffer.from(encoded, "base64").toString(), "hi");

console.log("dom_window_smoke.harness.mjs OK");
