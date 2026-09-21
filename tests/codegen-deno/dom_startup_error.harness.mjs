// SPDX-License-Identifier: MPL-2.0
import assert from "node:assert/strict";
import { startupError } from "./dom_startup_error.bun.js";

const logs = [];
const origLog = console.log;
console.log = (...a) => { logs.push(a); };
try {
  const node = startupError("boom");
  assert.equal(node.tag, "VElem");
  assert.equal(node.values[0], "div");
  const attrs = node.values[1];
  assert.equal(attrs[0][0], "class");
  const kids = node.values[2];
  assert.equal(kids[0].values[0], "h1");
  assert.equal(kids[1].values[0], "p");
} finally {
  console.log = origLog;
}
assert.ok(logs.some((x) => String(x[0]).includes("boom")));
console.log("dom_startup_error.harness.mjs OK");
