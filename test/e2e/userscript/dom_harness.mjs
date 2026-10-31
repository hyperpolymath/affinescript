// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Runtime check for the AffineScript DOM runtime (stdlib/UserscriptDom.affine),
// emitted by the userscript target.
//
// No browser and no jsdom: UserscriptDom only ever reaches the page through
// document.createElement / appendChild / removeChild / *Attribute /
// textContent, so a plain object graph that records those calls is enough to
// run the real emitted artefact and assert what it did.
//
// Usage: dom_harness.mjs <artefact.user.js>
// Exit:  0 all checks passed, 1 a check failed, 2 bad usage.
//
// This file is the second entry in the documented JS exemption for
// test/e2e/userscript/*.mjs (see .claude/CLAUDE.md): the artefact under test
// is JavaScript, so the thing that executes it has to be too.

import { pathToFileURL } from "node:url";
import { resolve } from "node:path";

const artefact = process.argv[2];
if (!artefact) {
  console.error("usage: dom_harness.mjs <artefact.user.js>");
  process.exit(2);
}

const ops = [];

class FakeElement {
  constructor(tag) {
    this.tagName = String(tag);
    this.children = [];
    this.attributes = {};
    this.parentNode = null;
    this.textContent = "";
  }
  appendChild(child) {
    this.children.push(child);
    child.parentNode = this;
    return child;
  }
  removeChild(child) {
    const i = this.children.indexOf(child);
    if (i >= 0) this.children.splice(i, 1);
    child.parentNode = null;
    return child;
  }
  setAttribute(name, value) {
    this.attributes[String(name)] = String(value);
    ops.push(`set:${name}`);
  }
  removeAttribute(name) {
    delete this.attributes[String(name)];
    ops.push(`del:${name}`);
  }
}

const body = new FakeElement("body");
globalThis.document = {
  body,
  createElement: (tag) => new FakeElement(tag),
  createTextNode: (text) => ({ textContent: String(text), parentNode: null }),
};
globalThis.window = globalThis;

const lines = [];
const realLog = console.log;
console.log = (s) => lines.push(String(s));
let threw = null;
try {
  await import(pathToFileURL(resolve(artefact)).href);
} catch (e) {
  threw = e;
}
console.log = realLog;

let failed = 0;
const check = (ok, label) => {
  if (ok) {
    realLog(`  ok   ${label}`);
  } else {
    failed += 1;
    realLog(`  FAIL ${label}`);
  }
};
const has = (marker) => lines.includes(marker);
const hasPrefix = (prefix) => lines.some((l) => l.startsWith(prefix));

check(threw === null, "artefact runs without throwing" + (threw ? `: ${threw}` : ""));
check(has("dom:created"), "create: element created and attached");
check(has("dom:patched"), "patch: element patched in place");
check(has("dom:retitled"), "patch: text-only update");
check(ops.includes("set:data-state"), "create: attribute applied");
check(ops.includes("del:data-state"), "patch: attribute absent from the new spec is removed");
check(ops.includes("set:class"), "patch: new attribute applied");
check(has("dom:grown=3"), "group: reconcile grew to 3 members");
check(has("dom:shrunk=2"), "group: reconcile shrank to 2 members");
check(ops.includes("del:data-step"), "group: reconcile removes an attribute it no longer wants");
check(has("dom:unmounted"), "teardown ran to the end");
check(body.children.length === 0, `teardown: page left empty (found ${body.children.length} child(ren))`);

if (failed === 0) {
  realLog("all DOM runtime checks passed");
  process.exit(0);
}
realLog(`dom runtime: ${failed} check(s) failed`);
realLog("markers: " + lines.join(" | "));
realLog("attribute ops: " + ops.join(", "));
process.exit(1);
