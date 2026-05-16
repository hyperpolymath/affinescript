// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #122 Phase 1 — Node-ESM harness for the Deno-ESM backend.
// Runnable with plain `node` (the generated module only touches the
// `Deno` global lazily inside unused helpers; nothing here calls them).
import assert from "node:assert/strict";
import {
  Counter,
  double,
  ANSWER,
  Red,
  Green,
} from "./class_basic.deno.js";

// struct + receiver-first fns -> class with synthesised constructor
const c = new Counter(10);
assert.equal(c.start, 10, "constructor assigns field");

// receiver-first fn -> async method, `c.start` -> `this.start`
assert.equal(await c.bumped(5), 15, "method reads this.start");

// cross-method call: Counter_bumped(c, by) -> this.bumped(by)
assert.equal(await c.bumpedTwice(5), 20, "cross-method this.* call");

// extern fn lowering: jsonStringify -> JSON.stringify
assert.equal(await c.encodedStart(), "10", "extern lowered to JSON.stringify");

// pub fn / pub const / pub enum -> export
assert.equal(double(21), 42, "free pub fn export");
assert.equal(ANSWER, 42, "pub const export");
assert.deepEqual(Red, { tag: "Red" }, "nullary enum variant");
assert.deepEqual(Green, { tag: "Green" }, "nullary enum variant");

console.log("class_basic.harness.mjs OK");
