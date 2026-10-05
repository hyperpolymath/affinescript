// SPDX-License-Identifier: MPL-2.0
// TEA-shaped program: qualified payload ctors + sync receiver-first update.
import assert from "node:assert/strict";
import { run, init, update, Inc, SetName, Batch, Model } from "./tea_shape.bun.js";

assert.equal(run(), "a:2:2", "batched update over qualified payload ctors");

const m = update(update(init(), Inc), SetName("q"));
assert.ok(!(m instanceof Promise), "update is synchronous");
assert.deepEqual({ ...m }, { count: 1, name: "q", xs: [] }, "chains on literals");
assert.equal(update(init(), Batch([Inc, Inc, Inc])).count, 3, "recursive update in a loop");
assert.equal(typeof Model, "function", "class surface still synthesised");

console.log("tea_shape.harness.mjs OK");
