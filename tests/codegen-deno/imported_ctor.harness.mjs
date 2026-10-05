// SPDX-License-Identifier: MPL-2.0
// Imported enum constructors are emitted and usable.
import assert from "node:assert/strict";
import { pick, both } from "./imported_ctor.bun.js";

assert.equal(pick(true).value.tag, "North");
assert.equal(pick(false).value.tag, "Step");
assert.equal(both(), "north,step -3,south");
console.log("imported_ctor.harness.mjs OK");
