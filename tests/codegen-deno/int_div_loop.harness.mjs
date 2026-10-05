// SPDX-License-Identifier: MPL-2.0
// Integer division in loop bodies truncates (#478).
import assert from "node:assert/strict";
import { lower_bound, halves } from "./int_div_loop.bun.js";

const xs = [1, 3, 5, 7, 9, 11];
assert.equal(lower_bound(xs, 7), 3);
assert.equal(lower_bound(xs, 8), 4);
assert.equal(lower_bound(xs, 0), 0);
assert.equal(lower_bound(xs, 99), 6);
assert.equal(halves(100), 6);
console.log("int_div_loop.harness.mjs OK");
