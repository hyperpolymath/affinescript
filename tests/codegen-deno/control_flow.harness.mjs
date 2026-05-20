// SPDX-License-Identifier: MPL-2.0
// issue #122 v2.1 — statement-position return / control flow.
import assert from "node:assert/strict";
import { sum_to, classify, first_even } from "./control_flow.deno.js";

assert.equal(sum_to(5), 15, "while-loop + trailing return");
assert.equal(sum_to(0), 0, "while-loop zero iterations");
assert.equal(classify(-3), "neg", "early return in if (neg)");
assert.equal(classify(0), "zero", "early return in if (zero)");
assert.equal(classify(7), "pos", "fallthrough return (pos)");
assert.equal(first_even([1, 3, 4, 7]), 4, "return inside for+if");
assert.equal(first_even([1, 3, 5]), -1, "fallthrough after loop");

console.log("control_flow.harness.mjs OK");
