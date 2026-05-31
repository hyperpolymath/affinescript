// SPDX-License-Identifier: MPL-2.0
// Node ESM harness for issue #478 — asserts integer `/` truncates toward
// zero while float `/` is preserved, in the Deno-ESM codegen.
import assert from "node:assert/strict";
import {
  idiv,
  fdiv,
  half,
  sum_naturals,
  lcm_like,
  binom,
  div_assign,
  float_let,
  for_sum_halves,
  for_lit,
  idx_half,
  for_float,
} from "./int_div.deno.js";

// Integer division truncates toward zero.
assert.equal(idiv(255, 16), 15, "idiv(255,16)");
assert.equal(idiv(7, 2), 3, "idiv(7,2)");
assert.equal(idiv(-7, 2), -3, "idiv truncates toward zero, not floor");
assert.equal(idiv(6, 3), 2, "idiv exact");
assert.equal(half(7), 3, "half(7)");
assert.equal(sum_naturals(4), 10, "1+2+3+4");
assert.equal(lcm_like(3, 4), 3, "abs(12)/4 via Int-returning call");
assert.equal(binom(5, 2), 10, "C(5,2)");
assert.equal(binom(6, 3), 20, "C(6,3)");
assert.equal(div_assign(255, 16), 15, "x /= b truncates");

// #478 finding 1+2: for-loop variable + indexed element over Array<Int>.
assert.equal(for_sum_halves([5]), 2, "for-loop var x/2 truncates (was 2.5)");
assert.equal(for_sum_halves([5, 9, 7]), 3, "for-loop last x/2 = 7/2 = 3");
assert.equal(for_lit(), 3, "for-loop over int array literal truncates");
assert.equal(idx_half([5]), 2, "indexed Array<Int> element truncates");

// Float division is NOT truncated (no regression).
assert.equal(fdiv(3, 2), 1.5, "fdiv stays float");
assert.equal(fdiv(7, 2), 3.5, "fdiv stays float");
assert.equal(float_let(), 3.5, "float let / float literal stays float");
assert.equal(for_float([5.0]), 2.5, "for-loop over Array<Float> stays float");

console.log("int_div.harness.mjs OK");
