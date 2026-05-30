// SPDX-License-Identifier: MPL-2.0
// issue #459 — exercises break/continue semantics across while + for.
import assert from "node:assert/strict";
import {
  sum_until,
  sum_odd,
  find_first,
  count_positive,
} from "./loop_break_continue.deno.js";

// sum_until: 0+1+2+3+4+5 = 15 (stops at total >= 10 once total reaches 15).
// Sequence: total=0 (i=0, +0=0), 1, 3, 6, 10 → break.
assert.equal(sum_until(10), 10, "while break exits at threshold");
assert.equal(sum_until(0), 0, "break on first iteration");
assert.equal(sum_until(99999), 4950, "no break — sum 0..99");

// sum_odd: 1+3+5+...+(n-1 if n even, else n) — skip evens via continue.
assert.equal(sum_odd(10), 25, "1+3+5+7+9");
assert.equal(sum_odd(0), 0, "no iterations");
assert.equal(sum_odd(1), 1, "just 1");

// find_first: returns index of first match, -1 if not found.
assert.equal(find_first([5, 3, 7, 9], 7), 2, "for break at match");
assert.equal(find_first([5, 3, 7, 9], 5), 0, "match at index 0");
assert.equal(find_first([5, 3, 7, 9], 42), -1, "no match");
assert.equal(find_first([], 42), -1, "empty array");

// count_positive: filter via continue.
assert.equal(count_positive([1, -2, 3, 0, -5, 7]), 3, "for continue skips non-positive");
assert.equal(count_positive([0, 0, 0]), 0, "all skipped");
assert.equal(count_positive([1, 2, 3]), 3, "none skipped");
assert.equal(count_positive([]), 0, "empty");

console.log("loop_break_continue.harness.mjs OK");
