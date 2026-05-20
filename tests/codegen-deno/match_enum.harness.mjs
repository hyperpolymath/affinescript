// SPDX-License-Identifier: MPL-2.0
// issue #122 v2.4 — qualified Type::Variant patterns + recursive ADTs.
import assert from "node:assert/strict";
import { rank, Red, Blue, len, sum, demo_list } from "./match_enum.deno.js";

assert.equal(rank(Red), 1, "match expr-body, nullary qualified pattern");
assert.equal(rank(Blue), 3, "match expr-body, last arm");

const xs = demo_list(); // Cons(10, Cons(20, Cons(30, Nil)))
assert.equal(len(xs), 3, "recursive len over ADT, payload pattern");
assert.equal(sum(xs), 60, "recursive sum binds payload (h, t)");

console.log("match_enum.harness.mjs OK");
