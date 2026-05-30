// SPDX-License-Identifier: MPL-2.0
// issue #458 — exercises String relational ops via the Deno-ESM
// backend. JS's <, >, <=, >= on strings are lex compare natively, so
// the typecheck fix is enough — codegen needs no special case.
import assert from "node:assert/strict";
import {
  lt, gt, le, ge,
  first_lt, first_gt,
  equal_strings_le, equal_strings_ge, equal_strings_lt,
  empty_lt, empty_le, prefix_lt,
} from "./string_lex_cmp.deno.js";

assert.equal(lt("abc", "abd"), true, "abc < abd");
assert.equal(lt("abd", "abc"), false, "abd not < abc");
assert.equal(gt("z", "a"), true, "z > a");
assert.equal(gt("a", "z"), false, "a not > z");
assert.equal(le("x", "x"), true, "x <= x (equal)");
assert.equal(le("x", "y"), true, "x <= y");
assert.equal(le("y", "x"), false, "y not <= x");
assert.equal(ge("x", "x"), true, "x >= x (equal)");
assert.equal(ge("y", "x"), true, "y >= x");
assert.equal(ge("x", "y"), false, "x not >= y");

assert.equal(first_lt(), true, "lit: abc < abd");
assert.equal(first_gt(), true, "lit: z > a");
assert.equal(equal_strings_le(), true, "equal le");
assert.equal(equal_strings_ge(), true, "equal ge");
assert.equal(equal_strings_lt(), false, "equal lt is false");
assert.equal(empty_lt(), true, "empty < non-empty");
assert.equal(empty_le(), true, "empty <= empty");
assert.equal(prefix_lt(), true, "prefix < longer");

console.log("string_lex_cmp.harness.mjs OK");
