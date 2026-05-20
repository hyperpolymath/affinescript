// SPDX-License-Identifier: MPL-2.0
// issue #122 v2.5 — honest string/number primitive lowering + `++` shape
// dispatch (string concat vs array concat).
import assert from "node:assert/strict";
import {
  ends_with2,
  strip_suffix,
  find,
  n2s,
  str_cat,
  arr_cat,
} from "./string_prims.deno.js";

assert.equal(ends_with2("foo.json", ".json"), true, "string_sub/len ends_with");
assert.equal(ends_with2("foo.txt", ".json"), false, "negative ends_with");
assert.equal(ends_with2("a", ".json"), false, "suffix longer than string");
assert.equal(strip_suffix("foo.json", ".json"), "foo", "strip_suffix");
assert.equal(strip_suffix("foo", ".json"), "foo", "strip_suffix no-op");
assert.equal(find("ab.cd", "."), 2, "string_find");
assert.equal(n2s(42), "42", "int_to_string");
assert.equal(str_cat("a", "b"), "ab", "`++` string concat");
assert.deepEqual(arr_cat([1, 2]), [1, 2, 99], "`++` array concat (not '1,299')");

console.log("string_prims.harness.mjs OK");
