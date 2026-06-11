// SPDX-License-Identifier: MPL-2.0
// #470 — verify the three previously-missing Deno externs now resolve.

import assert from "node:assert/strict";

const {
  is_ts,
  drop_ts_suffix,
  drop_missing_suffix,
  fmt_pct,
} = await import("./deno_string_fmt.deno.js");

// endsWith
assert.equal(is_ts("foo.ts"),   true,  "endsWith hit");
assert.equal(is_ts("foo.tsx"),  false, "endsWith miss");
assert.equal(is_ts(""),         false, "endsWith empty");

// stripSuffix — present
assert.equal(drop_ts_suffix("foo.ts"), "foo",   "stripSuffix removes matching suffix");
assert.equal(drop_ts_suffix(".ts"),    "",      "stripSuffix down to empty");

// stripSuffix — absent (must return input unchanged, not throw)
assert.equal(drop_missing_suffix("foo.ts"), "foo.ts", "stripSuffix passthrough when suffix absent");

// numToFixed2
assert.equal(fmt_pct(0),    "0.00",   "numToFixed2 of 0");
assert.equal(fmt_pct(42),   "42.00",  "numToFixed2 of int");

console.log("deno_string_fmt.harness.mjs OK");
