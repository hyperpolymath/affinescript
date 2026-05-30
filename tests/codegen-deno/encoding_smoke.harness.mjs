// SPDX-License-Identifier: MPL-2.0
// Node ESM harness for stdlib #25 (hex) — asserts the Deno-ESM codegen of
// the integer bit-op + string-primitive hex conversion round-trips.
import assert from "node:assert/strict";
import {
  hex_digit,
  to_hex,
  to_hex_padded,
  hex_value,
  from_hex,
} from "./encoding_smoke.deno.js";

// encode
assert.equal(to_hex(0), "0", "to_hex(0)");
assert.equal(to_hex(15), "f", "to_hex(15)");
assert.equal(to_hex(16), "10", "to_hex(16)");
assert.equal(to_hex(255), "ff", "to_hex(255)");
assert.equal(to_hex(2748), "abc", "to_hex(0xabc)");
assert.equal(to_hex(4096), "1000", "to_hex(4096)");
assert.equal(hex_digit(10), "a", "hex_digit(10)");
assert.equal(hex_digit(15), "f", "hex_digit(15)");

// zero-padding
assert.equal(to_hex_padded(255, 4), "00ff", "to_hex_padded(255,4)");
assert.equal(to_hex_padded(0, 2), "00", "to_hex_padded(0,2)");
assert.equal(to_hex_padded(4096, 2), "1000", "padding never truncates");

// decode (case-insensitive)
assert.equal(from_hex("ff"), 255, "from_hex(ff)");
assert.equal(from_hex("FF"), 255, "from_hex(FF) case-insensitive");
assert.equal(from_hex("1000"), 4096, "from_hex(1000)");
assert.equal(from_hex("abc"), 2748, "from_hex(abc)");
assert.equal(from_hex("0"), 0, "from_hex(0)");
assert.equal(hex_value("a"), 10, "hex_value(a)");
assert.equal(hex_value("F"), 15, "hex_value(F)");
assert.equal(hex_value("z"), -1, "hex_value(invalid)");

// round-trip
for (const n of [0, 1, 15, 16, 255, 4096, 65535, 1193046]) {
  assert.equal(from_hex(to_hex(n)), n, `round-trip ${n}`);
}

console.log("encoding_smoke.harness.mjs OK");
