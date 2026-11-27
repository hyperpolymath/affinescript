// SPDX-License-Identifier: MPL-2.0
// #818: Node-ESM harness for externs used as values.

import assert from "node:assert/strict";

const { digest_via_value, b64_via_value, digest_via_record } = await import("./extern_as_value.bun.js");

assert.equal(digest_via_value([]).length, 32);
assert.equal(b64_via_value([0xfb, 0xff, 0xfe]), "-__-");
assert.deepEqual(digest_via_record([1, 2]), digest_via_value([1, 2]));
// Same byte validation as a direct call.
assert.throws(() => digest_via_value([256]), RangeError);

console.log("extern_as_value: ok");
