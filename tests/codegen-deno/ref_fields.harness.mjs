// SPDX-License-Identifier: MPL-2.0
// issue #122 v2.3 — field access through ref/own/mut receivers.
// mk_point returns Point (constructor); sum_ref/get_x_own/get_y_mut
// take Point first (ref/own/mut) so they synthesise as Point methods —
// exercising auto-deref field access *and* class synthesis together.
import assert from "node:assert/strict";
import { Point } from "./ref_fields.deno.js";

const p = new Point(3, 4);
assert.equal(p.x, 3, "constructor assigns x");
assert.equal(p.y, 4, "constructor assigns y");
assert.equal(await p.sum_ref(), 7, "field access through ref receiver");
assert.equal(await p.get_x_own(), 3, "field access through own receiver");
assert.equal(await p.get_y_mut(), 4, "field access through mut receiver");

console.log("ref_fields.harness.mjs OK");
