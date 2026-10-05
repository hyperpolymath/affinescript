// SPDX-License-Identifier: MPL-2.0
// Enum bindings are emitted before top-level consts (no TDZ ReferenceError).
import assert from "node:assert/strict";
import { DEFAULT, START, describe } from "./const_before_enum.bun.js";

assert.equal(describe(DEFAULT), "fast");
assert.equal(describe(START), "slow 3");
console.log("const_before_enum.harness.mjs OK");
