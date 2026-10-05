// SPDX-License-Identifier: MPL-2.0
// Transitively imported functions are emitted.
import assert from "node:assert/strict";
import { both } from "./chain_import.bun.js";

assert.equal(both(), "it is north;step 2");
console.log("chain_import.harness.mjs OK");
