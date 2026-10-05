// SPDX-License-Identifier: MPL-2.0
// Importing the module at all proves it parses under V8 (Node).
import assert from "node:assert/strict";
import { make, describe } from "./method_lambda.bun.js";

assert.deepEqual(describe(make()), ["small", "b", "b"]);
console.log("method_lambda.harness.mjs OK");
