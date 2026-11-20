// SPDX-License-Identifier: MPL-2.0
import assert from "node:assert/strict";
import * as scope from "./int_div_scope.bun.js";
assert.equal(scope.declared_half(), 3, "declared Int call truncates");
assert.equal(scope.builtin_named_fraction(), 3.5, "Float function named len stays Float");
assert.equal(scope.lexical_call(), 3.5, "lexical binding shadows declared Int function");
console.log("int_div_scope.harness.mjs OK");
