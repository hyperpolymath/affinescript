// SPDX-License-Identifier: MPL-2.0
// Run by tools/run_codegen_bun_tests.sh step 9 (not by the step-6 loop, which
// only reads top-level harnesses).
import * as subject from "./main.bun.js";

const got = JSON.stringify(subject.both());
if (got !== "[2,100]") throw new Error(`both(): got ${got}, want [2,100]`);
console.log("collision/both: ok");
