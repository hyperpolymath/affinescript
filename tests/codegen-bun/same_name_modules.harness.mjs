// SPDX-License-Identifier: MPL-2.0
// #801: each module's code calls its own `label` and `helper`.
import * as subject from "./same_name_modules.bun.js";

const want = "from A|from B|A helper|B helper|from A|from B";
const got = subject.labels();
if (got !== want) {
  throw new Error(`same_name_modules: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);
}
console.log("same_name_modules: ok");
