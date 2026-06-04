// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// driver.mjs -- the Deno-side consumer for the deno-esm-spike. Imports the
// AffineScript-compiled ES module (sample.js) and calls its exports, proving
// the TypeScript-0 path (roadmap ask #3): a parity / scan harness like this
// could be authored in AffineScript-compiled JS instead of hand-written TS.
//
// Run: deno run ./driver.mjs

import { add, clamp_band } from "./sample.js";

let pass = 0;
let fail = 0;

function check(label, got, want) {
  const ok = got === want;
  if (ok) {
    pass += 1;
    console.log(`  ok   ${label}: ${got}`);
  } else {
    fail += 1;
    console.log(`  FAIL ${label}: got ${got}, want ${want}`);
  }
}

console.log("deno-esm-spike driver: importing AffineScript-compiled ESM");
console.log(`  typeof add        = ${typeof add}`);
console.log(`  typeof clamp_band = ${typeof clamp_band}`);

check("add(2, 3)", add(2, 3), 5);
check("add(-4, 4)", add(-4, 4), 0);
check("clamp_band(-7) [below band]", clamp_band(-7), 0);
check("clamp_band(2)  [in band]", clamp_band(2), 2);
check("clamp_band(99) [above band]", clamp_band(99), 3);

console.log(`\nresult: ${pass} passed, ${fail} failed`);
if (fail > 0) {
  Deno.exit(1);
}
