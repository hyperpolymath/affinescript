// SPDX-License-Identifier: MPL-2.0
import * as subject from "./diamond_internalised.bun.js";

const expect = (label, got, want) => {
  if (got !== want) throw new Error(`${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);
};
expect("small", subject.both(3), 12);
expect("big", subject.both(30), 24);
console.log("diamond_internalised: ok");
