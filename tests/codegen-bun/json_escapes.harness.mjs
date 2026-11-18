// SPDX-License-Identifier: MPL-2.0
import * as subject from "./json_escapes.bun.js";

const expect = (label, got, want) => {
  const g = JSON.stringify(got), w = JSON.stringify(want);
  if (g !== w) throw new Error(`${label}: got ${g}, want ${w}`);
};
for (const v of ["q\"uote", "back\\slash", "nl\nx", "cr\rtab\t", "bs\bff\f", "c\u0001\u001f", "é  ", ""]) {
  const out = subject.quote(v);
  let back;
  try { back = JSON.parse(out); } catch (e) { throw new Error(`invalid JSON for ${JSON.stringify(v)}: ${out}`); }
  expect(`escapes ${JSON.stringify(v)}`, back, [v]);
  expect(`array round-trip ${JSON.stringify(v)}`, JSON.parse(subject.roundtrip(JSON.stringify([v, [v]]))), [v, [v]]);
}
expect("non-finite element", subject.roundtrip("[1e400]"), "<none>");
expect("finite exponent", subject.roundtrip("[1e3]"), "[1000]");
expect("objects need key enumeration, not on this branch", subject.roundtrip('{"a":1}'), "<none>");
console.log("json_escapes: ok");
