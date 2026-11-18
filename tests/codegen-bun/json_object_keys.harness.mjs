// SPDX-License-Identifier: MPL-2.0
import * as subject from "./json_object_keys.bun.js";

const expect = (label, got, want) => {
  const g = JSON.stringify(got), w = JSON.stringify(want);
  if (g !== w) throw new Error(`${label}: got ${g}, want ${w}`);
};
expect("flat object keys in order", subject.keys('{"b":1,"a":2,"c":3}'), ["b", "a", "c"]);
expect("nested object round-trip", subject.roundtrip('{"p":{"q":[1,{"r":null}],"s":"t"},"u":true}'),
  '{"p":{"q":[1,{"r":null}],"s":"t"},"u":true}');
expect("empty object", subject.roundtrip("{}"), "{}");
expect("escaped key", subject.roundtrip('{"a\\"b":1}'), '{"a\\"b":1}');
expect("length of object", subject.len_of('{"x":1,"y":2}'), 2);
expect("length of array is 0", subject.len_of("[1,2]"), 0);
expect("length of scalar is 0", subject.len_of("5"), 0);
expect("key out of range", subject.key_at('{"x":1}', 1), "<none>");
expect("negative index", subject.key_at('{"x":1}', -1), "<none>");
expect("key of non-object", subject.key_at("[1]", 0), "<none>");
expect("own keys only", subject.keys('{"__proto__":1,"constructor":2}'), ["__proto__", "constructor"]);
// stringify must emit valid JSON that reparses to the same value (escape_char
// used to return bare quote, backslash and control characters).
for (const v of ["q\"uote", "back\\slash", "nl\nx", "cr\rtab\t", "bs\bff\f", "c\u0001\u001f", "\u00e9 \u2028", ""]) {
  const src = JSON.stringify({ k: v, [v]: [v] });
  const out = subject.roundtrip(src);
  let back;
  try { back = JSON.parse(out); } catch (e) { throw new Error(`invalid JSON for ${JSON.stringify(v)}: ${out}`); }
  expect(`string escapes ${JSON.stringify(v)}`, back, JSON.parse(src));
}
console.log("json_object_keys: ok");
