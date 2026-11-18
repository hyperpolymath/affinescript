// SPDX-License-Identifier: MPL-2.0
import * as subject from "./local_shadows_builtin.bun.js";

const expect = (label, got, want) => {
  if (got !== want) throw new Error(`${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);
};
expect("parameter shadows `get`",
  subject.call_param((k) => ({ tag: "Some", value: "got " + k })), "got x");
expect("`let len = len(s)` calls the intrinsic, then the local", subject.let_after_init("abc"), 4);
expect("lambda parameter shadows `trim`", subject.lambda_param(" a "), " a !");
expect("loop and arm binders end with their scope",
  subject.scope_ends(" s ", [(x) => x + "1", (x) => x + "2"]), " s 1 s 2[ s ]t");
expect("block `let` ends with its block", subject.let_scope_ends("v"), "v?u");
console.log("local_shadows_builtin: ok");
