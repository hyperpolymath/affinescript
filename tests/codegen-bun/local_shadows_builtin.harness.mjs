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
expect("intrinsic visible after the lambda", subject.lambda_then_intrinsic(" a "), " a !t");
expect("later arm calls the intrinsic", subject.later_arm_intrinsic({ tag: "None" }), "b");
expect("earlier arm binds", subject.later_arm_intrinsic({ tag: "Some", value: (x) => x + "!" }), "a!");
expect("guard reads its binder", subject.stmt_guard(9), "big");
expect("false guard falls through", subject.stmt_guard(2), "small:2");
expect("no match", subject.stmt_guard(0), "none");
expect("for evaluates its iterable first", subject.for_shadows_callee(), "xy");
console.log("local_shadows_builtin: ok");
