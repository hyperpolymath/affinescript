// SPDX-License-Identifier: MPL-2.0
import * as subject from "./result_payload.bun.js";

const expect = (label, got, want) => {
  if (got !== want) throw new Error(`${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);
};
expect("Err payload binds", subject.err_message("x"), "bad: x");
expect("Ok arm still binds", subject.ok_value("ok"), 1);
expect("Err payload in a nested pattern", subject.nested_err(), "inner");
console.log("result_payload: ok");
