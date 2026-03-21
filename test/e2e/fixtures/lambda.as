// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: lambda expressions and higher-order functions
// Tests: closures, lambda syntax, function application

fn apply(f: (Int) -> Int, x: Int) -> Int = f(x);

fn test_lambda() -> Int {
  let f = |x| x + 1;
  f(41);
}

fn test_typed_lambda() -> Int {
  let double = |x: Int| x * 2;
  double(21);
}

fn test_hof() -> Int {
  apply(|x| x * x, 5);
}
