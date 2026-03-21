// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: pattern matching
// Tests: literal patterns, constructor patterns, tuple destructuring

enum Option[T] {
  None,
  Some(T)
}

fn unwrap_or(opt: Option[Int], default: Int) -> Int {
  match opt {
    Some(x) => x,
    None => default
  };
}

fn fst(a: Int, b: Int) -> Int = a;

fn classify(n: Int) -> Int {
  match n {
    0 => 1,
    1 => 2,
    _ => 3
  };
}
