// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: trait system
// Tests: trait declarations, supertraits, impl blocks, method signatures

trait Eq {
  fn eq(a: Int, b: Int) -> Bool;
}

trait Ord: Eq {
  fn cmp(a: Int, b: Int) -> Int;
}

trait Show {
  fn show(x: Int) -> String;
}

impl Eq for Int {
  fn eq(a: Int, b: Int) -> Bool = true;
}

impl Show for Int {
  fn show(x: Int) -> String = "int";
}

struct Point {
  x: Int,
  y: Int
}

impl Eq for Point {
  fn eq(a: Int, b: Int) -> Bool = true;
}

fn generic_id[T](v: T) -> T = v;
