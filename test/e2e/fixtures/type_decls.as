// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: type declarations
// Tests: aliases, structs, enums, parameterised types

type Alias = Int;

struct Point {
  x: Int,
  y: Int
}

enum Color {
  Red,
  Green,
  Blue,
  Custom(Int, Int, Int)
}

enum Option[T] {
  None,
  Some(T)
}

enum Result[T, E] {
  Ok(T),
  Err(E)
}

fn make_point(x: Int, y: Int) -> Point {
  Point { x: x, y: y };
}

fn origin() -> Point {
  Point { x: 0, y: 0 };
}
