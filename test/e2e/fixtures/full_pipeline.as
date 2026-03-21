// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: full pipeline integration
// Tests: a non-trivial program through all compiler stages

struct Vec2 {
  x: Int,
  y: Int
}

enum Shape {
  Circle(Int),
  Rect(Vec2)
}

fn vec2_add(a: Vec2, b: Vec2) -> Vec2 {
  Vec2 { x: a.x + b.x, y: a.y + b.y };
}

fn area(s: Shape) -> Int {
  match s {
    Circle(r) => r * r * 3,
    Rect(size) => size.x * size.y
  };
}

fn max(a: Int, b: Int) -> Int {
  if a > b { a; } else { b; };
}

fn larger_area(s1: Shape, s2: Shape) -> Int = max(area(s1), area(s2));

fn main() -> () {
  let circle = Circle(5);
  let rect = Rect(Vec2 { x: 3, y: 4 });
  let result = larger_area(circle, rect);
  ();
}
