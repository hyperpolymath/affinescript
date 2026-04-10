// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: record types (row polymorphism building blocks)
// Tests: record type syntax, struct declarations

struct Point2D {
  x: Int,
  y: Int
}

struct Point3D {
  x: Int,
  y: Int,
  z: Int
}

fn getX(p: Point2D) -> Int = p.x;

fn mk_point(x: Int, y: Int) -> Point2D {
  Point2D { x: x, y: y };
}
