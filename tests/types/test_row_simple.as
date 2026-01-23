// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Simplest possible row polymorphism test

fn get_x(r: {x: Int, ..rest}) -> Int {
  return r.x;
}

fn main() -> Int {
  let r = {x: 42, y: 100};
  return get_x(r);
}
