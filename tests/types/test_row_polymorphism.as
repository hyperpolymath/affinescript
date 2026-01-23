// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test row polymorphism with extensible records

// Function that works on any record with an 'x' field
// The ..rest syntax allows extra fields
fn get_x(r: {x: Int, ..rest}) -> Int {
  return r.x;
}

// Function that adds to the x field
fn add_to_x(r: {x: Int, ..rest}, n: Int) -> Int {
  return r.x + n;
}

fn main() -> Int {
  // These should all work due to row polymorphism
  let r1 = {x: 10};
  let r2 = {x: 20, y: 30};
  let r3 = {x: 5, y: 10, z: 15};

  let a = get_x(r1);          // 10
  let b = get_x(r2);          // 20
  let c = get_x(r3);          // 5

  let d = add_to_x(r2, 5);    // 25

  return a + b + c + d;       // 10 + 20 + 5 + 25 = 60
}
