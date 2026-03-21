// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test multiple function calls and composition

fn add(a: Int, b: Int) -> Int {
  return a + b;
}

fn multiply(a: Int, b: Int) -> Int {
  return a * b;
}

fn square(x: Int) -> Int {
  return multiply(x, x);
}

fn main() -> Int {
  let a = add(10, 5);       // 15
  let b = square(3);        // 9
  let c = multiply(a, b);   // 135
  return c;
}
