// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test effect polymorphism with higher-order functions

// Higher-order function that applies a function twice
// Effect should be polymorphic - works with any effect
fn apply_twice(f: Int -> Int, x: Int) -> Int {
  let y = f(x);
  return f(y);
}

// Pure function
fn add_one(x: Int) -> Int {
  return x + 1;
}

// Another pure function
fn double(x: Int) -> Int {
  return x * 2;
}

fn main() -> Int {
  // Apply pure functions multiple times
  let a = apply_twice(add_one, 10);    // 10 + 1 + 1 = 12
  let b = apply_twice(double, 5);      // (5 * 2) * 2 = 20
  return a + b;                         // 12 + 20 = 32
}
