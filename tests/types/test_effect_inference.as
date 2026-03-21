// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test effect inference and polymorphism

// Pure function - should infer EPure effect
fn pure_add(x: Int, y: Int) -> Int {
  return x + y;
}

// Another pure function
fn pure_mul(x: Int, y: Int) -> Int {
  return x * y;
}

// Function that calls pure functions - should also be pure
fn compound_pure(x: Int) -> Int {
  let a = pure_add(x, 10);
  let b = pure_mul(a, 2);
  return b;
}

// Main function that uses multiple pure functions
fn main() -> Int {
  let x = pure_add(5, 10);
  let y = compound_pure(x);
  return y;
}
