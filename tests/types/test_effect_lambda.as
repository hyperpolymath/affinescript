// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test effect inference with lambdas

fn main() -> Int {
  // Lambda with pure body
  let add_ten = |x: Int| -> Int {
    return x + 10;
  };

  // Lambda with multiple operations
  let compute = |x: Int| -> Int {
    let a = x * 2;
    let b = a + 5;
    return b;
  };

  let result1 = add_ten(5);
  let result2 = compute(10);

  return result1 + result2;
}
