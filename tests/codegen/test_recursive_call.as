// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test recursive function calls

fn factorial(n: Int) -> Int {
  return if n <= 1 {
    return 1;
  } else {
    return n * factorial(n - 1);
  };
}

fn main() -> Int {
  return factorial(5);  // Should return 120
}
