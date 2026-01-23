// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// Test exception handling with try/catch/finally

fn safe_divide(a: Int, b: Int) -> Int {
  try {
    a / b
  } catch {
    DivisionByZero => -1
  }
}

fn test_with_finally() -> Int {
  try {
    42
  } finally {
    println("Finally executed")
  }
}

fn test_pattern_match() -> Int {
  try {
    let arr = [1, 2, 3];
    arr[10]
  } catch {
    IndexOutOfBounds(idx, len) => {
      println("Caught index out of bounds");
      -1
    }
  }
}
