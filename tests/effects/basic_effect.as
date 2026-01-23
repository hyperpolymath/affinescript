// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test basic effect handler

effect Ask {
  fn get_value() -> Int;
}

fn main() -> Int {
  let x = handle get_value() {
    get_value() => {
      return 42;
    }
  };
  return x;
}
