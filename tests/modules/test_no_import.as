// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test without imports - just local functions

fn min(a: Int, b: Int) -> Int {
  return if a < b { return a; } else { return b; };
}

fn main() -> Int {
  let a = min(10, 20);
  return a;
}
