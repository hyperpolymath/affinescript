// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Test basic function calls in WASM codegen

fn helper(x: Int) -> Int {
  return x + 1;
}

fn main() -> Int {
  return helper(41);  // Should return 42
}
