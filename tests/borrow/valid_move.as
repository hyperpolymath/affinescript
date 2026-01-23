// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Borrow checker test: Valid move should pass

fn consume(own x: Int) -> Int {
  return x;
}

fn test_valid_move() -> Int {
  let value = 42;
  let result = consume(value);
  return result;
}
