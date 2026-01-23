// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Borrow checker test: Use after move should fail

fn consume(own x: Int) -> Int {
  return x;
}

fn test_use_after_move() -> Int {
  let value = 42;
  let result = consume(value);
  let invalid = value;
  return result;
}
