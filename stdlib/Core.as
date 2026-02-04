// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Core - Basic utilities and operations

module Core;

// Identity function
pub fn id[T](x: T) -> T {
  return x;
}

// Constant function
pub fn const[A, B](x: A, own y: B) -> A {
  return x;
}

// TODO: Function composition and flip require lambda syntax
// which is not yet fully supported by the parser

// Comparison operators

pub fn min(a: Int, b: Int) -> Int {
  return if a < b { return a; } else { return b; };
}

pub fn max(a: Int, b: Int) -> Int {
  return if a > b { return a; } else { return b; };
}

pub fn clamp(x: Int, low: Int, high: Int) -> Int {
  return max(low, min(x, high));
}

// Absolute value
pub fn abs(x: Int) -> Int {
  return if x < 0 { return 0 - x; } else { return x; };
}

// Sign function
pub fn sign(x: Int) -> Int {
  return if x < 0 {
    return 0 - 1;
  } else {
    return if x > 0 { return 1; } else { return 0; };
  };
}

// Boolean operations

pub fn not(x: Bool) -> Bool {
  return if x { return false; } else { return true; };
}

pub fn and(a: Bool, b: Bool) -> Bool {
  return if a { return b; } else { return false; };
}

pub fn or(a: Bool, b: Bool) -> Bool {
  return if a { return true; } else { return b; };
}

pub fn xor(a: Bool, b: Bool) -> Bool {
  return if a { return not(b); } else { return b; };
}
