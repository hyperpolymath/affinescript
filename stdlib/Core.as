// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Core - Basic utilities and operations

module Core;

// Identity function
pub fn id[T](x: T) -> T {
  return x;
}

// Constant function
pub fn const[A, B](x: A, own _y: B) -> A {
  return x;
}

// Function composition
pub fn compose[A, B, C](f: fn(B) -> C, g: fn(A) -> B) -> fn(A) -> C {
  return |x| { return f(g(x)); };
}

// Flip arguments
pub fn flip[A, B, C](f: fn(A, B) -> C) -> fn(B, A) -> C {
  return |y, x| { return f(x, y); };
}

// Comparison operators

pub fn min(a: Int, b: Int) -> Int {
  return if a < b { a } else { b };
}

pub fn max(a: Int, b: Int) -> Int {
  return if a > b { a } else { b };
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
