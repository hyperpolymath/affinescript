// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Math - Mathematical functions

module Math;

// Constants (as functions since const declarations not yet supported)
pub fn pi() -> Float {
  return 3.141592653589793;
}

pub fn e() -> Float {
  return 2.718281828459045;
}

pub fn tau() -> Float {
  return 6.283185307179586;
}

// Integer operations

pub fn abs(x: Int) -> Int {
  return if x < 0 { return 0 - x; } else { return x; };
}

pub fn min(a: Int, b: Int) -> Int {
  return if a < b { return a; } else { return b; };
}

pub fn max(a: Int, b: Int) -> Int {
  return if a > b { return a; } else { return b; };
}

pub fn clamp(x: Int, low: Int, high: Int) -> Int {
  return max(low, min(x, high));
}

// Power (integer exponentiation)
pub fn pow(base: Int, exp: Int) -> Int {
  return if exp == 0 {
    return 1;
  } else {
    return if exp == 1 {
      return base;
    } else {
      let half = pow(base, exp / 2);
      return if exp % 2 == 0 {
        return half * half;
      } else {
        return base * half * half;
      };
    };
  };
}

// GCD using Euclid's algorithm
pub fn gcd(a: Int, b: Int) -> Int {
  let a_abs = abs(a);
  let b_abs = abs(b);
  return if b_abs == 0 {
    return a_abs;
  } else {
    return gcd(b_abs, a_abs % b_abs);
  };
}

// LCM
pub fn lcm(a: Int, b: Int) -> Int {
  return if a == 0 {
    return 0;
  } else {
    return if b == 0 {
      return 0;
    } else {
      return abs(a * b) / gcd(a, b);
    };
  };
}

// Factorial
pub fn factorial(n: Int) -> Int {
  return if n <= 1 {
    return 1;
  } else {
    return n * factorial(n - 1);
  };
}

// Fibonacci
pub fn fib(n: Int) -> Int {
  return if n <= 1 {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  };
}

// Check if number is even
pub fn is_even(n: Int) -> Bool {
  return n % 2 == 0;
}

// Check if number is odd
pub fn is_odd(n: Int) -> Bool {
  return n % 2 != 0;
}

// Note: Float operations and transcendental functions (sin, cos, sqrt, etc.)
// would require FFI or builtin implementation
// Currently disabled due to type checker limitations with Float comparisons
