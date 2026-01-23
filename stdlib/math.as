// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// AffineScript Standard Library - Mathematics

// ============================================================================
// Constants
// ============================================================================

const PI: Float = 3.141592653589793;
const E: Float = 2.718281828459045;
const TAU: Float = 6.283185307179586;  // 2 * PI

// ============================================================================
// Basic arithmetic
// ============================================================================

fn abs(x: Int) -> Int {
  if x < 0 { -x } else { x }
}

fn abs_float(x: Float) -> Float {
  if x < 0.0 { -x } else { x }
}

fn sign(x: Int) -> Int {
  if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
}

fn sign_float(x: Float) -> Int {
  if x > 0.0 { 1 } else if x < 0.0 { -1 } else { 0 }
}

// ============================================================================
// Power and roots
// ============================================================================

fn pow(base: Int, exp: Int) -> Int {
  if exp == 0 {
    return 1;
  }

  let result = 1;
  let i = 0;
  while i < exp {
    result = result * base;
    i = i + 1;
  }
  result
}

fn square(x: Int) -> Int {
  x * x
}

fn cube(x: Int) -> Int {
  x * x * x
}

// TODO: These require builtin implementations
// fn sqrt(x: Float) -> Float
// fn cbrt(x: Float) -> Float
// fn pow_float(base: Float, exp: Float) -> Float

// ============================================================================
// Rounding and truncation
// ============================================================================

// TODO: These require builtin implementations
// fn floor(x: Float) -> Int
// fn ceil(x: Float) -> Int
// fn round(x: Float) -> Int
// fn trunc(x: Float) -> Int

// ============================================================================
// Trigonometry
// ============================================================================

// TODO: These require builtin implementations
// fn sin(x: Float) -> Float
// fn cos(x: Float) -> Float
// fn tan(x: Float) -> Float
// fn asin(x: Float) -> Float
// fn acos(x: Float) -> Float
// fn atan(x: Float) -> Float
// fn atan2(y: Float, x: Float) -> Float

// ============================================================================
// Logarithms and exponentials
// ============================================================================

// TODO: These require builtin implementations
// fn exp(x: Float) -> Float
// fn log(x: Float) -> Float      // Natural logarithm
// fn log10(x: Float) -> Float
// fn log2(x: Float) -> Float

// ============================================================================
// Comparison
// ============================================================================

fn min_int(a: Int, b: Int) -> Int {
  if a < b { a } else { b }
}

fn max_int(a: Int, b: Int) -> Int {
  if a > b { a } else { b }
}

fn min_float(a: Float, b: Float) -> Float {
  if a < b { a } else { b }
}

fn max_float(a: Float, b: Float) -> Float {
  if a > b { a } else { b }
}

fn clamp_int(value: Int, min_val: Int, max_val: Int) -> Int {
  if value < min_val {
    min_val
  } else if value > max_val {
    max_val
  } else {
    value
  }
}

fn clamp_float(value: Float, min_val: Float, max_val: Float) -> Float {
  if value < min_val {
    min_val
  } else if value > max_val {
    max_val
  } else {
    value
  }
}

// ============================================================================
// Number theory
// ============================================================================

fn gcd(a: Int, b: Int) -> Int {
  let x = abs(a);
  let y = abs(b);

  while y != 0 {
    let temp = y;
    y = x % y;
    x = temp;
  }

  x
}

fn lcm(a: Int, b: Int) -> Int {
  if a == 0 || b == 0 {
    return 0;
  }
  abs(a * b) / gcd(a, b)
}

fn is_even(n: Int) -> Bool {
  n % 2 == 0
}

fn is_odd(n: Int) -> Bool {
  n % 2 != 0
}

// ============================================================================
// Sequences
// ============================================================================

fn factorial(n: Int) -> Int {
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
}

fn fibonacci(n: Int) -> Int {
  if n <= 1 {
    n
  } else {
    let a = 0;
    let b = 1;
    let i = 2;
    while i <= n {
      let temp = a + b;
      a = b;
      b = temp;
      i = i + 1;
    }
    b
  }
}

fn sum_naturals(n: Int) -> Int {
  n * (n + 1) / 2
}

fn sum_squares(n: Int) -> Int {
  n * (n + 1) * (2 * n + 1) / 6
}
