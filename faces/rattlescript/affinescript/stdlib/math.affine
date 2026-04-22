// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// AffineScript Standard Library - Mathematics
//
// Builtin functions (implemented in interpreter runtime):
//   sqrt(x: Float) -> Float
//   cbrt(x: Float) -> Float
//   pow_float(base: Float, exp: Float) -> Float
//   floor(x: Float) -> Int
//   ceil(x: Float) -> Int
//   round(x: Float) -> Int
//   trunc(x: Float) -> Int
//   sin(x: Float) -> Float
//   cos(x: Float) -> Float
//   tan(x: Float) -> Float
//   asin(x: Float) -> Float
//   acos(x: Float) -> Float
//   atan(x: Float) -> Float
//   atan2(y: Float, x: Float) -> Float
//   exp(x: Float) -> Float
//   log(x: Float) -> Float       (natural logarithm)
//   log10(x: Float) -> Float
//   log2(x: Float) -> Float

// ============================================================================
// Constants
// ============================================================================

/// Ratio of a circle's circumference to its diameter
const PI: Float = 3.141592653589793;

/// Euler's number, base of the natural logarithm
const E: Float = 2.718281828459045;

/// Full-turn constant (2 * PI)
const TAU: Float = 6.283185307179586;

/// Positive infinity sentinel (largest representable float)
const INFINITY: Float = 1.0 / 0.0;

/// Negative infinity sentinel
const NEG_INFINITY: Float = -1.0 / 0.0;

// ============================================================================
// Basic arithmetic
// ============================================================================

/// Absolute value of an integer
fn abs(x: Int) -> Int {
  if x < 0 { -x } else { x }
}

/// Absolute value of a float
fn abs_float(x: Float) -> Float {
  if x < 0.0 { -x } else { x }
}

/// Sign of an integer: -1, 0, or 1
fn sign(x: Int) -> Int {
  if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
}

/// Sign of a float: -1, 0, or 1
fn sign_float(x: Float) -> Int {
  if x > 0.0 { 1 } else if x < 0.0 { -1 } else { 0 }
}

// ============================================================================
// Power and roots
// ============================================================================

/// Integer exponentiation via repeated squaring
fn pow(base: Int, exp: Int) -> Int {
  if exp == 0 {
    return 1;
  }
  if exp == 1 {
    return base;
  }
  let half = pow(base, exp / 2);
  if exp % 2 == 0 {
    half * half
  } else {
    base * half * half
  }
}

/// Square of an integer
fn square(x: Int) -> Int {
  x * x
}

/// Cube of an integer
fn cube(x: Int) -> Int {
  x * x * x
}

// sqrt, cbrt, pow_float are builtins — see module header

// ============================================================================
// Rounding and truncation (builtins)
// ============================================================================

// floor, ceil, round, trunc are builtins — see module header

/// Convert an integer to a float
fn to_float(n: Int) -> Float {
  n + 0.0
}

/// Fractional part of a float (x - trunc(x))
fn fract(x: Float) -> Float {
  x - to_float(trunc(x))
}

// ============================================================================
// Trigonometry (builtins)
// ============================================================================

// sin, cos, tan, asin, acos, atan, atan2 are builtins — see module header

/// Convert degrees to radians
fn deg_to_rad(degrees: Float) -> Float {
  degrees * PI / 180.0
}

/// Convert radians to degrees
fn rad_to_deg(radians: Float) -> Float {
  radians * 180.0 / PI
}

/// Hyperbolic sine
fn sinh(x: Float) -> Float {
  (exp(x) - exp(-x)) / 2.0
}

/// Hyperbolic cosine
fn cosh(x: Float) -> Float {
  (exp(x) + exp(-x)) / 2.0
}

/// Hyperbolic tangent
fn tanh(x: Float) -> Float {
  sinh(x) / cosh(x)
}

// ============================================================================
// Logarithms and exponentials (builtins)
// ============================================================================

// exp, log, log10, log2 are builtins — see module header

/// Logarithm with arbitrary base
fn log_base(base: Float, x: Float) -> Float {
  log(x) / log(base)
}

// ============================================================================
// Comparison
// ============================================================================

/// Minimum of two integers
fn min_int(a: Int, b: Int) -> Int {
  if a < b { a } else { b }
}

/// Maximum of two integers
fn max_int(a: Int, b: Int) -> Int {
  if a > b { a } else { b }
}

/// Minimum of two floats
fn min_float(a: Float, b: Float) -> Float {
  if a < b { a } else { b }
}

/// Maximum of two floats
fn max_float(a: Float, b: Float) -> Float {
  if a > b { a } else { b }
}

/// Clamp an integer between min_val and max_val (inclusive)
fn clamp_int(value: Int, min_val: Int, max_val: Int) -> Int {
  if value < min_val {
    min_val
  } else if value > max_val {
    max_val
  } else {
    value
  }
}

/// Clamp a float between min_val and max_val (inclusive)
fn clamp_float(value: Float, min_val: Float, max_val: Float) -> Float {
  if value < min_val {
    min_val
  } else if value > max_val {
    max_val
  } else {
    value
  }
}

/// Linear interpolation between a and b by factor t (0.0 to 1.0)
fn lerp(a: Float, b: Float, t: Float) -> Float {
  a + (b - a) * t
}

// ============================================================================
// Number theory
// ============================================================================

/// Greatest common divisor via Euclid's algorithm
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

/// Least common multiple
fn lcm(a: Int, b: Int) -> Int {
  if a == 0 || b == 0 {
    return 0;
  }
  abs(a * b) / gcd(a, b)
}

/// Check if n is even
fn is_even(n: Int) -> Bool {
  n % 2 == 0
}

/// Check if n is odd
fn is_odd(n: Int) -> Bool {
  n % 2 != 0
}

/// Check if n is a prime number (trial division)
fn is_prime(n: Int) -> Bool {
  if n < 2 {
    return false;
  }
  if n < 4 {
    return true;
  }
  if n % 2 == 0 || n % 3 == 0 {
    return false;
  }
  let i = 5;
  while i * i <= n {
    if n % i == 0 || n % (i + 2) == 0 {
      return false;
    }
    i = i + 6;
  }
  true
}

/// Integer division rounding towards negative infinity (floor division)
fn div_floor(a: Int, b: Int) -> Int {
  let q = a / b;
  if (a % b != 0) && ((a < 0) != (b < 0)) {
    q - 1
  } else {
    q
  }
}

/// Modulo that always returns a non-negative result
fn mod_positive(a: Int, b: Int) -> Int {
  let r = a % b;
  if r < 0 {
    r + abs(b)
  } else {
    r
  }
}

// ============================================================================
// Sequences
// ============================================================================

/// Factorial of n (n!)
fn factorial(n: Int) -> Int {
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
}

/// n-th Fibonacci number (iterative)
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

/// Sum of first n natural numbers: 1 + 2 + ... + n
fn sum_naturals(n: Int) -> Int {
  n * (n + 1) / 2
}

/// Sum of first n squares: 1^2 + 2^2 + ... + n^2
fn sum_squares(n: Int) -> Int {
  n * (n + 1) * (2 * n + 1) / 6
}

/// Binomial coefficient C(n, k) = n! / (k! * (n-k)!)
fn binomial(n: Int, k: Int) -> Int {
  if k < 0 || k > n {
    return 0;
  }
  // Use the smaller of k and n-k for efficiency
  let k_eff = if k > n - k { n - k } else { k };
  let result = 1;
  let i = 0;
  while i < k_eff {
    result = result * (n - i) / (i + 1);
    i = i + 1;
  }
  result
}

// ============================================================================
// Statistics helpers
// ============================================================================

/// Arithmetic mean of a list of floats
fn mean(values: [Float]) -> Float {
  let n = len(values);
  if n == 0 {
    return 0.0;
  }
  let total = 0.0;
  for v in values {
    total = total + v;
  }
  total / to_float(n)
}

/// Sum of a list of floats
fn sum_float(values: [Float]) -> Float {
  let total = 0.0;
  for v in values {
    total = total + v;
  }
  total
}
