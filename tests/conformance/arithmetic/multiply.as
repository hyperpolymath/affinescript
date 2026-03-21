// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: arithmetic/multiply
// Source: aggregate-library/specs/arithmetic/multiply.md
//
// Conforms to aLib arithmetic/multiply spec v1.0

/// Test: Basic multiplication of positive integers
fn test_multiply_positive() -> TestResult {
  let result = 2 * 3;
  assert_eq(result, 6, "Basic multiplication of positive integers");
  Pass
}

/// Test: Multiplication with negative number
fn test_multiply_negative() -> TestResult {
  let result = -5 * 3;
  assert_eq(result, -15, "Multiplication with negative number");
  Pass
}

/// Test: Multiplication by zero
fn test_multiply_zero() -> TestResult {
  let result = 0 * 42;
  assert_eq(result, 0, "Multiplication by zero");
  Pass
}

/// Test: Multiplication by one (identity)
fn test_multiply_identity() -> TestResult {
  let result = 7 * 1;
  assert_eq(result, 7, "Multiplication by one (identity)");
  Pass
}

/// Test: Multiplication with decimal number
fn test_multiply_decimal() -> TestResult {
  let result = 2.5 * 4.0;
  assert_float_eq(result, 10.0, 0.0001, "Multiplication with decimal number");
  Pass
}

/// Test: Multiplication of two negative numbers
fn test_multiply_two_negatives() -> TestResult {
  let result = -3 * -4;
  assert_eq(result, 12, "Multiplication of two negative numbers");
  Pass
}

/// Run all multiply conformance tests
fn test_alib_arithmetic_multiply() -> TestResult {
  let suite = {
    name: "aLib arithmetic/multiply conformance",
    tests: [
      { name: "multiply_positive", test: test_multiply_positive },
      { name: "multiply_negative", test: test_multiply_negative },
      { name: "multiply_zero", test: test_multiply_zero },
      { name: "multiply_identity", test: test_multiply_identity },
      { name: "multiply_decimal", test: test_multiply_decimal },
      { name: "multiply_two_negatives", test: test_multiply_two_negatives }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some multiply conformance tests failed")
  }
}
