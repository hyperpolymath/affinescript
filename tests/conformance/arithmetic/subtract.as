// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: arithmetic/subtract
// Source: aggregate-library/specs/arithmetic/subtract.md
//
// Conforms to aLib arithmetic/subtract spec v1.0

/// Test: Basic subtraction of positive integers
fn test_subtract_positive() -> TestResult {
  let result = 5 - 3;
  assert_eq(result, 2, "Basic subtraction of positive integers");
  Pass
}

/// Test: Subtraction resulting in negative number
fn test_subtract_negative_result() -> TestResult {
  let result = 3 - 5;
  assert_eq(result, -2, "Subtraction resulting in negative number");
  Pass
}

/// Test: Subtraction of zeros
fn test_subtract_zeros() -> TestResult {
  let result = 0 - 0;
  assert_eq(result, 0, "Subtraction of zeros");
  Pass
}

/// Test: Subtracting positive from negative
fn test_subtract_from_negative() -> TestResult {
  let result = -5 - 3;
  assert_eq(result, -8, "Subtracting positive from negative");
  Pass
}

/// Test: Subtraction of decimal numbers
fn test_subtract_decimal() -> TestResult {
  let result = 10.5 - 2.5;
  assert_float_eq(result, 8.0, 0.0001, "Subtraction of decimal numbers");
  Pass
}

/// Test: Subtracting negative from negative
fn test_subtract_negatives() -> TestResult {
  let result = -10 - -20;
  assert_eq(result, 10, "Subtracting negative from negative");
  Pass
}

/// Run all subtract conformance tests
fn test_alib_arithmetic_subtract() -> TestResult {
  let suite = {
    name: "aLib arithmetic/subtract conformance",
    tests: [
      { name: "subtract_positive", test: test_subtract_positive },
      { name: "subtract_negative_result", test: test_subtract_negative_result },
      { name: "subtract_zeros", test: test_subtract_zeros },
      { name: "subtract_from_negative", test: test_subtract_from_negative },
      { name: "subtract_decimal", test: test_subtract_decimal },
      { name: "subtract_negatives", test: test_subtract_negatives }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some subtract conformance tests failed")
  }
}
