// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: arithmetic/add
// Source: aggregate-library/specs/arithmetic/add.md
//
// Conforms to aLib arithmetic/add spec v1.0

/// Test: Basic addition of positive integers
fn test_add_positive() -> TestResult {
  let result = 2 + 3;
  assert_eq(result, 5, "Basic addition of positive integers");
  Pass
}

/// Test: Addition with negative number
fn test_add_negative() -> TestResult {
  let result = -5 + 3;
  assert_eq(result, -2, "Addition with negative number");
  Pass
}

/// Test: Addition of zeros
fn test_add_zeros() -> TestResult {
  let result = 0 + 0;
  assert_eq(result, 0, "Addition of zeros");
  Pass
}

/// Test: Addition of decimal numbers
fn test_add_decimal() -> TestResult {
  let result = 1.5 + 2.5;
  assert_float_eq(result, 4.0, 0.0001, "Addition of decimal numbers");
  Pass
}

/// Test: Addition of two negative numbers
fn test_add_two_negatives() -> TestResult {
  let result = -10 + -20;
  assert_eq(result, -30, "Addition of two negative numbers");
  Pass
}

/// Run all add conformance tests
fn test_alib_arithmetic_add() -> TestResult {
  let suite = {
    name: "aLib arithmetic/add conformance",
    tests: [
      { name: "add_positive", test: test_add_positive },
      { name: "add_negative", test: test_add_negative },
      { name: "add_zeros", test: test_add_zeros },
      { name: "add_decimal", test: test_add_decimal },
      { name: "add_two_negatives", test: test_add_two_negatives }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some add conformance tests failed")
  }
}
