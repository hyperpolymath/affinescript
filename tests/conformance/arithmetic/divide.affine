// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: arithmetic/divide
// Source: aggregate-library/specs/arithmetic/divide.md
//
// Conforms to aLib arithmetic/divide spec v1.0

/// Test: Basic division with exact result
fn test_divide_exact() -> TestResult {
  let result = 6 / 2;
  assert_eq(result, 3, "Basic division with exact result");
  Pass
}

/// Test: Division with fractional result
fn test_divide_fractional() -> TestResult {
  let result = 7.0 / 2.0;
  assert_float_eq(result, 3.5, 0.0001, "Division with fractional result");
  Pass
}

/// Test: Zero divided by non-zero
fn test_divide_zero() -> TestResult {
  let result = 0 / 5;
  assert_eq(result, 0, "Zero divided by non-zero");
  Pass
}

/// Test: Division with negative dividend
fn test_divide_negative_dividend() -> TestResult {
  let result = -10 / 2;
  assert_eq(result, -5, "Division with negative dividend");
  Pass
}

/// Test: Division with negative divisor
fn test_divide_negative_divisor() -> TestResult {
  let result = 10 / -2;
  assert_eq(result, -5, "Division with negative divisor");
  Pass
}

/// Test: Division of two negative numbers
fn test_divide_two_negatives() -> TestResult {
  let result = -12 / -3;
  assert_eq(result, 4, "Division of two negative numbers");
  Pass
}

/// Run all divide conformance tests
fn test_alib_arithmetic_divide() -> TestResult {
  let suite = {
    name: "aLib arithmetic/divide conformance",
    tests: [
      { name: "divide_exact", test: test_divide_exact },
      { name: "divide_fractional", test: test_divide_fractional },
      { name: "divide_zero", test: test_divide_zero },
      { name: "divide_negative_dividend", test: test_divide_negative_dividend },
      { name: "divide_negative_divisor", test: test_divide_negative_divisor },
      { name: "divide_two_negatives", test: test_divide_two_negatives }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some divide conformance tests failed")
  }
}
