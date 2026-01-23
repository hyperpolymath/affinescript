// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: arithmetic/modulo
// Source: aggregate-library/specs/arithmetic/modulo.md
//
// Conforms to aLib arithmetic/modulo spec v1.0

/// Test: Basic modulo with positive integers
fn test_modulo_basic() -> TestResult {
  let result = 7 % 3;
  assert_eq(result, 1, "Basic modulo with positive integers");
  Pass
}

/// Test: Modulo with no remainder
fn test_modulo_no_remainder() -> TestResult {
  let result = 10 % 5;
  assert_eq(result, 0, "Modulo with no remainder");
  Pass
}

/// Test: Modulo returning non-zero remainder
fn test_modulo_nonzero() -> TestResult {
  let result = 15 % 4;
  assert_eq(result, 3, "Modulo returning non-zero remainder");
  Pass
}

/// Test: Zero modulo non-zero
fn test_modulo_zero() -> TestResult {
  let result = 0 % 5;
  assert_eq(result, 0, "Zero modulo non-zero");
  Pass
}

/// Test: Modulo with larger numbers
fn test_modulo_large() -> TestResult {
  let result = 100 % 7;
  assert_eq(result, 2, "Modulo with larger numbers");
  Pass
}

/// Run all modulo conformance tests
fn test_alib_arithmetic_modulo() -> TestResult {
  let suite = {
    name: "aLib arithmetic/modulo conformance",
    tests: [
      { name: "modulo_basic", test: test_modulo_basic },
      { name: "modulo_no_remainder", test: test_modulo_no_remainder },
      { name: "modulo_nonzero", test: test_modulo_nonzero },
      { name: "modulo_zero", test: test_modulo_zero },
      { name: "modulo_large", test: test_modulo_large }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some modulo conformance tests failed")
  }
}
