// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/equal
// Source: aggregate-library/specs/comparison/equal.md
//
// Conforms to aLib comparison/equal spec v1.0

fn test_equal_positive() -> TestResult {
  assert(5 == 5, "Equal positive integers");
  Pass
}

fn test_equal_unequal() -> TestResult {
  assert(!(5 == 3), "Unequal positive integers");
  Pass
}

fn test_equal_zero() -> TestResult {
  assert(0 == 0, "Zero equals zero");
  Pass
}

fn test_equal_negative() -> TestResult {
  assert(-5 == -5, "Equal negative integers");
  Pass
}

fn test_equal_neg_pos() -> TestResult {
  assert(!(-5 == 5), "Negative and positive not equal");
  Pass
}

fn test_equal_decimal() -> TestResult {
  assert(2.5 == 2.5, "Equal decimal numbers");
  Pass
}

fn test_equal_int_decimal() -> TestResult {
  assert(1.0 == 1, "Decimal and integer with same value");
  Pass
}

fn test_alib_comparison_equal() -> TestResult {
  let suite = {
    name: "aLib comparison/equal conformance",
    tests: [
      { name: "equal_positive", test: test_equal_positive },
      { name: "equal_unequal", test: test_equal_unequal },
      { name: "equal_zero", test: test_equal_zero },
      { name: "equal_negative", test: test_equal_negative },
      { name: "equal_neg_pos", test: test_equal_neg_pos },
      { name: "equal_decimal", test: test_equal_decimal },
      { name: "equal_int_decimal", test: test_equal_int_decimal }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some equal tests failed") }
}
