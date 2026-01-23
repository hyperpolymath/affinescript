// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/greater_than
// Source: aggregate-library/specs/comparison/greater_than.md
//
// Conforms to aLib comparison/greater_than spec v1.0

fn test_greater_than_true() -> TestResult {
  assert(5 > 2, "First number is greater than second");
  Pass
}

fn test_greater_than_false() -> TestResult {
  assert(!(2 > 5), "First number is less than second");
  Pass
}

fn test_greater_than_equal() -> TestResult {
  assert(!(3 > 3), "Numbers are equal");
  Pass
}

fn test_greater_than_negative() -> TestResult {
  assert(-2 > -5, "Negative numbers comparison");
  Pass
}

fn test_greater_than_zero() -> TestResult {
  assert(5 > 0, "Positive greater than zero");
  Pass
}

fn test_alib_comparison_greater_than() -> TestResult {
  let suite = {
    name: "aLib comparison/greater_than conformance",
    tests: [
      { name: "greater_than_true", test: test_greater_than_true },
      { name: "greater_than_false", test: test_greater_than_false },
      { name: "greater_than_equal", test: test_greater_than_equal },
      { name: "greater_than_negative", test: test_greater_than_negative },
      { name: "greater_than_zero", test: test_greater_than_zero }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some greater_than tests failed") }
}
