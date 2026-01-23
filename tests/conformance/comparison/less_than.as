// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/less_than
// Source: aggregate-library/specs/comparison/less_than.md
//
// Conforms to aLib comparison/less_than spec v1.0

fn test_less_than_true() -> TestResult {
  assert(2 < 5, "First number is less than second");
  Pass
}

fn test_less_than_false() -> TestResult {
  assert(!(5 < 2), "First number is greater than second");
  Pass
}

fn test_less_than_equal() -> TestResult {
  assert(!(3 < 3), "Numbers are equal");
  Pass
}

fn test_less_than_negative() -> TestResult {
  assert(-5 < -2, "Negative numbers comparison");
  Pass
}

fn test_less_than_zero() -> TestResult {
  assert(0 < 5, "Zero less than positive");
  Pass
}

fn test_alib_comparison_less_than() -> TestResult {
  let suite = {
    name: "aLib comparison/less_than conformance",
    tests: [
      { name: "less_than_true", test: test_less_than_true },
      { name: "less_than_false", test: test_less_than_false },
      { name: "less_than_equal", test: test_less_than_equal },
      { name: "less_than_negative", test: test_less_than_negative },
      { name: "less_than_zero", test: test_less_than_zero }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some less_than tests failed") }
}
