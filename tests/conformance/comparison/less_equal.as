// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/less_equal
// Source: aggregate-library/specs/comparison/less_equal.md
//
// Conforms to aLib comparison/less_equal spec v1.0

fn test_less_equal_less() -> TestResult {
  assert(2 <= 5, "First number is less than second");
  Pass
}

fn test_less_equal_equal() -> TestResult {
  assert(5 <= 5, "Numbers are equal");
  Pass
}

fn test_less_equal_greater() -> TestResult {
  assert(!(5 <= 2), "First number is greater than second");
  Pass
}

fn test_less_equal_negative() -> TestResult {
  assert(-5 <= -2, "Negative numbers comparison");
  Pass
}

fn test_less_equal_zero() -> TestResult {
  assert(0 <= 0, "Zero equals zero");
  Pass
}

fn test_alib_comparison_less_equal() -> TestResult {
  let suite = {
    name: "aLib comparison/less_equal conformance",
    tests: [
      { name: "less_equal_less", test: test_less_equal_less },
      { name: "less_equal_equal", test: test_less_equal_equal },
      { name: "less_equal_greater", test: test_less_equal_greater },
      { name: "less_equal_negative", test: test_less_equal_negative },
      { name: "less_equal_zero", test: test_less_equal_zero }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some less_equal tests failed") }
}
