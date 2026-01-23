// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/greater_equal
// Source: aggregate-library/specs/comparison/greater_equal.md
//
// Conforms to aLib comparison/greater_equal spec v1.0

fn test_greater_equal_greater() -> TestResult {
  assert(5 >= 2, "First number is greater than second");
  Pass
}

fn test_greater_equal_equal() -> TestResult {
  assert(5 >= 5, "Numbers are equal");
  Pass
}

fn test_greater_equal_less() -> TestResult {
  assert(!(2 >= 5), "First number is less than second");
  Pass
}

fn test_greater_equal_negative() -> TestResult {
  assert(-2 >= -5, "Negative numbers comparison");
  Pass
}

fn test_greater_equal_zero() -> TestResult {
  assert(0 >= 0, "Zero equals zero");
  Pass
}

fn test_alib_comparison_greater_equal() -> TestResult {
  let suite = {
    name: "aLib comparison/greater_equal conformance",
    tests: [
      { name: "greater_equal_greater", test: test_greater_equal_greater },
      { name: "greater_equal_equal", test: test_greater_equal_equal },
      { name: "greater_equal_less", test: test_greater_equal_less },
      { name: "greater_equal_negative", test: test_greater_equal_negative },
      { name: "greater_equal_zero", test: test_greater_equal_zero }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some greater_equal tests failed") }
}
