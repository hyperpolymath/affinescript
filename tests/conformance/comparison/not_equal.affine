// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: comparison/not_equal
// Source: aggregate-library/specs/comparison/not_equal.md
//
// Conforms to aLib comparison/not_equal spec v1.0

fn test_not_equal_unequal() -> TestResult {
  assert(5 != 3, "Unequal positive integers");
  Pass
}

fn test_not_equal_equal() -> TestResult {
  assert(!(5 != 5), "Equal positive integers");
  Pass
}

fn test_not_equal_zero() -> TestResult {
  assert(!(0 != 0), "Zero equals zero");
  Pass
}

fn test_not_equal_negative() -> TestResult {
  assert(!(-5 != -5), "Equal negative integers");
  Pass
}

fn test_not_equal_neg_pos() -> TestResult {
  assert(-5 != 5, "Negative and positive not equal");
  Pass
}

fn test_alib_comparison_not_equal() -> TestResult {
  let suite = {
    name: "aLib comparison/not_equal conformance",
    tests: [
      { name: "not_equal_unequal", test: test_not_equal_unequal },
      { name: "not_equal_equal", test: test_not_equal_equal },
      { name: "not_equal_zero", test: test_not_equal_zero },
      { name: "not_equal_negative", test: test_not_equal_negative },
      { name: "not_equal_neg_pos", test: test_not_equal_neg_pos }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some not_equal tests failed") }
}
