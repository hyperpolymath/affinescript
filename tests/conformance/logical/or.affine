// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: logical/or
// Source: aggregate-library/specs/logical/or.md
//
// Conforms to aLib logical/or spec v1.0

fn test_or_true_true() -> TestResult {
  assert(true || true, "Both values are true");
  Pass
}

fn test_or_true_false() -> TestResult {
  assert(true || false, "First is true, second is false");
  Pass
}

fn test_or_false_true() -> TestResult {
  assert(false || true, "First is false, second is true");
  Pass
}

fn test_or_false_false() -> TestResult {
  assert(!(false || false), "Both values are false");
  Pass
}

fn test_alib_logical_or() -> TestResult {
  let suite = {
    name: "aLib logical/or conformance",
    tests: [
      { name: "or_true_true", test: test_or_true_true },
      { name: "or_true_false", test: test_or_true_false },
      { name: "or_false_true", test: test_or_false_true },
      { name: "or_false_false", test: test_or_false_false }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some or tests failed") }
}
