// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: logical/and
// Source: aggregate-library/specs/logical/and.md
//
// Conforms to aLib logical/and spec v1.0

fn test_and_true_true() -> TestResult {
  assert(true && true, "Both values are true");
  Pass
}

fn test_and_true_false() -> TestResult {
  assert(!(true && false), "First is true, second is false");
  Pass
}

fn test_and_false_true() -> TestResult {
  assert(!(false && true), "First is false, second is true");
  Pass
}

fn test_and_false_false() -> TestResult {
  assert(!(false && false), "Both values are false");
  Pass
}

fn test_alib_logical_and() -> TestResult {
  let suite = {
    name: "aLib logical/and conformance",
    tests: [
      { name: "and_true_true", test: test_and_true_true },
      { name: "and_true_false", test: test_and_true_false },
      { name: "and_false_true", test: test_and_false_true },
      { name: "and_false_false", test: test_and_false_false }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some and tests failed") }
}
