// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: logical/not
// Source: aggregate-library/specs/logical/not.md
//
// Conforms to aLib logical/not spec v1.0

fn test_not_true() -> TestResult {
  assert(!true == false, "Negation of true is false");
  Pass
}

fn test_not_false() -> TestResult {
  assert(!false == true, "Negation of false is true");
  Pass
}

fn test_alib_logical_not() -> TestResult {
  let suite = {
    name: "aLib logical/not conformance",
    tests: [
      { name: "not_true", test: test_not_true },
      { name: "not_false", test: test_not_false }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some not tests failed") }
}
