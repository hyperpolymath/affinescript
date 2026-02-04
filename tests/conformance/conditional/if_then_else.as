// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: conditional/if_then_else
// Source: aggregate-library/specs/conditional/if_then_else.md
//
// Conforms to aLib conditional/if_then_else spec v1.0

fn test_if_then_else_true() -> TestResult {
  let result = if true { 10 } else { 20 };
  assert_eq(result, 10, "Condition is true, return then_value");
  Pass
}

fn test_if_then_else_false() -> TestResult {
  let result = if false { 10 } else { 20 };
  assert_eq(result, 20, "Condition is false, return else_value");
  Pass
}

fn test_if_then_else_string_true() -> TestResult {
  let result = if true { "yes" } else { "no" };
  assert_eq(result, "yes", "Works with string values");
  Pass
}

fn test_if_then_else_string_false() -> TestResult {
  let result = if false { "yes" } else { "no" };
  assert_eq(result, "no", "Returns else branch for false condition");
  Pass
}

fn test_if_then_else_comparison_true() -> TestResult {
  let result = if 5 == 5 { 1 } else { 0 };
  assert_eq(result, 1, "Condition from comparison operation");
  Pass
}

fn test_if_then_else_comparison_false() -> TestResult {
  let result = if 3 > 5 { "bigger" } else { "smaller" };
  assert_eq(result, "smaller", "False comparison leads to else branch");
  Pass
}

fn test_alib_conditional_if_then_else() -> TestResult {
  let suite = {
    name: "aLib conditional/if_then_else conformance",
    tests: [
      { name: "if_then_else_true", test: test_if_then_else_true },
      { name: "if_then_else_false", test: test_if_then_else_false },
      { name: "if_then_else_string_true", test: test_if_then_else_string_true },
      { name: "if_then_else_string_false", test: test_if_then_else_string_false },
      { name: "if_then_else_comparison_true", test: test_if_then_else_comparison_true },
      { name: "if_then_else_comparison_false", test: test_if_then_else_comparison_false }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some if_then_else tests failed") }
}
