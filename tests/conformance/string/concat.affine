// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: string/concat
// Source: aggregate-library/specs/string/concat.md
//
// Conforms to aLib string/concat spec v1.0

fn test_concat_basic() -> TestResult {
  let result = "hello" ++ "world";
  assert_eq(result, "helloworld", "Basic string concatenation");
  Pass
}

fn test_concat_with_space() -> TestResult {
  let result = "hello" ++ " world";
  assert_eq(result, "hello world", "Concatenation with space");
  Pass
}

fn test_concat_empty_left() -> TestResult {
  let result = "" ++ "test";
  assert_eq(result, "test", "Concatenation with empty string (left)");
  Pass
}

fn test_concat_empty_right() -> TestResult {
  let result = "test" ++ "";
  assert_eq(result, "test", "Concatenation with empty string (right)");
  Pass
}

fn test_concat_empty_both() -> TestResult {
  let result = "" ++ "";
  assert_eq(result, "", "Concatenation of two empty strings");
  Pass
}

fn test_alib_string_concat() -> TestResult {
  let suite = {
    name: "aLib string/concat conformance",
    tests: [
      { name: "concat_basic", test: test_concat_basic },
      { name: "concat_with_space", test: test_concat_with_space },
      { name: "concat_empty_left", test: test_concat_empty_left },
      { name: "concat_empty_right", test: test_concat_empty_right },
      { name: "concat_empty_both", test: test_concat_empty_both }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some concat tests failed") }
}
