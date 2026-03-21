// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: string/length
// Source: aggregate-library/specs/string/length.md
//
// Conforms to aLib string/length spec v1.0

fn test_length_simple() -> TestResult {
  let result = len("hello");
  assert_eq(result, 5, "Length of simple ASCII string");
  Pass
}

fn test_length_empty() -> TestResult {
  let result = len("");
  assert_eq(result, 0, "Length of empty string");
  Pass
}

fn test_length_single() -> TestResult {
  let result = len("a");
  assert_eq(result, 1, "Length of single character");
  Pass
}

fn test_length_with_space() -> TestResult {
  let result = len("hello world");
  assert_eq(result, 11, "Length including space");
  Pass
}

fn test_length_numeric() -> TestResult {
  let result = len("123");
  assert_eq(result, 3, "Length of numeric string");
  Pass
}

fn test_alib_string_length() -> TestResult {
  let suite = {
    name: "aLib string/length conformance",
    tests: [
      { name: "length_simple", test: test_length_simple },
      { name: "length_empty", test: test_length_empty },
      { name: "length_single", test: test_length_single },
      { name: "length_with_space", test: test_length_with_space },
      { name: "length_numeric", test: test_length_numeric }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some length tests failed") }
}
