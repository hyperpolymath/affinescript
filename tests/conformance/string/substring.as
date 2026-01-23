// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Conformance tests for aLib spec: string/substring
// Source: aggregate-library/specs/string/substring.md
//
// Conforms to aLib string/substring spec v1.0
//
// NOTE: These tests require builtin substring implementation
// Currently using placeholder - tests may fail until builtin is available

fn test_substring_entire() -> TestResult {
  let result = substring("hello", 0, 5);
  assert_eq(result, "hello", "Extract entire string");
  Pass
}

fn test_substring_middle() -> TestResult {
  let result = substring("hello", 1, 4);
  assert_eq(result, "ell", "Extract middle portion");
  Pass
}

fn test_substring_first() -> TestResult {
  let result = substring("hello", 0, 1);
  assert_eq(result, "h", "Extract first character");
  Pass
}

fn test_substring_last() -> TestResult {
  let result = substring("hello", 4, 5);
  assert_eq(result, "o", "Extract last character");
  Pass
}

fn test_substring_empty() -> TestResult {
  let result = substring("hello", 2, 2);
  assert_eq(result, "", "Extract empty substring (start == end)");
  Pass
}

fn test_alib_string_substring() -> TestResult {
  let suite = {
    name: "aLib string/substring conformance",
    tests: [
      { name: "substring_entire", test: test_substring_entire },
      { name: "substring_middle", test: test_substring_middle },
      { name: "substring_first", test: test_substring_first },
      { name: "substring_last", test: test_substring_last },
      { name: "substring_empty", test: test_substring_empty }
    ]
  };
  let (passed, failed) = run_suite(suite);
  if failed == 0 { Pass } else { Fail("Some substring tests failed") }
}
