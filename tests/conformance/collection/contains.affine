// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: collection/contains
// Source: aggregate-library/specs/collection/contains.md
//
// Conforms to aLib collection/contains spec v1.0

/// Test: Element is present in collection
fn test_contains_present() -> TestResult {
  let input = [1, 2, 3, 4, 5];
  let result = contains(input, 3);
  assert(result, "Element is present in collection");
  Pass
}

/// Test: Element is not present in collection
fn test_contains_not_present() -> TestResult {
  let input = [1, 2, 3, 4, 5];
  let result = contains(input, 6);
  assert(!result, "Element is not present in collection");
  Pass
}

/// Test: Empty collection contains no elements
fn test_contains_empty() -> TestResult {
  let input = [];
  let result = contains(input, 1);
  assert(!result, "Empty collection contains no elements");
  Pass
}

/// Test: Single element collection contains that element
fn test_contains_single() -> TestResult {
  let input = [1];
  let result = contains(input, 1);
  assert(result, "Single element collection contains that element");
  Pass
}

/// Test: String element is present
fn test_contains_string() -> TestResult {
  let input = ["a", "b", "c"];
  let result = contains(input, "b");
  assert(result, "String element is present");
  Pass
}

/// Test: Element appears multiple times (still returns true)
fn test_contains_multiple() -> TestResult {
  let input = [1, 2, 1, 3, 1];
  let result = contains(input, 1);
  assert(result, "Element appears multiple times (still returns true)");
  Pass
}

/// Run all contains conformance tests
fn test_alib_collection_contains() -> TestResult {
  let suite = {
    name: "aLib collection/contains conformance",
    tests: [
      { name: "contains_present", test: test_contains_present },
      { name: "contains_not_present", test: test_contains_not_present },
      { name: "contains_empty", test: test_contains_empty },
      { name: "contains_single", test: test_contains_single },
      { name: "contains_string", test: test_contains_string },
      { name: "contains_multiple", test: test_contains_multiple }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some contains conformance tests failed")
  }
}
