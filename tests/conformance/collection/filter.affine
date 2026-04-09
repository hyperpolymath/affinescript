// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: collection/filter
// Source: aggregate-library/specs/collection/filter.md
//
// Conforms to aLib collection/filter spec v1.0

/// Test: Filter numbers greater than 2
fn test_filter_greater_than() -> TestResult {
  let input = [1, 2, 3, 4, 5];
  let result = filter(input, fn(x) => x > 2);
  assert_eq(result, [3, 4, 5], "Filter numbers greater than 2");
  Pass
}

/// Test: Filter even numbers
fn test_filter_even() -> TestResult {
  let input = [1, 2, 3, 4, 5];
  let result = filter(input, fn(x) => x % 2 == 0);
  assert_eq(result, [2, 4], "Filter even numbers");
  Pass
}

/// Test: Filter with always-true predicate returns all elements
fn test_filter_always_true() -> TestResult {
  let input = [1, 2, 3];
  let result = filter(input, fn(x) => true);
  assert_eq(result, [1, 2, 3], "Filter with always-true predicate returns all elements");
  Pass
}

/// Test: Filter with always-false predicate returns empty collection
fn test_filter_always_false() -> TestResult {
  let input = [1, 2, 3];
  let result = filter(input, fn(x) => false);
  assert_eq(result, [], "Filter with always-false predicate returns empty collection");
  Pass
}

/// Test: Filter empty collection returns empty collection
fn test_filter_empty() -> TestResult {
  let input = [];
  let result = filter(input, fn(x) => true);
  assert_eq(result, [], "Filter empty collection returns empty collection");
  Pass
}

/// Run all filter conformance tests
fn test_alib_collection_filter() -> TestResult {
  let suite = {
    name: "aLib collection/filter conformance",
    tests: [
      { name: "filter_greater_than", test: test_filter_greater_than },
      { name: "filter_even", test: test_filter_even },
      { name: "filter_always_true", test: test_filter_always_true },
      { name: "filter_always_false", test: test_filter_always_false },
      { name: "filter_empty", test: test_filter_empty }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some filter conformance tests failed")
  }
}
