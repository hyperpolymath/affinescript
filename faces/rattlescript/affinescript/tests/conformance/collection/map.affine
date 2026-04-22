// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: collection/map
// Source: aggregate-library/specs/collection/map.md
//
// Conforms to aLib collection/map spec v1.0

/// Test: Double each number in collection
fn test_map_double() -> TestResult {
  let input = [1, 2, 3];
  let result = map(input, fn(x) => x * 2);
  assert_eq(result, [2, 4, 6], "Double each number in collection");
  Pass
}

/// Test: Add 10 to each number
fn test_map_add_ten() -> TestResult {
  let input = [1, 2, 3];
  let result = map(input, fn(x) => x + 10);
  assert_eq(result, [11, 12, 13], "Add 10 to each number");
  Pass
}

/// Test: Mapping over empty collection returns empty collection
fn test_map_empty() -> TestResult {
  let input = [];
  let result = map(input, fn(x) => x * 2);
  assert_eq(result, [], "Mapping over empty collection returns empty collection");
  Pass
}

/// Test: Identity function returns same element
fn test_map_identity() -> TestResult {
  let input = [5];
  let result = map(input, fn(x) => x);
  assert_eq(result, [5], "Identity function returns same element");
  Pass
}

/// Test: Map over string collection
fn test_map_string() -> TestResult {
  let input = ["a", "b", "c"];
  let result = map(input, fn(s) => s ++ s);
  assert_eq(result, ["aa", "bb", "cc"], "Map over string collection");
  Pass
}

/// Run all map conformance tests
fn test_alib_collection_map() -> TestResult {
  let suite = {
    name: "aLib collection/map conformance",
    tests: [
      { name: "map_double", test: test_map_double },
      { name: "map_add_ten", test: test_map_add_ten },
      { name: "map_empty", test: test_map_empty },
      { name: "map_identity", test: test_map_identity },
      { name: "map_string", test: test_map_string }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some map conformance tests failed")
  }
}
