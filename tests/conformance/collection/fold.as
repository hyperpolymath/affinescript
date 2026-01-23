// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Auto-generated conformance tests for aLib spec: collection/fold
// Source: aggregate-library/specs/collection/fold.md
//
// Conforms to aLib collection/fold spec v1.0

/// Test: Sum all numbers in collection
fn test_fold_sum() -> TestResult {
  let input = [1, 2, 3, 4];
  let result = fold(input, 0, fn(acc, x) => acc + x);
  assert_eq(result, 10, "Sum all numbers in collection");
  Pass
}

/// Test: Product of all numbers in collection
fn test_fold_product() -> TestResult {
  let input = [1, 2, 3, 4];
  let result = fold(input, 1, fn(acc, x) => acc * x);
  assert_eq(result, 24, "Product of all numbers in collection");
  Pass
}

/// Test: Folding empty collection returns initial value
fn test_fold_empty() -> TestResult {
  let input = [];
  let result = fold(input, 42, fn(acc, x) => acc + x);
  assert_eq(result, 42, "Folding empty collection returns initial value");
  Pass
}

/// Test: Folding single element
fn test_fold_single() -> TestResult {
  let input = [5];
  let result = fold(input, 10, fn(acc, x) => acc + x);
  assert_eq(result, 15, "Folding single element");
  Pass
}

/// Test: Concatenate strings using fold
fn test_fold_concat() -> TestResult {
  let input = ["a", "b", "c"];
  let result = fold(input, "", fn(acc, s) => acc ++ s);
  assert_eq(result, "abc", "Concatenate strings using fold");
  Pass
}

/// Test: Count elements using fold
fn test_fold_count() -> TestResult {
  let input = [1, 2, 3];
  let result = fold(input, 0, fn(acc, x) => acc + 1);
  assert_eq(result, 3, "Count elements using fold");
  Pass
}

/// Run all fold conformance tests
fn test_alib_collection_fold() -> TestResult {
  let suite = {
    name: "aLib collection/fold conformance",
    tests: [
      { name: "fold_sum", test: test_fold_sum },
      { name: "fold_product", test: test_fold_product },
      { name: "fold_empty", test: test_fold_empty },
      { name: "fold_single", test: test_fold_single },
      { name: "fold_concat", test: test_fold_concat },
      { name: "fold_count", test: test_fold_count }
    ]
  };

  let (passed, failed) = run_suite(suite);

  if failed == 0 {
    Pass
  } else {
    Fail("Some fold conformance tests failed")
  }
}
