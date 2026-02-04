// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Testing - Unit testing utilities

// ============================================================================
// Assertions
// ============================================================================

/// Assert that a condition is true
fn assert(condition: Bool, message: String) -> () {
  if !condition {
    panic("Assertion failed: " ++ message);
  }
}

/// Assert equality
fn assert_eq<T>(actual: T, expected: T, message: String) -> () {
  if actual != expected {
    panic("Assertion failed: " ++ message ++ "\n  Expected: " ++ show(expected) ++ "\n  Actual: " ++ show(actual));
  }
}

/// Assert inequality
fn assert_ne<T>(actual: T, expected: T, message: String) -> () {
  if actual == expected {
    panic("Assertion failed: values should not be equal: " ++ message);
  }
}

/// Assert Ok result
fn assert_ok<T, E>(result: Result<T, E>, message: String) -> T {
  match result {
    Ok(value) => value,
    Err(_) => panic("Assertion failed: expected Ok, got Err: " ++ message)
  }
}

/// Assert Err result
fn assert_err<T, E>(result: Result<T, E>, message: String) -> E {
  match result {
    Ok(_) => panic("Assertion failed: expected Err, got Ok: " ++ message),
    Err(e) => e
  }
}

/// Assert Some option
fn assert_some<T>(opt: Option<T>, message: String) -> T {
  match opt {
    Some(value) => value,
    None => panic("Assertion failed: expected Some, got None: " ++ message)
  }
}

/// Assert None option
fn assert_none<T>(opt: Option<T>, message: String) -> () {
  match opt {
    Some(_) => panic("Assertion failed: expected None, got Some: " ++ message),
    None => {}
  }
}

// ============================================================================
// Numeric Assertions
// ============================================================================

/// Assert less than
fn assert_lt<T>(actual: T, bound: T, message: String) -> () {
  if !(actual < bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be < " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert less than or equal
fn assert_le<T>(actual: T, bound: T, message: String) -> () {
  if !(actual <= bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be <= " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert greater than
fn assert_gt<T>(actual: T, bound: T, message: String) -> () {
  if !(actual > bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be > " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert greater than or equal
fn assert_ge<T>(actual: T, bound: T, message: String) -> () {
  if !(actual >= bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be >= " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert floating point equality within epsilon
fn assert_float_eq(actual: Float, expected: Float, epsilon: Float, message: String) -> () {
  let diff = if actual > expected { actual - expected } else { expected - actual };
  if diff > epsilon {
    panic("Assertion failed: floats not equal within epsilon: " ++ message);
  }
}

// ============================================================================
// Collection Assertions
// ============================================================================

/// Assert list is empty
fn assert_empty<T>(list: [T], message: String) -> () {
  if len(list) != 0 {
    panic("Assertion failed: list should be empty: " ++ message);
  }
}

/// Assert list is not empty
fn assert_not_empty<T>(list: [T], message: String) -> () {
  if len(list) == 0 {
    panic("Assertion failed: list should not be empty: " ++ message);
  }
}

/// Assert list length
fn assert_length<T>(list: [T], expected_len: Int, message: String) -> () {
  let actual_len = len(list);
  if actual_len != expected_len {
    panic("Assertion failed: expected length " ++ show(expected_len) ++ ", got " ++ show(actual_len) ++ ": " ++ message);
  }
}

/// Assert list contains element
fn assert_contains<T>(list: [T], element: T, message: String) -> () {
  let found = any(fn(x) => x == element, list);
  if !found {
    panic("Assertion failed: list should contain element: " ++ message);
  }
}

// ============================================================================
// Test Organization
// ============================================================================

type TestResult = Pass | Fail(String)

type TestCase = {
  name: String,
  test: () -> TestResult
}

type TestSuite = {
  name: String,
  tests: [TestCase]
}

/// Run a single test case
fn run_test(test: TestCase) -> TestResult {
  println("  Running: " ++ test.name);
  try {
    let result = test.test();
    match result {
      Pass => {
        println("    ✓ PASS");
        Pass
      },
      Fail(msg) => {
        println("    ✗ FAIL: " ++ msg);
        Fail(msg)
      }
    }
  } catch {
    RuntimeError(msg) => {
      println("    ✗ ERROR: " ++ msg);
      Fail(msg)
    },
    _ => {
      println("    ✗ PANIC");
      Fail("Test panicked")
    }
  }
}

/// Run a test suite
fn run_suite(suite: TestSuite) -> (Int, Int) {
  println("Running suite: " ++ suite.name);
  let mut passed = 0;
  let mut failed = 0;

  for test in suite.tests {
    match run_test(test) {
      Pass => passed = passed + 1,
      Fail(_) => failed = failed + 1
    }
  }

  println("\nResults: " ++ show(passed) ++ " passed, " ++ show(failed) ++ " failed");
  (passed, failed)
}

/// Run multiple test suites
fn run_suites(suites: [TestSuite]) -> (Int, Int) {
  let mut total_passed = 0;
  let mut total_failed = 0;

  for suite in suites {
    let (passed, failed) = run_suite(suite);
    total_passed = total_passed + passed;
    total_failed = total_failed + failed;
    println("");
  }

  println("Total: " ++ show(total_passed) ++ " passed, " ++ show(total_failed) ++ " failed");
  (total_passed, total_failed)
}

// ============================================================================
// Property-Based Testing Utilities
// ============================================================================

/// Check property holds for all elements
fn for_all<T>(values: [T], prop: T -> Bool) -> TestResult {
  for value in values {
    if !prop(value) {
      return Fail("Property failed for value: " ++ show(value));
    }
  }
  Pass
}

/// Check property holds for at least one element
fn exists<T>(values: [T], prop: T -> Bool) -> TestResult {
  for value in values {
    if prop(value) {
      return Pass;
    }
  }
  Fail("Property failed for all values")
}

// ============================================================================
// Benchmarking
// ============================================================================

type BenchResult = {
  iterations: Int,
  total_time: Float,
  avg_time: Float
}

/// Benchmark a function (simplified - needs actual timing)
fn bench(f: () -> (), iterations: Int) -> BenchResult {
  // TODO: Implement actual timing
  let mut i = 0;
  while i < iterations {
    f();
    i = i + 1;
  }
  {
    iterations: iterations,
    total_time: 0.0,
    avg_time: 0.0
  }
}

/// Compare performance of two functions
fn bench_compare(f1: () -> (), f2: () -> (), iterations: Int) -> (BenchResult, BenchResult) {
  let r1 = bench(f1, iterations);
  let r2 = bench(f2, iterations);
  (r1, r2)
}
