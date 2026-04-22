// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// AffineScript Standard Library - Testing
//
// Provides assertion functions, test organisation, property-based helpers,
// and lightweight benchmarking.
//
// Depends on builtins: show, panic, time_now

// ============================================================================
// Assertions
// ============================================================================

/// Assert that a condition is true; panic with message on failure
fn assert(condition: Bool, message: String) -> () {
  if !condition {
    panic("Assertion failed: " ++ message);
  }
}

/// Assert two values are equal
fn assert_eq<T>(actual: T, expected: T, message: String) -> () {
  if actual != expected {
    panic("Assertion failed: " ++ message ++ "\n  Expected: " ++ show(expected) ++ "\n  Actual:   " ++ show(actual));
  }
}

/// Assert two values are not equal
fn assert_ne<T>(actual: T, expected: T, message: String) -> () {
  if actual == expected {
    panic("Assertion failed: values should not be equal: " ++ message ++ "\n  Both: " ++ show(actual));
  }
}

/// Assert a Result is Ok and return the inner value
fn assert_ok<T, E>(result: Result<T, E>, message: String) -> T {
  match result {
    Ok(value) => value,
    Err(e) => panic("Assertion failed: expected Ok, got Err(" ++ show(e) ++ "): " ++ message)
  }
}

/// Assert a Result is Err and return the error value
fn assert_err<T, E>(result: Result<T, E>, message: String) -> E {
  match result {
    Ok(v) => panic("Assertion failed: expected Err, got Ok(" ++ show(v) ++ "): " ++ message),
    Err(e) => e
  }
}

/// Assert an Option is Some and return the inner value
fn assert_some<T>(opt: Option<T>, message: String) -> T {
  match opt {
    Some(value) => value,
    None => panic("Assertion failed: expected Some, got None: " ++ message)
  }
}

/// Assert an Option is None
fn assert_none<T>(opt: Option<T>, message: String) -> () {
  match opt {
    Some(v) => panic("Assertion failed: expected None, got Some(" ++ show(v) ++ "): " ++ message),
    None => {}
  }
}

// ============================================================================
// Numeric Assertions
// ============================================================================

/// Assert actual < bound
fn assert_lt<T>(actual: T, bound: T, message: String) -> () {
  if !(actual < bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be < " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert actual <= bound
fn assert_le<T>(actual: T, bound: T, message: String) -> () {
  if !(actual <= bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be <= " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert actual > bound
fn assert_gt<T>(actual: T, bound: T, message: String) -> () {
  if !(actual > bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be > " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert actual >= bound
fn assert_ge<T>(actual: T, bound: T, message: String) -> () {
  if !(actual >= bound) {
    panic("Assertion failed: " ++ show(actual) ++ " should be >= " ++ show(bound) ++ ": " ++ message);
  }
}

/// Assert two floats are equal within an epsilon tolerance
fn assert_float_eq(actual: Float, expected: Float, epsilon: Float, message: String) -> () {
  let diff = if actual > expected { actual - expected } else { expected - actual };
  if diff > epsilon {
    panic("Assertion failed: " ++ show(actual) ++ " != " ++ show(expected) ++ " (epsilon " ++ show(epsilon) ++ "): " ++ message);
  }
}

// ============================================================================
// Collection Assertions
// ============================================================================

/// Assert a list is empty
fn assert_empty<T>(list: [T], message: String) -> () {
  if len(list) != 0 {
    panic("Assertion failed: list should be empty (length " ++ show(len(list)) ++ "): " ++ message);
  }
}

/// Assert a list is not empty
fn assert_not_empty<T>(list: [T], message: String) -> () {
  if len(list) == 0 {
    panic("Assertion failed: list should not be empty: " ++ message);
  }
}

/// Assert list has expected length
fn assert_length<T>(list: [T], expected_len: Int, message: String) -> () {
  let actual_len = len(list);
  if actual_len != expected_len {
    panic("Assertion failed: expected length " ++ show(expected_len) ++ ", got " ++ show(actual_len) ++ ": " ++ message);
  }
}

/// Assert list contains a specific element
fn assert_contains<T>(list: [T], element: T, message: String) -> () {
  let found = false;
  for x in list {
    if x == element {
      found = true;
    }
  }
  if !found {
    panic("Assertion failed: list should contain " ++ show(element) ++ ": " ++ message);
  }
}

/// Assert a string contains a substring
fn assert_str_contains(haystack: String, needle: String, message: String) -> () {
  if string_find(haystack, needle) < 0 {
    panic("Assertion failed: \"" ++ haystack ++ "\" should contain \"" ++ needle ++ "\": " ++ message);
  }
}

// ============================================================================
// Test Organisation
// ============================================================================

/// Result of a single test execution
type TestResult = Pass | Fail(String)

/// A named test case
type TestCase = {
  name: String,
  test: () -> TestResult
}

/// A named collection of test cases
type TestSuite = {
  name: String,
  tests: [TestCase]
}

/// Run a single test case, catching panics
fn run_test(test: TestCase) -> TestResult {
  println("  Running: " ++ test.name);
  try {
    let result = test.test();
    match result {
      Pass => {
        println("    PASS");
        Pass
      },
      Fail(msg) => {
        println("    FAIL: " ++ msg);
        Fail(msg)
      }
    }
  } catch {
    RuntimeError(msg) => {
      println("    ERROR: " ++ msg);
      Fail(msg)
    },
    _ => {
      println("    PANIC");
      Fail("Test panicked")
    }
  }
}

/// Run all tests in a suite and return (passed, failed) counts
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

/// Run multiple test suites and return aggregate (passed, failed) counts
fn run_suites(suites: [TestSuite]) -> (Int, Int) {
  let mut total_passed = 0;
  let mut total_failed = 0;

  for suite in suites {
    let (passed, failed) = run_suite(suite);
    total_passed = total_passed + passed;
    total_failed = total_failed + failed;
    println("");
  }

  println("========================================");
  println("Total: " ++ show(total_passed) ++ " passed, " ++ show(total_failed) ++ " failed");
  (total_passed, total_failed)
}

// ============================================================================
// Property-Based Testing Utilities
// ============================================================================

/// Check that a property holds for all elements in a list
fn for_all<T>(values: [T], prop: T -> Bool) -> TestResult {
  for value in values {
    if !prop(value) {
      return Fail("Property failed for value: " ++ show(value));
    }
  }
  Pass
}

/// Check that a property holds for at least one element
fn exists<T>(values: [T], prop: T -> Bool) -> TestResult {
  for value in values {
    if prop(value) {
      return Pass;
    }
  }
  Fail("Property failed for all values")
}

/// Check that a property is an involution (applying twice yields the original)
fn assert_involution<T>(f: T -> T, values: [T], message: String) -> () {
  for v in values {
    let round_tripped = f(f(v));
    if round_tripped != v {
      panic("Involution failed for " ++ show(v) ++ ": f(f(x)) = " ++ show(round_tripped) ++ ": " ++ message);
    }
  }
}

/// Check that a binary operation is commutative over given pairs
fn assert_commutative<T, R>(op: (T, T) -> R, pairs: [(T, T)], message: String) -> () {
  for (a, b) in pairs {
    let lhs = op(a, b);
    let rhs = op(b, a);
    if lhs != rhs {
      panic("Commutativity failed: op(" ++ show(a) ++ ", " ++ show(b) ++ ") = " ++ show(lhs) ++
            " but op(" ++ show(b) ++ ", " ++ show(a) ++ ") = " ++ show(rhs) ++ ": " ++ message);
    }
  }
}

// ============================================================================
// Benchmarking
// ============================================================================

/// Result of a benchmark run
type BenchResult = {
  iterations: Int,
  total_time: Float,
  avg_time: Float
}

/// Benchmark a function by running it for the given number of iterations.
/// Returns timing statistics using the time_now() builtin.
fn bench(f: () -> (), iterations: Int) -> BenchResult {
  let start = time_now();

  let mut i = 0;
  while i < iterations {
    f();
    i = i + 1;
  }

  let total = time_now() - start;
  {
    iterations: iterations,
    total_time: total,
    avg_time: total / (iterations + 0.0)
  }
}

/// Compare performance of two functions side-by-side
fn bench_compare(name1: String, f1: () -> (), name2: String, f2: () -> (), iterations: Int) -> (BenchResult, BenchResult) {
  println("Benchmarking " ++ name1 ++ " vs " ++ name2 ++ " (" ++ show(iterations) ++ " iterations)");

  let r1 = bench(f1, iterations);
  let r2 = bench(f2, iterations);

  println("  " ++ name1 ++ ": " ++ show(r1.total_time) ++ "s total, " ++ show(r1.avg_time) ++ "s/iter");
  println("  " ++ name2 ++ ": " ++ show(r2.total_time) ++ "s total, " ++ show(r2.avg_time) ++ "s/iter");

  (r1, r2)
}
