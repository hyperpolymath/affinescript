// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Master conformance test runner for aLib specs
// Runs all conformance tests and generates report

import stdlib/testing as Testing;
import stdlib/prelude as Prelude;

// Import collection conformance tests
import tests/conformance/collection/map as MapTest;
import tests/conformance/collection/filter as FilterTest;
import tests/conformance/collection/fold as FoldTest;
import tests/conformance/collection/contains as ContainsTest;

// Import arithmetic conformance tests
import tests/conformance/arithmetic/add as AddTest;
import tests/conformance/arithmetic/multiply as MultiplyTest;
import tests/conformance/arithmetic/subtract as SubtractTest;
import tests/conformance/arithmetic/divide as DivideTest;
import tests/conformance/arithmetic/modulo as ModuloTest;

// Import comparison conformance tests
import tests/conformance/comparison/equal as EqualTest;
import tests/conformance/comparison/not_equal as NotEqualTest;
import tests/conformance/comparison/greater_than as GreaterThanTest;
import tests/conformance/comparison/less_than as LessThanTest;
import tests/conformance/comparison/greater_equal as GreaterEqualTest;
import tests/conformance/comparison/less_equal as LessEqualTest;

// Import logical conformance tests
import tests/conformance/logical/and as AndTest;
import tests/conformance/logical/or as OrTest;
import tests/conformance/logical/not as NotTest;

// Import string conformance tests
import tests/conformance/string/concat as ConcatTest;
import tests/conformance/string/length as LengthTest;
import tests/conformance/string/substring as SubstringTest;

/// Conformance report entry
type ConformanceResult = {
  category: String,
  operation: String,
  passed: Int,
  failed: Int,
  conformant: Bool
}

/// Generate conformance report
fn generate_report(results: [ConformanceResult]) -> () {
  println("================================================================================");
  println("aLib Conformance Report");
  println("================================================================================");
  println("");

  let mut total_passed = 0;
  let mut total_failed = 0;
  let mut total_conformant = 0;
  let mut total_operations = 0;

  for result in results {
    total_operations = total_operations + 1;
    total_passed = total_passed + result.passed;
    total_failed = total_failed + result.failed;

    if result.conformant {
      total_conformant = total_conformant + 1;
    }

    let status = if result.conformant { "✓ PASS" } else { "✗ FAIL" };
    let spec = result.category ++ "/" ++ result.operation;

    println(status ++ " " ++ spec ++ ": " ++ show(result.passed) ++ "/" ++ show(result.passed + result.failed) ++ " tests");
  }

  println("");
  println("================================================================================");
  println("Summary");
  println("================================================================================");
  println("Total operations tested: " ++ show(total_operations));
  println("Conformant operations: " ++ show(total_conformant) ++ "/" ++ show(total_operations));
  println("Total test cases: " ++ show(total_passed + total_failed));
  println("Tests passed: " ++ show(total_passed));
  println("Tests failed: " ++ show(total_failed));

  let conformance_rate = (total_conformant * 100) / total_operations;
  println("Conformance rate: " ++ show(conformance_rate) ++ "%");
  println("");

  if conformance_rate >= 95 {
    println("✓ Excellent aLib conformance (≥95%)");
  } else if conformance_rate >= 80 {
    println("⚠ Good aLib conformance (≥80%)");
  } else if conformance_rate >= 60 {
    println("⚠ Moderate aLib conformance (≥60%)");
  } else {
    println("✗ Low aLib conformance (<60%)");
  }

  println("================================================================================");
}

/// Run a single conformance test and capture result
fn run_conformance_test(category: String, operation: String, test_fn: () -> TestResult) -> ConformanceResult {
  match test_fn() {
    Pass => {
      category: category,
      operation: operation,
      passed: 1,
      failed: 0,
      conformant: true
    },
    Fail(_) => {
      category: category,
      operation: operation,
      passed: 0,
      failed: 1,
      conformant: false
    }
  }
}

/// Main conformance test runner
fn main() -> () {
  println("Running aLib conformance tests...");
  println("");

  let mut results = [];

  // Collection conformance tests
  results = results ++ [
    run_conformance_test("collection", "map", MapTest.test_alib_collection_map)
  ];
  results = results ++ [
    run_conformance_test("collection", "filter", FilterTest.test_alib_collection_filter)
  ];
  results = results ++ [
    run_conformance_test("collection", "fold", FoldTest.test_alib_collection_fold)
  ];
  results = results ++ [
    run_conformance_test("collection", "contains", ContainsTest.test_alib_collection_contains)
  ];

  // Arithmetic conformance tests
  results = results ++ [
    run_conformance_test("arithmetic", "add", AddTest.test_alib_arithmetic_add)
  ];
  results = results ++ [
    run_conformance_test("arithmetic", "multiply", MultiplyTest.test_alib_arithmetic_multiply)
  ];
  results = results ++ [
    run_conformance_test("arithmetic", "subtract", SubtractTest.test_alib_arithmetic_subtract)
  ];
  results = results ++ [
    run_conformance_test("arithmetic", "divide", DivideTest.test_alib_arithmetic_divide)
  ];
  results = results ++ [
    run_conformance_test("arithmetic", "modulo", ModuloTest.test_alib_arithmetic_modulo)
  ];

  // Comparison conformance tests
  results = results ++ [
    run_conformance_test("comparison", "equal", EqualTest.test_alib_comparison_equal)
  ];
  results = results ++ [
    run_conformance_test("comparison", "not_equal", NotEqualTest.test_alib_comparison_not_equal)
  ];
  results = results ++ [
    run_conformance_test("comparison", "greater_than", GreaterThanTest.test_alib_comparison_greater_than)
  ];
  results = results ++ [
    run_conformance_test("comparison", "less_than", LessThanTest.test_alib_comparison_less_than)
  ];
  results = results ++ [
    run_conformance_test("comparison", "greater_equal", GreaterEqualTest.test_alib_comparison_greater_equal)
  ];
  results = results ++ [
    run_conformance_test("comparison", "less_equal", LessEqualTest.test_alib_comparison_less_equal)
  ];

  // Logical conformance tests
  results = results ++ [
    run_conformance_test("logical", "and", AndTest.test_alib_logical_and)
  ];
  results = results ++ [
    run_conformance_test("logical", "or", OrTest.test_alib_logical_or)
  ];
  results = results ++ [
    run_conformance_test("logical", "not", NotTest.test_alib_logical_not)
  ];

  // String conformance tests
  results = results ++ [
    run_conformance_test("string", "concat", ConcatTest.test_alib_string_concat)
  ];
  results = results ++ [
    run_conformance_test("string", "length", LengthTest.test_alib_string_length)
  ];
  results = results ++ [
    run_conformance_test("string", "substring", SubstringTest.test_alib_string_substring)
  ];

  println("");
  generate_report(results);
}
