// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// Comprehensive interpreter test

// Test 1: Arithmetic and basic operations
fn test_arithmetic() -> Int {
  let a = 10;
  let b = 20;
  let c = a + b;
  let d = c * 2;
  let e = d - 5;
  e / 3
}

// Test 2: Conditionals
fn test_conditionals(x: Int) -> Int {
  if x > 0 {
    x * 2
  } else if x < 0 {
    x * -1
  } else {
    0
  }
}

// Test 3: Pattern matching with tuples
fn test_tuples() -> Int {
  let pair = (10, 20);
  let (a, b) = pair;
  a + b
}

// Test 4: Pattern matching with records
fn test_records() -> Int {
  let person = { name: "Alice", age: 30 };
  let { age } = person;
  age
}

// Test 5: Arrays and indexing
fn test_arrays() -> Int {
  let arr = [1, 2, 3, 4, 5];
  let first = arr[0];
  let last = arr[4];
  first + last
}

// Test 6: Nested functions and closures
fn make_adder(x: Int) -> (Int -> Int) {
  |y| x + y
}

fn test_closures() -> Int {
  let add5 = make_adder(5);
  add5(10)
}

// Test 7: Recursion (factorial)
fn factorial(n: Int) -> Int {
  if n <= 1 {
    1
  } else {
    n * factorial(n - 1)
  }
}

// Test 8: Exception handling with pattern matching
fn safe_array_access(arr: [Int], idx: Int) -> Int {
  try {
    arr[idx]
  } catch {
    IndexOutOfBounds(i, len) => {
      println("Index out of bounds");
      -1
    }
  }
}

// Test 9: Loops
fn sum_array(arr: [Int]) -> Int {
  let total = 0;
  for x in arr {
    total = total + x;
  }
  total
}

// Test 10: Complex control flow
fn find_max(arr: [Int]) -> Int {
  let max = arr[0];
  for x in arr {
    if x > max {
      max = x;
    }
  }
  max
}
