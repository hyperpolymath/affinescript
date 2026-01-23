// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// AffineScript Standard Library - Prelude
// Common functions and utilities automatically available

// ============================================================================
// Option type - represents optional values
// ============================================================================

type Option<T> = Some(T) | None

fn is_some<T>(opt: Option<T>) -> Bool {
  match opt {
    Some(_) => true,
    None => false
  }
}

fn is_none<T>(opt: Option<T>) -> Bool {
  match opt {
    Some(_) => false,
    None => true
  }
}

fn unwrap<T>(opt: Option<T>) -> T {
  match opt {
    Some(value) => value,
    None => {
      println("Called unwrap on None");
      // TODO: panic!() once implemented
    }
  }
}

fn unwrap_or<T>(opt: Option<T>, default: T) -> T {
  match opt {
    Some(value) => value,
    None => default
  }
}

// ============================================================================
// Result type - represents success or failure
// ============================================================================

type Result<T, E> = Ok(T) | Err(E)

fn is_ok<T, E>(res: Result<T, E>) -> Bool {
  match res {
    Ok(_) => true,
    Err(_) => false
  }
}

fn is_err<T, E>(res: Result<T, E>) -> Bool {
  match res {
    Ok(_) => false,
    Err(_) => true
  }
}

fn unwrap_result<T, E>(res: Result<T, E>) -> T {
  match res {
    Ok(value) => value,
    Err(_) => {
      println("Called unwrap on Err");
      // TODO: panic!() once implemented
    }
  }
}

fn unwrap_or_result<T, E>(res: Result<T, E>, default: T) -> T {
  match res {
    Ok(value) => value,
    Err(_) => default
  }
}

// ============================================================================
// List utilities
// ============================================================================

fn map<T, U>(arr: [T], f: T -> U) -> [U] {
  let result = [];
  for x in arr {
    result = result ++ [f(x)];
  }
  result
}

fn filter<T>(arr: [T], predicate: T -> Bool) -> [T] {
  let result = [];
  for x in arr {
    if predicate(x) {
      result = result ++ [x];
    }
  }
  result
}

fn fold<T, U>(arr: [T], init: U, f: (U, T) -> U) -> U {
  let acc = init;
  for x in arr {
    acc = f(acc, x);
  }
  acc
}

fn sum(arr: [Int]) -> Int {
  fold(arr, 0, |acc, x| acc + x)
}

fn product(arr: [Int]) -> Int {
  fold(arr, 1, |acc, x| acc * x)
}

// ============================================================================
// Comparison and ordering
// ============================================================================

fn min(a: Int, b: Int) -> Int {
  if a < b { a } else { b }
}

fn max(a: Int, b: Int) -> Int {
  if a > b { a } else { b }
}

fn clamp(value: Int, min_val: Int, max_val: Int) -> Int {
  if value < min_val {
    min_val
  } else if value > max_val {
    max_val
  } else {
    value
  }
}

// ============================================================================
// Boolean utilities
// ============================================================================

fn not(b: Bool) -> Bool {
  if b { false } else { true }
}

fn all(arr: [Bool]) -> Bool {
  for b in arr {
    if not(b) {
      return false;
    }
  }
  true
}

fn any(arr: [Bool]) -> Bool {
  for b in arr {
    if b {
      return true;
    }
  }
  false
}

// ============================================================================
// Range and iteration utilities
// ============================================================================

fn range(start: Int, end: Int) -> [Int] {
  let result = [];
  let i = start;
  while i < end {
    result = result ++ [i];
    i = i + 1;
  }
  result
}

fn repeat<T>(value: T, n: Int) -> [T] {
  let result = [];
  let i = 0;
  while i < n {
    result = result ++ [value];
    i = i + 1;
  }
  result
}
