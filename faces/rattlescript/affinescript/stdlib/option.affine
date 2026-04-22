// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Option - Utilities for Option<T> type

// Option type is defined in prelude, but here are utilities

// ============================================================================
// Combinators
// ============================================================================

/// Map over Some value
fn map<T, U>(f: T -> U, opt: Option<T>) -> Option<U> {
  match opt {
    Some(value) => Some(f(value)),
    None => None
  }
}

/// Apply function in Option to value in Option
fn apply<T, U>(f_opt: Option<T -> U>, value_opt: Option<T>) -> Option<U> {
  match (f_opt, value_opt) {
    (Some(f), Some(value)) => Some(f(value)),
    _ => None
  }
}

/// Flat map (bind) over Option
fn flat_map<T, U>(f: T -> Option<U>, opt: Option<T>) -> Option<U> {
  match opt {
    Some(value) => f(value),
    None => None
  }
}

/// Also known as 'and_then'
fn and_then<T, U>(opt: Option<T>, f: T -> Option<U>) -> Option<U> {
  flat_map(f, opt)
}

/// Or else - use alternative Option if None
fn or_else<T>(opt: Option<T>, alternative: Option<T>) -> Option<T> {
  match opt {
    Some(_) => opt,
    None => alternative
  }
}

/// Or else computed - compute alternative only if None
fn or_else_with<T>(opt: Option<T>, f: () -> Option<T>) -> Option<T> {
  match opt {
    Some(_) => opt,
    None => f()
  }
}

// ============================================================================
// Queries
// ============================================================================

/// Check if Option is Some
fn is_some<T>(opt: Option<T>) -> Bool {
  match opt {
    Some(_) => true,
    None => false
  }
}

/// Check if Option is None
fn is_none<T>(opt: Option<T>) -> Bool {
  match opt {
    Some(_) => false,
    None => true
  }
}

/// Check if Option contains specific value
fn contains<T>(opt: Option<T>, value: T) -> Bool {
  match opt {
    Some(x) => x == value,
    None => false
  }
}

// ============================================================================
// Extractors
// ============================================================================

/// Unwrap Some value or panic
fn unwrap<T>(opt: Option<T>) -> T {
  match opt {
    Some(value) => value,
    None => panic("Called unwrap on None")
  }
}

/// Unwrap with custom error message
fn expect<T>(opt: Option<T>, msg: String) -> T {
  match opt {
    Some(value) => value,
    None => panic(msg)
  }
}

/// Unwrap or provide default value
fn unwrap_or<T>(opt: Option<T>, default: T) -> T {
  match opt {
    Some(value) => value,
    None => default
  }
}

/// Unwrap or compute default value
fn unwrap_or_else<T>(opt: Option<T>, f: () -> T) -> T {
  match opt {
    Some(value) => value,
    None => f()
  }
}

/// Get value or return from function with default
fn ok_or<T, E>(opt: Option<T>, err: E) -> Result<T, E> {
  match opt {
    Some(value) => Ok(value),
    None => Err(err)
  }
}

/// Get value or compute error
fn ok_or_else<T, E>(opt: Option<T>, f: () -> E) -> Result<T, E> {
  match opt {
    Some(value) => Ok(value),
    None => Err(f())
  }
}

// ============================================================================
// Filtering and Zipping
// ============================================================================

/// Filter Option based on predicate
fn filter<T>(pred: T -> Bool, opt: Option<T>) -> Option<T> {
  match opt {
    Some(value) => if pred(value) { Some(value) } else { None },
    None => None
  }
}

/// Zip two Options together
fn zip<A, B>(a: Option<A>, b: Option<B>) -> Option<(A, B)> {
  match (a, b) {
    (Some(x), Some(y)) => Some((x, y)),
    _ => None
  }
}

/// Zip with function
fn zip_with<A, B, C>(f: (A, B) -> C, a: Option<A>, b: Option<B>) -> Option<C> {
  match (a, b) {
    (Some(x), Some(y)) => Some(f(x, y)),
    _ => None
  }
}

/// Unzip Option of pair
fn unzip<A, B>(opt: Option<(A, B)>) -> (Option<A>, Option<B>) {
  match opt {
    Some((a, b)) => (Some(a), Some(b)),
    None => (None, None)
  }
}

// ============================================================================
// Collections
// ============================================================================

/// Transpose Option of list to list of Option
fn transpose<T>(opt: Option<[T]>) -> [Option<T>] {
  match opt {
    Some(list) => map(fn(x) => Some(x), list),
    None => []
  }
}

/// Collect list of Options into Option of list (None on any None)
fn collect<T>(opts: [Option<T>]) -> Option<[T]> {
  let values = [];
  for opt in opts {
    match opt {
      Some(value) => values = values ++ [value],
      None => return None
    }
  }
  Some(values)
}

/// Filter out Nones from list
fn cat_options<T>(opts: [Option<T>]) -> [T] {
  let values = [];
  for opt in opts {
    match opt {
      Some(value) => values = values ++ [value],
      None => {}
    }
  }
  values
}

/// Map list with function returning Option, filtering Nones
fn map_filter<T, U>(f: T -> Option<U>, list: [T]) -> [U] {
  let results = map(f, list);
  cat_options(results)
}

/// Find first Some in list of Options
fn first_some<T>(opts: [Option<T>]) -> Option<T> {
  for opt in opts {
    match opt {
      Some(value) => return Some(value),
      None => {}
    }
  }
  None
}

/// Get head of list as Option
fn head<T>(list: [T]) -> Option<T> {
  if len(list) > 0 {
    Some(list[0])
  } else {
    None
  }
}

/// Get tail of list as Option
fn tail<T>(list: [T]) -> Option<[T]> {
  if len(list) > 0 {
    Some(list[1:])
  } else {
    None
  }
}

/// Get last element of list as Option
fn last<T>(list: [T]) -> Option<T> {
  let n = len(list);
  if n > 0 {
    Some(list[n - 1])
  } else {
    None
  }
}

/// Get element at index as Option
fn get<T>(list: [T], index: Int) -> Option<T> {
  if index >= 0 && index < len(list) {
    Some(list[index])
  } else {
    None
  }
}

// ============================================================================
// Boolean Options
// ============================================================================

/// Convert bool to Option
fn bool_to_option(b: Bool) -> Option<()> {
  if b {
    Some(())
  } else {
    None
  }
}

/// AND operation on Options (both must be Some)
fn option_and<T, U>(a: Option<T>, b: Option<U>) -> Option<(T, U)> {
  zip(a, b)
}

/// OR operation on Options (first Some wins)
fn option_or<T>(a: Option<T>, b: Option<T>) -> Option<T> {
  or_else(a, b)
}

/// XOR operation on Options (exactly one must be Some)
fn option_xor<T>(a: Option<T>, b: Option<T>) -> Option<T> {
  match (a, b) {
    (Some(x), None) => Some(x),
    (None, Some(y)) => Some(y),
    _ => None
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Flatten nested Option
fn flatten<T>(opt: Option<Option<T>>) -> Option<T> {
  match opt {
    Some(inner) => inner,
    None => None
  }
}

/// Replace None with provided Option
fn replace_none<T>(opt: Option<T>, replacement: Option<T>) -> Option<T> {
  or_else(opt, replacement)
}

/// Take value from Option, leaving None
fn take<T>(opt: &mut Option<T>) -> Option<T> {
  let result = opt;
  opt = None;
  result
}

/// Insert value into Option if None
fn get_or_insert<T>(opt: &mut Option<T>, value: T) -> T {
  match opt {
    Some(x) => x,
    None => {
      opt = Some(value);
      value
    }
  }
}
