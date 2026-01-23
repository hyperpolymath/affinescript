// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Result - Error handling with Result<T, E> type

// Result type is defined in prelude, but here are utilities

// ============================================================================
// Constructors and Conversions
// ============================================================================

/// Convert Option to Result
fn option_to_result<T, E>(opt: Option<T>, err: E) -> Result<T, E> {
  match opt {
    Some(value) => Ok(value),
    None => Err(err)
  }
}

/// Convert Result to Option (discarding error)
fn result_to_option<T, E>(result: Result<T, E>) -> Option<T> {
  match result {
    Ok(value) => Some(value),
    Err(_) => None
  }
}

// ============================================================================
// Combinators
// ============================================================================

/// Map over Ok value
fn map_ok<T, U, E>(f: T -> U, result: Result<T, E>) -> Result<U, E> {
  match result {
    Ok(value) => Ok(f(value)),
    Err(e) => Err(e)
  }
}

/// Map over Err value
fn map_err<T, E, F>(f: E -> F, result: Result<T, E>) -> Result<T, F> {
  match result {
    Ok(value) => Ok(value),
    Err(e) => Err(f(e))
  }
}

/// Apply function in Result to value in Result
fn apply<T, U, E>(f_result: Result<T -> U, E>, value_result: Result<T, E>) -> Result<U, E> {
  match (f_result, value_result) {
    (Ok(f), Ok(value)) => Ok(f(value)),
    (Err(e), _) => Err(e),
    (_, Err(e)) => Err(e)
  }
}

/// Flat map (bind) over Result
fn flat_map<T, U, E>(f: T -> Result<U, E>, result: Result<T, E>) -> Result<U, E> {
  match result {
    Ok(value) => f(value),
    Err(e) => Err(e)
  }
}

/// Also known as 'and_then'
fn and_then<T, U, E>(result: Result<T, E>, f: T -> Result<U, E>) -> Result<U, E> {
  flat_map(f, result)
}

/// Or else - use alternative Result on error
fn or_else<T, E, F>(result: Result<T, E>, f: E -> Result<T, F>) -> Result<T, F> {
  match result {
    Ok(value) => Ok(value),
    Err(e) => f(e)
  }
}

// ============================================================================
// Queries
// ============================================================================

/// Check if Result is Ok
fn is_ok<T, E>(result: Result<T, E>) -> Bool {
  match result {
    Ok(_) => true,
    Err(_) => false
  }
}

/// Check if Result is Err
fn is_err<T, E>(result: Result<T, E>) -> Bool {
  match result {
    Ok(_) => false,
    Err(_) => true
  }
}

// ============================================================================
// Extractors
// ============================================================================

/// Unwrap Ok value or panic
fn unwrap<T, E>(result: Result<T, E>) -> T {
  match result {
    Ok(value) => value,
    Err(_) => panic("Called unwrap on Err value")
  }
}

/// Unwrap Err value or panic
fn unwrap_err<T, E>(result: Result<T, E>) -> E {
  match result {
    Ok(_) => panic("Called unwrap_err on Ok value"),
    Err(e) => e
  }
}

/// Unwrap with custom error message
fn expect<T, E>(result: Result<T, E>, msg: String) -> T {
  match result {
    Ok(value) => value,
    Err(_) => panic(msg)
  }
}

/// Unwrap or provide default value
fn unwrap_or<T, E>(result: Result<T, E>, default: T) -> T {
  match result {
    Ok(value) => value,
    Err(_) => default
  }
}

/// Unwrap or compute default value
fn unwrap_or_else<T, E>(result: Result<T, E>, f: E -> T) -> T {
  match result {
    Ok(value) => value,
    Err(e) => f(e)
  }
}

// ============================================================================
// Collections
// ============================================================================

/// Transpose Option of Result to Result of Option
fn transpose_opt<T, E>(opt: Option<Result<T, E>>) -> Result<Option<T>, E> {
  match opt {
    Some(Ok(value)) => Ok(Some(value)),
    Some(Err(e)) => Err(e),
    None => Ok(None)
  }
}

/// Collect list of Results into Result of list (fails on first error)
fn collect<T, E>(results: [Result<T, E>]) -> Result<[T], E> {
  let values = [];
  for result in results {
    match result {
      Ok(value) => values = values ++ [value],
      Err(e) => return Err(e)
    }
  }
  Ok(values)
}

/// Partition list of Results into successes and failures
fn partition_results<T, E>(results: [Result<T, E>]) -> ([T], [E]) {
  let oks = [];
  let errs = [];
  for result in results {
    match result {
      Ok(value) => oks = oks ++ [value],
      Err(e) => errs = errs ++ [e]
    }
  }
  (oks, errs)
}

/// Try to apply function to all elements, collecting errors
fn try_map<T, U, E>(f: T -> Result<U, E>, list: [T]) -> Result<[U], E> {
  let results = map(f, list);
  collect(results)
}

// ============================================================================
// Boolean Results
// ============================================================================

/// Convert bool to Result
fn bool_to_result(b: Bool, err: E) -> Result<(), E> {
  if b {
    Ok(())
  } else {
    Err(err)
  }
}

/// AND operation on Results (both must be Ok)
fn result_and<T, U, E>(a: Result<T, E>, b: Result<U, E>) -> Result<(T, U), E> {
  match (a, b) {
    (Ok(x), Ok(y)) => Ok((x, y)),
    (Err(e), _) => Err(e),
    (_, Err(e)) => Err(e)
  }
}

/// OR operation on Results (first Ok wins)
fn result_or<T, E>(a: Result<T, E>, b: Result<T, E>) -> Result<T, E> {
  match a {
    Ok(value) => Ok(value),
    Err(_) => b
  }
}

// ============================================================================
// Error Handling Patterns
// ============================================================================

/// Try block emulation - execute function and catch panics as Err
fn try<T>(f: () -> T) -> Result<T, String> {
  // TODO: Requires exception handling support
  try {
    Ok(f())
  } catch {
    RuntimeError(msg) => Err(msg),
    _ => Err("Unknown error")
  }
}

/// Retry operation n times on failure
fn retry<T, E>(n: Int, f: () -> Result<T, E>) -> Result<T, E> {
  let result = f();
  match result {
    Ok(_) => result,
    Err(e) => {
      if n <= 1 {
        Err(e)
      } else {
        retry(n - 1, f)
      }
    }
  }
}
