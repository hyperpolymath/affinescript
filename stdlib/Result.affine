// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Result - Error handling utilities

module Result;

// Result type is built-in as:
// type Result[T, E] = Ok(T) | Err(E);

// Check if result is Ok
pub fn is_ok[T, E](r: Result[T, E]) -> Bool {
  return match r {
    Ok(_) => { return true; },
    Err(_) => { return false; }
  };
}

// Check if result is Err
pub fn is_err[T, E](r: Result[T, E]) -> Bool {
  return match r {
    Ok(_) => { return false; },
    Err(_) => { return true; }
  };
}

// Get value or panic
pub fn unwrap[T, E](own r: Result[T, E]) -> T {
  return match r {
    Ok(v) => { return v; },
    Err(_) => {
      // TODO: panic with error message
      return 0;  // Placeholder
    }
  };
}

// Get value or default
pub fn unwrap_or[T, E](own r: Result[T, E], default: T) -> T {
  return match r {
    Ok(v) => { return v; },
    Err(_) => { return default; }
  };
}

// Get error or panic
pub fn unwrap_err[T, E](own r: Result[T, E]) -> E {
  return match r {
    Ok(_) => {
      // TODO: panic
      return 0;  // Placeholder
    },
    Err(e) => { return e; }
  };
}

// Map over Ok value
pub fn map[T, U, E](own r: Result[T, E], f: fn(T) -> U) -> Result[U, E] {
  return match r {
    Ok(v) => { return Ok(f(v)); },
    Err(e) => { return Err(e); }
  };
}

// Map over Err value
pub fn map_err[T, E, F](own r: Result[T, E], f: fn(E) -> F) -> Result[T, F] {
  return match r {
    Ok(v) => { return Ok(v); },
    Err(e) => { return Err(f(e)); }
  };
}

// Chain operations (flatMap/bind)
pub fn and_then[T, U, E](own r: Result[T, E], f: fn(T) -> Result[U, E]) -> Result[U, E] {
  return match r {
    Ok(v) => { return f(v); },
    Err(e) => { return Err(e); }
  };
}

// Convert Result to Option
pub fn ok[T, E](own r: Result[T, E]) -> Option[T] {
  return match r {
    Ok(v) => { return Some(v); },
    Err(_) => { return None; }
  };
}

pub fn err[T, E](own r: Result[T, E]) -> Option[E] {
  return match r {
    Ok(_) => { return None; },
    Err(e) => { return Some(e); }
  };
}
