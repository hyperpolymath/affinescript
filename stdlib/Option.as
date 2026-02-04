// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

// Option - Optional value utilities

module Option;

// Option type is built-in as:
// type Option[T] = Some(T) | None;

// Check if option has a value
pub fn is_some[T](opt: Option[T]) -> Bool {
  return match opt {
    Some(_) => { return true; },
    None => { return false; }
  };
}

// Check if option is empty
pub fn is_none[T](opt: Option[T]) -> Bool {
  return match opt {
    Some(_) => { return false; },
    None => { return true; }
  };
}

// Get value or panic
pub fn unwrap[T](own opt: Option[T]) -> T {
  return match opt {
    Some(v) => { return v; },
    None => {
      // TODO: panic
      return 0;  // Placeholder
    }
  };
}

// Get value or default
pub fn unwrap_or[T](own opt: Option[T], default: T) -> T {
  return match opt {
    Some(v) => { return v; },
    None => { return default; }
  };
}

// Get value or compute default
pub fn unwrap_or_else[T](own opt: Option[T], f: fn() -> T) -> T {
  return match opt {
    Some(v) => { return v; },
    None => { return f(); }
  };
}

// Map over value
pub fn map[T, U](own opt: Option[T], f: fn(T) -> U) -> Option[U] {
  return match opt {
    Some(v) => { return Some(f(v)); },
    None => { return None; }
  };
}

// Map or return default
pub fn map_or[T, U](own opt: Option[T], default: U, f: fn(T) -> U) -> U {
  return match opt {
    Some(v) => { return f(v); },
    None => { return default; }
  };
}

// Chain operations (flatMap/bind)
pub fn and_then[T, U](own opt: Option[T], f: fn(T) -> Option[U]) -> Option[U] {
  return match opt {
    Some(v) => { return f(v); },
    None => { return None; }
  };
}

// Return opt if it has a value, otherwise return other
pub fn or[T](own opt: Option[T], own other: Option[T]) -> Option[T] {
  return match opt {
    Some(_) => { return opt; },
    None => { return other; }
  };
}

// Return opt if it has a value, otherwise compute alternative
pub fn or_else[T](own opt: Option[T], f: fn() -> Option[T]) -> Option[T] {
  return match opt {
    Some(_) => { return opt; },
    None => { return f(); }
  };
}

// Filter value by predicate
pub fn filter[T](own opt: Option[T], pred: fn(ref T) -> Bool) -> Option[T] {
  return match opt {
    Some(v) => {
      return if pred(v) { return Some(v); } else { return None; };
    },
    None => { return None; }
  };
}

// Convert Option to Result
pub fn ok_or[T, E](own opt: Option[T], err: E) -> Result[T, E] {
  return match opt {
    Some(v) => { return Ok(v); },
    None => { return Err(err); }
  };
}

pub fn ok_or_else[T, E](own opt: Option[T], f: fn() -> E) -> Result[T, E] {
  return match opt {
    Some(v) => { return Ok(v); },
    None => { return Err(f()); }
  };
}
