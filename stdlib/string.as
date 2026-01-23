// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// AffineScript Standard Library - String utilities

// Note: Many of these will require builtin implementations
// For now, they serve as API documentation

// ============================================================================
// String operations
// ============================================================================

// Check if string is empty
fn is_empty(s: String) -> Bool {
  len(s) == 0
}

// Get character at index (returns Option<Char>)
fn char_at(s: String, idx: Int) -> Option<Char> {
  if idx >= 0 && idx < len(s) {
    // TODO: Needs builtin string indexing
    Some('?')
  } else {
    None
  }
}

// Convert string to lowercase
// TODO: Requires builtin implementation
fn to_lowercase(s: String) -> String {
  s  // Placeholder
}

// Convert string to uppercase
// TODO: Requires builtin implementation
fn to_uppercase(s: String) -> String {
  s  // Placeholder
}

// Trim whitespace from both ends
// TODO: Requires builtin implementation
fn trim(s: String) -> String {
  s  // Placeholder
}

// ============================================================================
// String checking
// ============================================================================

// Check if string starts with prefix
// TODO: Requires builtin implementation
fn starts_with(s: String, prefix: String) -> Bool {
  false  // Placeholder
}

// Check if string ends with suffix
// TODO: Requires builtin implementation
fn ends_with(s: String, suffix: String) -> Bool {
  false  // Placeholder
}

// Check if string contains substring
// TODO: Requires builtin implementation
fn contains(s: String, substr: String) -> Bool {
  false  // Placeholder
}

// ============================================================================
// String manipulation
// ============================================================================

// Concatenate two strings
// TODO: Requires builtin operator or implementation
fn concat(a: String, b: String) -> String {
  a  // Placeholder - should be a ++ b
}

// Repeat string n times
fn repeat(s: String, n: Int) -> String {
  let result = "";
  let i = 0;
  while i < n {
    result = concat(result, s);
    i = i + 1;
  }
  result
}

// Split string by delimiter
// TODO: Requires builtin implementation
fn split(s: String, delimiter: String) -> [String] {
  [s]  // Placeholder
}

// Join array of strings with separator
fn join(arr: [String], separator: String) -> String {
  if len(arr) == 0 {
    return "";
  }

  let result = arr[0];
  let i = 1;
  while i < len(arr) {
    result = concat(result, concat(separator, arr[i]));
    i = i + 1;
  }
  result
}

// ============================================================================
// String conversion
// ============================================================================

// Convert integer to string
// TODO: Requires builtin implementation
fn int_to_string(n: Int) -> String {
  "0"  // Placeholder
}

// Parse string to integer
// TODO: Requires builtin implementation
fn parse_int(s: String) -> Option<Int> {
  None  // Placeholder
}

// Convert float to string
// TODO: Requires builtin implementation
fn float_to_string(f: Float) -> String {
  "0.0"  // Placeholder
}

// Parse string to float
// TODO: Requires builtin implementation
fn parse_float(s: String) -> Option<Float> {
  None  // Placeholder
}
