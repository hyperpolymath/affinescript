// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// AffineScript Standard Library - String utilities
//
// String operations backed by interpreter builtins:
//   string_get(s, idx)    -> Char          (character at index)
//   string_sub(s, start, length) -> String (substring extraction)
//   string_find(s, needle) -> Int          (-1 if not found)
//   to_lowercase(s)       -> String        (ASCII lowercase)
//   to_uppercase(s)       -> String        (ASCII uppercase)
//   trim(s)               -> String        (strip leading/trailing whitespace)
//   int_to_string(n)      -> String        (integer to decimal string)
//   float_to_string(f)    -> String        (float to string)
//   parse_int(s)          -> Option<Int>   (decimal string to integer)
//   parse_float(s)        -> Option<Float> (string to float)
//   char_to_int(c)        -> Int           (character to ASCII code point)
//   int_to_char(n)        -> Char          (ASCII code point to character)
//   show(v)               -> String        (any value to debug string)

// ============================================================================
// String inspection
// ============================================================================

/// Check if string is empty
fn is_empty(s: String) -> Bool {
  len(s) == 0
}

/// Get character at index, returning None for out-of-bounds access
fn char_at(s: String, idx: Int) -> Option<Char> {
  if idx >= 0 && idx < len(s) {
    Some(string_get(s, idx))
  } else {
    None
  }
}

/// Get the length of a string (alias for len)
/// Conforms to aLib string/length spec v1.0
fn length(s: String) -> Int {
  len(s)
}

// ============================================================================
// Case conversion (delegated to builtins)
// ============================================================================

// to_lowercase(s: String) -> String   — builtin
// to_uppercase(s: String) -> String   — builtin
// trim(s: String) -> String           — builtin

// ============================================================================
// String searching
// ============================================================================

/// Check if string starts with the given prefix
fn starts_with(s: String, prefix: String) -> Bool {
  let plen = len(prefix);
  if plen > len(s) {
    false
  } else {
    string_sub(s, 0, plen) == prefix
  }
}

/// Check if string ends with the given suffix
fn ends_with(s: String, suffix: String) -> Bool {
  let slen = len(s);
  let sfxlen = len(suffix);
  if sfxlen > slen {
    false
  } else {
    string_sub(s, slen - sfxlen, sfxlen) == suffix
  }
}

/// Check if string contains a substring
fn contains(s: String, substr: String) -> Bool {
  string_find(s, substr) >= 0
}

/// Find the first index of a substring, or -1 if not found
fn index_of(s: String, substr: String) -> Int {
  string_find(s, substr)
}

// ============================================================================
// String manipulation
// ============================================================================

/// Concatenate two strings
/// Conforms to aLib string/concat spec v1.0
fn concat(a: String, b: String) -> String {
  a ++ b
}

/// Extract substring from start (inclusive) to end (exclusive)
/// Conforms to aLib string/substring spec v1.0
fn substring(s: String, start: Int, end: Int) -> String {
  let slen = len(s);
  let clamped_start = if start < 0 { 0 } else if start > slen { slen } else { start };
  let clamped_end = if end < clamped_start { clamped_start } else if end > slen { slen } else { end };
  string_sub(s, clamped_start, clamped_end - clamped_start)
}

/// Repeat a string n times
fn repeat(s: String, n: Int) -> String {
  let result = "";
  let i = 0;
  while i < n {
    result = concat(result, s);
    i = i + 1;
  }
  result
}

/// Split string by a delimiter
fn split(s: String, delimiter: String) -> [String] {
  let slen = len(s);
  let dlen = len(delimiter);

  if dlen == 0 {
    // Split into individual characters
    let result = [];
    let i = 0;
    while i < slen {
      result = result ++ [string_sub(s, i, 1)];
      i = i + 1;
    }
    return result;
  }

  let result = [];
  let current_start = 0;
  let i = 0;

  while i <= slen - dlen {
    if string_sub(s, i, dlen) == delimiter {
      result = result ++ [string_sub(s, current_start, i - current_start)];
      current_start = i + dlen;
      i = i + dlen;
    } else {
      i = i + 1;
    }
  }

  // Append the remaining tail
  result = result ++ [string_sub(s, current_start, slen - current_start)];
  result
}

/// Join an array of strings with a separator
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

/// Replace all occurrences of `from` with `to_str` in a string
fn replace(s: String, from: String, to_str: String) -> String {
  join(split(s, from), to_str)
}

/// Reverse a string
fn reverse_string(s: String) -> String {
  let slen = len(s);
  let result = "";
  let i = slen - 1;
  while i >= 0 {
    result = concat(result, string_sub(s, i, 1));
    i = i - 1;
  }
  result
}

/// Pad a string on the left to reach a target length
fn pad_left(s: String, target_len: Int, pad_char: String) -> String {
  let slen = len(s);
  if slen >= target_len {
    s
  } else {
    concat(repeat(pad_char, target_len - slen), s)
  }
}

/// Pad a string on the right to reach a target length
fn pad_right(s: String, target_len: Int, pad_char: String) -> String {
  let slen = len(s);
  if slen >= target_len {
    s
  } else {
    concat(s, repeat(pad_char, target_len - slen))
  }
}

// ============================================================================
// String conversion (delegated to builtins)
// ============================================================================

// int_to_string(n: Int) -> String         — builtin
// float_to_string(f: Float) -> String     — builtin
// parse_int(s: String) -> Option<Int>     — builtin
// parse_float(s: String) -> Option<Float> — builtin

// ============================================================================
// Character classification
// ============================================================================

/// Check if character is an ASCII digit (0-9)
fn is_digit(c: Char) -> Bool {
  let code = char_to_int(c);
  code >= 48 && code <= 57
}

/// Check if character is an ASCII letter (a-z, A-Z)
fn is_alpha(c: Char) -> Bool {
  let code = char_to_int(c);
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
}

/// Check if character is alphanumeric
fn is_alphanumeric(c: Char) -> Bool {
  is_digit(c) || is_alpha(c)
}

/// Check if character is ASCII whitespace (space, tab, newline, carriage return)
fn is_whitespace(c: Char) -> Bool {
  let code = char_to_int(c);
  code == 32 || code == 9 || code == 10 || code == 13
}
