// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath

// Core traits for AffineScript standard library

/// Trait for types that can be compared for equality
pub trait Eq {
  /// Check if two values are equal
  pub fn eq(ref self, ref other: Self) -> Bool;

  /// Check if two values are not equal (has default implementation)
  pub fn ne(ref self, ref other: Self) -> Bool {
    return !self.eq(other);
  }
}

/// Ordering result for comparisons
pub enum Ordering {
  Less,
  Equal,
  Greater
}

/// Trait for types that can be ordered (requires Eq)
pub trait Ord: Eq {
  /// Compare two values and return ordering
  pub fn cmp(ref self, ref other: Self) -> Ordering;

  /// Check if self < other
  pub fn lt(ref self, ref other: Self) -> Bool {
    match self.cmp(other) {
      Ordering::Less => return true,
      _ => return false
    }
  }

  /// Check if self <= other
  pub fn le(ref self, ref other: Self) -> Bool {
    match self.cmp(other) {
      Ordering::Less => return true,
      Ordering::Equal => return true,
      _ => return false
    }
  }

  /// Check if self > other
  pub fn gt(ref self, ref other: Self) -> Bool {
    match self.cmp(other) {
      Ordering::Greater => return true,
      _ => return false
    }
  }

  /// Check if self >= other
  pub fn ge(ref self, ref other: Self) -> Bool {
    match self.cmp(other) {
      Ordering::Greater => return true,
      Ordering::Equal => return true,
      _ => return false
    }
  }
}

/// Trait for types that can be hashed
pub trait Hash {
  /// Compute hash value for this type
  pub fn hash(ref self) -> Int;
}

/// Trait for types that can be converted to string
pub trait Display {
  /// Convert value to string representation
  pub fn to_string(ref self) -> String;
}

/// Trait for types that can be debugged (detailed output)
pub trait Debug {
  /// Convert value to debug string representation
  pub fn debug(ref self) -> String;
}

/// Trait for types that can be cloned
pub trait Clone {
  /// Create a copy of this value
  pub fn clone(ref self) -> Self;
}

/// Trait for types that can be converted to another type
pub trait Into[T] {
  /// Convert self into target type
  pub fn into(own self) -> T;
}

/// Trait for types that can be created from another type
pub trait From[T] {
  /// Create self from source type
  pub fn from(own value: T) -> Self;
}

/// Trait for types that have a default value
pub trait Default {
  /// Create default value
  pub fn default() -> Self;
}

/// Trait for iterators
pub trait Iterator {
  type Item;

  /// Get next item from iterator
  pub fn next(mut self) -> Option[Item];

  /// Count remaining items
  pub fn count(mut self) -> Int {
    let mut n = 0;
    while let Some(_) = self.next() {
      n = n + 1;
    }
    return n;
  }

  /// Collect into Vec
  pub fn collect(mut self) -> Vec[Item] {
    let mut result = Vec::new();
    while let Some(item) = self.next() {
      result.push(item);
    }
    return result;
  }
}

// Implementations for built-in types

impl Eq for Int {
  pub fn eq(ref self, ref other: Int) -> Bool {
    return *self == *other;
  }
}

impl Ord for Int {
  pub fn cmp(ref self, ref other: Int) -> Ordering {
    if *self < *other {
      return Ordering::Less;
    } else if *self > *other {
      return Ordering::Greater;
    } else {
      return Ordering::Equal;
    }
  }
}

impl Hash for Int {
  pub fn hash(ref self) -> Int {
    return *self;
  }
}

impl Display for Int {
  pub fn to_string(ref self) -> String {
    // TODO: Implement actual int-to-string conversion
    return "int";
  }
}

impl Eq for Bool {
  pub fn eq(ref self, ref other: Bool) -> Bool {
    return *self == *other;
  }
}

impl Display for Bool {
  pub fn to_string(ref self) -> Bool {
    if *self {
      return "true";
    } else {
      return "false";
    }
  }
}
