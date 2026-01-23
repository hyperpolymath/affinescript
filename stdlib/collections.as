// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Collections - Advanced list, array, and data structure operations

// ============================================================================
// List Operations
// ============================================================================

/// Reverse a list
fn reverse<T>(list: [T]) -> [T] {
  let result = [];
  for x in list {
    result = [x] ++ result;
  }
  result
}

/// Take first n elements from list
fn take<T>(n: Int, list: [T]) -> [T] {
  if n <= 0 || len(list) == 0 {
    []
  } else {
    [list[0]] ++ take(n - 1, list[1:])
  }
}

/// Drop first n elements from list
fn drop<T>(n: Int, list: [T]) -> [T] {
  if n <= 0 {
    list
  } else if len(list) == 0 {
    []
  } else {
    drop(n - 1, list[1:])
  }
}

/// Zip two lists together
fn zip<A, B>(as: [A], bs: [B]) -> [(A, B)] {
  if len(as) == 0 || len(bs) == 0 {
    []
  } else {
    [(as[0], bs[0])] ++ zip(as[1:], bs[1:])
  }
}

/// Unzip a list of pairs
fn unzip<A, B>(pairs: [(A, B)]) -> ([A], [B]) {
  let as = [];
  let bs = [];
  for (a, b) in pairs {
    as = as ++ [a];
    bs = bs ++ [b];
  }
  (as, bs)
}

/// Find first element matching predicate
fn find<T>(pred: T -> Bool, list: [T]) -> Option<T> {
  for x in list {
    if pred(x) {
      return Some(x);
    }
  }
  None
}

/// Check if any element matches predicate
fn any<T>(pred: T -> Bool, list: [T]) -> Bool {
  for x in list {
    if pred(x) {
      return true;
    }
  }
  false
}

/// Check if all elements match predicate
fn all<T>(pred: T -> Bool, list: [T]) -> Bool {
  for x in list {
    if !pred(x) {
      return false;
    }
  }
  true
}

/// Partition list into two lists based on predicate
fn partition<T>(pred: T -> Bool, list: [T]) -> ([T], [T]) {
  let trues = [];
  let falses = [];
  for x in list {
    if pred(x) {
      trues = trues ++ [x];
    } else {
      falses = falses ++ [x];
    }
  }
  (trues, falses)
}

/// Group consecutive equal elements
fn group<T>(list: [T]) -> [[T]] {
  if len(list) == 0 {
    []
  } else {
    let first = list[0];
    let same = filter(fn(x) => x == first, list);
    let different = filter(fn(x) => x != first, list);
    [same] ++ group(different)
  }
}

/// Remove duplicate elements (requires Eq)
fn unique<T>(list: [T]) -> [T] {
  if len(list) == 0 {
    []
  } else {
    let first = list[0];
    let rest = list[1:];
    let filtered = filter(fn(x) => x != first, rest);
    [first] ++ unique(filtered)
  }
}

/// Intersperse element between all elements of list
fn intersperse<T>(sep: T, list: [T]) -> [T] {
  if len(list) <= 1 {
    list
  } else {
    [list[0], sep] ++ intersperse(sep, list[1:])
  }
}

/// Concatenate list of lists
fn concat<T>(lists: [[T]]) -> [T] {
  let result = [];
  for list in lists {
    result = result ++ list;
  }
  result
}

/// Flat map (map then concat)
fn flat_map<A, B>(f: A -> [B], list: [A]) -> [B] {
  concat(map(f, list))
}

// ============================================================================
// Array Operations
// ============================================================================

/// Fill array with value
fn array_fill<T>(size: Int, value: T) -> [T] {
  let arr = [];
  let mut i = 0;
  while i < size {
    arr = arr ++ [value];
    i = i + 1;
  }
  arr
}

/// Array from range
fn range(start: Int, end: Int) -> [Int] {
  if start >= end {
    []
  } else {
    [start] ++ range(start + 1, end)
  }
}

/// Array from range with step
fn range_step(start: Int, end: Int, step: Int) -> [Int] {
  if step <= 0 || start >= end {
    []
  } else {
    [start] ++ range_step(start + step, end, step)
  }
}

// ============================================================================
// Sorting and Searching
// ============================================================================

/// Sort list (requires Ord)
fn sort<T>(list: [T]) -> [T] {
  if len(list) <= 1 {
    list
  } else {
    let pivot = list[0];
    let rest = list[1:];
    let smaller = filter(fn(x) => x < pivot, rest);
    let greater = filter(fn(x) => x >= pivot, rest);
    sort(smaller) ++ [pivot] ++ sort(greater)
  }
}

/// Binary search in sorted array
fn binary_search<T>(target: T, arr: [T]) -> Option<Int> {
  binary_search_helper(target, arr, 0, len(arr))
}

fn binary_search_helper<T>(target: T, arr: [T], low: Int, high: Int) -> Option<Int> {
  if low >= high {
    None
  } else {
    let mid = (low + high) / 2;
    let mid_val = arr[mid];
    if mid_val == target {
      Some(mid)
    } else if mid_val < target {
      binary_search_helper(target, arr, mid + 1, high)
    } else {
      binary_search_helper(target, arr, low, mid)
    }
  }
}

// ============================================================================
// Set Operations (using lists)
// ============================================================================

/// Union of two sets
fn set_union<T>(a: [T], b: [T]) -> [T] {
  unique(a ++ b)
}

/// Intersection of two sets
fn set_intersection<T>(a: [T], b: [T]) -> [T] {
  filter(fn(x) => any(fn(y) => x == y, b), a)
}

/// Difference of two sets
fn set_difference<T>(a: [T], b: [T]) -> [T] {
  filter(fn(x) => !any(fn(y) => x == y, b), a)
}

/// Check if element is in set
fn set_member<T>(x: T, set: [T]) -> Bool {
  any(fn(y) => x == y, set)
}
