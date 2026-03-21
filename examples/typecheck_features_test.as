// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Test type checker completion features (parseable subset)

// Test 1: Variant constructors and pattern matching
type Result<T, E> = Ok(T) | Err(E)

fn test_variants() -> Result<Int, String> {
  let success = Result::Ok(42);
  let failure = Result::Err("error");

  match success {
    Ok(value) => Ok(value * 2),
    Err(msg) => Err(msg)
  }
}

// Test 2: Pattern matching with constructors
type Option<T> = Some(T) | None

fn test_pattern_matching(opt: Option<Int>) -> Int {
  match opt {
    Some(x) => x,
    None => 0
  }
}

// Test 3: Record spread syntax
fn test_record_spread() -> {x: Int, y: Int, z: Int} {
  let base = {x: 1, y: 2};
  let extended = {...base, z: 3};
  extended
}

// Test 4: Mutable bindings
fn test_mutable_bindings() -> Int {
  let mut counter = 0;
  counter = counter + 1;
  counter = counter + 1;
  counter
}

// Test 5: Immutable bindings are generalized
fn test_generalization() -> (Int, String) {
  let id = fn(x) => x;
  let num = id(42);
  let str = id("hello");
  (num, str)
}

// Test 6: Complex constructor patterns
type Tree<T> = Leaf | Node(T, Tree<T>, Tree<T>)

fn test_constructor_patterns(tree: Tree<Int>) -> Int {
  match tree {
    Leaf => 0,
    Node(value, left, right) => value + test_constructor_patterns(left) + test_constructor_patterns(right)
  }
}

// Test 7: Nested record spread
fn test_nested_spread() -> {a: Int, b: Int, c: Int, d: Int} {
  let r1 = {a: 1, b: 2};
  let r2 = {...r1, c: 3};
  let r3 = {...r2, d: 4};
  r3
}
