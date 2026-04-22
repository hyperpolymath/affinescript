// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Test file to verify type checker completion features

// Test 1: Unsafe operations
fn test_unsafe_ops() -> Int {
  unsafe {
    // UnsafeRead: dereference a pointer
    let ptr: &Int = &42;
    UnsafeRead(ptr);

    // UnsafeWrite: write through mutable pointer
    let mut x = 10;
    let mut_ptr: &mut Int = &mut x;
    UnsafeWrite(mut_ptr, 20);

    // UnsafeOffset: pointer arithmetic
    let arr = [1, 2, 3];
    let ptr2 = &arr[0];
    UnsafeOffset(ptr2, 1);

    // UnsafeTransmute: reinterpret bits
    UnsafeTransmute(Int, Float, 42);

    // UnsafeForget: prevent destructor
    let resource = acquire();
    UnsafeForget(resource);

    // UnsafeAssume: add constraint
    UnsafeAssume(n > 0);

    x
  }
}

// Test 2: Variant constructors
type Result<T, E> = Ok(T) | Err(E)

fn test_variants() -> Result<Int, String> {
  let success = Result::Ok(42);
  let failure = Result::Err("error");

  match success {
    Ok(value) => Ok(value * 2),
    Err(msg) => Err(msg)
  }
}

// Test 3: Pattern matching with constructors
type Option<T> = Some(T) | None

fn test_pattern_matching(opt: Option<Int>) -> Int {
  match opt {
    Some(x) => x,
    None => 0
  }
}

// Test 4: Record spread syntax
fn test_record_spread() -> {x: Int, y: Int, z: Int} {
  let base = {x: 1, y: 2};
  let extended = {...base, z: 3};
  extended
}

// Test 5: Mutable bindings
fn test_mutable_bindings() -> Int {
  let mut counter = 0;  // Wrapped in TMut, not generalized
  counter = counter + 1;
  counter = counter + 1;
  counter  // Should be 2
}

// Test 6: Immutable bindings are generalized
fn test_generalization() -> (Int, String) {
  let id = fn(x) => x;  // Generalized: forall a. a -> a
  let num = id(42);     // Instantiated to Int -> Int
  let str = id("hi");   // Instantiated to String -> String
  (num, str)
}

// Test 7: Complex constructor patterns
type Tree<T> = Leaf | Node(T, Tree<T>, Tree<T>)

fn test_constructor_patterns(tree: Tree<Int>) -> Int {
  match tree {
    Leaf => 0,
    Node(value, left, right) => value + count(left) + count(right)
  }
}

fn count<T>(tree: Tree<T>) -> Int {
  match tree {
    Leaf => 0,
    Node(_, left, right) => 1 + count(left) + count(right)
  }
}

// Test 8: Nested record spread
fn test_nested_spread() -> {a: Int, b: Int, c: Int, d: Int} {
  let r1 = {a: 1, b: 2};
  let r2 = {...r1, c: 3};
  let r3 = {...r2, d: 4};
  r3
}
