// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Parser test for unsafe operations

fn test_unsafe_operations() -> () {
  let ptr = 42;

  // UnsafeRead
  unsafe {
    read(ptr)
  };

  // UnsafeWrite
  unsafe {
    write(ptr, 100)
  };

  // UnsafeOffset
  unsafe {
    offset(ptr, 4)
  };

  // UnsafeForget
  unsafe {
    forget(ptr)
  };

  // UnsafeTransmute
  unsafe {
    transmute<Int, Float>(ptr)
  };

  // UnsafeAssume
  unsafe {
    assume(5 > 3)
  };

  // Multiple unsafe ops in one block
  unsafe {
    read(ptr)
    write(ptr, 200)
    assume(ptr > 0 && ptr < 1000)
  }
}
