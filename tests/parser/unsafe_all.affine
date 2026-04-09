// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Comprehensive unsafe operations parser test

fn test_all_unsafe_ops() -> () {
  let ptr = 100;
  let value = 42;

  // UnsafeRead
  unsafe {
    read(ptr);
  };

  // UnsafeWrite
  unsafe {
    write(ptr, value);
  };

  // UnsafeOffset
  unsafe {
    offset(ptr, 4);
  };

  // UnsafeForget
  unsafe {
    forget(value);
  };

  // UnsafeTransmute
  unsafe {
    transmute<Int, Float>(value);
  };

  // UnsafeAssume
  unsafe {
    assume(ptr > 0);
  };

  // Multiple operations in one block
  unsafe {
    read(ptr);
    write(ptr, 200);
    assume(ptr > 0 && ptr < 1000);
  };
}
