// SPDX-License-Identifier: PMPL-1.0-or-later
// Test simple unsafe block

fn test() -> () {
  let ptr = 42;

  unsafe {
    read(ptr);
  }
}
