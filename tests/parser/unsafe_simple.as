// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// Test simple unsafe block

fn test() -> () {
  let ptr = 42;

  unsafe {
    read(ptr);
  }
}
