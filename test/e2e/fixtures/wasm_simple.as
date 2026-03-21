// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: WASM backend codegen
// Tests: simple functions that should fully compile to WASM binary

fn add(a: Int, b: Int) -> Int = a + b;

fn factorial(n: Int) -> Int {
  if n <= 1 { 1; } else { n * factorial(n - 1); };
}

fn main() -> () {
  let result = add(3, 4);
  let fact5 = factorial(5);
  ();
}
