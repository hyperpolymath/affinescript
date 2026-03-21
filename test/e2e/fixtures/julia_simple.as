// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: Julia backend codegen
// Tests: simple functions that should fully compile to Julia

fn add(a: Int, b: Int) -> Int = a + b;

fn fibonacci(n: Int) -> Int {
  if n <= 1 { n; } else { fibonacci(n - 1) + fibonacci(n - 2); };
}

fn greet(name: String) -> String = "Hello";
