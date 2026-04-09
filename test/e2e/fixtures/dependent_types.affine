// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: dependent types
// Tests: parsing of Nat kind, parameterised types

enum Vec[n: Nat, T: Type] {
  Nil,
  Cons(T, Vec[n, T])
}

// Dependent function signatures with Nat kind parameters
total fn length[n: Nat, T](v: Vec[n, T]) -> Nat = n;

// Type-level arithmetic in type positions
type Matrix[rows: Nat, cols: Nat, T: Type] = Vec[rows, Vec[cols, T]];
