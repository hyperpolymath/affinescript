// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// AffineScript Dependent Types Example

// Length-indexed vectors
type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(head: T, tail: Vec[n, T]) : Vec[n + 1, T]

// Safe head - can only be called on non-empty vectors
total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v {
    Cons(h, _) => h
  }
}

// Safe tail
total fn tail[n: Nat, T](v: Vec[n + 1, T]) -> Vec[n, T] / Pure {
  match v {
    Cons(_, t) => t
  }
}

// Concatenate: result length is sum of input lengths
total fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] / Pure {
  match a {
    Nil => b,
    Cons(h, t) => Cons(h, append(t, b))
  }
}

// Refinement types: non-negative integers
type NonNeg = {x: Int where x >= 0}

// Safe division (divisor must be non-zero)
fn div(a: Int, b: {x: Int where x != 0}) -> Int / Pure {
  a / b
}

// Array indexing with bounds checking
fn get[n: Nat, T](arr: Vec[n, T], idx: {i: Nat where i < n}) -> T / Pure {
  // Type system ensures idx < n, so this is safe
  match arr {
    Cons(h, t) => if idx == 0 { h } else { get(t, idx - 1) },
    Nil => unreachable  // idx < 0 is impossible
  }
}

// Matrix with statically known dimensions
type Matrix[rows: Nat, cols: Nat, T: Type] = Vec[rows, Vec[cols, T]]

// Matrix multiplication with dimension checking
total fn matmul[m: Nat, n: Nat, p: Nat](
  a: Matrix[m, n, Float],
  b: Matrix[n, p, Float]
) -> Matrix[m, p, Float] / Pure {
  // Type system ensures dimensions match: (m×n) × (n×p) = (m×p)
  // Implementation omitted for brevity
  Nil
}

// Example usage
fn main() -> () / IO {
  // Create a vector of length 3
  let v3 = Cons(1, Cons(2, Cons(3, Nil)));  // Vec[3, Int]

  // Safe head access - type checker knows length > 0
  let first = head(v3);  // ✓ type checks

  // This would fail type checking:
  // let empty_head = head(Nil);  // ✗ type error: expected Vec[n+1], got Vec[0]

  // Concatenation preserves length
  let v2 = Cons(4, Cons(5, Nil));  // Vec[2, Int]
  let v5 = append(v3, v2);  // Vec[3+2, Int] = Vec[5, Int]

  // Refinement types prevent bad division
  let result = div(10, 5);  // ✓ 5 != 0 provable
  // let bad = div(10, 0);  // ✗ type error: cannot prove 0 != 0

  println("Dependent types ensure safety at compile time!");
}
