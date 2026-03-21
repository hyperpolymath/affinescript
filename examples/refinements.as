// Refinement types in AffineScript

// Positive integers - enforced at compile time
type PosInt = Int where (self > 0)

// Non-zero integers for division
type NonZero = Int where (self != 0)

// Percentage values (0-100)
type Percentage = Int where (self >= 0 && self <= 100)

// Safe division - cannot divide by zero
fn safeDiv(a: Int, b: NonZero) -> Int / Pure {
  a / b
}

// Square root only on non-negative numbers
fn sqrt(n: Int where (n >= 0)) -> Float / Pure {
  // Implementation
  0.0
}

// Array indexing with bounds checking at compile time
total fn safeGet[n: Nat, T](
  arr: ref Vec[n, T],
  i: Nat where (i < n)
) -> ref T / Pure {
  match (arr, i) {
    (Cons(h, _), 0) => ref h,
    (Cons(_, t), _) => safeGet(ref t, i - 1)
  }
}

// Bounded integers
type Age = Int where (self >= 0 && self <= 150)

type Port = Int where (self >= 0 && self <= 65535)

// Create validated values
fn mkPosInt(n: Int) -> Option[PosInt] / Pure {
  if n > 0 {
    Some(n)  // Compiler verifies the refinement
  } else {
    None
  }
}

fn mkPercentage(n: Int) -> Option[Percentage] / Pure {
  if n >= 0 && n <= 100 {
    Some(n)
  } else {
    None
  }
}

// Using unsafe assume when you know better than the compiler
fn unsafePos(n: Int) -> PosInt / Pure {
  unsafe {
    assume(n > 0)
  }
  n
}

// Length-indexed strings
type BoundedString[max: Nat] = String where (len(self) <= max)

type Username = BoundedString[32]
type Email = BoundedString[254]

// Matrices with compile-time dimension checking
type Matrix[rows: Nat, cols: Nat, T: Type] = Vec[rows, Vec[cols, T]]

// Matrix multiplication with dimension constraints
total fn matmul[m: Nat, n: Nat, p: Nat](
  a: ref Matrix[m, n, Float],
  b: ref Matrix[n, p, Float]
) -> Matrix[m, p, Float] / Pure {
  // The types guarantee dimensions are compatible
  // n in matrix 'a' must equal n in matrix 'b'
  // ...
  Nil
}

// Refined function parameters
fn processAge(age: Age) -> String / Pure {
  if age < 18 {
    "minor"
  } else if age < 65 {
    "adult"
  } else {
    "senior"
  }
}

// Contracts on return values
fn double(n: PosInt) -> Int where (self > n) / Pure {
  n * 2  // Compiler proves 2n > n for positive n
}

fn main() -> () / IO {
  // These compile - refinements satisfied
  let age: Age = 25;
  let port: Port = 8080;
  let pct: Percentage = 75;

  // This would fail at compile time:
  // let bad: Age = 200;  // Error: cannot prove 200 <= 150

  // Runtime validation for unknown values
  let input = readLine();
  match mkPercentage(parseInt(input)) {
    Some(p) => println("Valid percentage: " ++ p.show()),
    None => println("Invalid percentage")
  }
}
