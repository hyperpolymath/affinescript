// Length-indexed vectors in AffineScript

type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(head: T, tail: Vec[n, T]) : Vec[n + 1, T]

// Can only be called on non-empty vectors - enforced by types
total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v {
    Cons(h, _) => h
  }
}

// Can only be called on non-empty vectors
total fn tail[n: Nat, T](v: Vec[n + 1, T]) -> Vec[n, T] / Pure {
  match v {
    Cons(_, t) => t
  }
}

// Concatenate two vectors - result length is sum of input lengths
total fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] / Pure {
  match a {
    Nil => b,
    Cons(h, t) => Cons(h, append(t, b))
  }
}

// Map over a vector - preserves length
total fn map[n: Nat, A, B](
  v: Vec[n, A],
  f: A -> B / Pure
) -> Vec[n, B] / Pure {
  match v {
    Nil => Nil,
    Cons(h, t) => Cons(f(h), map(t, f))
  }
}
