// Test dependent type parsing

// Dependent function type: (x: T) -> U
fn dep_func(f: (x: Int) -> Int) -> Int {
  return 0;
}

// Refined type: T where (P)
fn take_positive(x: Int where (x > 0)) -> Int {
  return x;
}

// Dependent arrow with effect
fn dep_with_eff(f: (x: Int) -{IO}-> Int) -> Int {
  return 0;
}

// Refined type with complex predicate
fn bounded(x: Int where (x >= 0), y: Int where (y < 100)) -> Int {
  return x + y;
}

fn main() -> Int {
  return 42;
}
