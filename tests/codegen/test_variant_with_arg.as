// Test variant constructor with single argument

enum Option {
  None,
  Some(Int)
}

fn main() -> Int {
  let x = Option::Some(42);
  return x;  // Return pointer to variant
}
