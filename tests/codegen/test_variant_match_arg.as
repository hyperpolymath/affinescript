// Test pattern matching on variant with argument

enum Option {
  None,
  Some(Int)
}

fn main() -> Int {
  let x = Option::Some(42);

  let result = match x {
    Some(y) => y,
    None => 0
  };

  return result;  // Should return 42
}
