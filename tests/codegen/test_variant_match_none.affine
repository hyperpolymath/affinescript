// Test pattern matching on None variant

enum Option {
  None,
  Some(Int)
}

fn main() -> Int {
  let x = Option::None;

  let result = match x {
    Some(y) => y,
    None => 99
  };

  return result;  // Should return 99
}
