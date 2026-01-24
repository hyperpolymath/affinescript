// Test as pattern (x @ pattern)

fn main() -> Int {
  let x = 42;

  let result = match x {
    y @ 42 => y + 1,
    _ => 0
  };

  return result;  // Should return 43
}
