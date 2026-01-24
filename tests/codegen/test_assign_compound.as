// Test compound assignment operators

fn main() -> Int {
  let mut x = 10;
  x += 5;   // x = 15
  x *= 2;   // x = 30
  x -= 10;  // x = 20
  x /= 2;   // x = 10
  return x;  // Should return 10
}
