// Test constant folding optimization

fn main() -> Int {
  let a = 2 + 3;        // Should fold to 5
  let b = 10 * 4;       // Should fold to 40
  let c = 20 - 5;       // Should fold to 15
  return a + b + c;     // 5 + 40 + 15 = 60
}
