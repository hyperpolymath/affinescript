// Test pattern matching with mixed patterns

fn classify(x: Int) -> Int {
  return match x {
    0 => 1,
    1 => 2,
    n => n * 10
  };
}

fn main() -> Int {
  let a = classify(0);   // 1
  let b = classify(1);   // 2
  let c = classify(5);   // 50

  return a + b + c;  // 1 + 2 + 50 = 53
}
