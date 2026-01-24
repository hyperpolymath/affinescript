// Test if-expression constant folding

fn main() -> Int {
  let x = if true { 42 } else { 0 };   // Should fold to 42
  let y = if false { 99 } else { 10 }; // Should fold to 10
  return x + y;  // 42 + 10 = 52
}
