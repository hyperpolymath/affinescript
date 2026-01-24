// Test closure capturing and using a variable

fn main() -> Int {
  let x = 5;
  let f = |y: Int| -> Int {
    return x + y;
  };
  return f(10);  // Should return 15
}
