// Test lambda parameter scope fix
// This test ensures lambda parameters don't leak into outer scope

fn main() -> Int {
  // First lambda with parameter 'x'
  let f = |x: Int| -> Int {
    return x + 1;
  };

  // Second lambda with same parameter name 'x'
  // This should NOT see the 'x' from the first lambda
  let g = |x: Int| -> Int {
    return x + 2;
  };

  // Third lambda with parameter 'x' again
  let h = |x: Int| -> Int {
    return x * 2;
  };

  // Call all lambdas - each should use its own parameter binding
  let a = f(5);   // 5 + 1 = 6
  let b = g(10);  // 10 + 2 = 12
  let c = h(3);   // 3 * 2 = 6

  return a + b + c;  // 6 + 12 + 6 = 24
}
