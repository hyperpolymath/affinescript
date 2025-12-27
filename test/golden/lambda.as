// Test lambda expressions
fn test_lambda() -> Int {
  let f = |x| x + 1;
  f(42)
}

fn test_lambda_typed() -> Int {
  let f = |x: Int| x * 2;
  f(21)
}
