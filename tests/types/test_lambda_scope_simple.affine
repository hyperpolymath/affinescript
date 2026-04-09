// Simple test for lambda parameter scope
// Just check that two lambdas with same parameter name can be defined

fn main() -> Int {
  // First lambda with parameter 'x'
  let f = |x: Int| -> Int {
    return x + 1;
  };

  // Second lambda with same parameter name 'x'
  // This should work - 'x' from first lambda should not leak
  let g = |x: Int| -> Int {
    return x + 2;
  };

  return 42;
}
