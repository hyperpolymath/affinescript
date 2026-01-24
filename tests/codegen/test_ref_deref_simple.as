// Test reference and dereference operators

fn main() -> Int {
  let x = 42;
  let ptr = &x;     // Take reference to x
  let y = *ptr;     // Dereference to get value
  return y;         // Should return 42
}
