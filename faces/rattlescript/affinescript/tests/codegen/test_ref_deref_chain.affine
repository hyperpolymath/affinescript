// Test chained references and dereferences

fn main() -> Int {
  let x = 42;
  let p1 = &x;
  let p2 = &p1;     // Pointer to pointer
  let v1 = *p2;     // Get p1
  let v2 = *v1;     // Get x
  return v2;        // Should return 42
}
