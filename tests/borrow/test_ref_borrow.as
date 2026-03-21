// Test reference creation and borrowing

fn main() -> Int {
  let x = 42;
  let ptr = &x;    // Creates a shared borrow of x
  let y = *ptr;    // Dereference is OK
  return y;        // Returns 42
}
