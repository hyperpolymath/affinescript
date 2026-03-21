// Test use-after-move detection

fn consume(own x: Int) -> Int {
  return x;
}

fn main() -> Int {
  let x = 42;
  let y = consume(x);  // x is moved here
  return x;  // ERROR: use after move
}
