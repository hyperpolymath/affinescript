// Test pattern matching
fn test_match() -> Int {
  match x {
    0 => 1,
    n => n + 1
  }
}

fn test_destructure() -> Int {
  match pair {
    (a, b) => a + b
  }
}

fn test_constructor() -> Int {
  match opt {
    Some(x) => x,
    None => 0
  }
}
