// Test different strings get different pointers

fn main() -> Int {
  let s1 = "Hello";
  let s2 = "World";

  // Different strings should have different pointers
  // s1 at 2048, s2 at 2048 + 4 + 5 = 2057
  // Difference should be 9
  return s2 - s1;
}
