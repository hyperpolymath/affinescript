// Test variant-like behavior using integers as tags

fn main() -> Int {
  let color = 0;  // Representing "Red" as tag 0

  return match color {
    0 => 10,   // Red
    1 => 20,   // Green
    2 => 30,   // Blue
    _ => 999
  };  // Should return 10
}
