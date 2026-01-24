// Test record with multiple fields

fn main() -> Int {
  let r = {x: 10, y: 20, z: 30};
  return r.x + r.y + r.z;  // Should return 60
}
