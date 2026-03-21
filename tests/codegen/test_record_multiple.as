// Test multiple distinct records

fn main() -> Int {
  let r1 = {a: 5, b: 10};
  let r2 = {x: 20, y: 30, z: 40};

  return r1.a + r1.b + r2.x + r2.y + r2.z;  // 5 + 10 + 20 + 30 + 40 = 105
}
