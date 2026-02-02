// Test tuple/record/array construction together

type Point = { x: Int, y: Int, z: Int };

fn main() -> Int {
  let p = { x: 10, y: 20, z: 30 };
  let t = (1, 2, 3, 4);
  let a = [5, 6];
  return p.x + p.y + p.z + t.0 + t.1 + t.2 + t.3 + a[0] + a[1];
}
