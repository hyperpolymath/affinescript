// Test trait declarations
trait Eq {
  fn eq(ref self, other: ref Self) -> Bool;
}

trait Ord: Eq {
  fn cmp(ref self, other: ref Self) -> Ordering;
}

impl Eq for Int {
  fn eq(ref self, other: ref Int) -> Bool {
    true
  }
}
