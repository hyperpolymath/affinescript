// Test type declarations
type Alias = Int;

struct Point {
  x: Int,
  y: Int
}

enum Option[T] {
  None,
  Some(T)
}
