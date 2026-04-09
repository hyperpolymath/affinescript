// Test trait system for generic programming

// Simple trait with a method signature
trait Show {
  fn show(self: Self) -> String;
}

// Trait with higher-kinded type parameter
trait Functor[F: Type -> Type] {
  fn map[A, B](fa: F[A], f: A -> B) -> F[B];
}

// Trait with multiple methods
trait Eq {
  fn eq(self: Self, other: Self) -> Bool;
  fn ne(self: Self, other: Self) -> Bool;
}

// Trait with associated type
trait Container {
  type Item;
  fn get(self: Self, index: Int) -> Item;
}

fn main() -> Int {
  return 42;
}
