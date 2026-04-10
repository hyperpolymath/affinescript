// Test kind checking integration

// Function with higher-kinded type parameter
fn map[F: Type -> Type, A, B](fa: F[A], f: A -> B) -> F[B] {
  return fa;
}

// Simple polymorphic type alias
type Box[A] = {
  value: A
};

// Function using Box
fn box_value[A](x: A) -> Box[A] {
  return {value: x};
}

// Multiple type parameters
fn swap[A, B](x: A, y: B) -> {fst: B, snd: A} {
  return {fst: y, snd: x};
}

fn main() -> Int {
  return 42;
}
