// Test higher-kinded type parsing

// Function with higher-kinded type parameter
fn map[F: Type -> Type, A, B](fa: F[A], f: A -> B) -> F[B] {
  return fa;
}

// Multiple higher-kinded parameters
fn apply[F: Type -> Type, G: Type -> Type, A](f: F[A], g: G[A]) -> F[A] {
  return f;
}

// Nested arrow kinds
fn compose[F: Type -> Type, G: Type -> Type, A, B, C](
  f: F[A -> B],
  g: G[B -> C]
) -> F[A -> C] {
  return f;
}

// Simple polymorphic function
fn id[A](x: A) -> A {
  return x;
}

// Two type parameters
fn const[A, B](x: A, y: B) -> A {
  return x;
}

fn main() -> Int {
  return 42;
}
