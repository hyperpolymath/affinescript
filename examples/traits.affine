// Traits and type classes in AffineScript

// Equality trait
trait Eq {
  fn eq(ref self, other: ref Self) -> Bool / Pure;

  fn neq(ref self, other: ref Self) -> Bool / Pure {
    !self.eq(other)
  }
}

// Ordering trait (requires Eq)
trait Ord: Eq {
  fn cmp(ref self, other: ref Self) -> Ordering / Pure;

  fn lt(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) == Less
  }

  fn le(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) != Greater
  }

  fn gt(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) == Greater
  }

  fn ge(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) != Less
  }
}

type Ordering = Less | Equal | Greater

// Show trait for string representation
trait Show {
  fn show(ref self) -> String / Pure;
}

// Implement traits for Int
impl Eq for Int {
  fn eq(ref self, other: ref Int) -> Bool / Pure {
    *self == *other
  }
}

impl Ord for Int {
  fn cmp(ref self, other: ref Int) -> Ordering / Pure {
    if *self < *other { Less }
    else if *self > *other { Greater }
    else { Equal }
  }
}

impl Show for Int {
  fn show(ref self) -> String / Pure {
    intToString(*self)
  }
}

// Implement Show for Option
impl[T: Show] Show for Option[T] {
  fn show(ref self) -> String / Pure {
    match self {
      None => "None",
      Some(x) => "Some(" ++ x.show() ++ ")"
    }
  }
}

// Implement Show for Result
impl[T: Show, E: Show] Show for Result[T, E] {
  fn show(ref self) -> String / Pure {
    match self {
      Ok(x) => "Ok(" ++ x.show() ++ ")",
      Err(e) => "Err(" ++ e.show() ++ ")"
    }
  }
}

// Functor trait
trait Functor[F: Type -> Type] {
  fn map[A, B](self: F[A], f: A -> B / Pure) -> F[B] / Pure;
}

// Monad trait
trait Monad[M: Type -> Type]: Functor[M] {
  fn pure[A](value: A) -> M[A] / Pure;
  fn flatMap[A, B](self: M[A], f: A -> M[B] / Pure) -> M[B] / Pure;
}

// Implement Functor for Option
impl Functor[Option] for Option {
  fn map[A, B](self: Option[A], f: A -> B / Pure) -> Option[B] / Pure {
    match self {
      None => None,
      Some(x) => Some(f(x))
    }
  }
}

// Implement Monad for Option
impl Monad[Option] for Option {
  fn pure[A](value: A) -> Option[A] / Pure {
    Some(value)
  }

  fn flatMap[A, B](self: Option[A], f: A -> Option[B] / Pure) -> Option[B] / Pure {
    match self {
      None => None,
      Some(x) => f(x)
    }
  }
}

// Generic functions using trait bounds
fn max[T: Ord](a: T, b: T) -> T / Pure {
  if a.gt(ref b) { a } else { b }
}

fn min[T: Ord](a: T, b: T) -> T / Pure {
  if a.lt(ref b) { a } else { b }
}

fn printAny[T: Show](value: ref T) -> () / IO {
  println(value.show())
}

fn sortBy[T, K: Ord](items: mut [T], key: (ref T) -> K / Pure) -> () / Pure {
  // Sorting implementation using Ord trait
  // ...
}

// Custom type with trait implementations
type Point = { x: Int, y: Int }

impl Eq for Point {
  fn eq(ref self, other: ref Point) -> Bool / Pure {
    self.x == other.x && self.y == other.y
  }
}

impl Show for Point {
  fn show(ref self) -> String / Pure {
    "(" ++ self.x.show() ++ ", " ++ self.y.show() ++ ")"
  }
}

fn main() -> () / IO {
  let p1 = Point { x: 1, y: 2 };
  let p2 = Point { x: 3, y: 4 };

  printAny(ref p1);
  printAny(ref p2);

  let m = max(10, 20);
  println("Max: " ++ m.show());

  let opt = Some(42);
  printAny(ref opt);
}
