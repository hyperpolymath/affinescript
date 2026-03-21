// Conformance test: pattern matching

type Option[T] =
  | None
  | Some(T)

fn unwrap_or[T](opt: Option[T], default: T) -> T {
  match opt {
    None => default,
    Some(x) => x
  }
}

fn is_some[T](opt: Option[T]) -> Bool {
  match opt {
    None => false,
    Some(_) => true
  }
}

fn describe_number(n: Int) -> String {
  match n {
    0 => "zero",
    1 => "one",
    _ => "many"
  }
}
