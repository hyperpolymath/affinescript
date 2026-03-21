// Conformance test: effect definitions

effect IO {
  fn print(s: String);
  fn println(s: String);
  fn read_line() -> String;
}

effect State[S] {
  fn get() -> S;
  fn put(s: S);
}

effect Exn[E] {
  fn throw(err: E) -> Never;
}

effect Async {
  fn await[T](promise: Promise[T]) -> T;
  fn spawn[T](f: () -> T) -> Promise[T];
}
