// Test effect declarations
effect IO {
  fn print(s: String) -> ();
  fn read() -> String;
}

effect State[S] {
  fn get() -> S;
  fn put(s: S) -> ();
}
