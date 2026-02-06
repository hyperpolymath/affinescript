// SPDX-License-Identifier: PMPL-1.0-or-later
// End-to-end test: effect system
// Tests: effect declarations, effect annotations

effect IO {
  fn print(s: String) -> ();
  fn read() -> String;
}

effect State[S] {
  fn get() -> S;
  fn put(s: S) -> ();
}

effect Exn[E] {
  fn throw(err: E) -> Never;
}

fn hello() -> () = ();
