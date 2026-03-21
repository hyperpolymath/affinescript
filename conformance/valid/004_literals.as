// Conformance test: literal values

fn test_literals() -> () {
  // Integers
  let dec = 42;
  let neg = -10;
  let hex = 0xFF;
  let bin = 0b1010;
  let oct = 0o777;

  // Floats
  let pi = 3.14159;
  let neg_float = -2.5;
  let sci = 1.0e10;

  // Booleans
  let t = true;
  let f = false;

  // Strings
  let s = "hello world";
  let escaped = "line\nbreak\ttab";

  // Unit
  let u = ();
}
