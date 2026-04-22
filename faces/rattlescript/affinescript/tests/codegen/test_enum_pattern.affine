// Test enum-like pattern matching with integer tags
// This simulates how enums will work when parser support is added

fn classify_status(code: Int) -> Int {
  return match code {
    0 => 100,  // Success
    1 => 200,  // Warning
    2 => 300,  // Error
    _ => 999   // Unknown
  };
}

fn main() -> Int {
  let success = classify_status(0);    // 100
  let warning = classify_status(1);    // 200
  let error = classify_status(2);      // 300
  let unknown = classify_status(99);   // 999

  // Return sum: 100 + 200 + 300 + 999 = 1599
  return success + warning + error + unknown;
}
