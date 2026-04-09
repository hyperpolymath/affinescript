// End-to-end dependent types test

// Function that requires positive input
fn sqrt_approx(x: Int where (x >= 0)) -> Int {
  return x;
}

// Function that requires value in range
fn percent(x: Int where (x >= 0), max: Int where (max > 0)) -> Int {
  return x * 100;
}

// Multiple refined parameters
fn safe_div(num: Int, denom: Int where (denom != 0)) -> Int {
  return num;
}

fn main() -> Int {
  let a = sqrt_approx(25);
  let b = percent(5, 10);
  let c = safe_div(10, 2);
  return a + b + c;
}
