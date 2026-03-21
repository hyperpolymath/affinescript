// Test for loop over array

fn main() -> Int {
  let arr = [1, 2, 3, 4, 5];
  let mut sum = 0;
  for x in arr {
    sum += x;
  }
  return sum;  // Should return 15 (1+2+3+4+5)
}
