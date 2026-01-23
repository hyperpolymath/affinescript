// Test unsafe read operation
fn test() -> () {
  let ptr = 42;
  unsafe {
    read(ptr);
  };
}
