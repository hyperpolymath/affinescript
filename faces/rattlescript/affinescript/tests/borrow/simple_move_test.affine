fn consume(own x: Int) -> Int = x;
fn use_val(ref y: Int) -> Int = y;
fn test() -> Int = let x = 42 in let _ = consume(x) in use_val(x);
