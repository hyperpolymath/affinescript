# Lesson 6: Error Handling

## Result Types

Handle errors explicitly:
```affinescript
fn divide(a: Int, b: Int) -> Result[Int, String] {
  return if b == 0 {
    Err("division by zero")
  } else {
    Ok(a / b)
  };
}
```

## Propagating Errors

Use `?` operator to propagate:
```affinescript
fn compute() -> Result[Int, String] {
  let x = divide(10, 2)?;
  return Ok(x * 2);
}
```

Next: [Lesson 7: Effects](lesson-07-effects.md)
