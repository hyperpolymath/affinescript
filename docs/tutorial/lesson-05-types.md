# Lesson 5: Types and Inference

## Type Annotations

Always specify parameter and return types:
```affinescript
fn identity(x: Int) -> Int {
  return x;
}
```

## Type Inference

Let bindings infer types:
```affinescript
fn main() -> Int {
  let x = 42;        // x: Int inferred
  let y = x + 10;    // y: Int inferred
  return y;
}
```

Next: [Lesson 6: Error Handling](lesson-06-errors.md)
