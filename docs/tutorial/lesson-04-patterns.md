# Lesson 4: Pattern Matching

Destructure data with match expressions:

```affinescript
fn describe(x: Int) -> String {
  return match x {
    0 => "zero",
    1 => "one",
    _ => "many",
  };
}
```

## Tuple Patterns

```affinescript
fn first(pair: (Int, Int)) -> Int {
  return match pair {
    (a, b) => a,
  };
}
```

Next: [Lesson 5: Types and Inference](lesson-05-types.md)
