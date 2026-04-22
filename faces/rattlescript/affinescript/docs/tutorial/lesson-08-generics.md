# Lesson 8: Generic Types

## Type Parameters

Write functions that work with any type:
```affinescript
fn identity[T](x: T) -> T {
  return x;
}
```

## Generic Data Structures

```affinescript
type Box[T] = {
  value: T
};

fn make_box[T](x: T) -> Box[T] {
  return { value: x };
}
```

Next: [Lesson 9: Modules](lesson-09-modules.md)
