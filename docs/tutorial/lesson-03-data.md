# Lesson 3: Data Structures

## Tuples

Group multiple values:
```affinescript
fn make_pair() -> (Int, Int) {
  return (1, 2);
}
```

## Records

Named fields:
```affinescript
fn make_point() -> Point {
  return { x: 10, y: 20 };
}
```

## Arrays

Collections of same type:
```affinescript
fn make_list() -> [Int] {
  return [1, 2, 3];
}
```

Next: [Lesson 4: Pattern Matching](lesson-04-patterns.md)
