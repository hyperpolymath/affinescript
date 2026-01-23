# Lesson 2: Functions and Ownership

Learn how AffineScript's ownership system works with functions.

## Function Basics

Functions require explicit return statements:

```affinescript
fn add(x: Int, y: Int) -> Int {
  return x + y;
}
```

## Ownership Modes

### Owned Parameters (own)
Function takes ownership:
```affinescript
fn consume(own x: Int) -> Int {
  return x;
}
let value = 42;
let result = consume(value);  // value moved
// value cannot be used here!
```

### Shared Borrows (ref)
Function borrows without taking ownership:
```affinescript
fn read(ref x: Int) -> Int {
  return x;
}
let value = 42;
let result = read(value);
let also = value;  // OK - still valid!
```

Next: [Lesson 3: Data Structures](lesson-03-data.md)
