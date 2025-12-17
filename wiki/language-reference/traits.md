# Traits

Traits define shared behavior that types can implement. They enable polymorphism and code reuse.

## Table of Contents

1. [Defining Traits](#defining-traits)
2. [Implementing Traits](#implementing-traits)
3. [Using Traits](#using-traits)
4. [Associated Types](#associated-types)
5. [Default Implementations](#default-implementations)
6. [Trait Bounds](#trait-bounds)
7. [Coherence](#coherence)
8. [Standard Traits](#standard-traits)

---

## Defining Traits

### Basic Trait

```affine
trait Greet {
  fn greet(self: &Self) -> String
}
```

### Trait with Multiple Methods

```affine
trait Shape {
  fn area(self: &Self) -> Float64
  fn perimeter(self: &Self) -> Float64
  fn contains(self: &Self, point: Point) -> Bool
}
```

### Trait with Type Parameters

```affine
trait Container[T] {
  fn new() -> Self
  fn add(self: &mut Self, item: T)
  fn get(self: &Self, index: Int) -> Option[&T]
  fn len(self: &Self) -> Int
}
```

---

## Implementing Traits

### Basic Implementation

```affine
struct Person {
  name: String,
  age: Int
}

impl Greet for Person {
  fn greet(self: &Self) -> String {
    "Hello, my name is " ++ self.name
  }
}

let p = Person { name: "Alice", age: 30 }
p.greet()  // "Hello, my name is Alice"
```

### Implementation for Generic Types

```affine
impl[T] Container[T] for Vec[T] {
  fn new() -> Vec[T] {
    Vec::empty()
  }

  fn add(self: &mut Vec[T], item: T) {
    self.push(item)
  }

  fn get(self: &Vec[T], index: Int) -> Option[&T] {
    if index < self.len() {
      Some(&self.data[index])
    } else {
      None
    }
  }

  fn len(self: &Vec[T]) -> Int {
    self.data.len()
  }
}
```

### Conditional Implementation

```affine
// Only implement Show for Option[T] when T: Show
impl[T: Show] Show for Option[T] {
  fn show(self: &Self) -> String {
    match self {
      Some(x) -> "Some(" ++ x.show() ++ ")",
      None -> "None"
    }
  }
}
```

---

## Using Traits

### Method Calls

```affine
let circle = Circle { radius: 5.0 }
let area = circle.area()  // Uses Shape::area

let vec: Vec[Int] = Container::new()
vec.add(42)  // Uses Container::add
```

### Trait Objects (Dynamic Dispatch)

```affine
// dyn Trait for runtime polymorphism
fn describe_shape(shape: &dyn Shape) -> String {
  format("Area: {}, Perimeter: {}", shape.area(), shape.perimeter())
}

let shapes: Vec[Box[dyn Shape]] = vec![
  Box::new(Circle { radius: 1.0 }),
  Box::new(Rectangle { width: 2.0, height: 3.0 })
]

for shape in shapes {
  println(describe_shape(&*shape))
}
```

### Static Dispatch (Generics)

```affine
fn describe_shape_generic[S: Shape](shape: &S) -> String {
  format("Area: {}, Perimeter: {}", shape.area(), shape.perimeter())
}

// More efficient - no runtime dispatch
describe_shape_generic(&circle)
```

---

## Associated Types

### Defining Associated Types

```affine
trait Iterator {
  type Item

  fn next(self: &mut Self) -> Option[Self::Item]
}
```

### Implementing Associated Types

```affine
struct RangeIter {
  current: Int,
  end: Int
}

impl Iterator for RangeIter {
  type Item = Int

  fn next(self: &mut Self) -> Option[Int] {
    if self.current < self.end {
      let value = self.current
      self.current += 1
      Some(value)
    } else {
      None
    }
  }
}
```

### Using Associated Types in Bounds

```affine
fn sum_iter[I](iter: &mut I) -> I::Item
where
  I: Iterator,
  I::Item: Add + Default
{
  let mut total = I::Item::default()
  while let Some(x) = iter.next() {
    total = total + x
  }
  total
}
```

---

## Default Implementations

### Providing Defaults

```affine
trait Eq {
  fn eq(self: &Self, other: &Self) -> Bool

  // Default implementation using eq
  fn ne(self: &Self, other: &Self) -> Bool {
    !self.eq(other)
  }
}

impl Eq for Int {
  fn eq(self: &Int, other: &Int) -> Bool {
    // Only need to implement eq, ne comes free
    *self == *other
  }
}
```

### Overriding Defaults

```affine
trait Animal {
  fn speak(self: &Self) -> String

  fn introduce(self: &Self) -> String {
    "I am an animal that says: " ++ self.speak()
  }
}

struct Dog { name: String }

impl Animal for Dog {
  fn speak(self: &Self) -> String {
    "Woof!"
  }

  // Override default
  fn introduce(self: &Self) -> String {
    "I am " ++ self.name ++ " and I say: " ++ self.speak()
  }
}
```

---

## Trait Bounds

### Single Bound

```affine
fn print_value[T: Show](x: T) -{IO}-> Unit {
  println(x.show())
}
```

### Multiple Bounds

```affine
fn process[T: Clone + Show + Eq](x: T) -> T {
  let y = x.clone()
  if x.eq(&y) {
    println(x.show())
  }
  y
}
```

### Where Clauses

```affine
fn complex_function[A, B, C](a: A, b: B) -> C
where
  A: Clone,
  B: Into[A],
  C: From[A] + Default,
  A: Show
{
  let converted: A = b.into()
  println(converted.show())
  C::from(converted)
}
```

### Higher-Ranked Trait Bounds

```affine
fn apply_to_all[F](items: Vec[Int], f: F)
where
  F: for['a] Fn(&'a Int) -> Int
{
  for item in items {
    f(&item)
  }
}
```

### Supertraits

```affine
// Ord requires Eq
trait Ord: Eq {
  fn compare(self: &Self, other: &Self) -> Ordering
}

// Must implement both Eq and Ord
impl Eq for MyType { ... }
impl Ord for MyType { ... }
```

---

## Coherence

### Orphan Rules

You can only implement a trait if either:
- The trait is defined in your crate, OR
- The type is defined in your crate

```affine
// OK: Your trait, external type
trait MyTrait { }
impl MyTrait for Vec[Int] { }

// OK: External trait, your type
struct MyType { }
impl Show for MyType { }

// ERROR: External trait, external type
// impl Show for Vec[Int] { }  // Not allowed
```

### Blanket Implementations

```affine
// Implement for all types that satisfy a bound
impl[T: Show] Show for Vec[T] {
  fn show(self: &Self) -> String {
    let items = self.iter().map(|x| x.show()).join(", ")
    "[" ++ items ++ "]"
  }
}
```

---

## Standard Traits

### Comparison Traits

```affine
trait Eq {
  fn eq(self: &Self, other: &Self) -> Bool
}

trait Ord: Eq {
  fn compare(self: &Self, other: &Self) -> Ordering
}

enum Ordering { Less, Equal, Greater }
```

### Conversion Traits

```affine
trait From[T] {
  fn from(value: T) -> Self
}

trait Into[T] {
  fn into(self) -> T
}

// From implies Into
impl[T, U: From[T]] Into[U] for T {
  fn into(self) -> U {
    U::from(self)
  }
}
```

### Display and Debug

```affine
trait Show {
  fn show(self: &Self) -> String
}

trait Debug {
  fn debug(self: &Self) -> String
}
```

### Clone and Copy

```affine
trait Clone {
  fn clone(self: &Self) -> Self
}

// Copy is a marker trait - types are bitwise-copyable
trait Copy: Clone { }
```

### Default

```affine
trait Default {
  fn default() -> Self
}

impl Default for Int {
  fn default() -> Int { 0 }
}

impl Default for String {
  fn default() -> String { "" }
}
```

### Hashing

```affine
trait Hash {
  fn hash(self: &Self, hasher: &mut Hasher)
}
```

### Iterator Traits

```affine
trait Iterator {
  type Item
  fn next(self: &mut Self) -> Option[Self::Item]
}

trait IntoIterator {
  type Item
  type IntoIter: Iterator[Item = Self::Item]
  fn into_iter(self) -> Self::IntoIter
}
```

---

## Derive Macros

Automatically implement common traits:

```affine
#[derive(Eq, Ord, Clone, Show, Hash, Default)]
struct Point {
  x: Int,
  y: Int
}

// Generates:
// impl Eq for Point { ... }
// impl Ord for Point { ... }
// impl Clone for Point { ... }
// impl Show for Point { ... }
// impl Hash for Point { ... }
// impl Default for Point { ... }
```

---

## See Also

- [Types](types.md) - Type system overview
- [Functions](functions.md) - Generic functions
- [Standard Library](../stdlib/overview.md) - Standard traits
