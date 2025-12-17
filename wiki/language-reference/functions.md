# Functions

Functions are the primary building blocks of AffineScript programs.

## Table of Contents

1. [Function Declarations](#function-declarations)
2. [Parameters](#parameters)
3. [Return Types](#return-types)
4. [Generic Functions](#generic-functions)
5. [Closures](#closures)
6. [Totality](#totality)
7. [Methods](#methods)

---

## Function Declarations

### Basic Syntax

```affine
fn function_name(param1: Type1, param2: Type2) -> ReturnType {
  // body
}
```

### Simple Functions

```affine
fn add(x: Int, y: Int) -> Int {
  x + y
}

fn greet(name: String) -> String {
  "Hello, " ++ name ++ "!"
}

fn main() -{IO}-> Unit {
  let result = add(1, 2)
  println(greet("World"))
}
```

### Expression Bodies

Single-expression functions:

```affine
fn double(x: Int) -> Int = x * 2

fn square(x: Int) -> Int = x * x

fn is_even(n: Int) -> Bool = n % 2 == 0
```

---

## Parameters

### Ownership in Parameters

```affine
// Takes ownership (default for non-Copy)
fn consume(s: String) -> Int {
  s.len()  // s is consumed
}

// Shared borrow
fn read(s: &String) -> Int {
  s.len()  // s is borrowed
}

// Mutable borrow
fn modify(s: &mut String) {
  s.push_str("!")
}

// Explicit ownership annotation
fn transfer(file: own File) {
  file.close()
}
```

### Default Parameters (Planned)

```affine
fn greet(name: String, greeting: String = "Hello") -> String {
  greeting ++ ", " ++ name
}

greet("Alice")           // "Hello, Alice"
greet("Bob", "Hi")       // "Hi, Bob"
```

### Named Arguments (Planned)

```affine
fn create_user(name: String, age: Int, active: Bool) -> User {
  User { name, age, active }
}

// Call with named arguments
create_user(name: "Alice", age: 30, active: true)
create_user(age: 25, name: "Bob", active: false)  // Any order
```

### Variadic Parameters (Planned)

```affine
fn sum(..nums: [Int]) -> Int {
  nums.fold(0, |acc, n| acc + n)
}

sum(1, 2, 3, 4, 5)  // 15
```

---

## Return Types

### Explicit Returns

```affine
fn early_return(x: Int) -> Int {
  if x < 0 {
    return 0
  }
  x * 2
}
```

### Unit Return

```affine
fn side_effect() {
  // Implicit Unit return
  println("effect!")
}

fn explicit_unit() -> Unit {
  println("effect!")
  ()
}
```

### Never Return

Functions that don't return:

```affine
fn panic(msg: String) -> Never {
  print_error(msg)
  exit(1)
}

fn infinite_loop() -> Never {
  loop {
    process()
  }
}
```

### Effect Annotations

```affine
// Pure function
fn pure(x: Int) -> Int {
  x + 1
}

// With effects
fn impure() -{IO}-> Unit {
  println("Hello")
}

// Multiple effects
fn complex() -{IO, State[Int], Error[String]}-> Int {
  // ...
}
```

---

## Generic Functions

### Type Parameters

```affine
fn identity[T](x: T) -> T {
  x
}

fn swap[A, B](pair: (A, B)) -> (B, A) {
  let (a, b) = pair
  (b, a)
}

fn first[A, B](pair: (A, B)) -> A {
  pair.0
}
```

### Trait Bounds

```affine
fn print_value[T: Show](x: T) -{IO}-> Unit {
  println(x.show())
}

fn compare[T: Ord](a: T, b: T) -> Ordering {
  a.compare(b)
}

fn sort[T: Ord](mut list: Vec[T]) -> Vec[T] {
  // sorting implementation
  list
}
```

### Where Clauses

```affine
fn complex_bounds[A, B, C](a: A, b: B, c: C) -> C
where
  A: Clone + Show,
  B: Into[A],
  C: From[A] + Default
{
  let a2 = b.into()
  C::from(a2)
}
```

### Associated Type Constraints

```affine
fn sum_iterator[I](iter: I) -> I::Item
where
  I: Iterator,
  I::Item: Add + Default
{
  iter.fold(I::Item::default(), |acc, x| acc + x)
}
```

---

## Closures

### Lambda Syntax

```affine
// Type inferred
let double = |x| x * 2

// Explicit types
let add: (Int, Int) -> Int = |x, y| x + y

// Multi-line
let process = |x| {
  let y = x * 2
  y + 1
}
```

### Capturing Environment

```affine
fn make_adder(n: Int) -> (Int) -> Int {
  |x| x + n  // Captures n by value
}

let add5 = make_adder(5)
add5(10)  // 15
```

### Move Closures

```affine
fn make_counter() -> () -> Int {
  let mut count = 0
  move || {
    count += 1
    count
  }
}

let counter = make_counter()
counter()  // 1
counter()  // 2
```

### Closure Types

```affine
// Fn - can be called multiple times, borrows immutably
fn apply_fn[F: Fn(Int) -> Int](f: F, x: Int) -> Int {
  f(x)
}

// FnMut - can be called multiple times, borrows mutably
fn apply_fn_mut[F: FnMut(Int) -> Int](mut f: F, x: Int) -> Int {
  f(x)
}

// FnOnce - can only be called once, takes ownership
fn apply_fn_once[F: FnOnce(Int) -> Int](f: F, x: Int) -> Int {
  f(x)
}
```

---

## Totality

### Total Functions

Functions marked `total` must provably terminate:

```affine
total fn factorial(n: Nat) -> Nat {
  match n {
    0 -> 1,
    n -> n * factorial(n - 1)  // Decreasing argument
  }
}

total fn length[T](list: List[T]) -> Nat {
  match list {
    Nil -> 0,
    Cons(_, tail) -> 1 + length(tail)  // Structural recursion
  }
}
```

### Partial Functions (Default)

Functions without `total` may diverge:

```affine
fn partial_search(predicate: (Int) -> Bool) -> Int {
  let mut i = 0
  while !predicate(i) {
    i += 1  // Might never terminate!
  }
  i
}
```

### Termination Checking

```affine
// OK: Structural recursion on list
total fn map[A, B](f: (A) -> B, list: List[A]) -> List[B] {
  match list {
    Nil -> Nil,
    Cons(x, xs) -> Cons(f(x), map(f, xs))
  }
}

// ERROR: Not provably terminating
total fn collatz(n: Nat) -> Nat {
  match n {
    1 -> 1,
    n if n % 2 == 0 -> collatz(n / 2),
    n -> collatz(3 * n + 1)  // Not decreasing!
  }
}
```

---

## Methods

### Method Syntax

```affine
struct Point {
  x: Float64,
  y: Float64
}

impl Point {
  // Associated function (no self)
  fn origin() -> Point {
    Point { x: 0.0, y: 0.0 }
  }

  // Method with &self
  fn distance_from_origin(self: &Self) -> Float64 {
    (self.x * self.x + self.y * self.y).sqrt()
  }

  // Method with &mut self
  fn translate(self: &mut Self, dx: Float64, dy: Float64) {
    self.x += dx
    self.y += dy
  }

  // Method that consumes self
  fn into_tuple(self: Self) -> (Float64, Float64) {
    (self.x, self.y)
  }
}

// Usage
let p = Point::origin()
let dist = p.distance_from_origin()
let (x, y) = p.into_tuple()
```

### Self Type Shortcuts

```affine
impl Point {
  fn method1(&self) -> Float64 { ... }      // self: &Self
  fn method2(&mut self) { ... }              // self: &mut Self
  fn method3(self) -> (Float64, Float64) { } // self: Self (owned)
}
```

### Method Chaining

```affine
impl Builder {
  fn new() -> Self { ... }

  fn field1(self, v: Int) -> Self {
    Self { field1: v, ..self }
  }

  fn field2(self, v: String) -> Self {
    Self { field2: v, ..self }
  }

  fn build(self) -> Result {
    // ...
  }
}

let result = Builder::new()
  .field1(42)
  .field2("hello")
  .build()
```

---

## Operators as Functions

### Operator Definitions

```affine
trait Add[Rhs = Self] {
  type Output

  fn add(self, rhs: Rhs) -> Self::Output
}

impl Add for Int {
  type Output = Int

  fn add(self, rhs: Int) -> Int {
    // primitive addition
  }
}
```

### Custom Operators

```affine
impl Add for Point {
  type Output = Point

  fn add(self, rhs: Point) -> Point {
    Point {
      x: self.x + rhs.x,
      y: self.y + rhs.y
    }
  }
}

let p1 = Point { x: 1.0, y: 2.0 }
let p2 = Point { x: 3.0, y: 4.0 }
let p3 = p1 + p2  // Point { x: 4.0, y: 6.0 }
```

---

## See Also

- [Types](types.md) - Function types
- [Traits](traits.md) - Trait methods
- [Effects](effects.md) - Effect annotations
