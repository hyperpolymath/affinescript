# Type System

AffineScript features an advanced type system combining:
- **Affine types** for memory safety
- **Dependent types** for compile-time verification
- **Row polymorphism** for flexible records
- **Algebraic effects** for controlled side effects

## Table of Contents

1. [Primitive Types](#primitive-types)
2. [Compound Types](#compound-types)
3. [Function Types](#function-types)
4. [Generic Types](#generic-types)
5. [Ownership Types](#ownership-types)
6. [Dependent Types](#dependent-types)
7. [Row Types](#row-types)
8. [Effect Types](#effect-types)
9. [Refinement Types](#refinement-types)
10. [Type Inference](#type-inference)

---

## Primitive Types

### Numeric Types

| Type | Description | Size |
|------|-------------|------|
| `Int` | Signed integer | Platform-dependent |
| `Int8` | 8-bit signed | 1 byte |
| `Int16` | 16-bit signed | 2 bytes |
| `Int32` | 32-bit signed | 4 bytes |
| `Int64` | 64-bit signed | 8 bytes |
| `Nat` | Natural numbers | Platform-dependent |
| `Float32` | 32-bit IEEE float | 4 bytes |
| `Float64` | 64-bit IEEE float | 8 bytes |

### Other Primitives

| Type | Description |
|------|-------------|
| `Bool` | Boolean (`true` / `false`) |
| `Char` | Unicode scalar value |
| `String` | UTF-8 string |
| `Unit` | Unit type (single value `()`) |
| `Never` | Uninhabited type (no values) |

```affine
let age: Int = 25
let pi: Float64 = 3.14159
let active: Bool = true
let letter: Char = 'A'
let name: String = "Alice"
let nothing: Unit = ()
```

---

## Compound Types

### Tuples

Fixed-size, heterogeneous collections:

```affine
// Tuple type
let pair: (Int, String) = (42, "answer")

// Accessing elements
let x = pair.0  // 42
let y = pair.1  // "answer"

// Destructuring
let (num, text) = pair

// Unit is the empty tuple
let unit: () = ()
```

### Arrays

Fixed-size, homogeneous collections:

```affine
// Array type with length
let nums: [Int; 3] = [1, 2, 3]

// Indexing
let first = nums[0]

// Length is part of the type
fn sum_three(arr: [Int; 3]) -> Int {
  arr[0] + arr[1] + arr[2]
}
```

### Records (Structs)

Named fields with structural typing:

```affine
// Anonymous record
let person: {name: String, age: Int} = {
  name: "Alice",
  age: 30
}

// Field access
let n = person.name

// Named struct
struct Point {
  x: Float64,
  y: Float64
}

let p = Point { x: 1.0, y: 2.0 }
```

### Enums (Variants)

Sum types with constructors:

```affine
enum Option[T] {
  Some(T),
  None
}

enum Result[T, E] {
  Ok(T),
  Err(E)
}

enum Shape {
  Circle { radius: Float64 },
  Rectangle { width: Float64, height: Float64 },
  Point
}

let x: Option[Int] = Some(42)
let y: Result[String, Error] = Ok("success")
```

---

## Function Types

### Basic Function Types

```affine
// Simple function type
fn add(x: Int, y: Int) -> Int {
  x + y
}

// Function as value
let f: (Int, Int) -> Int = add

// Higher-order function
fn apply(f: (Int) -> Int, x: Int) -> Int {
  f(x)
}
```

### Effectful Function Types

Functions can declare their effects:

```affine
// Pure function (no effects)
fn pure_add(x: Int, y: Int) -> Int {
  x + y
}

// Function with IO effect
fn print_sum(x: Int, y: Int) -{IO}-> Unit {
  print(x + y)
}

// Multiple effects
fn read_and_log() -{IO, Log}-> String {
  let data = read_file("data.txt");
  log("Read file");
  data
}
```

### Closures (Lambdas)

```affine
// Lambda syntax
let double = |x| x * 2
let add = |x, y| x + y

// With type annotations
let typed: (Int) -> Int = |x: Int| -> Int { x * 2 }

// Capturing environment
let multiplier = 3
let times_three = |x| x * multiplier
```

---

## Generic Types

### Type Parameters

```affine
// Generic function
fn identity[T](x: T) -> T {
  x
}

// Generic struct
struct Pair[A, B] {
  first: A,
  second: B
}

// Generic enum
enum List[T] {
  Cons(T, Box[List[T]]),
  Nil
}
```

### Type Constraints

```affine
// Single constraint
fn compare[T: Ord](x: T, y: T) -> Ordering {
  x.compare(y)
}

// Multiple constraints
fn hash_and_show[T: Hash + Show](x: T) -> String {
  format("hash={}, value={}", x.hash(), x.show())
}

// Where clause for complex constraints
fn complex[A, B](x: A, y: B) -> Bool
where
  A: Eq + Clone,
  B: Eq
{
  x.clone() == x && y == y
}
```

### Associated Types

```affine
trait Iterator {
  type Item

  fn next(self: &mut Self) -> Option[Self::Item]
}

impl Iterator for Range {
  type Item = Int

  fn next(self: &mut Range) -> Option[Int] {
    if self.current < self.end {
      let value = self.current;
      self.current += 1;
      Some(value)
    } else {
      None
    }
  }
}
```

---

## Ownership Types

### Ownership Modifiers

| Modifier | Meaning | Rules |
|----------|---------|-------|
| `own` | Owned value | Must be consumed exactly once |
| `ref` or `&` | Shared borrow | Read-only, multiple allowed |
| `mut` or `&mut` | Mutable borrow | Exclusive access |

```affine
// Owned - must transfer or consume
fn consume(file: own File) {
  // file is consumed here
  file.close()
}

// Shared borrow - read only
fn read(file: &File) -> String {
  file.read_all()
}

// Mutable borrow - exclusive write
fn modify(file: &mut File) {
  file.write("data")
}
```

### Quantity Annotations

For fine-grained linearity control:

| Quantity | Symbol | Meaning |
|----------|--------|---------|
| Erased | `0` | Compile-time only |
| Linear | `1` | Used exactly once |
| Affine | `?` | Used at most once |
| Unrestricted | `w` | Used any number of times |

```affine
// Linear type - must use exactly once
fn use_linear(x: 1 Resource) -> Result {
  x.consume()  // Must be called
}

// Erased type parameter - exists at compile time only
fn phantom[0 T]() -> Unit {
  // T has no runtime representation
}
```

---

## Dependent Types

### Length-Indexed Types

```affine
// Vec with compile-time length
struct Vec[n: Nat, T] {
  data: [T; n]
}

// Safe head - requires non-empty
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T {
  vec.data[0]
}

// Append preserves length information
fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] {
  // Implementation...
}
```

### Dependent Functions (Pi Types)

```affine
// Return type depends on input
fn replicate[T](n: Nat, x: T) -> Vec[n, T] {
  // Creates vector of exactly n elements
}

// Type-level computation
fn zeros(n: Nat) -> Vec[n, Int] {
  replicate(n, 0)
}
```

### Sigma Types (Dependent Pairs)

```affine
// Existential quantification
type ExistsVec[T] = (n: Nat, Vec[n, T])

fn unknown_length[T](data: [T]) -> ExistsVec[T] {
  let n = data.len();
  (n, Vec::from_array(data))
}
```

---

## Row Types

### Extensible Records

```affine
// Open record type
type HasName = {name: String, ..}

// Function works on any record with 'name'
fn greet(person: HasName) -> String {
  "Hello, " ++ person.name
}

// Works with different record types
greet({name: "Alice"})
greet({name: "Bob", age: 30})
greet({name: "Carol", role: "Admin", active: true})
```

### Row Variables

```affine
// Explicit row variable
fn add_field[r](rec: {..r}) -> {id: Int, ..r} {
  {id: generate_id(), ..rec}
}

// Row constraint - field must be absent
fn safe_extend[r](rec: {..r}) -> {x: Int, ..r}
where
  r lacks x
{
  {x: 0, ..rec}
}
```

### Record Operations

```affine
let rec = {x: 1, y: 2, z: 3}

// Update
let rec2 = {rec with x = 10}  // {x: 10, y: 2, z: 3}

// Extend
let rec3 = {w: 4, ..rec}  // {w: 4, x: 1, y: 2, z: 3}

// Restrict (remove field)
let rec4 = rec \ z  // {x: 1, y: 2}
```

---

## Effect Types

### Effect Annotations

```affine
// Pure function
fn pure(x: Int) -> Int {
  x * 2
}

// Single effect
fn with_io() -{IO}-> Unit {
  print("Hello")
}

// Multiple effects
fn with_effects() -{IO, State[Int], Exn}-> Int {
  let state = get();
  print("Current: " ++ show(state));
  if state < 0 {
    raise(NegativeError)
  };
  state
}

// Effect polymorphism
fn map_effect[E, A, B](
  f: (A) -{E}-> B,
  opt: Option[A]
) -{E}-> Option[B] {
  match opt {
    Some(a) -> Some(f(a)),
    None -> None
  }
}
```

### Effect Rows

```affine
// Effect row variable
fn combine[e1, e2, A](
  f: () -{e1}-> A,
  g: (A) -{e2}-> A
) -{e1, e2}-> A {
  g(f())
}
```

---

## Refinement Types

### Basic Refinements

```affine
// Positive integers
type PosInt = Int where (self > 0)

// Non-empty strings
type NonEmpty = String where (self.len() > 0)

// Bounded integers
type Percentage = Int where (self >= 0 && self <= 100)

fn divide(x: Int, y: Int where (y != 0)) -> Int {
  x / y
}
```

### Refinement Inference

```affine
fn safe_index[n: Nat, T](
  vec: Vec[n, T],
  idx: Nat where (idx < n)  // Must be in bounds
) -> T {
  vec.data[idx]
}

// Refinement is checked at call site
let v: Vec[3, Int] = [1, 2, 3];
safe_index(v, 0)  // OK: 0 < 3
safe_index(v, 2)  // OK: 2 < 3
safe_index(v, 3)  // ERROR: 3 < 3 is false
```

---

## Type Inference

AffineScript uses bidirectional type inference:

### Inference Examples

```affine
// Type inferred from literal
let x = 42          // x: Int
let y = 3.14        // y: Float64
let z = "hello"     // z: String

// Type inferred from usage
let f = |x| x + 1   // f: (Int) -> Int

// Type inferred from context
let nums = [1, 2, 3]  // nums: [Int; 3]

// Generic type inferred
let opt = Some(42)    // opt: Option[Int]
```

### When Annotations Are Required

```affine
// Ambiguous literals need annotation
let x: Int32 = 42     // Could be Int8, Int16, etc.

// Empty collections
let empty: Vec[0, Int] = []

// Polymorphic return
fn id[T](x: T) -> T { x }
let result: String = id("hello")  // T inferred as String
```

### Type Ascription

```affine
// Inline type annotation
let x = (42: Int64)

// Expression type check
let y = some_function() : ExpectedType
```

---

## Type Aliases

```affine
// Simple alias
type UserId = Int

// Generic alias
type Pair[A] = (A, A)

// Complex alias
type Handler[E, R] = (E) -{IO}-> R

// Recursive alias (requires explicit type)
type JsonValue =
  | Null
  | Bool(Bool)
  | Number(Float64)
  | String(String)
  | Array(Vec[JsonValue])
  | Object(Map[String, JsonValue])
```

---

## See Also

- [Ownership](ownership.md) - Detailed ownership rules
- [Effects](effects.md) - Effect system details
- [Dependent Types](dependent-types.md) - Advanced dependent types
- [Row Polymorphism](rows.md) - Row type details
