# Dependent Types

Dependent types allow types to depend on values, enabling compile-time verification of invariants.

## Table of Contents

1. [Introduction](#introduction)
2. [Indexed Types](#indexed-types)
3. [Pi Types](#pi-types)
4. [Refinement Types](#refinement-types)
5. [Proof Terms](#proof-terms)
6. [Type-Level Computation](#type-level-computation)
7. [Practical Examples](#practical-examples)

---

## Introduction

### What Are Dependent Types?

In most languages, types and values are separate:
```affine
// Non-dependent: Type doesn't know about length
fn head[T](vec: Vec[T]) -> Option[T]
```

With dependent types, types can mention values:
```affine
// Dependent: Type knows length is at least 1
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T
```

### Why Dependent Types?

1. **Eliminate runtime errors** - Prove properties at compile time
2. **Self-documenting code** - Types express invariants
3. **Safer refactoring** - Compiler catches more mistakes
4. **Better optimization** - Compiler knows more

---

## Indexed Types

### Length-Indexed Vectors

```affine
// Vec is indexed by its length
struct Vec[n: Nat, T] {
  data: [T; n]
}

// Empty vector has length 0
fn empty[T]() -> Vec[0, T] {
  Vec { data: [] }
}

// Cons increases length by 1
fn cons[n: Nat, T](x: T, xs: Vec[n, T]) -> Vec[n + 1, T] {
  Vec { data: [x, ..xs.data] }
}

// Head requires non-empty vector
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T {
  vec.data[0]  // Safe! We know length >= 1
}

// Tail decreases length by 1
fn tail[n: Nat, T](vec: Vec[n + 1, T]) -> Vec[n, T] {
  Vec { data: vec.data[1..] }
}
```

### Safe Operations

```affine
// Append preserves exact length information
fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] {
  // Result has exactly n + m elements
}

// Map preserves length
fn map[n: Nat, A, B](f: (A) -> B, vec: Vec[n, A]) -> Vec[n, B] {
  // Output has same length as input
}

// Zip requires same length
fn zip[n: Nat, A, B](
  a: Vec[n, A],
  b: Vec[n, B]
) -> Vec[n, (A, B)] {
  // Both inputs must have same length
}
```

### Usage Examples

```affine
let v1: Vec[3, Int] = cons(1, cons(2, cons(3, empty())))
let v2: Vec[2, Int] = cons(4, cons(5, empty()))

let first = head(v1)     // OK: v1 has 3 elements
let rest = tail(v1)      // rest: Vec[2, Int]
let combined = append(v1, v2)  // combined: Vec[5, Int]

// let bad = head(empty())  // ERROR: Vec[0, T] doesn't match Vec[n+1, T]
```

---

## Pi Types

### Dependent Functions

Pi types (dependent function types) allow the return type to depend on the input value:

```affine
// Return type depends on input n
fn replicate[T](n: Nat, x: T) -> Vec[n, T] {
  // Creates exactly n copies of x
}

// Different inputs, different output types
replicate(3, "hi")  // Vec[3, String]
replicate(5, 42)    // Vec[5, Int]
```

### Implicit Arguments

```affine
// {n: Nat} is an implicit argument, inferred from context
fn length[{n: Nat}, T](vec: Vec[n, T]) -> Nat {
  n  // Just return the index!
}

let v: Vec[5, Int] = ...
length(v)  // Returns 5, n inferred
```

### First-Class Length

```affine
// Return the length as a value
fn len[{n: Nat}, T](vec: Vec[n, T]) -> (m: Nat, m == n) {
  (n, refl)  // Return n with proof it equals n
}
```

---

## Refinement Types

### Basic Refinements

Refinement types constrain values with predicates:

```affine
// Type of positive integers
type Pos = Int where (self > 0)

// Type of percentages
type Percent = Int where (self >= 0 && self <= 100)

// Type of valid indices
type Index[n: Nat] = Nat where (self < n)
```

### Using Refinements

```affine
fn divide(x: Int, y: Int where (y != 0)) -> Int {
  x / y  // Safe: y cannot be zero
}

fn safe_index[n: Nat, T](
  vec: Vec[n, T],
  i: Nat where (i < n)
) -> T {
  vec.data[i]  // Safe: i is in bounds
}

// At call site, refinement must be satisfied
divide(10, 2)  // OK
divide(10, 0)  // ERROR: 0 != 0 is false
```

### Refinement Inference

```affine
fn example(x: Int where (x > 0)) -> Int where (result > 0) {
  x + 1  // Compiler proves: x > 0 => x + 1 > 0
}

fn array_access(arr: [Int; 10], i: Int) -> Int {
  if i >= 0 && i < 10 {
    arr[i]  // Compiler knows: 0 <= i < 10
  } else {
    0
  }
}
```

### SMT Integration

The compiler uses SMT solvers (Z3/CVC5) for refinement checking:

```affine
fn complex_refinement(
  x: Int where (x > 10),
  y: Int where (y > 5 && y < x)
) -> Int where (result > 15) {
  x + y  // SMT proves: x > 10 && y > 5 => x + y > 15
}
```

---

## Proof Terms

### Equality Proofs

```affine
// Propositional equality type
type (==)[A, a: A, b: A]

// Reflexivity: a == a
fn refl[A, a: A]() -> (a == a)

// Symmetry: a == b => b == a
fn sym[A, a: A, b: A](pf: a == b) -> (b == a)

// Transitivity: a == b && b == c => a == c
fn trans[A, a: A, b: A, c: A](pf1: a == b, pf2: b == c) -> (a == c)
```

### Using Equality

```affine
fn append_length[n: Nat, m: Nat, T](
  xs: Vec[n, T],
  ys: Vec[m, T]
) -> (result: Vec[n + m, T], length(result) == n + m) {
  let combined = append(xs, ys)
  (combined, refl())  // Proof that length equals n + m
}
```

### Transport

```affine
// If a == b, we can substitute a for b in any type
fn transport[A, P: A -> Type, a: A, b: A](
  pf: a == b,
  pa: P(a)
) -> P(b) {
  // "Transport" pa along the equality proof
}

// Example: convert Vec[n, T] to Vec[m, T] when n == m
fn coerce_length[n: Nat, m: Nat, T](
  pf: n == m,
  vec: Vec[n, T]
) -> Vec[m, T] {
  transport(pf, vec)
}
```

---

## Type-Level Computation

### Type-Level Arithmetic

```affine
// Types can compute
type Add[n: Nat, m: Nat] = n + m
type Mul[n: Nat, m: Nat] = n * m

// Example with matrix dimensions
struct Matrix[rows: Nat, cols: Nat, T] { ... }

fn matmul[m: Nat, n: Nat, p: Nat, T: Num](
  a: Matrix[m, n, T],
  b: Matrix[n, p, T]
) -> Matrix[m, p, T] {
  // Inner dimensions must match (both n)
  // Result has outer dimensions (m x p)
}
```

### Type-Level Booleans

```affine
type If[cond: Bool, then: Type, else: Type] =
  match cond {
    true -> then,
    false -> else
  }

// Conditionally include field
struct MaybeNamed[named: Bool] {
  value: Int,
  name: If[named, String, Unit]
}
```

### Type-Level Lists

```affine
// Heterogeneous lists with type-level length
type HList[types: [Type]] =
  match types {
    [] -> Unit,
    [T, ..Ts] -> (T, HList[Ts])
  }

let hlist: HList[[Int, String, Bool]] = (42, ("hello", (true, ())))
```

---

## Practical Examples

### Safe Array Operations

```affine
struct Array[n: Nat, T] {
  data: [T; n]
}

impl[n: Nat, T] Array[n, T] {
  // Index with compile-time bounds check
  fn get(self: &Self, i: Nat where (i < n)) -> &T {
    &self.data[i]
  }

  // Slice with bounds preserved
  fn slice[start: Nat, len: Nat](
    self: &Self
  ) -> Array[len, T]
  where
    start + len <= n
  {
    Array { data: self.data[start..start+len] }
  }
}
```

### Type-Safe State Machines

```affine
// States as types
struct Disconnected { }
struct Connecting { addr: String }
struct Connected { socket: Socket }

// Transitions encoded in types
fn connect(state: Disconnected, addr: String) -> Connecting {
  Connecting { addr }
}

fn established(state: Connecting, socket: Socket) -> Connected {
  Connected { socket }
}

fn send(state: &Connected, data: &[Byte]) -> Result[(), Error] {
  state.socket.send(data)
}

fn disconnect(state: Connected) -> Disconnected {
  state.socket.close()
  Disconnected { }
}

// Cannot call send on Disconnected - type error!
```

### Units of Measure

```affine
// Phantom type for units
struct Quantity[unit: Unit, T] {
  value: T
}

// Units
type Meters
type Seconds
type MetersPerSecond = Meters / Seconds

fn distance(speed: Quantity[MetersPerSecond, Float], time: Quantity[Seconds, Float]) -> Quantity[Meters, Float] {
  Quantity { value: speed.value * time.value }
}

// Type error if units don't match!
let d = distance(
  Quantity[MetersPerSecond] { value: 10.0 },
  Quantity[Seconds] { value: 5.0 }
)  // d: Quantity[Meters, Float]
```

### Protocol Verification

```affine
// Message types indexed by protocol state
enum Message[from: State, to: State] {
  Hello: Message[Init, WaitingForHello],
  Ack: Message[WaitingForHello, Ready],
  Data(bytes: [Byte]): Message[Ready, Ready],
  Close: Message[Ready, Closed]
}

// Protocol must follow valid transitions
fn protocol_step[s1: State, s2: State](
  state: s1,
  msg: Message[s1, s2]
) -> s2 {
  // Type system ensures only valid transitions
}
```

---

## Limitations

### Decidability

Not all type-level computations are decidable. The compiler restricts:
- Terminating recursion only
- Limited universe of types
- SMT-solvable refinements

### Ergonomics

Dependent types require:
- More type annotations
- Explicit proofs in some cases
- Understanding of type theory

### Performance

Type checking dependent types is more expensive:
- SMT solver calls
- Type-level evaluation
- More complex unification

---

## See Also

- [Types](types.md) - Basic type system
- [Refinement Types](../design/type-system.md#refinements) - Theory
- [Vectors Example](../../examples/vectors.as) - Length-indexed vectors
