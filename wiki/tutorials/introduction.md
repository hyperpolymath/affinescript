# Introduction to AffineScript

AffineScript is a modern systems programming language that combines safety, expressiveness, and performance.

## Why AffineScript?

### The Safety-Performance Tradeoff

Traditional languages force a choice:
- **Safe languages** (Java, Python): Garbage collection, runtime checks
- **Fast languages** (C, C++): Manual memory, undefined behavior

AffineScript achieves both:
- **Memory safe** without garbage collection
- **Zero-cost abstractions** with predictable performance
- **Compile-time verification** catches bugs early

### Key Features

1. **Affine Types**: Memory safety through ownership
2. **Dependent Types**: Verify properties at compile time
3. **Row Polymorphism**: Flexible, type-safe records
4. **Algebraic Effects**: Controlled side effects
5. **WebAssembly Target**: Run anywhere

## Hello World

```affine
fn main() -{IO}-> Unit {
  println("Hello, World!")
}
```

Let's break this down:
- `fn main()` - Function named `main`
- `-{IO}->` - Has IO effect (can do I/O)
- `Unit` - Returns nothing
- `println(...)` - Print to console

## Core Concepts Preview

### Ownership

Values have exactly one owner. When the owner goes out of scope, the value is cleaned up:

```affine
fn example() {
  let s = String::from("hello")  // s owns the string
  println(s)
}  // s goes out of scope, string freed
```

No garbage collector needed!

### Borrowing

Instead of copying, you can borrow:

```affine
fn print_length(s: &String) {
  println("Length: " ++ show(s.len()))
}

fn main() -{IO}-> Unit {
  let s = String::from("hello")
  print_length(&s)  // Borrow s
  println(s)        // Still valid!
}
```

### Type Safety

The type system catches errors at compile time:

```affine
// Compile-time verified bounds checking
fn safe_index[n: Nat, T](
  vec: Vec[n, T],
  i: Nat where (i < n)  // Index must be in bounds
) -> T {
  vec[i]  // Guaranteed safe!
}
```

### Effects

Side effects are tracked in types:

```affine
// Pure function - no effects
fn add(x: Int, y: Int) -> Int {
  x + y
}

// Effectful function - must declare IO
fn greet(name: String) -{IO}-> Unit {
  println("Hello, " ++ name)
}
```

## What Can You Build?

### Systems Software
- Operating system components
- Device drivers
- Embedded systems

### Web Applications
- WebAssembly backends
- High-performance web services
- Browser-based tools

### Financial Systems
- Trading systems
- Smart contracts
- Verified algorithms

### Scientific Computing
- Numerical libraries
- Data processing
- Simulation software

## Getting Started

Ready to learn more?

1. [Installation](installation.md) - Set up your environment
2. [Quick Start](quickstart.md) - Your first project
3. [Tour of Features](tour.md) - Language overview

## Example: Safe Vector Operations

```affine
// Vector with compile-time length
struct Vec[n: Nat, T] {
  data: [T; n]
}

// Safe head - requires non-empty
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T {
  vec.data[0]  // Always safe - at least 1 element
}

// Append with length tracking
fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] {
  // Result has exactly n + m elements
  Vec { data: a.data ++ b.data }
}

fn main() -{IO}-> Unit {
  let v1 = Vec { data: [1, 2, 3] }  // Vec[3, Int]
  let v2 = Vec { data: [4, 5] }    // Vec[2, Int]

  let first = head(v1)             // 1 (safe!)
  let combined = append(v1, v2)    // Vec[5, Int]

  println(show(combined.data))     // [1, 2, 3, 4, 5]
}
```

## Philosophy

AffineScript is designed around:

1. **Safety First**: Memory safety, type safety, effect safety
2. **Predictability**: No hidden costs, clear semantics
3. **Expressiveness**: Rich types without complexity
4. **Practicality**: Real-world systems, not just theory

Welcome to AffineScript!
