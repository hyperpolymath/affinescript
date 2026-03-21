# Quick Start Guide

Get up and running with AffineScript in 10 minutes.

## Prerequisites

- OCaml 5.1 or later
- opam package manager
- dune build system

## Installation

```bash
# Clone the repository
git clone https://github.com/hyperpolymath/affinescript
cd affinescript

# Set up OCaml environment
opam switch create . 5.1.0
eval $(opam env)

# Install dependencies
opam install . --deps-only --with-test

# Build
dune build

# Verify installation
dune exec affinescript -- --version
```

## Your First Program

Create a file `hello.as`:

```affine
fn main() -{IO}-> Unit {
  println("Hello, AffineScript!")
}
```

Run it:

```bash
dune exec affinescript -- run hello.as
```

Output:
```
Hello, AffineScript!
```

## Variables and Types

```affine
fn main() -{IO}-> Unit {
  // Immutable binding
  let x = 42
  let name = "Alice"
  let pi = 3.14159

  // Type annotations (optional)
  let y: Int = 100
  let active: Bool = true

  // Mutable binding
  let mut counter = 0
  counter += 1

  println("x = " ++ show(x))
  println("counter = " ++ show(counter))
}
```

## Functions

```affine
// Basic function
fn add(x: Int, y: Int) -> Int {
  x + y
}

// Function with effects
fn greet(name: String) -{IO}-> Unit {
  println("Hello, " ++ name ++ "!")
}

// Generic function
fn identity[T](x: T) -> T {
  x
}

fn main() -{IO}-> Unit {
  let sum = add(1, 2)
  greet("World")
  let x = identity(42)
  let s = identity("hello")

  println("sum = " ++ show(sum))
}
```

## Control Flow

```affine
fn main() -{IO}-> Unit {
  let x = 10

  // If expression
  let description = if x > 0 {
    "positive"
  } else if x < 0 {
    "negative"
  } else {
    "zero"
  }

  // Match expression
  let result = match x {
    0 -> "zero",
    1..10 -> "small",
    _ -> "large"
  }

  // Loops
  let mut sum = 0
  for i in 0..5 {
    sum += i
  }

  while sum < 100 {
    sum *= 2
  }

  println(description)
  println(result)
  println("sum = " ++ show(sum))
}
```

## Data Types

### Structs

```affine
struct Point {
  x: Float64,
  y: Float64
}

impl Point {
  fn new(x: Float64, y: Float64) -> Point {
    Point { x, y }
  }

  fn distance(self: &Self, other: &Point) -> Float64 {
    let dx = other.x - self.x
    let dy = other.y - self.y
    (dx * dx + dy * dy).sqrt()
  }
}

fn main() -{IO}-> Unit {
  let p1 = Point::new(0.0, 0.0)
  let p2 = Point { x: 3.0, y: 4.0 }

  let d = p1.distance(&p2)
  println("Distance: " ++ show(d))  // 5.0
}
```

### Enums

```affine
enum Color {
  Red,
  Green,
  Blue,
  Rgb(Int, Int, Int)
}

fn color_name(c: Color) -> String {
  match c {
    Red -> "red",
    Green -> "green",
    Blue -> "blue",
    Rgb(r, g, b) -> format("rgb({}, {}, {})", r, g, b)
  }
}

fn main() -{IO}-> Unit {
  let c1 = Color::Red
  let c2 = Color::Rgb(255, 128, 0)

  println(color_name(c1))
  println(color_name(c2))
}
```

## Option and Result

```affine
fn divide(x: Int, y: Int) -> Option[Int] {
  if y == 0 {
    None
  } else {
    Some(x / y)
  }
}

fn parse_int(s: String) -> Result[Int, String] {
  // Simplified example
  match s.parse::<Int>() {
    Some(n) -> Ok(n),
    None -> Err("Invalid number: " ++ s)
  }
}

fn main() -{IO}-> Unit {
  // Option handling
  match divide(10, 2) {
    Some(result) -> println("Result: " ++ show(result)),
    None -> println("Cannot divide by zero")
  }

  // Or with combinators
  let doubled = divide(10, 2)
    .map(|n| n * 2)
    .unwrap_or(0)

  // Result with ?
  fn process() -> Result[Int, String] {
    let x = parse_int("42")?
    let y = parse_int("10")?
    Ok(x + y)
  }
}
```

## Ownership Basics

```affine
fn main() -{IO}-> Unit {
  // Ownership transfer (move)
  let s1 = String::from("hello")
  let s2 = s1  // s1 is moved to s2
  // println(s1)  // Error! s1 is no longer valid

  // Borrowing
  let s3 = String::from("world")
  print_string(&s3)  // Borrow s3
  println(s3)        // s3 still valid

  // Mutable borrowing
  let mut s4 = String::from("hello")
  append_world(&mut s4)
  println(s4)  // "hello world"
}

fn print_string(s: &String) -{IO}-> Unit {
  println(s)
}

fn append_world(s: &mut String) {
  s.push_str(" world")
}
```

## Collections

```affine
fn main() -{IO}-> Unit {
  // Vector
  let mut numbers: Vec[Int] = vec![1, 2, 3]
  numbers.push(4)
  numbers.push(5)

  // Iteration
  for n in numbers.iter() {
    println(show(n))
  }

  // Functional operations
  let doubled = numbers.iter()
    .map(|n| n * 2)
    .collect::<Vec[Int]>()

  let sum = numbers.iter().fold(0, |acc, n| acc + n)

  // HashMap
  let mut scores: HashMap[String, Int] = HashMap::new()
  scores.insert("Alice", 100)
  scores.insert("Bob", 85)

  match scores.get("Alice") {
    Some(score) -> println("Alice: " ++ show(score)),
    None -> println("Alice not found")
  }
}
```

## Error Handling

```affine
fn read_config(path: String) -> Result[Config, Error] {
  let content = fs::read_to_string(path)?  // Propagate errors
  let config = parse_config(&content)?
  Ok(config)
}

fn main() -{IO}-> Unit {
  match read_config("config.toml") {
    Ok(config) -> {
      println("Loaded config")
      run_with_config(config)
    },
    Err(e) -> {
      eprintln("Error: " ++ e.message)
      exit(1)
    }
  }
}
```

## Next Steps

You've learned the basics! Continue with:

1. [Ownership Deep Dive](ownership-tutorial.md) - Master ownership
2. [Effects Tutorial](effects-tutorial.md) - Understand effects
3. [Dependent Types](dependent-types-tutorial.md) - Type-level programming

## Example Project

Create a simple TODO app:

```affine
struct Todo {
  id: Int,
  title: String,
  done: Bool
}

struct TodoList {
  todos: Vec[Todo],
  next_id: Int
}

impl TodoList {
  fn new() -> TodoList {
    TodoList { todos: vec![], next_id: 1 }
  }

  fn add(self: &mut Self, title: String) -> Int {
    let id = self.next_id
    self.next_id += 1
    self.todos.push(Todo { id, title, done: false })
    id
  }

  fn complete(self: &mut Self, id: Int) -> Bool {
    for todo in self.todos.iter_mut() {
      if todo.id == id {
        todo.done = true
        return true
      }
    }
    false
  }

  fn list(self: &Self) -{IO}-> Unit {
    for todo in self.todos.iter() {
      let status = if todo.done { "[x]" } else { "[ ]" }
      println(format("{} {} {}", status, todo.id, todo.title))
    }
  }
}

fn main() -{IO}-> Unit {
  let mut todos = TodoList::new()

  todos.add("Learn AffineScript")
  todos.add("Build something cool")
  todos.add("Share with others")

  todos.complete(1)

  todos.list()
}
```

Output:
```
[x] 1 Learn AffineScript
[ ] 2 Build something cool
[ ] 3 Share with others
```

Happy coding!
