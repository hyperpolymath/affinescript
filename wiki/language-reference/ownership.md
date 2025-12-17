# Ownership and Borrowing

AffineScript uses an ownership system inspired by Rust and linear types from type theory. This ensures memory safety without garbage collection.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [Ownership Rules](#ownership-rules)
3. [Moving Values](#moving-values)
4. [Borrowing](#borrowing)
5. [Lifetimes](#lifetimes)
6. [Quantity Annotations](#quantity-annotations)
7. [Patterns](#patterns)
8. [Common Errors](#common-errors)

---

## Core Concepts

### What is Ownership?

Every value in AffineScript has exactly one **owner** - the variable that holds it. When the owner goes out of scope, the value is automatically cleaned up.

```affine
fn example() {
  let s = String::from("hello")  // s owns the string
  // s is valid here
}  // s goes out of scope, string is dropped
```

### The Three Rules

1. **Each value has exactly one owner**
2. **Owned values must be used exactly once** (affine/linear)
3. **When the owner goes out of scope, the value is dropped**

---

## Ownership Rules

### Owned Values (`own`)

Values marked `own` must be consumed exactly once:

```affine
struct File {
  handle: FileHandle
}

fn process(file: own File) {
  // file must be consumed in this function
  file.close()  // Consumes file
}

fn bad_example(file: own File) {
  // ERROR: file is never consumed
}  // Compile error: unused owned value

fn also_bad(file: own File) {
  file.read()
  file.read()  // ERROR: file already used
}
```

### Implicit Ownership

By default, function parameters take ownership:

```affine
fn consume(s: String) {  // Takes ownership
  println(s)
}  // s is dropped here

fn main() {
  let s = String::from("hello")
  consume(s)  // Ownership transferred to consume
  // s is no longer valid here
  println(s)  // ERROR: use of moved value
}
```

### Copy Types

Some types implement `Copy` and are duplicated instead of moved:

```affine
// Primitive types are Copy
let x: Int = 42
let y = x  // x is copied, not moved
println(x)  // OK: x still valid

// Explicit Copy trait
struct Point: Copy {
  x: Float64,
  y: Float64
}

let p1 = Point { x: 0.0, y: 0.0 }
let p2 = p1  // Copied
println(p1.x)  // OK
```

---

## Moving Values

### Move Semantics

When a value is assigned or passed, it **moves** (unless it's `Copy`):

```affine
let s1 = String::from("hello")
let s2 = s1  // s1 is MOVED to s2
// s1 is now invalid

fn take(s: String) {
  // s is owned here
}

let s3 = String::from("world")
take(s3)  // s3 is moved into take()
// s3 is now invalid
```

### Partial Moves

Moving part of a struct makes the whole struct unusable:

```affine
struct Person {
  name: String,
  age: Int
}

let p = Person { name: "Alice".into(), age: 30 }
let n = p.name  // Partial move of 'name'
// p is now partially moved

println(p.age)   // ERROR: p is partially moved
println(p.name)  // ERROR: p.name was moved
```

### Returning Ownership

Functions can return ownership:

```affine
fn create() -> String {
  String::from("created")  // Ownership returned to caller
}

fn transform(s: String) -> String {
  s.to_uppercase()  // Takes ownership, returns new value
}

let s = create()           // s owns the string
let s2 = transform(s)      // s moved in, s2 owns result
```

---

## Borrowing

### Shared Borrows (`&` or `ref`)

Multiple shared borrows allow reading:

```affine
fn calculate_length(s: &String) -> Int {
  s.len()  // Can read s
}  // s is not dropped (we don't own it)

fn main() {
  let s = String::from("hello")
  let len = calculate_length(&s)  // Borrow s
  println(s)  // OK: s still valid
}
```

### Mutable Borrows (`&mut` or `mut ref`)

Exactly one mutable borrow for exclusive access:

```affine
fn append(s: &mut String) {
  s.push_str(" world")  // Can modify s
}

fn main() {
  let mut s = String::from("hello")
  append(&mut s)  // Mutable borrow
  println(s)      // "hello world"
}
```

### Borrowing Rules

1. **At any time, you can have either:**
   - One mutable reference, OR
   - Any number of immutable references

2. **References must always be valid** (no dangling)

```affine
let mut s = String::from("hello")

// OK: Multiple shared borrows
let r1 = &s
let r2 = &s
println(r1, r2)

// ERROR: Can't have mutable while shared exist
let r3 = &mut s  // ERROR if r1, r2 still in use

// OK after r1, r2 are done
println(r1, r2)  // Last use of r1, r2
let r3 = &mut s  // OK now
```

### Reborrowing

Mutable references can be reborrowed:

```affine
fn use_ref(s: &mut String) {
  s.push('!')
}

fn main() {
  let mut s = String::from("hello")
  let r = &mut s

  // Reborrow: r is temporarily "frozen"
  use_ref(r)  // Implicitly reborrows

  r.push('?')  // r usable again
}
```

---

## Lifetimes

### Implicit Lifetimes

Most lifetimes are inferred:

```affine
fn first(s: &String) -> &Char {
  &s.chars()[0]
}  // Lifetime of return tied to input
```

### Explicit Lifetimes

When needed, lifetimes are annotated:

```affine
fn longest['a](x: &'a String, y: &'a String) -> &'a String {
  if x.len() > y.len() { x } else { y }
}

fn main() {
  let s1 = String::from("hello")
  let result;
  {
    let s2 = String::from("hi")
    result = longest(&s1, &s2)
  }  // s2 dropped here

  println(result)  // ERROR: result might reference s2
}
```

### Lifetime Bounds

```affine
struct Reader['a] {
  data: &'a [Byte]
}

impl['a] Reader['a] {
  fn new(data: &'a [Byte]) -> Reader['a] {
    Reader { data }
  }

  fn read(self: &Self) -> &'a [Byte] {
    self.data
  }
}
```

---

## Quantity Annotations

### Linear Types (`1`)

Must be used exactly once:

```affine
fn must_close(file: 1 File) {
  // Must call exactly one consuming method
  file.close()
}

fn bad(file: 1 File) {
  // ERROR: Not used
}

fn also_bad(file: 1 File) {
  file.read()
  file.close()  // ERROR: Used twice
}
```

### Affine Types (default for `own`)

Can be used at most once:

```affine
fn maybe_use(resource: own Resource) {
  if condition {
    resource.consume()
  }
  // OK: resource dropped if not consumed
}
```

### Unrestricted Types (`w`)

Can be used any number of times (for Copy types):

```affine
fn many_uses(x: w Int) -> Int {
  x + x + x  // OK: unrestricted
}
```

### Erased Types (`0`)

Compile-time only, no runtime representation:

```affine
fn phantom[0 T](x: Int) -> Int {
  // T exists for type checking but has no runtime cost
  x
}
```

---

## Patterns

### RAII (Resource Acquisition Is Initialization)

```affine
fn process_file(path: &str) -> Result[(), Error] {
  let file = File::open(path)?
  // file is automatically closed when function returns
  // even if there's an error

  let data = file.read_all()?
  process(data)?

  Ok(())
}  // file.close() called automatically
```

### Handle Pattern

```affine
fn with_file[T](
  path: &str,
  f: (file: &mut File) -> T
) -> Result[T, Error] {
  let mut file = File::open(path)?
  let result = f(&mut file)
  file.close()?
  Ok(result)
}

// Usage
let content = with_file("data.txt", |f| {
  f.read_all()
})?
```

### Builder Pattern

```affine
struct RequestBuilder {
  url: Option[String],
  method: Method,
  headers: Vec[(String, String)]
}

impl RequestBuilder {
  fn new() -> own RequestBuilder { ... }

  fn url(self: own Self, url: String) -> own Self {
    Self { url: Some(url), ..self }
  }

  fn header(self: own Self, k: String, v: String) -> own Self {
    let mut headers = self.headers;
    headers.push((k, v));
    Self { headers, ..self }
  }

  fn build(self: own Self) -> Result[Request, Error] {
    // Consumes builder, returns Request
  }
}

let request = RequestBuilder::new()
  .url("https://example.com")
  .header("Content-Type", "application/json")
  .build()?
```

---

## Common Errors

### Use After Move

```affine
let s = String::from("hello")
let s2 = s
println(s)  // ERROR: value moved
```

**Fix**: Clone if you need both:
```affine
let s = String::from("hello")
let s2 = s.clone()
println(s)  // OK
```

### Borrow While Mutably Borrowed

```affine
let mut v = vec![1, 2, 3]
let first = &v[0]
v.push(4)  // ERROR: v is borrowed
println(first)
```

**Fix**: Don't overlap borrows:
```affine
let mut v = vec![1, 2, 3]
let first = v[0]  // Copy the value
v.push(4)         // OK
println(first)
```

### Dangling Reference

```affine
fn dangle() -> &String {
  let s = String::from("hello")
  &s  // ERROR: s will be dropped
}
```

**Fix**: Return owned value:
```affine
fn no_dangle() -> String {
  String::from("hello")  // Ownership transferred
}
```

### Unused Linear Value

```affine
fn unused(x: 1 Resource) {
  // ERROR: x must be consumed
}
```

**Fix**: Consume or explicitly drop:
```affine
fn used(x: 1 Resource) {
  x.close()  // Consumed
}

fn dropped(x: 1 Resource) {
  drop(x)  // Explicitly dropped
}
```

---

## See Also

- [Types](types.md) - Type system overview
- [Effects](effects.md) - Effect system
- [Memory Model](../design/memory.md) - Memory layout details
