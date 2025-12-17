# Row Polymorphism

Row polymorphism enables flexible, type-safe operations on extensible records and variants.

## Table of Contents

1. [Introduction](#introduction)
2. [Row Types](#row-types)
3. [Record Operations](#record-operations)
4. [Polymorphic Functions](#polymorphic-functions)
5. [Row Constraints](#row-constraints)
6. [Variants](#variants)
7. [Effect Rows](#effect-rows)
8. [Advanced Patterns](#advanced-patterns)

---

## Introduction

### The Problem

Traditional record types are rigid:

```affine
struct Person { name: String, age: Int }
struct Employee { name: String, age: Int, department: String }

// These are completely different types!
// Can't write a function that works on both
```

### Row Polymorphism Solution

Row polymorphism allows functions to work on records with certain fields, regardless of other fields:

```affine
// Works on ANY record with a 'name' field
fn greet[r](person: {name: String, ..r}) -> String {
  "Hello, " ++ person.name
}

greet({name: "Alice"})  // OK
greet({name: "Bob", age: 30})  // OK
greet({name: "Carol", department: "Engineering", salary: 100000})  // OK
```

---

## Row Types

### Closed Rows

All fields are known:

```affine
// Exactly these fields, nothing more
type Point = {x: Float64, y: Float64}

let p: Point = {x: 1.0, y: 2.0}
// let bad: Point = {x: 1.0, y: 2.0, z: 3.0}  // ERROR: extra field
```

### Open Rows

Additional fields allowed:

```affine
// Has x and y, plus any other fields
type HasXY = {x: Float64, y: Float64, ..}

let p1: HasXY = {x: 1.0, y: 2.0}           // OK
let p2: HasXY = {x: 1.0, y: 2.0, z: 3.0}   // OK
let p3: HasXY = {x: 1.0, y: 2.0, name: "origin"}  // OK
```

### Row Variables

Named row variables for polymorphism:

```affine
// r is a row variable - represents "the rest of the fields"
fn with_x[r](rec: {..r}) -> {x: Int, ..r} {
  {x: 42, ..rec}
}

let a = with_x({})           // {x: 42}
let b = with_x({y: 1})       // {x: 42, y: 1}
let c = with_x({y: 1, z: 2}) // {x: 42, y: 1, z: 2}
```

---

## Record Operations

### Field Access

```affine
let rec = {x: 1, y: 2, z: 3}

rec.x  // 1
rec.y  // 2
```

### Record Construction

```affine
// Literal syntax
let point = {x: 1.0, y: 2.0}

// From variables (shorthand)
let x = 1.0
let y = 2.0
let point = {x, y}  // Same as {x: x, y: y}
```

### Record Extension

```affine
let base = {x: 1, y: 2}
let extended = {z: 3, ..base}  // {z: 3, x: 1, y: 2}

// Extension overrides existing fields
let updated = {x: 10, ..base}  // {x: 10, y: 2}
```

### Record Update

```affine
let rec = {x: 1, y: 2, z: 3}

// Update specific fields
let rec2 = {rec with x = 10}  // {x: 10, y: 2, z: 3}
let rec3 = {rec with x = 10, y = 20}  // {x: 10, y: 20, z: 3}
```

### Record Restriction

```affine
let rec = {x: 1, y: 2, z: 3}

// Remove a field
let rec2 = rec \ z  // {x: 1, y: 2}
let rec3 = rec \ x \ y  // {z: 3}
```

---

## Polymorphic Functions

### Working with Specific Fields

```affine
// Access 'name' field from any record that has it
fn get_name[r](rec: {name: String, ..r}) -> String {
  rec.name
}

// Access multiple fields
fn full_name[r](person: {first: String, last: String, ..r}) -> String {
  person.first ++ " " ++ person.last
}
```

### Transforming Records

```affine
// Add a field
fn add_id[r](rec: {..r}) -> {id: Int, ..r} {
  {id: generate_id(), ..rec}
}

// Modify a field
fn uppercase_name[r](rec: {name: String, ..r}) -> {name: String, ..r} {
  {rec with name = rec.name.to_uppercase()}
}

// Remove a field
fn remove_secret[r](rec: {secret: String, ..r}) -> {..r} {
  rec \ secret
}
```

### Preserving Extra Fields

```affine
fn process_person[r](person: {name: String, age: Int, ..r}) -> {name: String, age: Int, ..r} {
  // Process and return - extra fields preserved!
  {person with age = person.age + 1}
}

let employee = {name: "Alice", age: 30, department: "Eng", salary: 100000}
let updated = process_person(employee)
// updated: {name: "Alice", age: 31, department: "Eng", salary: 100000}
// department and salary preserved!
```

---

## Row Constraints

### Lacks Constraint

Ensure a field is NOT present:

```affine
fn safe_add_x[r](rec: {..r}) -> {x: Int, ..r}
where
  r lacks x  // r must not already have 'x'
{
  {x: 0, ..rec}
}

safe_add_x({y: 1})      // OK: {x: 0, y: 1}
// safe_add_x({x: 1})   // ERROR: r already has 'x'
```

### Has Constraint

Ensure a field IS present:

```affine
fn requires_name[r](rec: {..r}) -> String
where
  r has name: String
{
  rec.name
}
```

### Multiple Constraints

```affine
fn complex[r](rec: {..r}) -> {id: Int, ..r}
where
  r has name: String,
  r lacks id
{
  let id = hash(rec.name)
  {id, ..rec}
}
```

---

## Variants

Row polymorphism also works with variants (sum types):

### Extensible Variants

```affine
// Open variant type
type Error = [
  | NotFound(String)
  | Unauthorized
  | ..
]

fn handle_error[r](err: [NotFound(String) | Unauthorized | ..r]) -> String {
  match err {
    NotFound(path) -> "Not found: " ++ path,
    Unauthorized -> "Not authorized",
    other -> "Other error"  // Handles ..r
  }
}
```

### Variant Extension

```affine
type BaseError = [NotFound(String) | Unauthorized]
type ExtendedError = [Timeout | ..BaseError]
// ExtendedError = [Timeout | NotFound(String) | Unauthorized]
```

### Polymorphic Variant Functions

```affine
fn map_error[e1, e2](
  result: Result[T, [..e1]],
  f: ([..e1]) -> [..e2]
) -> Result[T, [..e2]] {
  match result {
    Ok(v) -> Ok(v),
    Err(e) -> Err(f(e))
  }
}
```

---

## Effect Rows

Effects use row polymorphism internally:

### Effect Row Variables

```affine
// e is an effect row variable
fn map_effect[e, A, B](f: (A) -{e}-> B, opt: Option[A]) -{e}-> Option[B] {
  match opt {
    Some(a) -> Some(f(a)),
    None -> None
  }
}

// Works with any effects
map_effect(|x| x + 1, Some(5))       // Pure
map_effect(|x| { print(x); x }, Some(5))  // With IO
```

### Effect Combination

```affine
fn combine[e1, e2](
  f: () -{e1}-> Int,
  g: () -{e2}-> Int
) -{e1, e2}-> Int {
  f() + g()
}
```

---

## Advanced Patterns

### Record-Based APIs

```affine
// Configuration with defaults
type Config = {
  host: String,
  port: Int,
  timeout: Int,
  ..
}

fn with_defaults[r](partial: {..r}) -> {host: String, port: Int, timeout: Int, ..r}
where
  r lacks host,
  r lacks port,
  r lacks timeout
{
  {
    host: "localhost",
    port: 8080,
    timeout: 30,
    ..partial
  }
}

let config = with_defaults({debug: true, max_connections: 100})
// {host: "localhost", port: 8080, timeout: 30, debug: true, max_connections: 100}
```

### Builder Pattern with Rows

```affine
fn builder[r]() -> {..r} where r = {} {
  {}
}

fn with_name[r](b: {..r}, name: String) -> {name: String, ..r}
where r lacks name
{
  {name, ..b}
}

fn with_age[r](b: {..r}, age: Int) -> {age: Int, ..r}
where r lacks age
{
  {age, ..b}
}

let person = builder()
  |> with_name("Alice")
  |> with_age(30)
// {name: "Alice", age: 30}
```

### Lenses with Rows

```affine
// A lens focuses on a field
struct Lens[S, A] {
  get: (S) -> A,
  set: (S, A) -> S
}

fn field_lens[r, A](field: String) -> Lens[{field: A, ..r}, A] {
  Lens {
    get: |s| s.field,
    set: |s, a| {s with field = a}
  }
}

let name_lens = field_lens[_, String]("name")
name_lens.get({name: "Alice", age: 30})  // "Alice"
name_lens.set({name: "Alice", age: 30}, "Bob")  // {name: "Bob", age: 30}
```

### Structural Subtyping

```affine
// Wider records can be used where narrower expected
fn needs_point(p: {x: Float64, y: Float64}) -> Float64 {
  p.x + p.y
}

let point3d = {x: 1.0, y: 2.0, z: 3.0}
needs_point(point3d)  // OK: point3d has x and y
```

---

## Implementation Notes

### Row Unification

The type checker unifies row types:

```
{x: Int, y: String, ..r1} ~ {y: String, z: Bool, ..r2}

Unifies to:
r1 = {z: Bool, ..r3}
r2 = {x: Int, ..r3}
```

### Performance

Row polymorphism is typically monomorphized:
- At compile time, concrete record types are generated
- Runtime performance equals hand-written code
- Some code size increase from specialization

### Limitations

- Cannot iterate over all fields (no reflection)
- Row variable cannot be "split" arbitrarily
- Some complex constraints are undecidable

---

## See Also

- [Types](types.md) - Record types
- [Effects](effects.md) - Effect rows
- [Design: Rows](../design/type-system.md#rows) - Theory
