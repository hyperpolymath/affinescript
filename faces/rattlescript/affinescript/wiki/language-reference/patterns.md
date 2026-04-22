# Pattern Matching

Pattern matching is a powerful feature for destructuring and conditionally binding values.

## Table of Contents

1. [Basic Patterns](#basic-patterns)
2. [Compound Patterns](#compound-patterns)
3. [Match Expressions](#match-expressions)
4. [Let Patterns](#let-patterns)
5. [Function Parameters](#function-parameters)
6. [Guards](#guards)
7. [Exhaustiveness](#exhaustiveness)

---

## Basic Patterns

### Wildcard Pattern

Matches anything, binds nothing:

```affine
match value {
  _ -> "matched anything"
}

let _ = unused_result()  // Discard value
```

### Variable Pattern

Matches anything, binds to name:

```affine
match value {
  x -> "bound to x: " ++ show(x)
}

let x = some_value  // Bind to x
```

### Literal Patterns

Match specific values:

```affine
match number {
  0 -> "zero",
  1 -> "one",
  42 -> "answer",
  _ -> "other"
}

match char {
  'a'..'z' -> "lowercase",
  'A'..'Z' -> "uppercase",
  '0'..'9' -> "digit",
  _ -> "other"
}

match string {
  "hello" -> "greeting",
  "" -> "empty",
  _ -> "other"
}
```

---

## Compound Patterns

### Tuple Patterns

```affine
let (x, y) = (1, 2)
let (first, _, third) = (1, 2, 3)  // Ignore second

match pair {
  (0, 0) -> "origin",
  (x, 0) -> "on x-axis",
  (0, y) -> "on y-axis",
  (x, y) -> "at (" ++ show(x) ++ ", " ++ show(y) ++ ")"
}
```

### Record Patterns

```affine
struct Point { x: Int, y: Int }

let Point { x, y } = point
let Point { x: a, y: b } = point  // Rename bindings

match point {
  Point { x: 0, y: 0 } -> "origin",
  Point { x: 0, .. } -> "on y-axis",
  Point { y: 0, .. } -> "on x-axis",
  Point { x, y } -> format("({}, {})", x, y)
}
```

### Constructor Patterns

```affine
enum Option[T] {
  Some(T),
  None
}

match opt {
  Some(value) -> "has " ++ show(value),
  None -> "empty"
}

enum Result[T, E] {
  Ok(T),
  Err(E)
}

match result {
  Ok(value) -> process(value),
  Err(e) -> handle_error(e)
}
```

### Nested Patterns

```affine
match value {
  Some(Some(x)) -> "doubly wrapped: " ++ show(x),
  Some(None) -> "wrapped None",
  None -> "outer None"
}

match data {
  Ok(Person { name, age: 0..17 }) -> "minor: " ++ name,
  Ok(Person { name, age }) -> name ++ " (" ++ show(age) ++ ")",
  Err(e) -> "error: " ++ e.message
}
```

---

## Match Expressions

### Basic Match

```affine
let description = match color {
  Color::Red -> "red",
  Color::Green -> "green",
  Color::Blue -> "blue"
}
```

### Match with Blocks

```affine
match event {
  Event::Click { x, y } -> {
    log("Click at ({}, {})", x, y)
    handle_click(x, y)
  },
  Event::KeyPress { key, modifiers } -> {
    log("Key: {}", key)
    handle_key(key, modifiers)
  },
  Event::Resize { width, height } -> {
    log("Resize to {}x{}", width, height)
    resize(width, height)
  }
}
```

### Or Patterns

```affine
match day {
  "Saturday" | "Sunday" -> "weekend",
  _ -> "weekday"
}

match number {
  0 | 1 -> "binary",
  2..9 -> "single digit",
  _ -> "multiple digits"
}
```

### Binding with `@`

```affine
match value {
  opt @ Some(_) -> {
    // opt is the whole Some value
    process(opt)
  },
  None -> default()
}

match number {
  n @ 1..100 -> {
    // n is bound AND must be in range
    "small number: " ++ show(n)
  },
  n -> "large number: " ++ show(n)
}
```

---

## Let Patterns

### Irrefutable Patterns in Let

```affine
// Always succeeds
let (x, y) = get_pair()
let Point { x, y } = get_point()
let [a, b, c] = [1, 2, 3]
```

### Let-Else for Refutable Patterns

```affine
// Pattern might not match
let Some(value) = opt else {
  return None
}

let Ok(data) = result else {
  panic("Expected Ok")
}

let [first, ..rest] = list else {
  return "empty list"
}
```

---

## Function Parameters

### Destructuring in Parameters

```affine
fn distance((x1, y1): (Float, Float), (x2, y2): (Float, Float)) -> Float {
  let dx = x2 - x1
  let dy = y2 - y1
  (dx * dx + dy * dy).sqrt()
}

fn greet(Person { name, .. }: Person) -> String {
  "Hello, " ++ name
}

fn process_result(Ok(value) | Err(value): Result[Int, Int]) -> Int {
  value
}
```

---

## Guards

### Pattern Guards

```affine
match number {
  n if n < 0 -> "negative",
  n if n == 0 -> "zero",
  n if n > 0 -> "positive"
}

match point {
  Point { x, y } if x == y -> "on diagonal",
  Point { x, y } if x == -y -> "on anti-diagonal",
  Point { x, y } -> "elsewhere"
}
```

### Complex Guards

```affine
match user {
  User { role: Admin, .. } if is_authenticated() -> {
    show_admin_panel()
  },
  User { role: Member, subscription } if subscription.is_active() -> {
    show_member_content()
  },
  _ -> {
    show_login_prompt()
  }
}
```

---

## Exhaustiveness

### Exhaustive Matching

The compiler checks that all cases are covered:

```affine
enum Direction { North, South, East, West }

// Exhaustive - all cases covered
match dir {
  North -> (0, 1),
  South -> (0, -1),
  East -> (1, 0),
  West -> (-1, 0)
}

// ERROR: non-exhaustive
match dir {
  North -> (0, 1),
  South -> (0, -1)
}
// error: patterns `East` and `West` not covered
```

### Non-Exhaustive with Wildcard

```affine
match http_status {
  200 -> "OK",
  404 -> "Not Found",
  500 -> "Server Error",
  _ -> "Unknown Status"  // Catch-all
}
```

### Unreachable Patterns

```affine
match value {
  _ -> "catches all",
  42 -> "never reached"  // WARNING: unreachable pattern
}
```

---

## Advanced Patterns

### Slice Patterns

```affine
match array {
  [] -> "empty",
  [x] -> "single: " ++ show(x),
  [x, y] -> "pair: " ++ show(x) ++ ", " ++ show(y),
  [first, ..middle, last] -> {
    "first: " ++ show(first) ++ ", last: " ++ show(last)
  }
}
```

### Reference Patterns

```affine
match &opt {
  &Some(ref x) -> {
    // x is a reference, opt not consumed
    println(x)
  },
  &None -> println("empty")
}
```

### Box Patterns

```affine
match boxed_value {
  Box(inner) -> process(inner)
}
```

---

## See Also

- [Types](types.md) - Enum and struct types
- [Expressions](expressions.md) - Match expressions
- [Functions](functions.md) - Parameter patterns
