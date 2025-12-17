# Algebraic Effects

AffineScript features an algebraic effect system for controlled side effects, enabling pure functional programming with practical I/O capabilities.

## Table of Contents

1. [Introduction](#introduction)
2. [Declaring Effects](#declaring-effects)
3. [Using Effects](#using-effects)
4. [Effect Handlers](#effect-handlers)
5. [Effect Polymorphism](#effect-polymorphism)
6. [Standard Effects](#standard-effects)
7. [Advanced Patterns](#advanced-patterns)
8. [Implementation Notes](#implementation-notes)

---

## Introduction

### Why Effects?

Traditional approaches to side effects:
- **Impure functions**: No control, hard to reason about
- **Monads**: Powerful but complex, don't compose easily
- **Effect systems**: Track effects in types, compose naturally

AffineScript uses **algebraic effects**:
- Effects are declared explicitly
- Functions declare what effects they need
- Effects are handled (interpreted) by handlers
- Pure core with controlled impurity at boundaries

### Basic Example

```affine
// Declare an effect
effect Console {
  fn print(msg: String) -> Unit
  fn read() -> String
}

// Use the effect
fn greet() -{Console}-> Unit {
  print("What's your name?")
  let name = read()
  print("Hello, " ++ name ++ "!")
}

// Handle the effect
fn main() -{IO}-> Unit {
  handle greet() {
    print(msg) -> {
      io_print(msg)
      resume(())
    },
    read() -> {
      let line = io_read_line()
      resume(line)
    }
  }
}
```

---

## Declaring Effects

### Effect Syntax

```affine
effect EffectName[TypeParams] {
  fn operation1(params) -> ReturnType
  fn operation2(params) -> ReturnType
  // ...
}
```

### Simple Effect

```affine
effect Log {
  fn log(level: Level, msg: String) -> Unit
}

enum Level { Debug, Info, Warn, Error }
```

### Parameterized Effect

```affine
// State effect with type parameter
effect State[S] {
  fn get() -> S
  fn put(s: S) -> Unit
}

// Error effect with error type
effect Error[E] {
  fn raise(e: E) -> Never
}
```

### Effect with Multiple Operations

```affine
effect FileSystem {
  fn read_file(path: String) -> Result[String, IoError]
  fn write_file(path: String, content: String) -> Result[Unit, IoError]
  fn delete_file(path: String) -> Result[Unit, IoError]
  fn exists(path: String) -> Bool
}
```

---

## Using Effects

### Effect Annotations

Functions declare effects in their type:

```affine
// No effects (pure)
fn add(x: Int, y: Int) -> Int {
  x + y
}

// Single effect
fn log_add(x: Int, y: Int) -{Log}-> Int {
  log(Info, "Adding " ++ show(x) ++ " + " ++ show(y))
  x + y
}

// Multiple effects
fn stateful_log() -{State[Int], Log}-> Int {
  let current = get()
  log(Debug, "Current state: " ++ show(current))
  put(current + 1)
  current
}
```

### Performing Operations

Effect operations are called like regular functions:

```affine
fn counter() -{State[Int]}-> Int {
  let n = get()      // Perform get
  put(n + 1)         // Perform put
  n
}

fn might_fail() -{Error[String]}-> Int {
  if problem {
    raise("Something went wrong")  // Perform raise
  }
  42
}
```

### Effect Subsumption

A function with fewer effects can be used where more are expected:

```affine
fn pure_fn() -> Int { 42 }

fn effectful() -{IO}-> Int {
  pure_fn()  // OK: pure can be used in effectful context
}
```

---

## Effect Handlers

### Basic Handler Syntax

```affine
handle expression {
  operation(args) -> handler_body,
  operation2(args) -> handler_body,
  return(x) -> final_body  // Optional: handle pure return
}
```

### Handling State

```affine
fn run_state[S, A](initial: S, f: () -{State[S]}-> A) -> (A, S) {
  let mut state = initial

  let result = handle f() {
    get() -> resume(state),
    put(s) -> {
      state = s
      resume(())
    }
  }

  (result, state)
}

// Usage
let (value, final_state) = run_state(0, || {
  put(get() + 1)
  put(get() + 1)
  get()
})
// value = 2, final_state = 2
```

### Handling Errors

```affine
fn run_error[E, A](f: () -{Error[E]}-> A) -> Result[A, E] {
  handle f() {
    raise(e) -> Err(e),  // Don't resume, return error
    return(x) -> Ok(x)
  }
}

// Usage
let result = run_error(|| {
  if x < 0 {
    raise("Negative value")
  }
  x * 2
})
```

### The `resume` Keyword

`resume` continues execution after the effect:

```affine
handle computation() {
  // resume(value) - continue with 'value' as operation result
  log(msg) -> {
    println("[LOG] " ++ msg)
    resume(())  // Continue, operation returns ()
  },

  // Not calling resume aborts the computation
  fail(msg) -> {
    println("[FAIL] " ++ msg)
    // No resume - computation ends here
    None
  }
}
```

### Multi-shot Handlers

Resume can be called multiple times (for non-determinism):

```affine
effect Choice {
  fn choose[A](options: Vec[A]) -> A
}

fn all_choices[A](f: () -{Choice}-> A) -> Vec[A] {
  handle f() {
    choose(options) -> {
      let mut results = []
      for option in options {
        results.extend(resume(option))  // Resume for each!
      }
      results
    },
    return(x) -> [x]
  }
}

// Usage
let paths = all_choices(|| {
  let x = choose([1, 2])
  let y = choose(["a", "b"])
  (x, y)
})
// paths = [(1, "a"), (1, "b"), (2, "a"), (2, "b")]
```

---

## Effect Polymorphism

### Polymorphic Effects

Functions can be polymorphic over effects:

```affine
// Works with any effect row
fn map_option[E, A, B](
  opt: Option[A],
  f: (A) -{E}-> B
) -{E}-> Option[B] {
  match opt {
    Some(a) -> Some(f(a)),
    None -> None
  }
}

// Usage
let result = map_option(Some(5), |x| {
  log(Info, "Doubling")
  x * 2
})
```

### Effect Row Variables

```affine
// e is an effect row variable
fn sequence[e, A](actions: Vec[() -{e}-> A]) -{e}-> Vec[A] {
  let mut results = []
  for action in actions {
    results.push(action())
  }
  results
}
```

### Effect Constraints

```affine
// Require specific effects in the row
fn with_logging[e: Log, A](f: () -{e}-> A) -{e}-> A {
  log(Info, "Starting")
  let result = f()
  log(Info, "Finished")
  result
}
```

---

## Standard Effects

### IO Effect

The primitive effect for I/O:

```affine
effect IO {
  // These are primitive operations
  fn io_print(s: String) -> Unit
  fn io_read() -> String
  fn io_exit(code: Int) -> Never
}
```

### Exception Effect

For recoverable errors:

```affine
effect Exn[E] {
  fn throw(e: E) -> Never
}

fn catch[E, A](f: () -{Exn[E]}-> A, handler: (E) -> A) -> A {
  handle f() {
    throw(e) -> handler(e)
  }
}
```

### Async Effect

For asynchronous programming:

```affine
effect Async {
  fn await[T](future: Future[T]) -> T
  fn spawn[T](f: () -{Async}-> T) -> Future[T]
}

fn fetch_all(urls: Vec[String]) -{Async, IO}-> Vec[Response] {
  let futures = urls.map(|url| spawn(|| fetch(url)))
  futures.map(|f| await(f))
}
```

### Reader Effect

For dependency injection:

```affine
effect Reader[R] {
  fn ask() -> R
}

fn with_reader[R, A](r: R, f: () -{Reader[R]}-> A) -> A {
  handle f() {
    ask() -> resume(r)
  }
}

// Usage
fn get_config_value() -{Reader[Config]}-> String {
  ask().database_url
}
```

### Writer Effect

For logging/accumulation:

```affine
effect Writer[W: Monoid] {
  fn tell(w: W) -> Unit
}

fn run_writer[W: Monoid, A](f: () -{Writer[W]}-> A) -> (A, W) {
  let mut log = W::empty()

  let result = handle f() {
    tell(w) -> {
      log = log.append(w)
      resume(())
    }
  }

  (result, log)
}
```

---

## Advanced Patterns

### Effect Interpretation

Same effect, different interpretations:

```affine
effect Database {
  fn query(sql: String) -> Vec[Row]
  fn execute(sql: String) -> Int
}

// Production handler - real database
fn run_production[A](f: () -{Database}-> A) -{IO}-> A {
  let conn = connect_db()
  handle f() {
    query(sql) -> resume(conn.query(sql)),
    execute(sql) -> resume(conn.execute(sql))
  }
}

// Test handler - in-memory
fn run_test[A](f: () -{Database}-> A) -> A {
  let mut data = HashMap::new()
  handle f() {
    query(sql) -> resume(mock_query(data, sql)),
    execute(sql) -> resume(mock_execute(&mut data, sql))
  }
}
```

### Effect Composition

```affine
fn program() -{State[Int], Log, Error[String]}-> Int {
  log(Info, "Starting")
  let n = get()
  if n < 0 {
    raise("Negative state")
  }
  put(n + 1)
  get()
}

fn run() -> Result[Int, String] {
  run_error(|| {
    run_state(0, || {
      run_log(|| {
        program()
      })
    }).0
  })
}
```

### Delimited Continuations

Effects give you access to delimited continuations:

```affine
effect Yield[A, R] {
  fn yield(a: A) -> R
}

fn generator[A](f: () -{Yield[A, Unit]}-> Unit) -> Iterator[A] {
  // Captures continuation at each yield point
  // Can resume later to get next value
}
```

### Scoped Effects

Effects with resource management:

```affine
effect Resource {
  fn bracket[A](
    acquire: () -> Handle,
    release: (Handle) -> Unit,
    use: (Handle) -> A
  ) -> A
}

fn with_file[A](path: String, f: (&File) -> A) -{Resource, IO}-> A {
  bracket(
    || File::open(path),
    |h| h.close(),
    f
  )
}
```

---

## Implementation Notes

### Effect Compilation

Effects are compiled using one of:

1. **CPS Transformation**: Convert to continuation-passing style
2. **Evidence Passing**: Pass handler implementations as implicit parameters
3. **Monadic Translation**: Translate to free monad style

### Performance Considerations

```affine
// Effects have overhead - use strategically
fn tight_loop() -> Int {
  let mut sum = 0
  for i in 0..1000000 {
    sum += i
  }
  sum  // Pure is fastest
}

// Batch effect operations when possible
fn log_batch() -{Log}-> Unit {
  let messages = compute_messages()
  log(Info, messages.join("\n"))  // One effect, not many
}
```

### Effect Safety

The type system ensures:
- All effects are declared
- All effects are handled before program exit
- Effect rows compose correctly

```affine
fn main() -{IO}-> Unit {
  // Must handle all non-IO effects before here
  let result = handle program() {
    // handlers...
  }
  println(result)
}
```

---

## See Also

- [Types](types.md) - Type system overview
- [Design: Effect System](../design/effects.md) - Theory and design
- [Standard Effects](../stdlib/effects.md) - Standard library effects
