# AffineScript Wiki

Welcome to the AffineScript language wiki - your comprehensive guide to learning and using AffineScript.

## Quick Navigation

### Getting Started
- [Introduction](tutorials/introduction.md) - What is AffineScript?
- [Installation](tutorials/installation.md) - Setting up your environment
- [Hello World](tutorials/hello-world.md) - Your first program
- [Quick Start Guide](tutorials/quickstart.md) - Get productive fast

### Language Reference
- [Lexical Structure](language-reference/lexical.md) - Tokens, literals, comments
- [Types](language-reference/types.md) - Type system overview
- [Expressions](language-reference/expressions.md) - Expression syntax and semantics
- [Patterns](language-reference/patterns.md) - Pattern matching
- [Functions](language-reference/functions.md) - Function definitions
- [Ownership](language-reference/ownership.md) - Affine types and borrowing
- [Effects](language-reference/effects.md) - Algebraic effect system
- [Traits](language-reference/traits.md) - Type classes
- [Modules](language-reference/modules.md) - Module system
- [Dependent Types](language-reference/dependent-types.md) - Indexed and refined types
- [Row Polymorphism](language-reference/rows.md) - Extensible records

### Compiler
- [Architecture Overview](compiler/architecture.md) - Compiler pipeline
- [Lexer](compiler/lexer.md) - Lexical analysis
- [Parser](compiler/parser.md) - Syntactic analysis
- [Type Checker](compiler/type-checker.md) - Type inference and checking
- [Borrow Checker](compiler/borrow-checker.md) - Ownership verification
- [Code Generation](compiler/codegen.md) - WASM backend
- [Error Messages](compiler/errors.md) - Diagnostic system

### Tooling
- [CLI Reference](tooling/cli.md) - Command-line interface
- [REPL Guide](tooling/repl.md) - Interactive environment
- [Package Manager](tooling/package-manager.md) - aspm reference
- [LSP Server](tooling/lsp.md) - Editor integration
- [Formatter](tooling/formatter.md) - Code formatting
- [Linter](tooling/linter.md) - Code analysis

### Standard Library
- [Overview](stdlib/overview.md) - Library organization
- [Primitives](stdlib/primitives.md) - Basic types
- [Collections](stdlib/collections.md) - Data structures
- [Effects](stdlib/effects.md) - Standard effects
- [I/O](stdlib/io.md) - Input/output
- [Concurrency](stdlib/concurrency.md) - Threading and async

### Testing
- [Testing Guide](testing/guide.md) - Writing tests
- [Property-Based Testing](testing/property-based.md) - QuickCheck-style testing
- [Fuzzing](testing/fuzzing.md) - Fuzz testing
- [Benchmarking](testing/benchmarks.md) - Performance testing

### Design Documents
- [Language Design](design/language.md) - Design philosophy
- [Type System](design/type-system.md) - Type theory background
- [Effect System](design/effects.md) - Effect theory
- [Memory Model](design/memory.md) - Memory management

## Quick Links

| Resource | Description |
|----------|-------------|
| [Full Specification](../docs/spec.md) | Complete language specification |
| [Roadmap](../ROADMAP.md) | Development roadmap |
| [Examples](../examples/) | Example programs |
| [Contributing](../CONTRIBUTING.md) | How to contribute |

## Language Features at a Glance

### Ownership & Borrowing
```affine
fn transfer(file: own File) -> own File {
  // file is owned, must be returned or consumed
  file
}

fn read_only(file: &File) -> String {
  // file is borrowed immutably
  file.read_all()
}

fn modify(file: &mut File) {
  // file is borrowed mutably
  file.write("data")
}
```

### Dependent Types
```affine
// Length-indexed vectors
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T {
  vec[0]  // Statically safe - vector has at least 1 element
}

fn append[n: Nat, m: Nat, T](
  a: Vec[n, T],
  b: Vec[m, T]
) -> Vec[n + m, T] {
  // Return type precisely tracks length
  ...
}
```

### Row Polymorphism
```affine
// Works on any record with 'name' field
fn greet[r](person: {name: String, ..r}) -> String {
  "Hello, " ++ person.name
}

// Can call with any matching record
greet({name: "Alice", age: 30})
greet({name: "Bob", role: "Admin", active: true})
```

### Algebraic Effects
```affine
effect Ask[A] {
  fn ask() -> A
}

fn double_ask[A]() -{Ask[A]}-> (A, A) {
  (ask(), ask())
}

fn main() -{IO}-> Unit {
  let result = handle double_ask() {
    ask() -> resume(42)
  };
  print(result)  // (42, 42)
}
```

### Totality Checking
```affine
// Guaranteed to terminate
total fn factorial(n: Nat) -> Nat {
  match n {
    0 -> 1,
    n -> n * factorial(n - 1)
  }
}

// May diverge (partial by default)
fn loop_forever() -> Never {
  loop_forever()
}
```

## Community

- **GitHub**: [github.com/hyperpolymath/affinescript](https://github.com/hyperpolymath/affinescript)
- **Issues**: Report bugs and request features
- **Discussions**: Ask questions and share ideas

---

*AffineScript - Safe, Expressive, Predictable*
