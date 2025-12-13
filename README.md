# AffineScript

[![CI](https://github.com/hyperpolymath/affinescript/actions/workflows/ci.yml/badge.svg)](https://github.com/hyperpolymath/affinescript/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**AffineScript** is a programming language that combines:

- **Affine Types** — Rust-style ownership without a garbage collector
- **Dependent Types** — Types that depend on values for compile-time guarantees
- **Row Polymorphism** — Extensible records with type-safe field access
- **Extensible Effects** — User-defined, trackable side effects

It compiles to **WebAssembly** for efficient, portable execution.

## Features

### Ownership Without GC

```affinescript
fn processFile(path: ref String) -> Result[String, IOError] / IO {
  let file = open(path)?;          // file is owned
  let content = read(ref file)?;   // borrow file
  close(file)?;                    // consume file
  Ok(content)
}
```

### Compile-Time Size Verification

```affinescript
total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v {
    Cons(h, _) => h  // Can't call on empty vec - type prevents it
  }
}
```

### Extensible Records

```affinescript
fn greet[..r](person: {name: String, ..r}) -> String / Pure {
  "Hello, " ++ person.name  // Works on any record with 'name'
}

greet({name: "Alice", age: 30})  // OK
greet({name: "Bob", role: "Admin"})  // Also OK
```

### Trackable Effects

```affinescript
effect State[S] {
  fn get() -> S;
  fn put(s: S);
}

fn counter() -> Int / State[Int] {
  let n = get();
  put(n + 1);
  n
}
```

## Installation

### From Source

Requires OCaml 5.1+ and opam:

```bash
git clone https://github.com/hyperpolymath/affinescript.git
cd affinescript
opam install . --deps-only
dune build
```

### Usage

```bash
# Lex a file
affinescript lex example.as

# Parse a file
affinescript parse example.as

# Type check
affinescript check example.as

# Compile to WebAssembly
affinescript compile example.as -o output.wasm
```

## Documentation

- [Language Specification](docs/spec.md)
- [Tutorial](docs/tutorial.md)
- [Standard Library](docs/stdlib.md)

## Project Status

AffineScript is under active development. Current status:

| Component | Status |
|-----------|--------|
| Lexer | In Progress |
| Parser | Planned |
| Type Checker | Planned |
| Borrow Checker | Planned |
| Effect System | Planned |
| WASM Backend | Planned |

## Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) before submitting PRs.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Related Projects

- [Rust](https://www.rust-lang.org/) — Inspiration for affine types
- [Idris](https://www.idris-lang.org/) — Inspiration for dependent types
- [Koka](https://koka-lang.github.io/) — Inspiration for effect system
- [PureScript](https://www.purescript.org/) — Inspiration for row polymorphism
