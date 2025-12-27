# AffineScript v0.1.0 - Reference Parser Release

This is the first public release of AffineScript, featuring a complete specification and reference parser.

## What's Included

### Specification
- `SPEC.md` - Condensed language specification (essential grammar and semantics)
- `affinescript-spec.md` - Complete language specification v2.0

### Reference Implementation
- **Lexer** (sedlex) - Complete tokenization
- **Parser** (menhir) - Complete parsing to AST
- **AST** - Full abstract syntax tree definitions
- **Error Handling** - Structured diagnostics with source locations

### Examples
- `examples/hello.as` - Hello World with effects
- `examples/vectors.as` - Dependent types with length-indexed vectors
- `examples/ownership.as` - Ownership and borrowing patterns
- `examples/rows.as` - Row polymorphism
- `examples/effects.as` - Effect handling and state
- `examples/traits.as` - Traits and type classes
- `examples/refinements.as` - Refinement types

## Building from Source

### Prerequisites

- OCaml 5.1+
- opam 2.1+
- dune 3.14+

### Quick Start

```bash
# Clone the repository
git clone https://github.com/hyperpolymath/affinescript.git
cd affinescript

# Install dependencies
opam install . --deps-only

# Build
dune build

# Run tests
dune runtest

# Install locally
dune install
```

### Using Guix (Preferred)

```bash
guix shell -f guix.scm
dune build
```

### Using Nix

```bash
nix develop
dune build
```

## Usage

### Lex a File

```bash
dune exec affinescript -- lex examples/hello.as
```

Output:
```
EFFECT @ 1:1-1:7
UPPER_IDENT(IO) @ 1:8-1:10
LBRACE @ 1:11-1:12
FN @ 2:3-2:5
...
```

### Parse a File

```bash
dune exec affinescript -- parse examples/hello.as
```

Output:
```
{ prog_module = None; prog_imports = [];
  prog_decls =
    [TopEffect { ed_vis = Private; ed_name = { name = "IO"; ... }; ...};
     TopFn { fd_vis = Private; fd_total = false; fd_name = { name = "main"; ...}; ...}]
}
```

### Check a File (WIP)

```bash
dune exec affinescript -- check examples/hello.as
```

Note: Type checking is not yet implemented in this release.

## Implementation Status

| Component | Status | Notes |
|-----------|--------|-------|
| Lexer | Complete | All tokens, comments, string escapes |
| Parser | Complete | Full grammar, 50+ test cases |
| AST | Complete | All language constructs |
| Diagnostics | Complete | Structured errors with locations |
| Name Resolution | Not Started | Planned for v0.2 |
| Type Checker | Not Started | Planned for v0.2 |
| Borrow Checker | Not Started | Planned for v0.3 |
| Effect Checker | Not Started | Planned for v0.3 |
| WASM Codegen | Not Started | Planned for v0.4 |

## Language Features

### Affine Types (Ownership)

```affinescript
type File = own { fd: Int }

fn processFile(file: own File) -> () / IO {
  // file is consumed here - cannot use after
  close(file)
}

fn readFile(file: ref File) -> String / IO {
  // Borrows file - doesn't consume it
  read(file)
}
```

### Dependent Types

```affinescript
type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(T, Vec[n, T]) : Vec[n + 1, T]

// Type system prevents calling on empty vectors
total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v { Cons(h, _) => h }
}
```

### Row Polymorphism

```affinescript
// Works on any record with 'name' field
fn greet[..r](person: {name: String, ..r}) -> String / Pure {
  "Hello, " ++ person.name
}

// Both work:
greet({name: "Alice", age: 30})
greet({name: "Bob", role: "Engineer"})
```

### Extensible Effects

```affinescript
effect State[S] {
  fn get() -> S;
  fn put(s: S);
}

fn counter() -> Int / State[Int] {
  let n = State.get();
  State.put(n + 1);
  n
}

// Handle the effect
handle counter() with {
  return x => x,
  get() => resume(0),
  put(s) => resume(())
}
```

## Running Tests

```bash
# All tests
dune runtest

# With verbose output
dune runtest --force --verbose

# Specific test suite
dune exec test/test_main.exe -- test "Lexer"
dune exec test/test_main.exe -- test "Parser"
```

## Documentation

```bash
# Generate documentation
dune build @doc

# View in browser
open _build/default/_doc/_html/index.html
```

## File Structure

```
affinescript/
├── lib/                    # Core compiler library
│   ├── ast.ml              # Abstract syntax tree
│   ├── token.ml            # Token definitions
│   ├── lexer.ml            # Sedlex-based lexer
│   ├── parser.mly          # Menhir grammar
│   ├── parse.ml            # Parser wrapper
│   ├── span.ml             # Source locations
│   └── error.ml            # Diagnostics
├── bin/                    # CLI executable
│   └── main.ml             # Command-line interface
├── test/                   # Test suite
│   ├── test_lexer.ml       # Lexer tests
│   └── test_parser.ml      # Parser tests
├── examples/               # Example programs
├── wiki/                   # Documentation
├── SPEC.md                 # Condensed specification
├── affinescript-spec.md    # Full specification
└── RELEASE.md              # This file
```

## Contributing

Contributions welcome! Areas of interest:

1. **Type Checker** - Bidirectional type checking with dependent types
2. **Borrow Checker** - Ownership verification
3. **Effect Checker** - Effect tracking and handling
4. **Standard Library** - Core types and functions
5. **WASM Backend** - Code generation

See `affinescript-spec.md` Part 10 for implementation guidance.

## License

MIT License - see LICENSE file.

## Links

- Repository: https://github.com/hyperpolymath/affinescript
- Specification: See `SPEC.md` or `affinescript-spec.md`
- Issues: https://github.com/hyperpolymath/affinescript/issues
