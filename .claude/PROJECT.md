# AffineScript - Claude Code Instructions

This is the AffineScript compiler, written in OCaml.

## Project Structure

```
affinescript/
├── lib/           # Core compiler library
│   ├── ast.ml     # Abstract syntax tree
│   ├── token.ml   # Token definitions
│   ├── lexer.ml   # Lexer (sedlex-based)
│   ├── parser.ml  # Parser (menhir-based) [TODO]
│   ├── span.ml    # Source location tracking
│   └── error.ml   # Diagnostics and error handling
├── bin/           # CLI executable
│   └── main.ml    # Command-line interface
├── test/          # Test suite
└── docs/          # Documentation
```

## Build Commands

```bash
# Build
dune build

# Run tests
dune runtest

# Format code
dune fmt

# Generate docs
dune build @doc

# Run compiler
dune exec affinescript -- <command> <args>
```

## Coding Conventions

- Use descriptive variable names
- All files should have type annotations where helpful
- Error messages should follow the format in `error.ml`
- Use `ppx_deriving` for show, eq, ord on types
- Use `sexp` for serialization of AST types

## Language Specification

The full language specification is at `/var/home/hyper/affinescript-spec.md`.

Key language features:
- **Partial by default**: Functions are partial unless marked `total`
- **Quantity annotations**: `0` (erased), `1` (linear), `ω` (unrestricted)
- **Row polymorphism**: `{x: Int, ..r}` for extensible records
- **Extensible effects**: User-defined effects with `effect` keyword
- **Ownership**: `own`, `ref`, `mut` modifiers

## Implementation Priority

1. Lexer (sedlex) - in progress
2. Parser (menhir)
3. Name resolution
4. Type checker (bidirectional)
5. Borrow checker
6. Effect checking
7. WASM codegen

## Testing

Tests go in `test/` directory. Use Alcotest:

```ocaml
let test_something () =
  Alcotest.(check string) "description" expected actual
```
