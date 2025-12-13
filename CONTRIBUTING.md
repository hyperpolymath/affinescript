# Contributing to AffineScript

Thank you for your interest in contributing to AffineScript!

## Getting Started

1. Fork the repository
2. Clone your fork
3. Set up the development environment:

```bash
opam switch create . 5.1.0
opam install . --deps-only --with-test --with-doc
```

4. Build and test:

```bash
dune build
dune runtest
```

## Development Workflow

1. Create a feature branch: `git checkout -b feature/your-feature`
2. Make your changes
3. Format code: `dune fmt`
4. Run tests: `dune runtest`
5. Commit with clear messages
6. Push and open a PR

## Code Style

- Use `ocamlformat` (config in `.ocamlformat`)
- Write descriptive variable names
- Add type annotations for public functions
- Include tests for new functionality

## Commit Messages

Follow conventional commits:

```
feat: add row polymorphism to type checker
fix: handle empty vectors in pattern matching
docs: update README with installation instructions
test: add lexer tests for string literals
```

## Pull Requests

- Keep PRs focused on a single change
- Update documentation if needed
- Add tests for new features
- Ensure CI passes

## Areas to Contribute

- **Compiler**: Parser, type checker, borrow checker, codegen
- **Tests**: More test cases, property-based testing
- **Documentation**: Tutorials, examples, API docs
- **Tooling**: Editor support, REPL, formatter

## Questions?

Open an issue with the "question" label.
