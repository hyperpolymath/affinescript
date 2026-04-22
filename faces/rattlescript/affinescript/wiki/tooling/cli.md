# Command-Line Interface

The AffineScript CLI provides commands for compiling, testing, and managing projects.

## Installation

```bash
# Install from source
git clone https://github.com/hyperpolymath/affinescript
cd affinescript
dune build && dune install

# Or with opam (planned)
opam install affinescript
```

## Commands

### affinescript

The main compiler executable:

```bash
# Lex a file (output tokens)
affinescript lex <file.affine>

# Parse a file (output AST)
affinescript parse <file.affine>

# Type-check a file
affinescript check <file.affine>

# Compile to WASM
affinescript compile <file.affine> -o output.wasm

# Run directly
affinescript run <file.affine>
```

### Common Options

```bash
--help, -h          Show help message
--version, -v       Show version
--verbose           Verbose output
--quiet, -q         Suppress output
--color <auto|always|never>  Color output
```

### lex

Lexical analysis - tokenize source file:

```bash
affinescript lex src/main.affine

# Output format
affinescript lex --format json src/main.affine
affinescript lex --format pretty src/main.affine
```

Example output:
```
FN @ 1:1-1:3
IDENT("main") @ 1:4-1:8
LPAREN @ 1:8-1:9
RPAREN @ 1:9-1:10
ARROW @ 1:11-1:13
IDENT("Unit") @ 1:14-1:18
LBRACE @ 1:19-1:20
...
```

### parse

Parse source file to AST:

```bash
affinescript parse src/main.affine

# Output options
affinescript parse --format sexp src/main.affine
affinescript parse --format json src/main.affine
affinescript parse --format pretty src/main.affine
```

### check

Type-check without compiling:

```bash
affinescript check src/main.affine

# Check entire project
affinescript check .

# Show inferred types
affinescript check --show-types src/main.affine
```

### compile

Compile to WebAssembly:

```bash
# Basic compilation
affinescript compile src/main.affine -o output.wasm

# With optimizations
affinescript compile --release src/main.affine -o output.wasm

# Emit text format
affinescript compile --emit wat src/main.affine -o output.wat

# Debug info
affinescript compile --debug src/main.affine -o output.wasm

# Target options
affinescript compile --target wasm32 src/main.affine
```

### run

Compile and run:

```bash
# Run with default WASM runtime
affinescript run src/main.affine

# Pass arguments
affinescript run src/main.affine -- arg1 arg2

# With specific runtime
affinescript run --runtime wasmtime src/main.affine
```

## aspm (Package Manager)

AffineScript Package Manager (planned):

### Project Management

```bash
# Create new project
aspm init my-project
aspm init --lib my-library

# Build project
aspm build
aspm build --release

# Run main
aspm run

# Clean build artifacts
aspm clean
```

### Dependencies

```bash
# Add dependency
aspm add json
aspm add http@2.0

# Remove dependency
aspm remove json

# Update dependencies
aspm update
aspm update json
```

### Testing

```bash
# Run all tests
aspm test

# Run specific tests
aspm test --filter "test_parser"

# With coverage
aspm test --coverage

# Verbose
aspm test --verbose
```

### Documentation

```bash
# Generate docs
aspm doc

# Open in browser
aspm doc --open

# Include private items
aspm doc --document-private-items
```

### Publishing

```bash
# Login to registry
aspm login

# Publish package
aspm publish

# Check before publishing
aspm publish --dry-run
```

## aslsp (Language Server)

Language Server Protocol implementation (planned):

```bash
# Start LSP server
aslsp

# With logging
aslsp --log-level debug

# Specify port (for TCP mode)
aslsp --port 5007
```

## asfmt (Formatter)

Code formatter (planned):

```bash
# Format file
asfmt src/main.affine

# Format in place
asfmt --write src/main.affine

# Format all files
asfmt --write .

# Check formatting
asfmt --check src/main.affine

# Diff mode
asfmt --diff src/main.affine
```

Configuration (`.asfmt.toml`):
```toml
max_width = 100
indent_size = 2
use_tabs = false
trailing_comma = true
```

## aslint (Linter)

Static analysis (planned):

```bash
# Run all lints
aslint src/

# Specific rules
aslint --warn unused_variables src/
aslint --deny unsafe_code src/

# Fix automatically
aslint --fix src/
```

## Environment Variables

| Variable | Description |
|----------|-------------|
| `AFFINE_HOME` | Installation directory |
| `AFFINE_CACHE` | Cache directory |
| `AFFINE_LOG` | Log level (error, warn, info, debug, trace) |
| `NO_COLOR` | Disable colored output |

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Command-line usage error |
| 101 | Compilation failed |
| 102 | Type error |
| 103 | Borrow check error |

---

## See Also

- [Installation](../tutorials/installation.md) - Setup guide
- [Package Manager](package-manager.md) - aspm details
- [LSP](lsp.md) - Editor integration
