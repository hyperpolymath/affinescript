# AffineScript for Visual Studio Code

Official Visual Studio Code extension for AffineScript - a language with affine types, algebraic effects, and dependent types that compiles to WebAssembly.

## Features

### Syntax Highlighting
- Complete TextMate grammar for AffineScript syntax
- Highlighting for effects, quantities (linear/affine/unrestricted), types, and keywords
- Custom colors for effect annotations and ownership markers

### Language Server Protocol (LSP)
- **Real-time diagnostics** - Type errors, effect violations, borrow check errors
- **Hover information** - See types and documentation
- **Go to definition** - Navigate to function/type definitions
- **Find references** - Find all uses of a symbol
- **Code completion** - Context-aware suggestions
- **Rename** - Rename symbols across files
- **Formatting** - Auto-format code
- **Code actions** - Quick fixes for common errors

### Commands
- **AffineScript: Type Check Current File** (`Ctrl+Shift+C` / `Cmd+Shift+C`)
- **AffineScript: Evaluate Current File** (`Ctrl+Shift+R` / `Cmd+Shift+R`)
- **AffineScript: Compile to WebAssembly**
- **AffineScript: Format Document**
- **AffineScript: Restart Language Server**

## Requirements

- **AffineScript compiler** - Install from [github.com/hyperpolymath/affinescript](https://github.com/hyperpolymath/affinescript)
- **affinescript-lsp** - Language server (optional, for LSP features)

```bash
# Install AffineScript
git clone https://github.com/hyperpolymath/affinescript
cd affinescript
dune build
dune install

# Install LSP server
cd tools/affinescript-lsp
cargo build --release
cargo install --path .
```

## Extension Settings

This extension contributes the following settings:

* `affinescript.lsp.enabled`: Enable/disable the Language Server
* `affinescript.lsp.serverPath`: Path to affinescript-lsp executable
* `affinescript.format.indentSize`: Number of spaces per indentation level
* `affinescript.format.maxLineLength`: Maximum line length before wrapping
* `affinescript.lint.enabled`: Enable/disable linting
* `affinescript.lint.unusedVariables`: Diagnostic level for unused variables
* `affinescript.lint.missingEffectAnnotations`: Diagnostic level for missing effect annotations

## Usage

### Basic Example

```affinescript
// Pure function - no effects
fn add(x: Int, y: Int) -> Int {
  return x + y;
}

// Impure function with IO effect
fn main() -> Unit / IO {
  println("Hello, AffineScript!");
  let result = add(40, 2);
  println(int_to_string(result));
}
```

### Effect System

```affinescript
// Pure functions cannot call impure functions
fn pure() -> Int {
  return read_line();  // ❌ ERROR: Cannot perform effect IO in pure context
}

// Impure functions can call pure or impure
fn impure() -> Int / IO {
  return read_line();  // ✅ OK
}
```

### Affine Types (Ownership)

```affinescript
fn use_value(x: @affine String) -> Unit {
  println(x);  // x is consumed here
}

fn main() -> Unit / IO {
  let s = "hello";
  use_value(s);
  println(s);  // ❌ ERROR: use after move
}
```

## Known Issues

- LSP server implementation is in progress (Phase 8)
- Some LSP features not yet implemented (hover, completion)
- Tree-sitter grammar for advanced highlighting coming soon

## Contributing

Contributions welcome! See [CONTRIBUTING.md](https://github.com/hyperpolymath/affinescript/blob/main/CONTRIBUTING.md)

### Smoke testing the compiled extension

The extension source of truth is [`src/extension.affine`](src/extension.affine);
it compiles to [`out/extension.cjs`](out/extension.cjs) which is what VS Code
loads. A headless smoke harness verifies the compiled `.cjs` against the
acceptance criteria in
[issue #139](https://github.com/hyperpolymath/affinescript/issues/139):
activation, command registration + invocation, `restartLsp` cycling, and
`deactivate` teardown.

To run it locally:

```bash
cd editors/vscode
npm install              # one-time: fetches @vscode/test-electron, mocha, glob
xvfb-run npm test        # on Linux servers (or `npm test` on a desktop)
```

`@vscode/test-electron` downloads a pinned VS Code binary on first run, launches
it with `--extensionDevelopmentPath` pointing at this directory, and runs the
Mocha suite at [`test/suite/`](test/suite/) inside the extension host. The
[`vscode-smoke`](../../.github/workflows/ci.yml) CI job runs the same harness
under xvfb on every PR.

Notes:

- The harness covers the documented `showWarningMessage` short-circuit when
  `affinescript-lsp` is not on `PATH`. Set `AFFINESCRIPT_LSP_PATH` to a real
  binary if you want to exercise the LSP-attach branch end-to-end.
- The Node-only runner is a documented carve-out from the repo's
  "no Node.js / no Bun" policy (see `.claude/CLAUDE.md` → Runtime Exemptions).
  Scope is strictly `editors/vscode/test/`; no production code adopts Node.

## License

MPL-2.0

## Release Notes

### 0.1.0

- Initial release
- Syntax highlighting via TextMate grammar
- Language configuration (brackets, comments, folding)
- Basic LSP integration scaffolding
- Commands for check, eval, compile, format
- Keyboard shortcuts for common operations
