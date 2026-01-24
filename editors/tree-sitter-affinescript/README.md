# tree-sitter-affinescript

Tree-sitter grammar for AffineScript - affine types, effects, and dependent types.

## Features

- **Incremental parsing** - Fast, efficient parsing for large files
- **Syntax highlighting** - Semantic token-based highlighting
- **Code navigation** - Structural queries for IDE features
- **Error recovery** - Continues parsing even with syntax errors

## Installation

### Neovim (with nvim-treesitter)

```lua
local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
parser_config.affinescript = {
  install_info = {
    url = "~/path/to/tree-sitter-affinescript",
    files = {"src/parser.c"},
    branch = "main",
  },
  filetype = "as",
}
```

### Emacs (with tree-sitter)

```elisp
(add-to-list 'tree-sitter-major-mode-language-alist '(affinescript-mode . affinescript))
```

### VSCode

Integrated automatically when using the AffineScript VSCode extension.

## Development

```bash
# Generate parser
npm install
npm run build

# Test grammar
npm test

# Or using tree-sitter CLI
tree-sitter generate
tree-sitter test
```

## Grammar Highlights

The grammar supports all AffineScript features:

- **Affine types** and ownership annotations
- **Effect system** - effect declarations, annotations, handlers
- **Dependent types** - forall, exists quantifiers
- **Pattern matching** - exhaustive, with guards
- **Traits and impls** - polymorphic dispatch
- **Module system** - namespaces and imports

## Queries

### Highlights (`queries/highlights.scm`)

Syntax highlighting for:
- Keywords (fn, let, type, effect, etc.)
- Effects and effect operators
- Types and type parameters
- Functions and function calls
- Literals and comments

### Locals (planned)

Scope analysis for:
- Variable definitions and references
- Function scopes
- Block scopes

### Injections (planned)

Language injections for:
- Inline documentation
- String interpolation

## License

MIT
