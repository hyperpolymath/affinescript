# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

# AffineScript Faces — Design & Implementation Reference

This document describes the *face* architecture (ADR-010) for the AffineScript
compiler and toolchain.

---

## Motivation

AffineScript has a canonical surface syntax.  A *face* is an alternative
surface presentation — a different syntax *and* error vocabulary — layered on
top of the compiler without changing any compiler internals.

The canonical use-case is the **Python face**: developers who write Python can
write `def f(x: Int) -> Int:` with indentation-delimited blocks, `True`/`False`,
`None`, `and`/`or`, etc. and receive compiler errors in Python-idiomatic terms
("single-use variable" rather than "linear binding", "Name not found" rather
than "UnboundVariable").

---

## Architecture (ADR-010 §2)

```
source text (face-specific)
      ↓
  face preprocessor       ← lib/python_face.ml  (text → canonical text)
      ↓
  standard lex + parse    ← lib/lexer.ml / lib/parser.mly
      ↓
  type-checker / QTT      ← lib/typecheck.ml / lib/quantity.ml
      ↓
  canonical error term    ← face-agnostic; carries structured fields
      ↓
  face-aware formatter    ← lib/face.ml  (error × face → display string)
      ↓
  terminal / LSP / IDE
```

The compiler is face-agnostic throughout.  The two face-aware layers are:

1. `lib/python_face.ml` — source-level text preprocessor
2. `lib/face.ml` — error formatter

---

## Active Faces

| Face | CLI flag | File extension | Status |
|------|----------|----------------|--------|
| Canonical | `--face canonical` (default) | `.affine` | Stable |
| Python | `--face python` | `.pyaff` | Stable (ADR-010) |
| JS | `--face js` or `--face javascript` | `.jsaff` | Beta (2026-04-11) |
| Pseudocode | `--face pseudocode` or `--face pseudo` | `.pseudoaff` | Beta (2026-04-11) |

---

## Roadmap Faces

These faces are on the roadmap but have not been designed in detail.  The
face architecture makes implementation cheap once the surface mapping is
settled; the bottleneck is design, not code.

### CoffeeScript-face (strategic priority)

**Rationale:** CoffeeScript has a loyal displaced community that never loved
JavaScript and were forced away when the ecosystem moved on.  Their syntax
preferences (significant whitespace, `->` / `=>`, `is`/`isnt`/`unless`,
`@` for self, `do`, list comprehensions) are coherent and well-documented.
A CoffeeScript face would offer that community a new home: their familiar
syntax, but with a sound type system, affine resource semantics, and typed
WASM output.

**Rough surface mapping (to be designed):**

| CoffeeScript surface | Canonical AffineScript |
|----------------------|------------------------|
| `x = expr` | `let x = expr` |
| `f = (x, y) -> expr` | `fn f(x, y) { expr }` |
| `f = (x, y) => expr` | `fn f(x, y) { expr }` (bound form) |
| `unless cond` | `if !cond` |
| `is` / `isnt` | `==` / `!=` |
| `and` / `or` / `not` | `&&` / `\|\|` / `!` |
| `@field` | `self.field` |
| `class Name` | `type Name` |
| `null` / `undefined` | `()` |
| `yes` / `no` | `true` / `false` |
| Significant whitespace | blocks (like Python-face) |
| `#` comment | `//` comment |

**Design questions before implementing:**
- How do CoffeeScript string interpolation (`"#{x}"`) map to AffineScript?
- List comprehensions — do we support them or emit a hint?
- Splats (`args...`) — map to variadic or array?
- Prototype chain (`::`) — not applicable; needs a hint pointing to traits.

**File:** `lib/coffee_face.ml`, `--face coffee` / `--face coffeescript`

---

### ActionScript-face (strategic priority)

**Rationale:** ActionScript 3 was a capable, statically-typed OO language
with a loyal community (Flash game developers, Flex/AIR developers).  It was
killed by platform death (Flash end-of-life), not by language quality.
AS3 developers are comfortable with explicit types, classes, and access
modifiers — all of which map naturally to AffineScript concepts.

**Rough surface mapping (to be designed):**

| ActionScript surface | Canonical AffineScript |
|----------------------|------------------------|
| `var x: Type = expr` | `let x: Type = expr` |
| `function f(x: T): R { }` | `fn f(x: T) -> R { }` |
| `public/private/protected` | (access control — AffineScript is module-scoped; emit hint) |
| `class Name extends Base` | `type Name` (traits for inheritance) |
| `new Name(args)` | `Name { fields }` or constructor fn |
| `null` | `()` |
| `Boolean` / `int` / `Number` | `Bool` / `Int` / `Float` |
| `String` | `String` (direct) |
| `Array.<T>` | `Array[T]` |
| `trace(expr)` | `IO.println(expr)` |
| `import pkg.Class` | `use pkg::Class` |
| `package { ... }` | module-level (stripped) |
| `//` / `/* */` comments | (already valid) |

**Design questions before implementing:**
- AS3 inheritance (`extends`) — map to trait bounds or emit a migration hint?
- Interfaces (`implements`) — map to traits?
- `override` / `super` — no direct equivalent; design needed.
- Event model (`addEventListener`) — emit a hint pointing to effects.
- `Vector.<T>` vs `Array.<T>` — both map to `Array[T]`?

**File:** `lib/action_face.ml`, `--face action` / `--face actionscript`

---

## Python Face Details

### Source Transform (`lib/python_face.ml`)

`Python_face.transform_source : string -> string` converts Python-style
AffineScript source text to canonical AffineScript before lex and parse.

Surface mappings:

| Python surface | Canonical AffineScript |
|----------------|------------------------|
| `def name(...)` | `fn name(...)` |
| `True` / `False` | `true` / `false` |
| `None` | `()` |
| `and` / `or` | `&&` / `\|\|` |
| `not EXPR` | `! EXPR` |
| `class Name` | `type Name` |
| `pass` | `()` |
| `# comment` | `// comment` |
| `import a.b` | `use a::b;` |
| `from a import b` | `use a::b;` |
| `if cond:` | `if cond {` |
| `else:` | `} else {` |
| `elif cond:` | `} else if cond {` |
| `while cond:` | `while cond {` |
| `for x in e:` | `for x in e {` |
| `match e:` | `match e {` |
| `handle e:` | `handle e {` |
| INDENT | (block already opened by preceding `{`) |
| DEDENT | `}` |
| mid-block statement | `statement;` |
| tail expression | `expression` (no `;` — preserves return-value semantics) |

**Tail-position detection**: The preprocessor detects when a statement is the
last expression in its block (the next meaningful line has a smaller indent, or
EOF) and suppresses the trailing `;`.  Without this, every block would yield
`()` instead of the last expression's value, breaking AffineScript's
expression-oriented block semantics.

**Limitation — span fidelity**: Error spans currently refer to positions in
the transformed canonical text, not the original Python-face source.  A span
remapping table (tracking line/column shifts introduced by `{`/`}` injection) is
planned as a follow-up (see `docs/specs/SETTLED-DECISIONS.adoc`, ADR-010 §4).

### Error Vocabulary (`lib/face.ml`)

`Face.format_type_error : face -> Typecheck.type_error -> string` (and its
counterparts for `quantity_error`, `unify_error`, `resolve_error`) map
canonical error terms to face-appropriate display strings.

Python-face vocabulary selections:

| Canonical term | Python-face display |
|----------------|---------------------|
| `LinearVariableUnused id` | "Ownership error: single-use variable 'x' must be used exactly once, but was never used" |
| `LinearVariableUsedMultiple id` | "Ownership error: single-use variable 'x' can only be used once, but was used more than once" |
| `ErasedVariableUsed id` | "Ownership error: erased variable 'x' was declared as compile-time-only and cannot appear at runtime" |
| `UnboundVariable v` | "Name not found: 'v' — hint: check spelling or add a def declaration" |
| `TypeMismatch` | "Type error: expected T but got U" |
| `BranchTypeMismatch` | "if/else type mismatch: both branches must return the same type" |
| `UndefinedVariable id` | "Name not found: 'x' — hint: define it with def or let" |
| `UndefinedType id` | "Type not found: 'T' — hint: define it with class or type" |
| `UndefinedModule id` | "Module not found: 'M' — hint: use import M" |
| `Unit` type | rendered as `None` |
| `Bool` type | rendered as `bool` |

---

## CLI Usage

```
# Canonical face (default)
affinescript check foo.affine

# Python face
affinescript check --face python foo.pyaff
affinescript eval  --face python foo.pyaff
affinescript compile --face python foo.pyaff

# Debug: preview what the Python preprocessor produces
affinescript preview-python-transform foo.pyaff
```

---

## Adding a New Face

1. Add a variant to `type face` in `lib/face.ml`.
2. Add a preprocessor in a new `lib/<name>_face.ml` (if syntax differs).
3. Add branches to each `format_*_for_face` function in `lib/face.ml`.
4. Wire the face into `bin/main.ml` (`face_arg`, `parse_with_face`).
5. Add the file extension to `lib/dune` modules list.
6. Add E2E tests in `test/test_e2e.ml`.
7. Update this document.

No compiler internals change.

---

## Test Coverage

End-to-end Python-face tests live in `test/test_e2e.ml` under the
"E2E Python-Face" section:

- `def → fn` transformation
- `if`/`elif`/`else` chain
- Keyword substitution (`True`, `False`, `None`, `and`, `or`, `not`)
- Three-function fixture parses without error
- Transform preview output correctness

Seam check (items 1 + 2): `affinescript check --face python` on a file with a
doubly-used `@linear` variable correctly produces the Python-face error message
"Ownership error: single-use variable 'x' can only be used once..." confirming
the full pipeline from `.pyaff` → text transform → canonical parse → typecheck
→ face-aware error formatter.
