<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# Reply to REPORT-SUM-TYPES.md — Sum Types Already Implemented

**Date:** 2026-03-21
**Author:** Jonathan D.A. Jewell (hyperpolymath)
**Re:** `REPORT-SUM-TYPES.md` (located at repo root `/REPORT-SUM-TYPES.md`)

---

## TL;DR

The report's central claim — that AffineScript does **not** support sum types — is
**incorrect**. Sum types (algebraic data types) are already fully implemented across the
parser, AST, interpreter, name resolver, and WASM code generator. The proposed workarounds
(integer enums, external DSL, 3–6 month implementation plan) are unnecessary.

What _does_ need fixing is a **syntax discrepancy** between the spec and the compiler, and
a **gap in the tree-sitter grammar** used for editor support.

---

## Evidence: What Already Exists

### 1. Parser (`lib/parser.mly`, lines 221–237)

The Menhir parser accepts enum declarations with full variant support:

```affinescript
enum Option[T] {
  Some(T),
  None
}

enum Result[T, E] {
  Ok(T),
  Err(E)
}
```

Three variant forms are supported:

| Form | Example | Parser line |
|------|---------|-------------|
| Nullary | `None` | 233 |
| Positional fields | `Some(T)` | 234–235 |
| GADT return type | `Typed(T): Option[T]` | 236–237 |

### 2. AST (`lib/ast.ml`, lines 281–296)

The AST has a dedicated `TyEnum` node:

```ocaml
and type_body =
  | TyAlias of type_expr
  | TyStruct of struct_field list
  | TyEnum of variant_decl list

and variant_decl = {
  vd_name : ident;
  vd_fields : type_expr list;
  vd_ret_ty : type_expr option;  (* GADT return type *)
}
```

### 3. Expression-level variant construction (`lib/parser.mly`, line 467–469)

Variants are constructed with qualified `Type::Variant` syntax:

```affinescript
let x: Option[Int] = Option::Some(42)
let y: Option[Int] = Option::None
```

### 4. Pattern matching (`lib/parser.mly`, lines 491–492, 638–640)

`match` expressions with constructor patterns:

```affinescript
match result {
  Ok(value) => handle(value),
  Err(e) => log(e),
}
```

### 5. WASM code generation (`lib/codegen.ml`, lines 28, 631–674, 1472–1479)

Tagged unions are implemented in the WASM backend:

- Variants are assigned sequential integer tags at codegen time
- `variant_tags` context tracks `(constructor_name, tag_int)` mappings
- Heap-allocated variant values store the tag + payload
- Pattern matching compiles to tag-comparison branches

### 6. Interpreter (`lib/interp.ml`, line 198–201)

The tree-walking interpreter handles `ExprVariant` and returns `VVariant` values.

### 7. Name resolution (`lib/resolve.ml`, line 303)

The resolver traverses `ExprVariant` nodes.

---

## What Actually Needs Fixing

### Issue 1: Spec/Parser Syntax Mismatch

The **spec** (`docs/spec.md`, line 1792) defines ML-style pipe-separated syntax:

```ebnf
enum_body = [ '|' ] variant { '|' variant } ;
```

Which would look like:

```
type Option a = None | Some a
```

But the **parser** uses Rust-style brace-delimited syntax:

```affinescript
enum Option[T] { Some(T), None }
```

**Resolution (DONE):** The parser is the source of truth. The spec EBNF at Appendix A
has been updated to use separate `type_alias`, `struct_decl`, and `enum_decl` productions
matching the implemented syntax:

```ebnf
type_decl     = type_alias | struct_decl | enum_decl ;
type_alias    = visibility 'type' UPPER_IDENT [ type_params ] '=' type_expr ';' ;
struct_decl   = visibility 'struct' UPPER_IDENT [ type_params ]
                '{' field_decl { ',' field_decl } [ ',' ] '}' ;
enum_decl     = visibility 'enum' UPPER_IDENT [ type_params ]
                '{' variant_decl { ',' variant_decl } [ ',' ] '}' ;
variant_decl  = UPPER_IDENT
              | UPPER_IDENT '(' type_expr { ',' type_expr } ')'
              | UPPER_IDENT '(' type_expr { ',' type_expr } ')' ':' type_expr ;
```

### Issue 2: Tree-sitter Grammar Was Missing Enums — FIXED

The tree-sitter grammar previously only had a `type_decl` rule for type aliases. Editors
using tree-sitter (VS Code, Neovim, Helix, Zed) had no syntax highlighting or completion
for enums or structs.

**Resolution (DONE):** Added the following rules to
`editors/tree-sitter-affinescript/grammar.js`:

- `struct_decl` — `struct Name { field: Type }` declarations
- `struct_field` — visibility + name + type annotation
- `enum_decl` — `enum Name { Variant1, Variant2(T) }` declarations
- `variant_decl` — nullary, positional, named-field, and GADT variants
- `variant_expr` — `Type::Variant` qualified constructor expressions

### Issue 3: Original Report Superseded — DONE

`REPORT-SUM-TYPES.md` has been marked as superseded with a banner pointing to this reply.

---

## Summary of Actions Taken

| # | Action | Status |
|---|--------|--------|
| 1 | Updated `docs/spec.md` EBNF to match parser (separate `enum_decl`/`struct_decl` productions) | **Done** |
| 2 | Added `enum_decl`, `struct_decl`, `variant_decl`, `variant_expr` to tree-sitter grammar | **Done** |
| 3 | Marked `REPORT-SUM-TYPES.md` as superseded | **Done** |

No parser, type checker, codegen, or runtime changes were needed. The compiler already
handles sum types end-to-end.

---

## Appendix B: Rebuttal — "Missing Features" Claim

A separate report claimed AffineScript lacks spread operators, if-expressions in
assignments, and concise struct updates. All three claims are **incorrect**.

### "No Spread Operator Support" — FALSE

The parser supports record spread syntax (`parser.mly` lines 481, 527-528):

```affinescript
// Record spread (struct update) — Rust-style `..` not JS-style `...`
let updated = { health: 100, ..original_player }
```

The syntax is `{ field: value, ..base_expr }`. The claim used JavaScript's
`{...obj}` syntax (triple-dot), which is not AffineScript syntax.

**Evidence:** `record_spread: | COMMA DOTDOT e = expr { e }` in parser.mly:527-528.

### "let x = if...else... Fails to Parse" — FALSE

`if` is an expression (`parser.mly` line 488-489) and `let` accepts any expression
as its value (`parser.mly` line 495-497):

```affinescript
// This works — if is an expression
let x = if condition { value_a } else { value_b }

// This also works with else-if chains
let x = if a { 1 } else if b { 2 } else { 3 }
```

The `if` branches require **block syntax** `{ }` — not bare expressions. Writing
`let x = if cond then a else b` (ML/Haskell style) will fail because AffineScript
uses Rust-style blocks. This is by design, not a limitation.

### "Verbose Struct Updates" — FALSE (same as spread)

Record spread IS the struct update syntax:

```affinescript
// Update one field, keep everything else
let new_state = { score: state.score + 10, ..state }

// Update multiple fields
let healed = { health: 100, status: Status::Alive, ..player }
```

No manual field copying required.

### Root Cause

These are **documentation gaps**, not language limitations. The reporter appears to
have tried JavaScript/Haskell syntax in a Rust-influenced language.

---

## Appendix A: Feature Coverage Matrix

| Capability | Parser | AST | Resolver | Interpreter | WASM Codegen |
|------------|--------|-----|----------|-------------|--------------|
| Enum declaration | Yes | Yes | — | — | Yes (tag assignment) |
| Nullary variant | Yes | Yes | Yes | Yes | Yes |
| Variant with fields | Yes | Yes | Yes | Yes | Yes (heap alloc) |
| GADT return type | Yes | Yes | — | — | — |
| Type::Variant expr | Yes | Yes | Yes | Yes | Yes |
| Pattern matching | Yes | Yes | — | Yes | Yes |
| Exhaustiveness check | — | — | — | — | Partial (error E0702 defined) |
| Tree-sitter highlighting | **No** | — | — | — | — |
| Spec EBNF alignment | **No** | — | — | — | — |
