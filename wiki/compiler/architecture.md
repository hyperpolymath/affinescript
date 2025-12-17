# Compiler Architecture

This document describes the architecture of the AffineScript compiler.

## Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         AffineScript Compiler                           │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   Source Code (.as)                                                     │
│        │                                                                │
│        ▼                                                                │
│   ┌─────────┐                                                          │
│   │  Lexer  │  sedlex-based, Unicode support                           │
│   └────┬────┘                                                          │
│        │ Token Stream                                                   │
│        ▼                                                                │
│   ┌─────────┐                                                          │
│   │ Parser  │  Menhir-based, error recovery                            │
│   └────┬────┘                                                          │
│        │ Concrete Syntax Tree (CST)                                    │
│        ▼                                                                │
│   ┌───────────┐                                                        │
│   │ Desugarer │  CST → AST transformation                              │
│   └─────┬─────┘                                                        │
│         │ Abstract Syntax Tree (AST)                                   │
│         ▼                                                               │
│   ┌──────────────┐                                                     │
│   │Name Resolver │  Scope analysis, module resolution                  │
│   └──────┬───────┘                                                     │
│          │ Resolved AST                                                │
│          ▼                                                              │
│   ┌──────────────┐                                                     │
│   │ Type Checker │  Bidirectional inference, dependent types           │
│   └──────┬───────┘                                                     │
│          │ Typed AST + Constraints                                     │
│          ▼                                                              │
│   ┌───────────────┐                                                    │
│   │ Borrow Checker│  Ownership, lifetimes, linearity                   │
│   └───────┬───────┘                                                    │
│           │ Verified AST                                               │
│           ▼                                                             │
│   ┌───────────────┐                                                    │
│   │ Trait Solver  │  Instance resolution, coherence                    │
│   └───────┬───────┘                                                    │
│           │ Elaborated AST                                             │
│           ▼                                                             │
│   ┌───────────────┐                                                    │
│   │Monomorphizer  │  Generic specialization                            │
│   └───────┬───────┘                                                    │
│           │ Monomorphic AST                                            │
│           ▼                                                             │
│   ┌───────────┐                                                        │
│   │ IR Lower  │  ANF/CPS transformation                                │
│   └─────┬─────┘                                                        │
│         │ Intermediate Representation                                  │
│         ▼                                                               │
│   ┌───────────┐                                                        │
│   │ Optimizer │  Inlining, DCE, constant folding                       │
│   └─────┬─────┘                                                        │
│         │ Optimized IR                                                 │
│         ▼                                                               │
│   ┌────────────┐                                                       │
│   │ Code Gen   │  Target-specific emission                             │
│   └─────┬──────┘                                                       │
│         │                                                               │
│         ▼                                                               │
│   WebAssembly (.wasm)                                                  │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

## Compiler Phases

### Phase 1: Lexical Analysis

**Module**: `lib/lexer.ml`
**Library**: sedlex

Converts source text into tokens:

```ocaml
type token =
  | KEYWORD of keyword
  | IDENT of string
  | INT_LIT of int
  | FLOAT_LIT of float
  | STRING_LIT of string
  | OPERATOR of string
  | PUNCT of char
  | EOF
```

Features:
- Unicode identifier support
- Nested block comments
- Source location tracking (Span)
- Lexer error recovery

### Phase 2: Parsing

**Module**: `lib/parser.ml` (planned)
**Library**: Menhir

Converts token stream to Concrete Syntax Tree (CST):

```ocaml
type cst_expr =
  | CST_Literal of literal * span
  | CST_Ident of string * span
  | CST_Binary of cst_expr * binop * cst_expr * span
  | CST_App of cst_expr * cst_expr list * span
  (* ... *)
```

Features:
- Operator precedence handling
- Error recovery with synchronization points
- Detailed parse error messages
- Source location preservation

### Phase 3: Desugaring

**Module**: `lib/desugar.ml` (planned)

Transforms CST to cleaner AST:

Desugarings include:
- `if`/`else` chains → nested matches
- `for` loops → `while` + iterators
- Operator sections → lambdas
- Method calls → function application
- Field punning expansion

### Phase 4: Name Resolution

**Module**: `lib/resolve.ml` (planned)

Resolves all names to their definitions:

```ocaml
type resolved_name =
  | Local of int (* de Bruijn index *)
  | Global of path
  | Builtin of builtin

type scope = {
  vars: (string, resolved_name) Map.t;
  types: (string, type_def) Map.t;
  modules: (string, module_sig) Map.t;
  parent: scope option;
}
```

Responsibilities:
- Variable binding and lookup
- Type name resolution
- Module path resolution
- Import handling
- Visibility checking

### Phase 5: Type Checking

**Module**: `lib/typecheck.ml` (planned)

Bidirectional type inference and checking:

```ocaml
(* Synthesis: infer type from term *)
val synth : ctx -> expr -> typ * elaborated_expr

(* Checking: check term against expected type *)
val check : ctx -> expr -> typ -> elaborated_expr

(* Unification: equate two types *)
val unify : ctx -> typ -> typ -> substitution
```

Components:
- **Kind checker**: Ensures types are well-formed
- **Type synthesizer**: Infers types from expressions
- **Type checker**: Verifies expressions against types
- **Constraint solver**: Solves type constraints
- **Effect inferencer**: Infers effect annotations

### Phase 6: Borrow Checking

**Module**: `lib/borrow.ml` (planned)

Verifies ownership and borrowing rules:

```ocaml
type place = {
  base: var;
  projections: projection list;
}

type loan = {
  place: place;
  kind: Shared | Mutable;
  region: region;
}

type ownership_state = {
  loans: loan set;
  moves: place set;
  drops: place set;
}
```

Analyses:
- **Ownership tracking**: Who owns what
- **Borrow tracking**: Active borrows and their regions
- **Move analysis**: Detect use-after-move
- **Drop insertion**: Where values are freed
- **Linearity checking**: Linear types used exactly once

### Phase 7: Trait Resolution

**Module**: `lib/traits.ml` (planned)

Resolves trait methods to implementations:

```ocaml
type instance = {
  trait: path;
  typ: typ;
  methods: (string, expr) Map.t;
  assoc_types: (string, typ) Map.t;
}

val resolve_method : ctx -> typ -> trait -> string -> instance * expr
```

Features:
- Instance search
- Coherence checking (no overlapping instances)
- Associated type resolution
- Superclass resolution

### Phase 8: Monomorphization

**Module**: `lib/mono.ml` (planned)

Specializes generic code for concrete types:

```ocaml
val monomorphize : typed_program -> mono_program

(* Tracks which specializations are needed *)
type specialization_queue = (generic_fn * typ list) Queue.t
```

Process:
1. Start from `main` and entry points
2. Collect required type instantiations
3. Generate specialized versions
4. Replace generic calls with specialized calls

### Phase 9: IR Lowering

**Module**: `lib/lower.ml` (planned)

Transforms to intermediate representation:

```ocaml
type ir_expr =
  | IR_Var of var
  | IR_Lit of literal
  | IR_Let of var * ir_expr * ir_expr
  | IR_App of var * var list
  | IR_If of var * ir_block * ir_block
  | IR_Match of var * (pattern * ir_block) list
  | IR_Return of var
  | IR_Unreachable
```

Transformations:
- ANF conversion (all subexpressions named)
- Closure conversion (closures become structs)
- Effect compilation (CPS or evidence passing)
- Pattern match compilation

### Phase 10: Optimization

**Module**: `lib/optimize.ml` (planned)

Standard compiler optimizations:

- Dead code elimination
- Constant folding and propagation
- Inlining (guided by heuristics)
- Common subexpression elimination
- Tail call optimization
- Escape analysis (stack allocation)

### Phase 11: Code Generation

**Module**: `lib/codegen.ml` (planned)

Emits target code (initially WASM):

```ocaml
type wasm_instr =
  | I32_const of int32
  | I64_const of int64
  | Local_get of int
  | Local_set of int
  | Call of func_idx
  | If of block_type * wasm_instr list * wasm_instr list
  | Loop of block_type * wasm_instr list
  (* ... *)

val emit_module : mono_program -> wasm_module
```

## Data Structures

### Source Locations

```ocaml
type position = {
  line: int;
  column: int;
  offset: int;
}

type span = {
  start: position;
  end_: position;
  file: string;
}
```

### Abstract Syntax Tree

See `lib/ast.ml` for complete definitions.

Key types:
- `expr` - Expressions
- `typ` - Type expressions
- `pattern` - Patterns
- `stmt` - Statements
- `decl` - Declarations

### Error Handling

```ocaml
type error_code =
  | E0001 (* Lexer: unexpected character *)
  | E0100 (* Parser: unexpected token *)
  | E0300 (* Type: mismatch *)
  | E0500 (* Borrow: use after move *)
  (* ... *)

type diagnostic = {
  code: error_code;
  message: string;
  span: span;
  labels: (span * string) list;
  notes: string list;
  help: string option;
}
```

## File Organization

```
lib/
├── ast.ml           # AST definitions
├── token.ml         # Token definitions
├── span.ml          # Source locations
├── lexer.ml         # Lexical analysis
├── parser.ml        # Syntactic analysis (planned)
├── desugar.ml       # CST → AST (planned)
├── resolve.ml       # Name resolution (planned)
├── typecheck.ml     # Type checking (planned)
├── borrow.ml        # Borrow checking (planned)
├── traits.ml        # Trait resolution (planned)
├── mono.ml          # Monomorphization (planned)
├── ir.ml            # IR definitions (planned)
├── lower.ml         # AST → IR (planned)
├── optimize.ml      # IR optimizations (planned)
├── codegen.ml       # Code generation (planned)
├── error.ml         # Diagnostics
└── driver.ml        # Compiler driver (planned)
```

## Implementation Order

1. **Parser** (blocks everything)
2. **Name resolution** (enables type checking)
3. **Basic type checking** (simple types first)
4. **Borrow checking** (ownership semantics)
5. **Basic codegen** (WASM text format)
6. **Advanced types** (dependent, rows)
7. **Effects** (handlers, CPS)
8. **Optimizations** (incremental)

## Testing Strategy

Each phase has corresponding tests:

```
test/
├── test_lexer.ml    # Lexer unit tests
├── test_parser.ml   # Parser unit tests (planned)
├── test_types.ml    # Type checker tests (planned)
├── test_borrow.ml   # Borrow checker tests (planned)
├── test_codegen.ml  # Code generation tests (planned)
└── integration/     # End-to-end tests (planned)
```

---

## See Also

- [Lexer](lexer.md) - Lexer implementation details
- [Parser](parser.md) - Parser implementation details
- [Type Checker](type-checker.md) - Type checking algorithm
- [Borrow Checker](borrow-checker.md) - Ownership analysis
- [Code Generation](codegen.md) - WASM emission
