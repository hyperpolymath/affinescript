# AffineScript Language Roadmap

> A comprehensive development roadmap for AffineScript: a next-generation systems programming language with affine types, dependent types, row polymorphism, and extensible effects.

**Version**: 0.1.0-alpha
**Status**: Active Development
**Target**: Production-ready v1.0 by Q4 2026

---

## Table of Contents

1. [Vision & Goals](#vision--goals)
2. [Phase Overview](#phase-overview)
3. [Phase 1: Foundation](#phase-1-foundation-current)
4. [Phase 2: Core Language](#phase-2-core-language)
5. [Phase 3: Advanced Features](#phase-3-advanced-features)
6. [Phase 4: Tooling & Ecosystem](#phase-4-tooling--ecosystem)
7. [Phase 5: Optimization & Production](#phase-5-optimization--production)
8. [Testing Strategy](#testing-strategy)
9. [Standard Library Roadmap](#standard-library-roadmap)
10. [Framework Ecosystem](#framework-ecosystem)
11. [Milestones & Releases](#milestones--releases)

---

## Vision & Goals

### Core Language Principles

1. **Safety by Default**: Memory safety through affine types without garbage collection
2. **Expressive Types**: Dependent types for compile-time guarantees
3. **Flexible Records**: Row polymorphism for extensible, type-safe records
4. **Controlled Effects**: First-class effect system with algebraic effect handlers
5. **Predictable Performance**: Zero-cost abstractions targeting WebAssembly

### Target Use Cases

- **Systems Programming**: OS components, embedded systems, device drivers
- **WebAssembly Applications**: High-performance web applications
- **Blockchain & Smart Contracts**: Formally verified contract code
- **Safety-Critical Systems**: Aerospace, medical, automotive software
- **High-Performance Computing**: Numerical computing with static guarantees

---

## Phase Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│  PHASE 1: FOUNDATION (Current)                                               │
│  ├── Lexer (95% complete)                                                   │
│  ├── Parser (0% - PRIORITY)                                                 │
│  ├── AST & Core Types (100%)                                                │
│  └── Error Infrastructure (100%)                                            │
├─────────────────────────────────────────────────────────────────────────────┤
│  PHASE 2: CORE LANGUAGE                                                     │
│  ├── Name Resolution                                                        │
│  ├── Bidirectional Type Checker                                             │
│  ├── Borrow Checker                                                         │
│  └── Basic Code Generation                                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│  PHASE 3: ADVANCED FEATURES                                                 │
│  ├── Dependent Types                                                        │
│  ├── Row Polymorphism                                                       │
│  ├── Effect System                                                          │
│  └── Trait System                                                           │
├─────────────────────────────────────────────────────────────────────────────┤
│  PHASE 4: TOOLING & ECOSYSTEM                                               │
│  ├── REPL & Interpreter                                                     │
│  ├── Package Manager                                                        │
│  ├── LSP Server                                                             │
│  └── Formatter & Linter                                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│  PHASE 5: OPTIMIZATION & PRODUCTION                                         │
│  ├── WASM Backend Optimization                                              │
│  ├── Standard Library                                                       │
│  ├── Documentation & Tutorials                                              │
│  └── Framework Libraries                                                    │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Foundation (Current)

### 1.1 Lexer (sedlex-based) - 95% Complete

**Status**: ✅ Functional, needs refinement

| Feature | Status | Notes |
|---------|--------|-------|
| Keywords | ✅ Complete | All 40+ keywords recognized |
| Identifiers | ✅ Complete | Unicode support via sedlex |
| Numeric literals | ✅ Complete | Int, float, hex, binary, octal |
| String literals | ✅ Complete | Escape sequences, Unicode |
| Operators | ✅ Complete | All operators and punctuation |
| Comments | ✅ Complete | Line `//`, nested block `/* */` |
| Quantity annotations | ✅ Complete | `0`, `1`, `ω` (omega) |
| Row variables | ✅ Complete | `..rest` syntax |
| Source locations | ✅ Complete | Span tracking |

**Remaining Work**:
- [ ] Raw string literals (`r"..."`, `r#"..."#`)
- [ ] String interpolation lexing (`${}`)
- [ ] Documentation comments (`///`, `//!`)
- [ ] Attribute syntax (`#[...]`)

### 1.2 Parser (Menhir) - 0% Complete

**Status**: 🔴 Not Started - HIGHEST PRIORITY

**Implementation Plan**:

```
Parser Implementation Phases:
├── 1.2.1 Basic Expressions
│   ├── Literals (int, float, string, char, bool)
│   ├── Identifiers and paths
│   ├── Unary and binary operators
│   ├── Parenthesized expressions
│   └── Operator precedence
│
├── 1.2.2 Type Expressions
│   ├── Simple types (Int, Bool, String)
│   ├── Type constructors (Option[T], Vec[n, T])
│   ├── Function types (A -> B, A -{E}-> B)
│   ├── Tuple and record types
│   ├── Ownership modifiers (own, ref, mut)
│   └── Row types ({x: Int, ..r})
│
├── 1.2.3 Patterns
│   ├── Wildcard and variable patterns
│   ├── Literal patterns
│   ├── Constructor patterns
│   ├── Tuple and record patterns
│   ├── Or-patterns (p1 | p2)
│   └── Binding patterns (p @ pattern)
│
├── 1.2.4 Statements & Control Flow
│   ├── Let bindings
│   ├── Assignment statements
│   ├── If/else expressions
│   ├── Match expressions
│   ├── While/for loops
│   └── Return/break/continue
│
├── 1.2.5 Functions & Declarations
│   ├── Function declarations
│   ├── Type parameters and constraints
│   ├── Where clauses
│   ├── Total/partial annotations
│   └── Effect annotations
│
├── 1.2.6 Type Declarations
│   ├── Type aliases
│   ├── Struct definitions
│   ├── Enum definitions
│   ├── Associated types
│   └── Dependent type parameters
│
├── 1.2.7 Traits & Effects
│   ├── Trait declarations
│   ├── Trait implementations
│   ├── Effect declarations
│   ├── Effect handlers
│   └── Effect rows
│
└── 1.2.8 Modules
    ├── Module declarations
    ├── Import statements
    ├── Visibility modifiers
    └── Re-exports
```

**Grammar Conflicts to Resolve**:
- Function application vs tuple construction
- Type ascription vs ternary expressions
- Generic types vs comparison operators
- Closure syntax vs blocks

### 1.3 AST & Core Types - 100% Complete

**Status**: ✅ Complete

All AST node types defined in `lib/ast.ml`:
- Expression AST nodes
- Type expression nodes
- Pattern nodes
- Statement nodes
- Declaration nodes
- Module system nodes

### 1.4 Error Infrastructure - 100% Complete

**Status**: ✅ Complete

- Error codes organized by category (E0001-E0799, W0001-W0999)
- Diagnostic message system with labels and notes
- Terminal formatting with ANSI colors
- Source code display with error indicators

---

## Phase 2: Core Language

### 2.1 Name Resolution

**Dependencies**: Parser

**Implementation Tasks**:

```ocaml
(* Name resolution module structure *)
module Name_resolution : sig
  type scope = {
    variables: (string, var_info) Hashtbl.t;
    types: (string, type_info) Hashtbl.t;
    modules: (string, module_info) Hashtbl.t;
    parent: scope option;
  }

  val resolve_program : Ast.program -> Resolved.program
  val resolve_imports : Ast.import list -> import_info list
end
```

| Task | Description | Priority |
|------|-------------|----------|
| Scope management | Lexical scope tracking with nesting | High |
| Variable resolution | Link uses to definitions | High |
| Type name resolution | Resolve type constructors | High |
| Module resolution | Handle import paths | High |
| Shadowing rules | Correct shadowing semantics | Medium |
| Forward references | Handle mutual recursion | Medium |
| Visibility checking | Public/private enforcement | Medium |

### 2.2 Bidirectional Type Checker

**Dependencies**: Name Resolution

**Architecture**:

```
Type Checker Phases:
├── Kind Checking
│   ├── Type well-formedness
│   ├── Kind inference for type constructors
│   └── Kind unification
│
├── Type Inference (Bidirectional)
│   ├── Synthesis mode (infer type from term)
│   ├── Checking mode (check term against type)
│   ├── Application synthesis
│   └── Subsumption checking
│
├── Unification
│   ├── First-order unification
│   ├── Occurs check
│   ├── Constraint generation
│   └── Constraint solving
│
├── Quantity Checking
│   ├── Usage counting (0, 1, ω)
│   ├── Subquantity relation
│   └── Linearity enforcement
│
└── Effect Inference (Basic)
    ├── Effect synthesis
    ├── Effect subsumption
    └── Effect constraints
```

**Type Rules Implementation Order**:

1. **Literals & Variables** - Basic type lookup
2. **Functions** - Arrow types, application
3. **Let bindings** - Polymorphism, generalization
4. **Records** - Record types, field access
5. **Tuples** - Product types
6. **Match expressions** - Pattern typing
7. **If expressions** - Boolean conditions
8. **Loops** - Unit typing

### 2.3 Borrow Checker

**Dependencies**: Type Checker

**Implementation Strategy**:

```
Borrow Checking Phases:
├── Ownership Analysis
│   ├── Owned value tracking
│   ├── Move semantics
│   ├── Drop insertion
│   └── Use-after-move detection
│
├── Borrow Analysis
│   ├── Shared borrow (&T)
│   ├── Mutable borrow (&mut T)
│   ├── Borrow lifetime tracking
│   └── Reborrow rules
│
├── Linearity Checking
│   ├── Linear type enforcement
│   ├── Affine type enforcement
│   └── Quantity verification
│
└── Region Inference
    ├── Lifetime inference
    ├── Region constraints
    └── Outlives relations
```

**Key Algorithms**:
- Non-lexical lifetimes (NLL)
- Polonius-style borrow checking
- Linear type consumption tracking

### 2.4 Basic Code Generation

**Dependencies**: Borrow Checker

**Initial Target**: WASM (text format)

```
Code Generation Phases:
├── IR Lowering
│   ├── ANF transformation
│   ├── Closure conversion
│   ├── Monomorphization
│   └── Defunctionalization (for effects)
│
├── WASM Emission (MVP)
│   ├── Function compilation
│   ├── Local variable allocation
│   ├── Control flow (if, loop, block)
│   ├── Memory operations
│   └── Function calls
│
└── Runtime Support
    ├── Memory allocator stub
    ├── Drop glue
    └── Panic handler
```

---

## Phase 3: Advanced Features

### 3.1 Dependent Types

**Implementation Complexity**: High

```
Dependent Types Features:
├── Pi Types (Dependent Functions)
│   ├── (x: A) -> B[x]
│   ├── Implicit arguments
│   └── Type-level computation
│
├── Sigma Types (Dependent Pairs)
│   ├── (x: A, B[x])
│   └── Dependent records
│
├── Indexed Types
│   ├── Vec[n, T] - length-indexed vectors
│   ├── Matrix[m, n, T] - dimension-indexed matrices
│   └── Bounded integers
│
├── Refinement Types
│   ├── Int where (x > 0)
│   ├── Refinement checking
│   └── SMT solver integration
│
└── Equality Types
    ├── Propositional equality
    ├── Rewrite rules
    └── Transport
```

**SMT Integration for Refinements**:
- Z3 or CVC5 for constraint solving
- Liquid types style inference
- Decidable fragment restrictions

### 3.2 Row Polymorphism

**Implementation Strategy**:

```
Row Polymorphism Components:
├── Row Types
│   ├── {x: Int, y: String}  - Closed row
│   ├── {x: Int, ..r}        - Open row
│   └── {..r}                - Row variable
│
├── Row Operations
│   ├── Field access         - record.field
│   ├── Field extension      - {z: Bool, ..record}
│   ├── Field restriction    - record \ field
│   └── Field update         - {record with x = 5}
│
├── Row Unification
│   ├── Row variable unification
│   ├── Lacks constraints    - r lacks x
│   └── Presence constraints - r has x: T
│
├── Row Kinds
│   ├── Row : * -> Row
│   ├── Record : Row -> *
│   └── Variant : Row -> *
│
└── Applications
    ├── Extensible records
    ├── Polymorphic variants
    ├── Effect rows
    └── Module signatures
```

### 3.3 Effect System

**Algebraic Effects Implementation**:

```
Effect System Architecture:
├── Effect Declarations
│   │  effect State[S] {
│   │    fn get() -> S
│   │    fn put(s: S) -> Unit
│   │  }
│   └── Operation signatures
│
├── Effect Handlers
│   │  handle expr {
│   │    get() -> resume(state)
│   │    put(s) -> { state = s; resume(()) }
│   │    return(x) -> x
│   │  }
│   └── Handler semantics
│
├── Effect Rows
│   ├── Effect polymorphism
│   ├── Effect constraints
│   └── Effect subsumption
│
├── Effect Compilation
│   ├── CPS transformation
│   ├── Evidence passing
│   └── Handler optimization
│
└── Standard Effects
    ├── IO            - I/O operations
    ├── Exn           - Exceptions
    ├── Async         - Async/await
    ├── State[S]      - Mutable state
    ├── Reader[R]     - Environment reading
    ├── Writer[W]     - Logging/accumulation
    └── Div           - Divergence/non-termination
```

### 3.4 Trait System

**Trait Implementation**:

```
Trait System Components:
├── Trait Declarations
│   │  trait Eq {
│   │    fn eq(self: &Self, other: &Self) -> Bool
│   │  }
│   └── Method signatures
│
├── Implementations
│   │  impl Eq for Int {
│   │    fn eq(self: &Int, other: &Int) -> Bool = ...
│   │  }
│   └── Instance definitions
│
├── Associated Types
│   │  trait Iterator {
│   │    type Item
│   │    fn next(self: &mut Self) -> Option[Item]
│   │  }
│   └── Type family support
│
├── Trait Bounds
│   ├── fn sort[T: Ord](xs: Vec[T]) -> Vec[T]
│   ├── Where clauses
│   └── Higher-ranked bounds
│
├── Coherence Checking
│   ├── Orphan rules
│   ├── Overlap detection
│   └── Instance resolution
│
└── Trait Objects (Optional)
    ├── dyn Trait
    ├── Vtable generation
    └── Object safety rules
```

---

## Phase 4: Tooling & Ecosystem

### 4.1 REPL (Read-Eval-Print Loop)

**Features**:

```
REPL Implementation:
├── Core REPL
│   ├── Expression evaluation
│   ├── Type inference display
│   ├── Multi-line input
│   └── History management
│
├── Interactive Features
│   ├── :type <expr>     - Show type
│   ├── :kind <type>     - Show kind
│   ├── :effect <expr>   - Show effects
│   ├── :doc <name>      - Show documentation
│   ├── :source <name>   - Show definition
│   └── :browse <module> - List exports
│
├── Development Tools
│   ├── :load <file>     - Load module
│   ├── :reload          - Reload changes
│   ├── :set <option>    - Set options
│   └── :debug           - Debug mode
│
└── Advanced Features
    ├── Tab completion
    ├── Syntax highlighting
    ├── Error underlining
    └── Integrated help
```

### 4.2 Interpreter

**For Development & Testing**:

```
Interpreter Components:
├── Tree-Walking Interpreter
│   ├── Expression evaluation
│   ├── Pattern matching
│   ├── Environment management
│   └── Effect handling
│
├── Value Representation
│   ├── Primitive values
│   ├── Closures
│   ├── Records and variants
│   └── References
│
├── Runtime
│   ├── Memory management (simple GC)
│   ├── Stack management
│   ├── Effect handlers
│   └── Foreign function interface
│
└── Debugging Support
    ├── Breakpoints
    ├── Step execution
    ├── Variable inspection
    └── Call stack display
```

### 4.3 Package Manager (`aspm`)

**Package Manager Design**:

```
Package Manager (aspm):
├── Package Definition
│   │  # affine.toml
│   │  [package]
│   │  name = "my-library"
│   │  version = "1.0.0"
│   │  edition = "2025"
│   │
│   │  [dependencies]
│   │  std = "1.0"
│   │  json = "2.3"
│
├── Commands
│   ├── aspm init        - Create new project
│   ├── aspm build       - Build project
│   ├── aspm test        - Run tests
│   ├── aspm run         - Run main
│   ├── aspm add <pkg>   - Add dependency
│   ├── aspm remove      - Remove dependency
│   ├── aspm update      - Update dependencies
│   ├── aspm publish     - Publish package
│   └── aspm doc         - Generate documentation
│
├── Registry
│   ├── Central package registry
│   ├── Version resolution
│   ├── Dependency locking
│   └── Security auditing
│
└── Features
    ├── Workspaces
    ├── Build profiles
    ├── Feature flags
    └── Platform targeting
```

### 4.4 Language Server Protocol (LSP)

**LSP Implementation**:

```
LSP Server Features:
├── Core Features
│   ├── Diagnostics (errors, warnings)
│   ├── Go to definition
│   ├── Find references
│   ├── Hover information
│   └── Signature help
│
├── Completions
│   ├── Keyword completion
│   ├── Variable completion
│   ├── Type completion
│   ├── Method completion
│   └── Import completion
│
├── Refactoring
│   ├── Rename symbol
│   ├── Extract function
│   ├── Inline variable
│   └── Organize imports
│
├── Code Actions
│   ├── Quick fixes
│   ├── Import suggestions
│   ├── Type annotations
│   └── Effect annotations
│
└── Semantic Features
    ├── Semantic highlighting
    ├── Folding ranges
    ├── Document symbols
    └── Inlay hints (types, effects)
```

### 4.5 Formatter (`asfmt`)

**Code Formatter**:

```
Formatter Features:
├── Formatting Rules
│   ├── Indentation (spaces/tabs)
│   ├── Line length limits
│   ├── Brace style
│   ├── Import ordering
│   └── Comment formatting
│
├── Configuration (.asfmt.toml)
│   ├── max_line_length = 100
│   ├── indent_size = 2
│   ├── use_tabs = false
│   └── trailing_comma = true
│
└── Integration
    ├── CLI tool
    ├── Editor integration
    ├── Pre-commit hook
    └── CI integration
```

### 4.6 Linter (`aslint`)

**Linter Design**:

```
Linter Features:
├── Style Checks
│   ├── Naming conventions
│   ├── Unused bindings
│   ├── Redundant patterns
│   └── Documentation coverage
│
├── Correctness Checks
│   ├── Unused imports
│   ├── Dead code
│   ├── Unreachable patterns
│   └── Suspicious comparisons
│
├── Performance Checks
│   ├── Unnecessary allocations
│   ├── Inefficient patterns
│   └── Missing inlining hints
│
├── Safety Checks
│   ├── Unsafe block auditing
│   ├── Panic paths
│   └── Effect leaks
│
└── Configuration
    ├── Rule enablement
    ├── Severity levels
    └── Custom rules (plugin system)
```

---

## Phase 5: Optimization & Production

### 5.1 WASM Backend Optimization

```
WASM Optimization Phases:
├── High-Level Optimizations
│   ├── Inlining
│   ├── Constant folding
│   ├── Dead code elimination
│   ├── Common subexpression elimination
│   └── Tail call optimization
│
├── Type-Directed Optimizations
│   ├── Monomorphization
│   ├── Devirtualization
│   ├── Specialization
│   └── Unboxing
│
├── WASM-Specific
│   ├── Stack allocation
│   ├── Linear memory layout
│   ├── WASM GC integration
│   ├── SIMD utilization
│   └── Multi-memory support
│
└── Link-Time Optimization
    ├── Cross-module inlining
    ├── Unused function elimination
    └── Code size optimization
```

### 5.2 Additional Backends (Future)

```
Future Backend Targets:
├── Native (LLVM)
│   ├── x86_64
│   ├── ARM64
│   └── RISC-V
│
├── JavaScript
│   ├── ESM output
│   ├── TypeScript declarations
│   └── Source maps
│
└── Other
    ├── C (for embedding)
    └── SPIR-V (GPU compute)
```

---

## Testing Strategy

### Property-Based Testing (Echidna-Inspired)

Drawing from property-based fuzzing principles (similar to [Echidna](https://github.com/crytic/echidna)), we implement comprehensive testing:

```
Testing Architecture:
├── Unit Tests (Alcotest)
│   ├── Lexer token tests
│   ├── Parser AST tests
│   ├── Type checker tests
│   └── Code generation tests
│
├── Property-Based Tests (QCheck)
│   ├── Lexer Properties
│   │   ├── lex(print(tokens)) = tokens (roundtrip)
│   │   ├── Valid source never crashes
│   │   └── Location tracking consistency
│   │
│   ├── Parser Properties
│   │   ├── parse(print(ast)) = ast (roundtrip)
│   │   ├── Associativity preservation
│   │   └── Precedence correctness
│   │
│   ├── Type System Properties
│   │   ├── Well-typed terms don't get stuck
│   │   ├── Type preservation (subject reduction)
│   │   ├── Progress (well-typed can step)
│   │   └── Substitution lemma
│   │
│   ├── Borrow Checker Properties
│   │   ├── No use-after-free in valid programs
│   │   ├── No data races
│   │   └── Linear resources used exactly once
│   │
│   └── Code Generation Properties
│       ├── Semantics preservation
│       ├── No undefined behavior
│       └── Memory safety
│
├── Fuzzing (AFL/LibFuzzer)
│   ├── Lexer fuzzing
│   ├── Parser fuzzing
│   ├── Type checker fuzzing
│   └── WASM output fuzzing
│
├── Integration Tests
│   ├── End-to-end compilation
│   ├── Standard library tests
│   └── Example program tests
│
├── Differential Testing
│   ├── Interpreter vs compiler
│   ├── Optimized vs unoptimized
│   └── Different backends
│
└── Benchmark Suite
    ├── Compilation time
    ├── Runtime performance
    └── Memory usage
```

### Test Infrastructure

```ocaml
(* Property-based test example using QCheck *)
let lexer_roundtrip =
  QCheck.Test.make ~name:"lexer roundtrip"
    ~count:1000
    arbitrary_valid_source
    (fun source ->
      let tokens = Lexer.lex source in
      let printed = Token.print_tokens tokens in
      let tokens' = Lexer.lex printed in
      tokens = tokens')

let type_preservation =
  QCheck.Test.make ~name:"type preservation"
    ~count:1000
    arbitrary_well_typed_expr
    (fun (expr, ty) ->
      match Eval.step expr with
      | None -> true  (* normal form *)
      | Some expr' ->
        let ty' = Type_check.infer expr' in
        Type.equal ty ty')
```

### Formal Verification (Long-term)

```
Formal Verification Goals:
├── Type System Soundness
│   ├── Coq/Lean formalization
│   ├── Progress theorem
│   └── Preservation theorem
│
├── Borrow Checker Correctness
│   ├── Safety guarantees
│   └── Memory model
│
└── Compiler Correctness
    ├── Semantics preservation
    └── Optimization correctness
```

---

## Standard Library Roadmap

### Core Library (`std`)

```
Standard Library Structure:
├── Primitives
│   ├── std.int       - Integer operations
│   ├── std.float     - Floating point
│   ├── std.bool      - Boolean operations
│   ├── std.char      - Character operations
│   └── std.string    - String operations
│
├── Collections
│   ├── std.vec       - Dynamic arrays
│   ├── std.array     - Fixed-size arrays
│   ├── std.list      - Linked lists
│   ├── std.map       - Hash maps
│   ├── std.set       - Hash sets
│   ├── std.btree     - B-tree map/set
│   └── std.deque     - Double-ended queues
│
├── Core Types
│   ├── std.option    - Option[T]
│   ├── std.result    - Result[T, E]
│   ├── std.tuple     - Tuple operations
│   └── std.unit      - Unit type
│
├── Memory
│   ├── std.box       - Heap allocation
│   ├── std.rc        - Reference counting
│   ├── std.arc       - Atomic ref counting
│   └── std.ptr       - Raw pointers (unsafe)
│
├── Traits
│   ├── std.eq        - Equality
│   ├── std.ord       - Ordering
│   ├── std.hash      - Hashing
│   ├── std.show      - String conversion
│   ├── std.clone     - Cloning
│   ├── std.default   - Default values
│   └── std.iter      - Iteration
│
├── Effects
│   ├── std.io        - I/O operations
│   ├── std.exn       - Exceptions
│   ├── std.async     - Async/await
│   ├── std.state     - Mutable state
│   └── std.random    - Random numbers
│
├── Concurrency
│   ├── std.thread    - Threading
│   ├── std.sync      - Synchronization
│   ├── std.channel   - Message passing
│   └── std.atomic    - Atomic operations
│
├── I/O
│   ├── std.fs        - File system
│   ├── std.net       - Networking
│   ├── std.path      - Path manipulation
│   └── std.process   - Process spawning
│
├── Text
│   ├── std.fmt       - Formatting
│   ├── std.regex     - Regular expressions
│   └── std.unicode   - Unicode utilities
│
└── Utilities
    ├── std.time      - Time and duration
    ├── std.env       - Environment variables
    └── std.debug     - Debugging utilities
```

### Extended Libraries

```
Extended Libraries:
├── Data Formats
│   ├── json         - JSON parsing/serialization
│   ├── toml         - TOML configuration
│   ├── yaml         - YAML support
│   ├── xml          - XML parsing
│   └── csv          - CSV handling
│
├── Cryptography
│   ├── crypto.hash  - Hash functions
│   ├── crypto.aes   - AES encryption
│   ├── crypto.rsa   - RSA encryption
│   └── crypto.rand  - Secure random
│
├── Serialization
│   ├── serde        - Serialization framework
│   ├── bincode      - Binary encoding
│   └── protobuf     - Protocol buffers
│
├── HTTP
│   ├── http.client  - HTTP client
│   ├── http.server  - HTTP server
│   └── http.router  - Request routing
│
├── Database
│   ├── sql          - SQL query builder
│   ├── postgres     - PostgreSQL driver
│   ├── sqlite       - SQLite driver
│   └── redis        - Redis client
│
└── Testing
    ├── test         - Test framework
    ├── mock         - Mocking utilities
    └── bench        - Benchmarking
```

---

## Framework Ecosystem

### Web Framework (`affine-web`)

```
Web Framework Design:
├── Core
│   ├── Request/Response types
│   ├── Middleware system
│   ├── Routing (type-safe)
│   └── Error handling
│
├── Features
│   ├── Static file serving
│   ├── Template rendering
│   ├── Session management
│   ├── Authentication
│   └── WebSocket support
│
└── Example
    fn main() -{IO}-> Unit = {
      let app = router()
        .get("/", home_handler)
        .get("/users/:id", get_user)
        .post("/users", create_user)
        .middleware(logging)
        .middleware(auth);

      serve(app, "0.0.0.0:8080")
    }
```

### CLI Framework (`affine-cli`)

```
CLI Framework:
├── Argument Parsing
│   ├── Positional arguments
│   ├── Named flags
│   ├── Subcommands
│   └── Auto-generated help
│
├── Features
│   ├── Tab completion
│   ├── Progress bars
│   ├── Colored output
│   └── Interactive prompts
│
└── Example
    #[derive(Args)]
    struct Options {
      #[arg(short, long)]
      verbose: Bool,

      #[arg(positional)]
      files: Vec[String],
    }
```

### Embedded Framework (`affine-embedded`)

```
Embedded Framework:
├── HAL (Hardware Abstraction)
│   ├── GPIO
│   ├── SPI
│   ├── I2C
│   ├── UART
│   └── Timers
│
├── RTOS Integration
│   ├── Task scheduling
│   ├── Interrupt handling
│   └── Memory pools
│
└── Targets
    ├── ARM Cortex-M
    ├── RISC-V
    └── ESP32 (via WASM)
```

### Game Framework (`affine-game`)

```
Game Framework:
├── Core
│   ├── Game loop
│   ├── Entity-Component-System
│   ├── Scene management
│   └── Asset loading
│
├── Graphics (WASM-focused)
│   ├── WebGL/WebGPU backend
│   ├── 2D sprites
│   ├── 3D rendering
│   └── Shaders
│
├── Audio
│   ├── Sound effects
│   ├── Music streaming
│   └── Spatial audio
│
└── Input
    ├── Keyboard/Mouse
    ├── Gamepad
    └── Touch
```

---

## Milestones & Releases

### v0.1.0 - Lexer & Parser (Current Target)

- [x] Complete lexer implementation
- [ ] Complete parser implementation
- [ ] Basic error recovery
- [ ] 100+ parser test cases
- [ ] CLI: `lex`, `parse` commands

### v0.2.0 - Type System Foundation

- [ ] Name resolution
- [ ] Basic bidirectional type checker
- [ ] Simple type inference
- [ ] No dependent types yet
- [ ] CLI: `check` command

### v0.3.0 - Ownership & Borrowing

- [ ] Ownership tracking
- [ ] Borrow checker
- [ ] Linearity checking
- [ ] Memory safety guarantees

### v0.4.0 - Basic Code Generation

- [ ] ANF transformation
- [ ] WASM text format output
- [ ] Basic runtime
- [ ] CLI: `compile` command

### v0.5.0 - MVP (Minimum Viable Product)

- [ ] Complete core language
- [ ] REPL
- [ ] Basic standard library
- [ ] Interpreter mode
- [ ] Documentation

### v0.6.0 - Dependent Types

- [ ] Pi types (dependent functions)
- [ ] Indexed types (Vec[n, T])
- [ ] Basic refinement types
- [ ] SMT integration

### v0.7.0 - Row Polymorphism

- [ ] Extensible records
- [ ] Row unification
- [ ] Lacks constraints
- [ ] Polymorphic variants

### v0.8.0 - Effect System

- [ ] Effect declarations
- [ ] Effect handlers
- [ ] Effect polymorphism
- [ ] Standard effects

### v0.9.0 - Trait System

- [ ] Trait declarations
- [ ] Implementations
- [ ] Associated types
- [ ] Coherence checking

### v1.0.0 - Production Ready

- [ ] Complete standard library
- [ ] Optimized WASM backend
- [ ] LSP server
- [ ] Package manager
- [ ] Comprehensive documentation
- [ ] Formal verification of type system

### Future (Post-1.0)

- [ ] Native backend (LLVM)
- [ ] JavaScript backend
- [ ] GPU compute (SPIR-V)
- [ ] Incremental compilation
- [ ] IDE plugins
- [ ] Macro system

---

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on contributing to AffineScript.

### Priority Areas

1. **Parser Implementation** - Highest priority, blocks all other work
2. **Test Coverage** - Property-based tests, fuzzing
3. **Documentation** - Tutorials, examples, wiki
4. **Tooling** - LSP, formatter, linter

### Getting Started

```bash
# Clone the repository
git clone https://github.com/hyperpolymath/affinescript.git

# Set up OCaml environment
opam switch create . 5.1.0
opam install . --deps-only --with-test --with-doc

# Build and test
dune build
dune runtest
```

---

## References

- [Language Specification](docs/spec.md)
- [Wiki Documentation](wiki/README.md)
- [Example Programs](examples/)
- [API Documentation](https://docs.affinescript.org)

---

*Last updated: 2025-12-17*
