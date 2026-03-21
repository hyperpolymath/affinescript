# AffineScript Implementation Roadmap

## Overview

This roadmap outlines the path from current state (lexer + parser) to a complete, production-ready language.

```
Current State                                                   Goal
     │                                                            │
     ▼                                                            ▼
┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐
│  Lexer  │ → │ Parser  │ → │  Type   │ → │ Codegen │ → │ Runtime │
│   ✅    │   │   ✅    │   │ Checker │   │  WASM   │   │  Rust   │
└─────────┘   └─────────┘   └─────────┘   └─────────┘   └─────────┘
                                 │
                    ┌────────────┼────────────┐
                    ▼            ▼            ▼
               ┌────────┐  ┌─────────┐  ┌─────────┐
               │ Borrow │  │ Effect  │  │Refinemt │
               │Checker │  │Inference│  │  SMT    │
               └────────┘  └─────────┘  └─────────┘
```

---

## Phase 0: Foundation ✅ COMPLETE

**Status**: Done

- [x] Project structure (Dune)
- [x] Lexer (Sedlex)
- [x] Parser (Menhir)
- [x] AST definitions
- [x] Error infrastructure
- [x] CLI skeleton
- [x] Test framework
- [x] Academic documentation

---

## Phase 1: Core Type System

**Goal**: Type check simple programs without effects or ownership

**Duration**: 8-12 weeks

### 1.1 Name Resolution
- [ ] Symbol table structure
- [ ] Scope management
- [ ] Module path resolution
- [ ] Import resolution
- [ ] Visibility checking (pub, pub(crate), etc.)

**Files**: `lib/resolve.ml`, `lib/symbol.ml`

### 1.2 Kind Checking
- [ ] Kind inference
- [ ] Kind unification
- [ ] Higher-kinded types
- [ ] Row kinds
- [ ] Effect kinds

**Files**: `lib/kind.ml`

### 1.3 Type Checker Core
- [ ] Bidirectional type checking
- [ ] Type synthesis
- [ ] Type checking mode
- [ ] Subsumption rule

**Files**: `lib/typecheck.ml`, `lib/check.ml`, `lib/synth.ml`

### 1.4 Unification Engine
- [ ] Type unification
- [ ] Occurs check
- [ ] Union-find structure
- [ ] Error messages for unification failures

**Files**: `lib/unify.ml`, `lib/union_find.ml`

### 1.5 Polymorphism
- [ ] Let-generalization
- [ ] Instantiation
- [ ] Value restriction
- [ ] Type application

### 1.6 Row Unification
- [ ] Record row unification
- [ ] Variant row unification
- [ ] Lacks constraints
- [ ] Row rewriting

**Files**: `lib/row_unify.ml`

### 1.7 Basic Error Messages
- [ ] Type mismatch errors
- [ ] Undefined variable errors
- [ ] Source location tracking
- [ ] Suggested fixes

**Milestone**: `affinescript check` works for simple programs

---

## Phase 2: Quantities & Effects

**Goal**: Full QTT and effect system

**Duration**: 8-12 weeks

### 2.1 Quantity Checking
- [ ] Quantity context tracking
- [ ] Context scaling
- [ ] Context addition
- [ ] Usage analysis
- [ ] Quantity error messages

**Files**: `lib/quantity.ml`

### 2.2 Effect Inference
- [ ] Effect signature checking
- [ ] Effect row inference
- [ ] Handler typing
- [ ] Effect unification
- [ ] Effect polymorphism

**Files**: `lib/effect.ml`, `lib/effect_infer.ml`

### 2.3 Handler Verification
- [ ] Handler completeness
- [ ] Return clause typing
- [ ] Operation clause typing
- [ ] Continuation typing (linear vs multi-shot)

### 2.4 Effect Error Messages
- [ ] Unhandled effect errors
- [ ] Effect mismatch errors
- [ ] Handler clause errors

**Milestone**: Effects and quantities fully checked

---

## Phase 3: Ownership & Borrowing

**Goal**: Memory safety verification

**Duration**: 6-10 weeks

### 3.1 Ownership Tracking
- [ ] Move semantics
- [ ] Ownership transfer
- [ ] Drop insertion
- [ ] Copy trait handling

**Files**: `lib/ownership.ml`

### 3.2 Borrow Checker
- [ ] Borrow tracking
- [ ] Conflict detection
- [ ] Non-lexical lifetimes
- [ ] Dataflow analysis

**Files**: `lib/borrow.ml`, `lib/dataflow.ml`

### 3.3 Lifetime Inference
- [ ] Lifetime constraints
- [ ] Lifetime solving
- [ ] Lifetime elision
- [ ] Lifetime bounds

**Files**: `lib/lifetime.ml`

### 3.4 Ownership-Quantity Integration
- [ ] Quantity affects ownership
- [ ] Linear ownership (1)
- [ ] Shared ownership (ω + Copy)
- [ ] Erased types (0)

**Milestone**: `affinescript check` catches all ownership errors

---

## Phase 4: Dependent Types & Refinements

**Goal**: Dependent types with SMT verification

**Duration**: 6-10 weeks

### 4.1 Dependent Type Checking
- [ ] Π-type checking
- [ ] Σ-type checking
- [ ] Type-level computation
- [ ] Normalization

**Files**: `lib/dependent.ml`, `lib/normalize.ml`

### 4.2 SMT Integration
- [ ] Z3 OCaml bindings
- [ ] Predicate translation
- [ ] Validity checking
- [ ] Model extraction (for errors)

**Files**: `lib/smt.ml`, `lib/smt_translate.ml`

### 4.3 Refinement Checking
- [ ] Refinement subtyping
- [ ] VC generation
- [ ] SMT queries
- [ ] Refinement error messages

**Files**: `lib/refinement.ml`

### 4.4 Totality Checking (Optional)
- [ ] Termination analysis
- [ ] Structural recursion
- [ ] Custom measures
- [ ] Total annotation

**Files**: `lib/totality.ml`

**Milestone**: Full type system complete

---

## Phase 5: Code Generation

**Goal**: Compile to WebAssembly

**Duration**: 10-14 weeks

### 5.1 Intermediate Representation
- [ ] ANF transformation
- [ ] Effect evidence insertion
- [ ] Closure conversion
- [ ] Lambda lifting

**Files**: `lib/ir.ml`, `lib/anf.ml`, `lib/closure.ml`

### 5.2 Optimization
- [ ] Dead code elimination
- [ ] Inlining
- [ ] Constant folding
- [ ] Linearity-aware optimizations

**Files**: `lib/optimize.ml`

### 5.3 WASM Code Generation
- [ ] WASM module structure
- [ ] Function compilation
- [ ] Type mapping
- [ ] Memory layout

**Files**: `lib/wasm.ml`, `lib/codegen.ml`

### 5.4 Effect Compilation
- [ ] Evidence-passing transform
- [ ] Handler compilation
- [ ] Continuation representation
- [ ] One-shot optimization

**Files**: `lib/effect_compile.ml`

### 5.5 Ownership Erasure
- [ ] Drop insertion points
- [ ] Move compilation
- [ ] Borrow elimination

**Milestone**: `affinescript compile` produces working WASM

---

## Phase 6: Runtime

**Goal**: Minimal Rust runtime for WASM

**Duration**: 6-8 weeks

### 6.1 Runtime Core (Rust)
- [ ] Project structure
- [ ] Memory allocator
- [ ] Panic handling
- [ ] Stack management

**Location**: `runtime/` (new Rust crate)

### 6.2 Effect Runtime
- [ ] Evidence structures
- [ ] Handler frames
- [ ] Continuation allocation
- [ ] Resume implementation

### 6.3 GC (Optional)
- [ ] Mark-sweep for ω cycles
- [ ] Root tracking
- [ ] Finalization

### 6.4 Host Bindings
- [ ] WASI integration
- [ ] JavaScript interop
- [ ] Console I/O
- [ ] File system

**Milestone**: Programs run correctly

---

## Phase 7: Standard Library

**Goal**: Usable standard library

**Duration**: Ongoing (8+ weeks initial)

### 7.1 Core Types
- [ ] Prelude
- [ ] Option, Result
- [ ] Tuples
- [ ] Unit, Bool, Never

**Location**: `stdlib/core/`

### 7.2 Collections
- [ ] List
- [ ] Vec (growable array)
- [ ] HashMap
- [ ] HashSet
- [ ] BTreeMap

**Location**: `stdlib/collections/`

### 7.3 Text
- [ ] String
- [ ] Char
- [ ] Formatting (Display, Debug)
- [ ] Parsing

**Location**: `stdlib/text/`

### 7.4 Effects
- [ ] IO effect
- [ ] State effect
- [ ] Exn effect
- [ ] Async effect
- [ ] Reader effect

**Location**: `stdlib/effects/`

### 7.5 Numeric
- [ ] Int, Float
- [ ] Numeric traits
- [ ] Math functions

**Location**: `stdlib/num/`

**Milestone**: Self-sufficient programs possible

---

## Phase 8: Tooling

**Goal**: Developer experience

**Duration**: Ongoing (10+ weeks initial)

### 8.1 Language Server (Rust)
- [ ] LSP implementation
- [ ] Diagnostics
- [ ] Hover information
- [ ] Go to definition
- [ ] Find references
- [ ] Completion
- [ ] Rename

**Location**: `tools/affinescript-lsp/`

### 8.2 Formatter (OCaml)
- [ ] Canonical formatting
- [ ] Configuration options
- [ ] Editor integration

**Location**: `lib/format.ml`, `bin/fmt.ml`

### 8.3 REPL (OCaml)
- [ ] Expression evaluation
- [ ] Type printing
- [ ] Effect handling
- [ ] History

**Location**: `bin/repl.ml`

### 8.4 Package Manager (Rust)
- [ ] Manifest parsing
- [ ] Dependency resolution
- [ ] Package fetching
- [ ] Lock file
- [ ] Publishing

**Location**: `tools/affine-pkg/`

### 8.5 Documentation Generator
- [ ] Doc comments
- [ ] API documentation
- [ ] Search index

**Location**: `tools/affine-doc/`

**Milestone**: Professional development experience

---

## Phase 9: Ecosystem

**Goal**: Community and adoption

**Duration**: Ongoing

### 9.1 VS Code Extension
- [ ] Syntax highlighting
- [ ] LSP client
- [ ] Snippets
- [ ] Debugging

**Location**: `editors/vscode/`

### 9.2 Playground
- [ ] Web REPL
- [ ] Shareable links
- [ ] Examples

**Location**: `playground/`

### 9.3 Package Registry
- [ ] Registry backend
- [ ] Web frontend
- [ ] CLI publishing

### 9.4 Documentation Site
- [ ] Tutorial
- [ ] Language reference
- [ ] API docs
- [ ] Blog

### 9.5 Example Projects
- [ ] Hello World
- [ ] Web server
- [ ] CLI tool
- [ ] Library

**Milestone**: Community can build real projects

---

## Version Milestones

| Version | Contents | Target |
|---------|----------|--------|
| 0.1.0 | Type checker (no effects/ownership) | Phase 1 |
| 0.2.0 | Full type system | Phase 2-4 |
| 0.3.0 | WASM compilation | Phase 5-6 |
| 0.4.0 | Standard library | Phase 7 |
| 0.5.0 | Tooling (LSP, formatter) | Phase 8 |
| 0.9.0 | Release candidate | Phase 9 |
| 1.0.0 | Stable release | All phases |

---

## Resource Requirements

### Core Team Skills Needed
- OCaml (compiler)
- Rust (runtime, tooling)
- Type theory (checker design)
- WASM (code generation)
- ReScript/Deno (web tooling)

### Infrastructure
- CI/CD (GitHub Actions)
- Package registry hosting
- Documentation hosting
- Playground hosting

---

## Success Criteria

### 0.1.0 (Type Checker MVP)
- [ ] 100+ test programs type check correctly
- [ ] Error messages are helpful
- [ ] <1s for typical file

### 1.0.0 (Stable Release)
- [ ] All language features work
- [ ] Performance competitive with Rust/Go
- [ ] 10+ community packages
- [ ] 3+ production users
- [ ] Complete documentation

---

*Last updated: 2024*
