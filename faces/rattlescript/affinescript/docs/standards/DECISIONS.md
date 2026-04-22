# AffineScript Design Decisions

This document records the key design decisions for AffineScript's implementation.

## 1. Runtime Model

**Decision**: Minimal runtime, rely on host for most services

- Runtime is small and focused on AffineScript-specific needs
- No garbage collector for most data (ownership handles memory)
- Small tracing GC only for cyclic ω-quantity data (opt-in)
- Host provides: I/O, networking, filesystem, timers

**Rationale**: Keeps WASM binaries small, maximizes portability, leverages host capabilities.

## 2. Effect Compilation Strategy

**Decision**: Evidence-passing (Koka-style)

- Effects compiled via evidence-passing transformation
- Each effect operation receives an "evidence" parameter
- Handlers install evidence at runtime
- One-shot continuations optimized to avoid allocation

**Rationale**: Better performance than CPS, proven in Koka, good balance of complexity/speed.

**Implementation**:
```
// Source
handle computation() with {
    return x → x,
    get(_, k) → resume(k, state)
}

// Compiled (evidence-passing)
computation({
    get: (ev, k) => k(state)
})
```

## 3. WASM Target

**Decision**: WASM core + WASI, with Component Model readiness

| Feature | Decision |
|---------|----------|
| WASM Core | ✅ Required baseline |
| WASM GC | ❌ Not required (ownership handles memory) |
| WASI | ✅ For CLI/server use cases |
| Component Model | ✅ Design for future compatibility |
| Threads | ⚠️ Optional, for parallel effects |

**Rationale**: Broad compatibility now, future-proofed for Component Model.

## 4. SMT Solver

**Decision**: Z3 as optional external dependency

- Z3 bindings (ocaml-z3) for refinement type checking
- SMT is optional: refinements work without it (runtime checks)
- Can be disabled for faster compilation
- Future: support CVC5 as alternative

**Configuration**:
```nickel
# affinescript.ncl
{
  smt = {
    enabled = true,
    solver = "z3",
    timeout_ms = 5000,
  }
}
```

## 5. Package Manager

**Decision**: Workspace-aware, Cargo-inspired

- Single `affine.toml` manifest per package
- Workspace support for monorepos
- Lock file for reproducibility
- Content-addressed storage (like pnpm)
- Written in Rust

**Manifest format**:
```toml
[package]
name = "my-project"
version = "0.1.0"
edition = "2024"

[dependencies]
std = "1.0"
http = { version = "0.5", features = ["async"] }

[dev-dependencies]
test = "1.0"
```

## 6. Standard Library Philosophy

**Decision**: Small core + blessed packages

**Core (always available)**:
- `Prelude`: Basic types, traits, operators
- `Option`, `Result`: Error handling
- `List`, `Vec`, `Array`: Collections
- `String`, `Char`: Text

**Blessed Effects (in std)**:
- `IO`: Console, file system
- `Exn`: Exceptions
- `State`: Mutable state
- `Async`: Async/await
- `Reader`: Environment access

**Community Packages**:
- HTTP, JSON, databases, etc.
- Not in std, but curated/recommended

## 7. Self-Hosting

**Decision**: Long-term goal, not immediate priority

- Phase 1-4: OCaml compiler
- Phase 5+: Gradually rewrite in AffineScript
- Start with: lexer, parser (simpler)
- End with: type checker, codegen (complex)

**Timeline**: After 1.0 stable release

## 8. Interop Priority

**Decision**: JavaScript first, Rust second

| Target | Priority | Method |
|--------|----------|--------|
| JavaScript | 🥇 Primary | wasm-bindgen, host bindings |
| Rust | 🥈 Secondary | Native FFI for tools |
| C | 🥉 Tertiary | Via Rust FFI |

**Rationale**: WASM's primary deployment is web/JS; tooling benefits from Rust.

## 9. Error Messages

**Decision**: Rust-style elaborate diagnostics

- Multi-line errors with source context
- Color-coded by severity
- Suggestions for fixes
- Error codes with documentation links
- Machine-readable JSON output option

**Example**:
```
error[E0312]: cannot borrow `x` as mutable because it is already borrowed
  --> src/main.afs:12:5
   |
10 |     let r = &x;
   |             -- immutable borrow occurs here
11 |
12 |     mutate(&mut x);
   |            ^^^^^^ mutable borrow occurs here
13 |
14 |     use(r);
   |         - immutable borrow later used here
   |
   = help: consider moving the mutable borrow before the immutable borrow
```

## 10. Proof Assistant Mode

**Decision**: Refinement types with SMT only (no interactive proving)

- Refinements checked via SMT solver
- No tactic language or proof terms
- Proofs are erased (quantity 0)
- Future: optional Lean/Coq extraction for critical code

**Rationale**: Practical verification without complexity of full theorem prover.

## 11. Primary Use Cases

**Decision**: Priority order

1. **Web applications** (frontend + backend)
2. **CLI tools**
3. **Libraries/packages**
4. **Embedded/WASM plugins**
5. **Scientific computing** (future)

## 12. Community Model

**Decision**: Benevolent dictator initially, open governance post-1.0

- Pre-1.0: Core team makes decisions quickly
- Post-1.0: RFC process for major changes
- Open source from day one (Apache-2.0 OR MIT)
- Community contributions welcome

## 13. Backward Compatibility

**Decision**: Breaking changes during 0.x, strict semver from 1.0

- 0.x releases may break compatibility
- Migration guides for breaking changes
- 1.0+ follows strict semver
- Edition system for language-level changes (like Rust)

---

## Technology Stack Summary

| Layer | Technology | Notes |
|-------|------------|-------|
| Compiler | OCaml 5.1+ | Existing codebase |
| Parser | Menhir | Existing |
| Lexer | Sedlex | Existing |
| SMT | Z3 (ocaml-z3) | Optional |
| Runtime | Rust | WASM target |
| Allocator | Custom (Rust) | Ownership-optimized |
| Package Manager | Rust | CLI tool |
| LSP Server | Rust | Performance |
| Formatter | OCaml | Shares AST |
| REPL | OCaml | Interpreter mode |
| Web Tooling | ReScript + Deno | Per standards |
| Build Meta | Deno | Per standards |
| Config | Nickel | Per standards |
| Docs | Custom generator | From types |

---

## File Format Decisions

| Purpose | Format | Extension |
|---------|--------|-----------|
| Source code | AffineScript | `.afs` |
| Package manifest | TOML | `affine.toml` |
| Lock file | TOML | `affine.lock` |
| Configuration | Nickel | `*.ncl` |
| Build scripts | Deno/TS | `*.ts` |
| Documentation | Markdown | `*.md` |

---

## Versioning

- **Language**: `2024` edition (year-based)
- **Compiler**: Semver (0.1.0, 0.2.0, ... 1.0.0)
- **Stdlib**: Tied to compiler version
- **Packages**: Independent semver

---

*Last updated: 2024*
*Status: Approved*
