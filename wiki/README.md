<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2024-2026 hyperpolymath -->

# AffineScript Wiki

A guide to learning and using AffineScript — a language with affine types,
algebraic effects, and a typed-WebAssembly compilation target.

> **⚠️ Authoritative status lives in the repo, not this wiki.**
> Where this wiki's prose implies broader maturity than the in-repo matrices
> state, *the matrices win* — see
> [`docs/CAPABILITY-MATRIX.adoc`](../docs/CAPABILITY-MATRIX.adoc) for live
> per-feature readiness and [`docs/TECH-DEBT.adoc`](../docs/TECH-DEBT.adoc)
> for the coordination ledger. AffineScript is **alpha** today: CORE-01
> (#177) closed 2026-05-30, and the base-language soundness holes tracked
> through 2026-06 are now fixed, fenced, or removed — #554 (use-after-move
> via a callee-returned borrow) is rejected, and effect handlers (#555) now
> **fail loud** on every compiled backend rather than dropping arms. `handle`/
> `resume` examples in these pages still execute only under `--interp`
> (single-shot tail-resume); the compiled backends reject them loudly. The
> single source of truth for soundness-hole status is the test-anchored
> [`docs/SOUNDNESS.adoc`](../docs/SOUNDNESS.adoc). Dated narrative:
> [`docs/STATE-2026-06-11.adoc`](../docs/STATE-2026-06-11.adoc) ·
> v1 release-readiness ledger: #563.

## Quick Navigation

### Getting Started

- [Introduction](tutorials/introduction.md) — What is AffineScript?
- [Quick Start](tutorials/quickstart.md) — Get productive with the CLI

> 📌 For setup instructions, the canonical source is the root
> [`README.adoc`](../README.adoc) §"Getting Started" and the
> [`CONTRIBUTING.md`](../CONTRIBUTING.md) §"Quick Start".

### Language Reference

These pages cover the language surface as designed. **For each feature,
cross-reference the [Capability Matrix](../docs/CAPABILITY-MATRIX.adoc) to
see whether it is `enforced`, `works`, `partial`, `declared-but-unwired`,
or `parse-only` today.**

- [Lexical Structure](language-reference/lexical.md) — Tokens, literals, comments
- [Types](language-reference/types.md) — Type system overview
- [Patterns](language-reference/patterns.md) — Pattern matching
- [Functions](language-reference/functions.md) — Function definitions
- [Ownership](language-reference/ownership.md) — Affine types and borrowing
- [Effects](language-reference/effects.md) — Algebraic effect system
- [Traits](language-reference/traits.md) — Type classes *(currently `partial`)*
- [Modules](language-reference/modules.md) — Module system
- [Dependent Types](language-reference/dependent-types.md) — Indexed and refined types *(REMOVED in v1 — #558; page is historical)*
- [Row Polymorphism](language-reference/rows.md) — Extensible records

### Compiler

- [Architecture Overview](compiler/architecture.md) — Compiler pipeline
- [Lexer](compiler/lexer.md) — Lexical analysis
- [Parser](compiler/parser.md) — Syntactic analysis
- [Type Checker](compiler/type-checker.md) — Type inference and checking
- [Borrow Checker](compiler/borrow-checker.md) — Ownership verification
- [Code Generation](compiler/codegen.md) — WASM backend

### Tooling

- [CLI Reference](tooling/cli.md) — Command-line interface
- [REPL Guide](tooling/repl.md) — Interactive environment

### Standard Library

- [Overview](stdlib/overview.md) — Library organisation

> The standard library is **AOT-coherent** (per
> [`docs/CAPABILITY-MATRIX.adoc`](../docs/CAPABILITY-MATRIX.adoc) STDLIB-AOT).
> `stdlib/json.affine` and `stdlib/dict.affine` are live; the
> Http/Promise/IO surface is documented under STDLIB-01..05 in
> [`docs/TECH-DEBT.adoc`](../docs/TECH-DEBT.adoc).

### Testing

- [Testing Guide](testing/guide.md) — Writing tests
- [Property-Based Testing](testing/property-based.md) — QuickCheck-style testing

> Canonical testing standards: [`docs/standards/TESTING.adoc`](../docs/standards/TESTING.adoc).

## Quick Links

| Resource | Description |
|----------|-------------|
| [`README.adoc`](../README.adoc) | Project overview + Getting Started |
| [`docs/CAPABILITY-MATRIX.adoc`](../docs/CAPABILITY-MATRIX.adoc) | **Authoritative** per-feature readiness |
| [`docs/TECH-DEBT.adoc`](../docs/TECH-DEBT.adoc) | Coordination ledger (CORE / STDLIB / INT / DOC) |
| [`docs/ECOSYSTEM.adoc`](../docs/ECOSYSTEM.adoc) | Spine + AS↔typed-wasm contract |
| [`docs/ROADMAP.adoc`](../docs/ROADMAP.adoc) | Development roadmap |
| [`docs/specs/SPEC.adoc`](../docs/specs/SPEC.adoc) | Core language specification |
| [`examples/`](../examples/) | Example programs |
| [`CONTRIBUTING.md`](../CONTRIBUTING.md) | How to contribute |

## Language Features at a Glance

### Ownership & Borrowing *(enforced)*

```affine
fn transfer(file: own File) -> own File {
  // file is owned, must be returned or consumed
  file
}

fn read_only(file: ref File) -> String {
  // file is borrowed immutably
  file.read_all()
}

fn modify(file: mut File) {
  // file is borrowed mutably
  file.write("data")
}
```

QTT (Quantitative Type Theory) semiring enforced on every `check`, `compile`,
and `eval` invocation — see [`lib/quantity.ml`](../lib/quantity.ml). Linear
arrows enforced. Borrow-graph validation + NLL last-use landed
(CORE-01 parts 1–3 Slice A, see CAPABILITY-MATRIX).

### Algebraic Effects *(partial — interpreter complete; WasmGC dispatch via CPS in flight)*

```affine
effect Ask[A] {
  fn ask() -> A
}

fn double_ask[A]() -{Ask[A]}-> (A, A) {
  (ask(), ask())
}

fn main() -{IO}-> Unit {
  let result = handle double_ask() {
    ask() -> resume(42)
  };
  print(result)  // (42, 42)
}
```

Pure/impure separation + effect polymorphism enforced in the typechecker.
Handlers are interpreter-complete; WasmGC dispatch is the CPS-transform
line (#225, closed end-to-end; #234 generalised the boundary recogniser).

### Row Polymorphism *(partial)*

```affine
// Works on any record with 'name' field
fn greet[r](person: {name: String, ..r}) -> String {
  "Hello, " ++ person.name
}

// Can call with any matching record
greet({name: "Alice", age: 30})
greet({name: "Bob", role: "Admin", active: true})
```

Records + effect rows in typecheck/unify. Not fully exercised end-to-end yet.

### Dependent Types *(REMOVED in v1 — historical; see #558 / [`docs/SOUNDNESS.adoc`](../docs/SOUNDNESS.adoc))*

```affine
// Length-indexed vectors — SYNTAX PARSES, but predicates do not
// reduce and there is no SMT/decision procedure. These types are
// not enforced at runtime today.
fn head[n: Nat, T](vec: Vec[n + 1, T]) -> T {
  vec[0]
}
```

This surface parses but has *no semantics behind it yet* — `TRefined`
constructors exist in the AST but the typechecker does not reduce
predicates or check refinements. See `docs/CAPABILITY-MATRIX.adoc`
row "Dependent / refinement types".

## Community

- **GitHub**: [github.com/hyperpolymath/affinescript](https://github.com/hyperpolymath/affinescript)
- **Issues**: [Report bugs and request features](https://github.com/hyperpolymath/affinescript/issues)
- **Discussions**: [Ask questions and share ideas](https://github.com/hyperpolymath/affinescript/discussions)

---

*AffineScript — affine types + algebraic effects + typed-WebAssembly.
Status (live): [`docs/CAPABILITY-MATRIX.adoc`](../docs/CAPABILITY-MATRIX.adoc).*
