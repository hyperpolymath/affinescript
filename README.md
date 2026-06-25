<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

> [!IMPORTANT]
> This is **AffineScript**, a successor-to-JS/TS/ReScript application
> language at `hyperpolymath/affinescript`. It is **not**
> [Ephapax](https://github.com/hyperpolymath/ephapax), a separate
> research language for WebAssembly memory safety at
> `hyperpolymath/ephapax`.
>
> The two share **exactly one thing**: both target
> [typed-wasm](https://github.com/hyperpolymath/typed-wasm) (and
> interface with that repo’s `crates/typed-wasm-verify/`). They have
> separate ASTs, separate type checkers, separate compilers, separate
> proof stories. The word `affine` overlaps in both names because it
> names a substructural-logic property — that’s a logic-family fact, not
> a project relationship.
>
> Also: Ephapax is internally dyadic, containing both `ephapax-linear`
> and `ephapax-affine` sublanguages. **The `ephapax-affine` sublanguage
> is NOT AffineScript.** When ambiguous, write "ephapax-affine
> sublanguage (of ephapax)" vs "AffineScript language (this repo)".
>
> Canonical disambiguation:
> [nextgen-languages/docs/disambiguation/ephapax-vs-affinescript.md](https://github.com/hyperpolymath/nextgen-languages/blob/main/docs/disambiguation/ephapax-vs-affinescript.md).

> [!IMPORTANT]
> **Authoritative status lives in
> [docs/CAPABILITY-MATRIX.adoc](docs/CAPABILITY-MATRIX.adoc) (feature
> readiness) and [docs/SOUNDNESS.adoc](docs/SOUNDNESS.adoc)
> (soundness-hole status — test-anchored, the single source of truth for
> what is fixed / fenced / removed / residual).** Where this README’s
> prose implies broader maturity than those state, they win.
> AffineScript is **alpha**: CORE-01 (#177) closed 2026-05-30, and the
> base-language soundness holes tracked through 2026-06 are now fixed,
> fenced, or removed — use-after-move via a callee-returned borrow
> (#554) is rejected, effect handlers (#555) fail loud on every compiled
> backend rather than silently dropping arms, the async sync-fallback
> (#556) fails loud, and refinement predicates (#558) were removed
> rather than left unenforced. One pinned interpreter residual remains;
> see [docs/SOUNDNESS.adoc](docs/SOUNDNESS.adoc) for the per-issue
> ledger. Closing these **implementation** holes is not the same as
> **proving** soundness — the metatheory is still prose
> ([docs/PROOF-NEEDS.adoc](docs/PROOF-NEEDS.adoc)). The v1
> release-readiness ledger is issue \#563. typed-wasm is a **separate**
> language-agnostic target (`hyperpolymath/typed-wasm`), not an
> AffineScript subsystem — see
> [docs/ECOSYSTEM.adoc](docs/ECOSYSTEM.adoc).

License: MPL-2.0\
Built alongside Gossamer, typed-wasm, and Burble

Write software where the compiler helps enforce resource lifecycles,
protocol states, and effect boundaries before bugs become runtime
failures.

> [!NOTE]
> Honest status sync (2026-06-21): affine/QTT and borrow checking are
> wired into the standard CLI paths today (`check`, `compile`, `eval`)
> and gate user programs. Refinement/dependent-type surface was
> **removed** in v1 rather than left parse-only-and-unenforced (#558);
> effect-handler lowering on the compiled backends now **fails loud**
> rather than silently dropping arms (#555). See
> [docs/CAPABILITY-MATRIX.adoc](docs/CAPABILITY-MATRIX.adoc) for
> authoritative per-feature status and
> [docs/SOUNDNESS.adoc](docs/SOUNDNESS.adoc) for soundness-hole status
> (`.machine_readable/6a2/STATE.a2ml` mirrors the matrix; it does not
> lead).

# Start here — by audience

| You are a… | Start here |
|----|----|
| **New user / learning the language** | [Tutorial (lessons 1–10)](docs/tutorial/lesson-01-hello.adoc) · [runnable warm-ups](docs/guides/warmup/) · [language reference](wiki/language-reference/) |
| **Developer / compiler contributor** | [Repository map](docs/NAVIGATION.adoc) · [compiler architecture](wiki/compiler/architecture.md) · [ADRs](docs/decisions/) · [capability matrix](docs/CAPABILITY-MATRIX.adoc) |
| **Maintainer / releaser** | [Maintainers](docs/governance/MAINTAINERS.adoc) · [contributing](docs/governance/CONTRIBUTING.adoc) · [ops playbook](.machine_readable/6a2/PLAYBOOK.a2ml) · [soundness ledger](docs/SOUNDNESS.adoc) |

# What AffineScript Is

AffineScript is a practical programming language for building software
where the hard problems are not just “does it parse?” or “does it
type-check?”, but:

- is this resource used correctly?

- are these state transitions valid?

- are effects explicit?

- is this extension boundary safe?

- can this code be portable across runtimes and targets?

It combines:

- affine and quantitative typing for ownership and usage tracking

- algebraic effects for explicit side effects and handlers

- row polymorphism for extensible records and interfaces

- refinement-oriented type structure for stronger invariants

- WebAssembly-oriented deployment paths for portable execution

The goal is not to turn ordinary programming into theorem proving. The
goal is to make it easier to write programs whose *resource lifecycles*,
*protocol states*, and *effect boundaries* are correct by construction.

AffineScript is not a “game language.” Games have been one useful
proving ground for it, but the language is intended more broadly for:

- plugin systems

- desktop/runtime shells

- protocol-driven software

- real-time systems

- data and document pipelines

- simulation and game logic

- extensible applications and toolchains

# The Core Idea

Many bugs in real software are really *type-system omissions in
disguise*.

Examples:

- using a resource after it should have been consumed

- forgetting to release a resource

- calling an operation in the wrong protocol state

- hiding I/O or mutation inside supposedly pure logic

- making “extensible” interfaces that are really unchecked convention

- evolving data structures in ways that quietly break consumers

AffineScript moves these constraints into the language.

It is designed for code where:

- resources matter

- state matters

- sequencing matters

- extensibility matters

- portability matters

# The Important Distinction: Core Language vs Faces

AffineScript is the semantic core.

On top of that core, the language supports *faces*: sugared surface
syntaxes that let people approach the same semantics through different
aesthetic and ergonomic styles.

A face is not a separate language. It is a different presentation layer
over the same core model.

The established faces are:

- **AffineScript** — the canonical face (this repo)

- [**JaffaScript**](https://github.com/hyperpolymath/jaffascript) — a
  JavaScript / TypeScript-like face

- [**RattleScript**](https://github.com/hyperpolymath/rattlescript) — a
  Python-like face (also positioned as "Python for the web" via
  typed-wasm)

- [**PseudoScript**](https://github.com/hyperpolymath/pseudoscript) — a
  pseudocode-oriented face for CS pedagogy

- [**LucidScript**](https://github.com/hyperpolymath/lucidscript) — a
  PureScript / Haskell-like face

- [**CafeScripto**](https://github.com/hyperpolymath/cafescripto) — a
  CoffeeScript-like face

The five non-canonical face repos are **brand surfaces only** —
examples, docs, and a thin shim that defaults `--face`. The compiler,
type checker, borrow checker, and codegen all live here in
`affinescript`.

Every face shares the canonical `.affine` file extension; the active
face is selected by an optional `face:` pragma on the first comment line
of the file (e.g. `#` `face:` `rattlescript`, , `--` `face:`
`lucidscript`), or by `--face` `NAME` on the CLI.

This means people can bring familiarity from a language family they
already love, while still entering a system with stronger guarantees
around ownership, effects, state, and resource usage.

In other words:

> Different faces, same cube.

The syntax may feel familiar in different ways, but the semantic
guarantees come from the same checked core.

# Why Faces Matter

Most programming languages force a false choice:

- familiar syntax with weak guarantees, or

- strong guarantees with alien ergonomics

AffineScript’s face system is meant to break that tradeoff.

With faces, you can let users write in a style that feels:

- JavaScript-like

- Python-like

- pseudocode-like

- domain-specific

- pedagogy-friendly

while still preserving:

- type checking

- ownership/usage constraints

- effect tracking

- state and protocol correctness

- portable code generation targets

This is especially useful for:

- onboarding

- teaching

- domain experts entering safer systems programming

- plugin authors

- teams migrating from dynamic or weakly typed languages

# What Makes AffineScript Distinctive

AffineScript brings together capabilities that are rarely available in
one practical system:

| Capability | What it is for |
|----|----|
| Affine / QTT-style usage tracking | Ownership, resource protocols, “use once / don’t duplicate / don’t forget” |
| Algebraic effects | Explicit side effects, handler-based composition, safer impurity boundaries |
| Row polymorphism | Extensible records and interfaces without collapsing into ad hoc objects |
| Refinement-oriented typing | Stronger invariants over state and structure |
| Portable code generation | Wasm-oriented deployment and integration paths |
| Multiple faces | Different surface syntaxes over the same checked semantic core |

The practical result is a language aimed at software where correctness
depends on more than ordinary static typing.

# What AffineScript Helps Express and Verify

AffineScript is especially suited to programs that need guarantees
around:

- resource acquisition and disposal

- protocol state transitions

- plugin and extension boundaries

- explicit capability/effect boundaries

- extensible structured data

- portable execution targets

- interpreters, runtimes, and shells

- host/guest integration layers

This includes code such as:

- network/session flows

- document or file pipelines

- desktop shell logic

- stateful embedded runtimes

- simulation/game systems

- tool plugins

- language tooling and automation

# Example: Resource-Safe Ownership

```affine
type Texture = own { id: Int, width: Int, height: Int }

fn load_texture(path: ref String) -{IO + Exn[LoadError]}-> own Texture {
  Texture #{ id: 42, width: 1024, height: 1024 }
}

fn render(scene: ref Scene, texture: ref Texture) -{Render}-> () {
  ()
}

fn unload(@linear texture: own Texture) -{IO}-> () {
  ()
}

fn frame() -{IO + Render + Exn[LoadError]}-> () {
  let texture = load_texture("player.png");
  render(scene, texture);
  unload(texture);
  // texture cannot be used here
}
```

What the checker is trying to protect you from:

- using a consumed resource

- forgetting to release a resource

- duplicating exclusive ownership

- hiding lifecycle mistakes behind convention

# Example: State-Checked Protocol Transitions

```affine
type Connection[..state] = own {
  socket: own Socket,
  ..state
}

fn authenticate(
  @linear conn: own Connection[{status: Unauthenticated}]
) -{Session + IO}-> Connection[{status: Authenticated, user: String}] {
  let creds = recv();
  Connection #{ socket: conn.socket, status: Authenticated, user: creds.user }
}

fn query(
  conn: ref Connection[{status: Authenticated, ..r}],
  sql: ref String
) -{IO}-> Result[Rows, DbError] {
  // ...
}
```

What the checker can rule out:

- querying before authentication

- authenticating twice through the same consumed handle

- using a connection after close

- silently invalid protocol transitions

# Example: Effect-Explicit Code

```affine
effect IO {
  fn print(s: String) -> ();
  fn read_line() -> String;
}

effect State[S] {
  fn get() -> S;
  fn put(s: S) -> ();
}

fn interactive_counter() -{IO + State[Int]}-> Int {
  let input = read_line();
  let current = get();
  let next = current + 1;
  put(next);
  print("Count: " ++ int_to_string(next));
  next
}

fn add(a: Int, b: Int) -> Int {
  a + b
}
```

The point is not “effects are fancy.” The point is that effect
boundaries become visible and checkable.

# Example: Extensible Data with Row Polymorphism

```affine
fn greet[..r](person: {name: String, ..r}) -> String {
  "Hello, " ++ person.name
}

let alice = #{name: "Alice", age: 30, role: "Engineer"};
let bob = #{name: "Bob", department: "Sales"};

greet(alice);
greet(bob);
```

This matters for extensible systems, plugins, records, evolving schemas,
and interface stability.

# Faces in More Detail

A face is a syntax layer, not a semantic fork.

That means:

- the same ownership rules still apply

- the same effect rules still apply

- the same state/protocol guarantees still apply

- the same backend targets still apply

The purpose of faces is not fragmentation. It is *approachability*.

A likely way to think about the stack is:

    face syntax  ->  AffineScript core AST  ->  type/effect/ownership checks  ->  backend target

Examples:

- JaffaScript lowers JavaScript / TypeScript-like syntax into the
  AffineScript core

- RattleScript lowers Python-like syntax into the same core

- PseudoScript lowers structured pedagogical pseudocode into the same
  core

- LucidScript lowers PureScript / Haskell-like syntax into the same core

- CafeScripto lowers CoffeeScript-like syntax into the same core

Side-by-side examples for each face live under `examples/faces/`; you
can preview the canonical lowering of any file with `affinescript`
`preview-python` / `preview-js` / `preview-pseudocode` / `preview-lucid`
/ `preview-cafe`.

So the question is not “which face is the real language?” The answer is:
the core semantics are the language; faces are entrances.

# Backends and Targets

AffineScript is intended to support portable deployment, especially
through WebAssembly-oriented targets.

Depending on feature maturity and backend coverage, this includes:

- Wasm-oriented deployment paths

- integration with typed-wasm conventions

- native/runtime-specific backends where implemented

- host integration via Gossamer and related infrastructure

The important point is this:

> AffineScript is not “JavaScript with types” and not “Python with
> ownership syntax.” It is a checked core language that can be surfaced
> through multiple familiar faces and lowered into portable execution
> targets.

# Relationship to typed-wasm

AffineScript and typed-wasm are related, but they are not the same
thing.

- **AffineScript** is a source language and semantic system.

- **typed-wasm** is about typed structure and safety around
  Wasm-oriented memory/layout/interface conventions.

- **Gossamer** is a resource-safe host/runtime/shell layer.

- **Burble** is a high-assurance real-time systems application of the
  same broader philosophy.

A helpful mental model is:

    face syntax -> AffineScript core -> typed-wasm / Wasm conventions -> host/runtime integration

typed-wasm matters because portable low-level targets need more than “it
compiles.” They need conventions, structure, and safe composition across
boundaries.

# Games Are a Proving Ground, Not the Definition

Games have been a useful proving ground because they combine:

- stateful logic

- resources with lifecycles

- extensible entity data

- eventful and asynchronous behaviour

- portability requirements

- host/runtime integration concerns

That makes them an excellent stress test.

But AffineScript is not limited to games, and should not be presented as
if that were its sole or primary identity.

Games are one place where the language’s properties are easy to
demonstrate. They are not the language’s entire reason for existing.

# Getting Started

## Prerequisites

- OCaml 4.14.2+

- Dune 3.14+

- opam packages: `sedlex`, `menhir`, `ppx_deriving`, `ppx_sexp_conv`,
  `sexplib0`, `fmt`, `cmdliner`, `yojson`, `alcotest` (test),
  `ocamlformat` (test), `js_of_ocaml`, `js_of_ocaml-ppx`,
  `js_of_ocaml-compiler`

> [!NOTE]
> The top-level `dune` `build` compiles every target including
> `js/playground.bc.js`, so `js_of_ocaml*` are required for an
> out-of-the-box build. If you only need the CLI and tests, you can
> build the subtrees explicitly:
>
> ``` bash
> dune build lib/ bin/ test/
> ```

## One-shot opam setup

```bash
# Install the full dep set into the active switch
opam install -y \
  sedlex menhir ppx_deriving ppx_sexp_conv sexplib0 fmt cmdliner yojson \
  alcotest ocamlformat \
  js_of_ocaml js_of_ocaml-ppx js_of_ocaml-compiler

# Make `dune`, `ocaml`, etc. visible to non-interactive shells
eval "$(opam env --switch=default --set-switch)"
```

For login shells, persist the PATH wiring by appending the equivalent to
`~/.profile` — opam’s own `opam-init/init.sh` gates PATH-setup behind
`[` `-t` `0` `]`, so `wsl` `bash` `-lc` `’…’`-style invocations don’t
pick it up without an explicit eval.

## Build

```bash
dune build
```

## Type check a file

```bash
_build/default/bin/main.exe check test/e2e/fixtures/affine_basic.affine
```

## Evaluate with interpreter

```bash
_build/default/bin/main.exe eval test/e2e/fixtures/interp_simple.affine
```

## Compile

```bash
_build/default/bin/main.exe compile -o hello.wasm test/e2e/fixtures/wasm_simple.affine
```

## Format

```bash
_build/default/bin/main.exe fmt test/e2e/fixtures/affine_basic.affine
```

## Lint

```bash
_build/default/bin/main.exe lint test/e2e/fixtures/affine_basic.affine
```

## JSON diagnostics

```bash
_build/default/bin/main.exe check --json test/e2e/fixtures/affine_basic.affine
```

> [!NOTE]
> The standard source extension for AffineScript is `.affine`.

# Status

AffineScript is in active development.

This repository contains a live compiler and tooling stack, but some
advanced features remain incomplete or partial. Current state is best
understood from the machine-readable status files and current docs
rather than from aspirational claims.

| Component | Status | Notes |
|----|----|----|
| Lexer + Parser | Complete | Menhir grammar, sedlex tokenizer, broad syntax coverage |
| Name Resolution | Complete | Module loading, scoping, imports |
| Type Checker | Wired | Active in standard CLI paths |
| Quantity Checking | Live gate | QTT quantity checks fail builds on violations |
| Borrow Checker | Live gate | Compile-time borrow checks active; advanced phases ongoing |
| Effect System | Interpreter-complete | Handlers/interpreter path active; backend parity incomplete |
| Trait System | Partial | Core registry and lookup work exists; advanced coherence remains |
| Interpreter | High | Closures, matching, effects, and recent exception plumbing |
| WASM Codegen | Advanced but incomplete | Primary deployment backend; feature gaps remain |
| WASM GC Codegen | Partial | Available, not full feature parity |
| LSP Server | Complete | Hover, goto-def, completion, diagnostics |
| Formatter + Linter | Complete | AST-based formatter and lint rules |

# Design Principles

- **Correctness should be structural, not ceremonial.**

- **Types should clarify what code may do, not merely classify data.**

- **Effects should be explicit.**

- **Resources should not depend on discipline alone.**

- **Extensibility should not collapse into unchecked runtime
  convention.**

- **Strong guarantees should not require alien syntax.**

- **Different faces should be able to share one checked core.**

# Repository Structure

    affinescript/
    +-- lib/                      # Core compiler (OCaml)
    |   +-- ast.ml
    |   +-- lexer.ml
    |   +-- parser.mly
    |   +-- typecheck.ml
    |   +-- unify.ml
    |   +-- quantity.ml
    |   +-- types.ml
    |   +-- codegen.ml
    |   +-- interp.ml
    |   +-- json_output.ml
    +-- bin/main.ml               # CLI
    +-- tools/affinescript-lsp/   # Language server
    +-- editors/                  # Editor integrations
    +-- stdlib/                   # Standard library modules
    +-- examples/                 # Example programs
    +-- test/                     # Golden tests and fixtures
    +-- docs/                     # Specifications, guides, papers
    +-- .machine_readable/        # Authoritative machine-readable status

# Ecosystem

AffineScript sits inside a broader ecosystem built around
correctness-by-construction.

## Gossamer

A resource-safe host/runtime shell for desktop and embedded application
structures.

## typed-wasm

A typed Wasm-oriented layer for memory/layout/interface discipline and
safer low-level convergence.

## Burble

A high-assurance real-time communications system using similar design
principles around correctness, performance, and explicit structure.

These are related projects, but each has a distinct role.

# What AffineScript Is Not

AffineScript is not:

- merely “Rust with different syntax”

- merely “TypeScript but safer”

- merely “a game scripting language”

- merely “a proof assistant disguised as a language”

- merely “a Wasm frontend”

It is better understood as:

> A practical language core for resource-aware, stateful,
> effect-explicit, extensible software — with multiple faces and
> portable targets.

# Documentation

- `docs/` — language and architecture documentation

- `docs/CAPABILITY-MATRIX.adoc` — authoritative per-feature status
  (overrides everything else)

- `docs/TECH-DEBT.adoc` — coordination ledger (DOC/CORE/STDLIB/INT/SAT)

- `docs/ECOSYSTEM.adoc` — spine, AS↔typed-wasm contract, satellite
  registry, INT-01..12

- `.machine_readable/6a2/STATE.a2ml` — machine-readable mirror of
  CAPABILITY-MATRIX (does not lead)

- `examples/` — example programs

- `tools/affinescript-lsp/` — language tooling

- `editors/` — editor integrations

# License

This repository’s core technology — the compiler, runtime, standard
library, tooling, and editor integrations — is licensed under MPL-2.0;
documentation and prose under CC-BY-SA-4.0.

Game content remains under AGPL-3.0-or-later (the `proposals/idaptik/`
migration and the game-specific `examples/`), and the foundational
technologies (Gossamer, Burble) under the Palimpsest-MPL layer. See
[docs/governance/LICENSING-GUIDE.md](docs/governance/LICENSING-GUIDE.md)
for the full three-tier breakdown.

See also:

- MPL-2.0 (core technology): <https://www.mozilla.org/en-US/MPL/2.0/>

- AGPL-3.0-or-later (game content):
  <https://www.gnu.org/licenses/agpl-3.0.html>

- PMPL-1.0 (foundational):
  <https://github.com/hyperpolymath/palimpsest-license>

# Final Positioning Summary

AffineScript should be presented as:

> A practical checked core language for resource-safe, stateful,
> extensible software, with multiple familiar faces and portable
> Wasm-oriented targets.

Not as:

> A secret weapon for games.
