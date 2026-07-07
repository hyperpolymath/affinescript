# Contributing to AffineScript

Thank you for your interest in AffineScript — a practical language for resource-safe systems, compiling to typed WebAssembly. This guide covers how to set up a working tree, file useful bugs, and submit changes.

For the language itself, start from [`README.adoc`](README.adoc). For project state, blockers, and next-actions, see [`.machine_readable/descriptiles/STATE.a2ml`](.machine_readable/descriptiles/STATE.a2ml).

---

## Quick Start

```bash
git clone https://github.com/hyperpolymath/affinescript.git
cd affinescript

# Provision the OCaml toolchain (one-shot). See README.adoc "Getting Started"
# for the full list of opam packages and the `eval $(opam env)` note for
# non-interactive shells.
opam install -y \
  sedlex menhir ppx_deriving ppx_sexp_conv sexplib0 fmt cmdliner yojson \
  alcotest ocamlformat \
  js_of_ocaml js_of_ocaml-ppx js_of_ocaml-compiler
eval "$(opam env --switch=default --set-switch)"

# Verify setup
dune build
dune runtest
```

Tested on OCaml 4.14.2 (the constraint in `dune-project` is `>= 4.14`).

### Repository Structure

```
affinescript/
├── lib/                 # Compiler core (lexer, parser, typechecker, codegen, verifier)
├── bin/                 # CLI driver — `_build/default/bin/main.exe`
├── stdlib/              # Standard library `.affine` modules
├── test/                # Alcotest suites (lexer, golden, e2e fixtures)
├── tests/               # Topic-grouped tests (borrow, codegen, effects, parser, …)
├── examples/            # Self-contained example programs
├── conformance/         # Conformance test corpus
├── docs/                # Specs, decisions, guides
├── packages/            # Aggregate JS/TS/ReScript binding packages
├── editors/             # Editor integrations
├── js/                  # `js_of_ocaml` playground (built into `playground.bc.js`)
├── .machine_readable/   # Machine-readable metadata (`.a2ml`) — see 0-AI-MANIFEST.a2ml
├── .github/             # CI workflows, issue templates
├── CODE_OF_CONDUCT.md
├── CONTRIBUTING.md      # This file
├── LICENSE / LICENSES   # MIT OR AGPL-3.0-or-later
├── MAINTAINERS.adoc
├── README.adoc
├── SECURITY.md
├── dune-project
└── justfile
```

---

## How to Contribute

### Reporting Bugs

**Before reporting:**
1. Search [existing issues](https://github.com/hyperpolymath/affinescript/issues).
2. Check that the bug reproduces against `main` (`git pull && dune build`).

**When reporting:** use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md) and include:

- Clear, descriptive title.
- Environment: OCaml version, opam switch, OS.
- Steps to reproduce, ideally as a minimal `.affine` file plus the exact `dune exec affinescript -- <subcommand> <file>` invocation.
- Expected vs actual behaviour (compiler output, generated Wasm, runtime trap, etc.).

### Suggesting Features

**Before suggesting:**
1. Skim [`docs/ROADMAP.adoc`](docs/ROADMAP.adoc) and `.machine_readable/descriptiles/STATE.a2ml`.
2. Search existing issues and discussions.

**When suggesting:** use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md) and include:

- Problem statement — what pain point does this solve?
- Proposed solution and any alternatives considered.
- Whether the change touches the core language, a face (frontend surface), a backend, or the stdlib.

### Your First Contribution

Look for issues labelled:

- [`good first issue`](https://github.com/hyperpolymath/affinescript/labels/good%20first%20issue)
- [`help wanted`](https://github.com/hyperpolymath/affinescript/labels/help%20wanted)
- [`documentation`](https://github.com/hyperpolymath/affinescript/labels/documentation)

---

## Development Workflow

### Branch Naming

```
docs/short-description       # Documentation
test/what-added              # Test additions
feat/short-description       # New features
fix/issue-number-description # Bug fixes
refactor/what-changed        # Code improvements
security/what-fixed          # Security fixes
ci/what-changed              # CI / tooling
```

Branch from `main` and target `main` in your PR.

### Commit Messages

We follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <description>

[optional body]

[optional footer, e.g. closes #N or Co-Authored-By: ...]
```

Common types: `feat`, `fix`, `docs`, `test`, `refactor`, `chore`, `ci`. Common scopes: `lexer`, `parser`, `typecheck`, `codegen`, `verify`, `stdlib`, `cli`, `readme`.

### Required Checks

Before opening a PR, locally:

```bash
dune build       # must exit 0
dune runtest     # must be green
dune fmt         # optional — auto-formats with ocamlformat
```

The `methodology.a2ml` file lists the canonical gate set. CI will rerun `build` + `runtest` plus the security, lint, and policy workflows in `.github/workflows/`.

### Pull Requests

1. Push your branch and open a PR against `main`.
2. Use a Conventional-Commit-shaped title.
3. In the body, summarise the change and link the issue it closes.
4. Keep PRs focused — split unrelated changes into separate PRs.
5. CI must be green. Maintainers squash-merge by default; commit message lineage is preserved in the PR body.

---

## Code of Conduct

This project follows the [Code of Conduct](CODE_OF_CONDUCT.md). By participating you agree to abide by it.

## License

By contributing you agree your contribution is licensed under the project's dual licence (MIT OR AGPL-3.0-or-later), as recorded in [`LICENSE`](LICENSE) and per-file SPDX headers.
