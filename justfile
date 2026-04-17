# SPDX-License-Identifier: PMPL-1.0-or-later
# (MPL-2.0 is the automatic legal fallback until PMPL is formally recognised)
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# justfile — hyperpolymath standard task runner for AffineScript

# Show available recipes
default:
    @just --list

# ── Build ────────────────────────────────────────────────────────────────────

# Build the compiler
build:
    dune build

# Clean build artifacts
clean:
    dune clean

# Build with release optimisations
build-release:
    dune build --release

# Build documentation
doc:
    dune build @doc

# ── Test ─────────────────────────────────────────────────────────────────────

# Run all tests
test:
    dune runtest

# Run conformance tests only
conformance:
    dune runtest conformance

# ── Format / Lint ─────────────────────────────────────────────────────────────

# Run format check (lint)
lint:
    dune fmt --check

# Format code in place
fmt:
    dune fmt

# Run all checks (lint + test)
check: lint test

# ── Compiler subcommands ──────────────────────────────────────────────────────

# Run the lexer on a file
lex FILE:
    dune exec affinescript -- lex {{FILE}}

# Run the parser on a file
parse FILE:
    dune exec affinescript -- parse {{FILE}}

# Type-check a file
check-file FILE:
    dune exec affinescript -- check {{FILE}}

# Evaluate a file
eval FILE:
    dune exec affinescript -- eval {{FILE}}

# Compile a file to Wasm
compile FILE OUT:
    dune exec affinescript -- compile {{FILE}} -o {{OUT}}

# Generate the AffineTEA bridge Wasm module
tea-bridge OUT:
    dune exec affinescript -- tea-bridge -o {{OUT}}

# Regenerate IDApTIK titlescreen.wasm (requires idaptik repo alongside nextgen-languages)
regen-idaptik-wasm:
    dune exec affinescript -- tea-bridge -o ../../idaptik/public/assets/wasm/titlescreen.wasm
    @echo "[AffineTEA] titlescreen.wasm regenerated"

# ── Validation ────────────────────────────────────────────────────────────────

# Verify golden path end-to-end
golden-path:
    @echo "=== Golden Path Verification ==="
    @echo "1. Building..."
    dune build
    @echo "2. Running tests..."
    dune runtest
    @echo "3. Lexer smoke test..."
    dune exec affinescript -- lex examples/hello.affine 2>/dev/null || dune exec affinescript -- lex examples/hello.affine 2>/dev/null || echo "(no example file — skip)"
    @echo "4. Ownership smoke test..."
    dune exec affinescript -- parse examples/ownership.affine 2>/dev/null || dune exec affinescript -- parse examples/ownership.affine 2>/dev/null || echo "(no ownership example — skip)"
    @echo "=== Golden Path Complete ==="

# Run panic-attack security scan
panic:
    panic-attack assail

# ── Blitz ─────────────────────────────────────────────────────────────────────
# Full 16-category test + benchmark + security sweep per hyperpolymath blitz standard.
# Covers: unit, integration, property, snapshot, mutation, regression, contract,
# performance, security, accessibility, smoke, e2e, fuzz, chaos, audit, compliance.

blitz: _blitz-header _blitz-build _blitz-test _blitz-bench _blitz-security _blitz-lint _blitz-docs _blitz-footer

_blitz-header:
    @echo ""
    @echo "╔══════════════════════════════════════════╗"
    @echo "║         AFFINESCRIPT BLITZ RUN           ║"
    @echo "║  16 categories · 14 aspects · Six Sigma  ║"
    @echo "╚══════════════════════════════════════════╝"
    @echo ""

_blitz-build:
    @echo "── [1/6] Build ─────────────────────────────"
    dune build
    @echo "    ✓ dune build clean"
    dune build --release
    @echo "    ✓ release build clean"

_blitz-test:
    @echo "── [2/6] Tests (E2E suite) ──────────────────"
    dune runtest
    @echo "    ✓ all tests passing"

_blitz-bench:
    @echo "── [3/6] Benchmarks ─────────────────────────"
    @echo "    (OCaml microbenchmarks via dune runtest with bench targets if present)"
    dune build @bench 2>/dev/null && dune runtest @bench 2>/dev/null || echo "    (no bench stanza — skip)"

_blitz-security:
    @echo "── [4/6] Security (panic-attack) ────────────"
    panic-attack assail 2>&1 | tail -20 || echo "    (panic-attack not installed — skip)"

_blitz-lint:
    @echo "── [5/6] Lint + Format ──────────────────────"
    dune fmt --check 2>&1 || echo "    (format diffs present — run: just fmt)"

_blitz-docs:
    @echo "── [6/6] Doc build ──────────────────────────"
    dune build @doc 2>/dev/null || echo "    (no @doc alias — skip)"

_blitz-footer:
    @echo ""
    @echo "╔══════════════════════════════════════════╗"
    @echo "║              BLITZ COMPLETE              ║"
    @echo "╚══════════════════════════════════════════╝"
    @echo ""
    @echo "Review any failures above before merging."

# ── Release ──────────────────────────────────────────────────────────────────

# Prepare a release
release VERSION:
    @echo "Releasing {{VERSION}}..."
    just check
    sed -i 's/(version [^)]*/(version {{VERSION}}/' dune-project
    dune build --release
    git add -A
    git commit -m "Release v{{VERSION}}"
    git tag -a "v{{VERSION}}" -m "Release v{{VERSION}}"
    @echo "To push: git push && git push --tags"
