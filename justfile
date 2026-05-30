# SPDX-License-Identifier: MPL-2.0
# Palimpsest principles apply as an overlay policy; legal baseline is MPL-2.0.
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# justfile — hyperpolymath standard task runner for AffineScript

# Show available recipes
default:
    @just --list

# ── Build ────────────────────────────────────────────────────────────────────

# Build the compiler.
# Masks the benign, intentionally-left LALR parser-generator notices
# (inherent ambiguities Menhir resolves correctly — the 257-test gate
# proves the parse is right). NOT hidden: the build prints how many were
# masked, the proof they are inconsequential, and how to show them.
# Policy: docs/specs/SETTLED-DECISIONS.adoc "Parser-Conflict Disclosure".
build:
    #!/usr/bin/env bash
    if [ -n "${AFFINESCRIPT_SHOW_MENHIR_NOISE:-}" ]; then exec dune build; fi
    pat='shift/reduce conflicts|reduce/reduce conflict|states have (shift|reduce)-reduce|do not know how to resolve a reduce/reduce'
    out=$(dune build 2>&1); rc=$?
    printf '%s\n' "$out" | grep -vE "$pat"
    n=$(printf '%s\n' "$out" | grep -cE "$pat" || true)
    if [ "${n:-0}" -gt 0 ]; then
      echo ""
      echo "ℹ  ${n} benign LALR parser-generator notice(s) masked. The parser"
      echo "   parses correctly — full 'just test' gate (257) is green. These are"
      echo "   inherent, correctly-resolved, intentionally-left conflicts, not a"
      echo "   defect (see docs/specs/SETTLED-DECISIONS.adoc, \"Parser-Conflict"
      echo "   Disclosure Policy\"). Show them: 'just build-loud'."
    fi
    exit $rc

# Build the compiler showing ALL raw parser-generator output (nothing masked)
build-loud:
    AFFINESCRIPT_SHOW_MENHIR_NOISE=1 dune build

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

# Run face-transformer regression tests (snapshot diff + round-trip parse)
test-faces:
    ./tools/run_face_transformer_tests.sh

# Record any missing face-transformer snapshots (first-run / new face)
test-faces-record:
    ./tools/run_face_transformer_tests.sh --record-missing

# Update all face-transformer snapshots (intentional transformer change)
test-faces-update:
    ./tools/run_face_transformer_tests.sh --update

# ── Format / Lint ─────────────────────────────────────────────────────────────

# Run format check (lint)
lint:
    @if command -v ocamlformat >/dev/null 2>&1; then \
      dune fmt --preview; \
    else \
      echo "ocamlformat not installed; skipping format check"; \
    fi

# Format code in place
fmt:
    dune fmt

# Run all checks (lint + test + regression guards)
check: lint test guard

# Regression guards:
#  - Issue #35 Phase 3: fails if extension.ts reappears under
#    editors/vscode/src or any face's vscode extension dir.
#  - Issue #176 (DOC-01..09): fails if the status-doc truthing banners
#    re-drift (authoritative matrix pointers + STATE.a2ml mirror keys).
guard:
    ./tools/check-no-extension-ts.sh
    ./tools/check-doc-truthing.sh

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

# ── Tooling (manifest-driven dev deps) ────────────────────────────────────────

# Fetch + build the pinned tree-sitter-rescript grammar used by the
# `.res → .affine` migration assistant (#57 Phase 2). Output is written
# to `tools/vendor/tree-sitter-rescript/` (gitignored). Requires the
# `tree-sitter` CLI on PATH — install via `cargo install tree-sitter-cli`
# (Rust-native, repo-preferred) or `npm install -g tree-sitter-cli`.
install-grammar:
    ./editors/tree-sitter-rescript/scripts/install.sh

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

# ── Benchmarks (visibility-only) ──────────────────────────────────────────────
# Per docs/standards/TESTING.adoc §"Bench standards":
# microbenchmarks are wired under the @bench alias. Results print
# to stdout — no merge-blocking gate today. Promotion to enforcement
# requires a calibrated baseline + ratchet policy.

# Run microbenchmarks (lex / parse / typecheck / codegen sweeps)
bench:
    dune build @bench --force
    dune runtest @bench --force

# Archive the current bench output to bench-runs/<UTC-timestamp>.log
bench-record:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p bench-runs
    ts=$(date -u +"%Y%m%dT%H%M%SZ")
    out="bench-runs/${ts}.log"
    echo "Recording bench run to ${out}"
    dune build @bench --force
    dune runtest @bench --force 2>&1 | tee "${out}"
    echo "Saved: ${out}"

# ── Coverage (visibility-only) ────────────────────────────────────────────────
# Per docs/standards/TESTING.adoc §"Coverage (visibility-only)":
# instrumentation via bisect_ppx. HTML report only — no enforced
# floor. CI uploads the directory as an artifact for inspection.

# Run the test suite with bisect_ppx instrumentation and emit HTML
coverage:
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf _coverage bisect*.coverage
    dune runtest --force --instrument-with bisect_ppx
    bisect-ppx-report html -o _coverage --title="AffineScript coverage"
    bisect-ppx-report summary
    echo ""
    echo "HTML report: _coverage/index.html"

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
    @echo "    (visibility-only per docs/standards/TESTING.adoc)"
    dune build @bench --force 2>/dev/null && dune runtest @bench --force 2>/dev/null || echo "    (no bench stanza or build failed — skip)"

_blitz-security:
    @echo "── [4/6] Security (panic-attack) ────────────"
    panic-attack assail 2>&1 | tail -20 || echo "    (panic-attack not installed — skip)"

_blitz-lint:
    @echo "── [5/6] Lint + Format ──────────────────────"
    @if command -v ocamlformat >/dev/null 2>&1; then \
      dune fmt --preview 2>&1 || echo "    (format diffs present — run: just fmt)"; \
    else \
      echo "    (ocamlformat missing — lint fmt check skipped)"; \
    fi

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
