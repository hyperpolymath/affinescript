# SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2024-2025 hyperpolymath
# Justfile - hyperpolymath standard task runner for AffineScript

# Show available recipes
default:
    @just --list

# Build the compiler
build:
    dune build

# Run all tests
test:
    dune runtest

# Run format check (lint)
lint:
    dune fmt --check

# Clean build artifacts
clean:
    dune clean

# Format code in place
fmt:
    dune fmt

# Run all checks (lint + test)
check: lint test

# Build documentation
doc:
    dune build @doc

# Run the lexer on a file
lex FILE:
    dune exec affinescript -- lex {{FILE}}

# Run the parser on a file
parse FILE:
    dune exec affinescript -- parse {{FILE}}

# Run conformance tests
conformance:
    dune runtest conformance

# Verify golden path (smoke test)
golden-path:
    @echo "=== Golden Path Verification ==="
    @echo "1. Building..."
    dune build
    @echo "2. Running tests..."
    dune runtest
    @echo "3. Testing lexer on hello.as..."
    dune exec affinescript -- lex examples/hello.as
    @echo "4. Testing parser on ownership.as..."
    dune exec affinescript -- parse examples/ownership.as
    @echo "=== Golden Path Complete ==="

# Prepare a release
release VERSION:
    @echo "Releasing {{VERSION}}..."
    @echo "=== Pre-release Checks ==="
    just check
    @echo "=== Updating Version ==="
    # Update version in dune-project
    sed -i 's/(version [^)]*/(version {{VERSION}}/' dune-project
    @echo "=== Building Release ==="
    dune build --release
    @echo "=== Creating Git Tag ==="
    git add -A
    git commit -m "Release v{{VERSION}}"
    git tag -a "v{{VERSION}}" -m "Release v{{VERSION}}"
    @echo "=== Release Complete ==="
    @echo "To push: git push && git push --tags"
