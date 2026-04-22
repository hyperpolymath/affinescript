# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

# RattleScript distribution justfile.
# Most work lives in the parent affinescript justfile.
# This file handles the thin Rust wrapper binary only.

default:
    just --list

# Build the rattle binary (dev mode, uses sibling _build/)
build:
    cargo build

# Build optimised release binary
release:
    cargo build --release

# Run the example
run-hello:
    cargo run -- eval examples/hello.rattle

# Check example typechecks
check-hello:
    cargo run -- check examples/hello.rattle

# Show the Python-face transform for the hello example
preview-hello:
    cargo run -- preview-python-transform examples/hello.rattle

# Run all examples
check-examples:
    cargo run -- check examples/hello.rattle
    cargo run -- check examples/ownership.rattle

# Install rattle binary to ~/.cargo/bin
install:
    cargo install --path .

# Bump AffineScript submodule pointer (called by CI after affinescript releases)
update-affinescript:
    #!/usr/bin/env bash
    set -euo pipefail
    git -C ../.. submodule update --remote
    git add ../..
    git commit -m "chore(affinescript): advance pointer to latest release"
