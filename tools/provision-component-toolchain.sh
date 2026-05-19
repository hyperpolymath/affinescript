#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# INT-03 S2 / issue #251 (ADR-019 unblocker for ADR-015 S3+): provision
# the WASM Component-Model toolchain the WASI-preview2 migration needs.
#
# Reproducible by construction: exact pinned versions + `cargo install
# --locked` (uses each crate's committed Cargo.lock, so the built
# binary is deterministic for the pinned version). Re-running is
# idempotent (cargo skips an already-installed matching version).
#
# Canonical provisioning is this script for now; a guix.scm / flake.nix
# devShell entry is a tracked follow-up (the repo has neither yet —
# introducing one is out of #251's bounded scope). CI and contributors
# run this to obtain a byte-stable toolchain.
#
# Pinned 2026-05-19 (resolved + verified on rustc 1.95.0):
set -euo pipefail

WASM_TOOLS_VERSION="1.249.0"
WASM_COMPONENT_LD_VERSION="0.5.22"
WAC_CLI_VERSION="0.10.0"

echo "Provisioning component-model toolchain (pinned, --locked)…"
cargo install --locked --version "${WASM_TOOLS_VERSION}"        wasm-tools
cargo install --locked --version "${WASM_COMPONENT_LD_VERSION}" wasm-component-ld
cargo install --locked --version "${WAC_CLI_VERSION}"           wac-cli

echo
echo "Installed:"
wasm-tools --version
wasm-component-ld --version
wac --version
echo "OK — ADR-015 S3 (componentize on-ramp) is now unblocked."
