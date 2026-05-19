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

# The official `wasi_snapshot_preview1` -> preview2 *reactor* adapter
# (ADR-015 S3). AffineScript modules export functions (`main`, …), not
# a WASI command `_start`, so the REACTOR adapter is the correct one
# (the command adapter requires `_start`). Fetch-pinned (provenance +
# sha256 recorded here; verified fail-closed) rather than vendored as a
# binary blob in the source tree. Source: bytecodealliance/wasmtime
# release v44.0.1 (matches the pinned wasmtime in this estate).
ADAPTER_WASMTIME_TAG="v44.0.1"
ADAPTER_URL="https://github.com/bytecodealliance/wasmtime/releases/download/${ADAPTER_WASMTIME_TAG}/wasi_snapshot_preview1.reactor.wasm"
ADAPTER_SHA256="e352bf5b74aec62d8e7da7e0536ede4b3d1ccdb4d9a1767031f4d6f007d22e85"
ADAPTER_DIR="$(cd "$(dirname "$0")" && pwd)/vendor"
ADAPTER_PATH="${ADAPTER_DIR}/wasi_snapshot_preview1.reactor.wasm"

echo "Provisioning component-model toolchain (pinned, --locked)…"
cargo install --locked --version "${WASM_TOOLS_VERSION}"        wasm-tools
cargo install --locked --version "${WASM_COMPONENT_LD_VERSION}" wasm-component-ld
cargo install --locked --version "${WAC_CLI_VERSION}"           wac-cli

echo "Fetch-pinning the preview1->preview2 reactor adapter…"
mkdir -p "${ADAPTER_DIR}"
if [ -f "${ADAPTER_PATH}" ] && \
   [ "$(sha256sum "${ADAPTER_PATH}" | cut -d' ' -f1)" = "${ADAPTER_SHA256}" ]; then
  echo "adapter already present and verified"
else
  curl -fsSL -o "${ADAPTER_PATH}.tmp" "${ADAPTER_URL}"
  got="$(sha256sum "${ADAPTER_PATH}.tmp" | cut -d' ' -f1)"
  if [ "${got}" != "${ADAPTER_SHA256}" ]; then
    rm -f "${ADAPTER_PATH}.tmp"
    echo "FATAL: adapter checksum mismatch (expected ${ADAPTER_SHA256}, got ${got})" >&2
    exit 1
  fi
  mv "${ADAPTER_PATH}.tmp" "${ADAPTER_PATH}"
  echo "adapter verified + pinned -> ${ADAPTER_PATH}"
fi

echo
echo "Installed:"
wasm-tools --version
wasm-component-ld --version
wac --version
echo "adapter: ${ADAPTER_WASMTIME_TAG} (${ADAPTER_SHA256})"
echo "OK — ADR-015 S3 (componentize on-ramp) is now unblocked."
