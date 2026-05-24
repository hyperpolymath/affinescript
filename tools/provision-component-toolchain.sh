#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
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

# The official `wasi_snapshot_preview1` -> preview2 adapters. Two
# flavours, both fetch-pinned (provenance + sha256 recorded here,
# verified fail-closed) rather than vendored as binary blobs:
#
#   * REACTOR (ADR-015 S3) — for AffineScript modules used as libraries.
#     The core module exports plain functions (`main`, game-loop hooks,
#     ...) and the reactor adapter wraps them without requiring a
#     command entrypoint.
#   * COMMAND (ADR-015 S6) — for AffineScript programs invoked under a
#     WASI 0.2 host (`wasmtime run`, jco). Requires the core module to
#     export `_start : () -> ()`; the S6 codegen change in
#     `lib/codegen.ml` emits a `_start` shim that calls `main` and
#     drops its i32 result. The resulting component exports
#     `wasi:cli/run@0.2.x` per `wit/affinescript.wit`.
#
# Source: bytecodealliance/wasmtime release v44.0.1 (matches the
# pinned wasmtime in this estate).
ADAPTER_WASMTIME_TAG="v44.0.1"
ADAPTER_DIR="$(cd "$(dirname "$0")" && pwd)/vendor"

ADAPTER_REACTOR_URL="https://github.com/bytecodealliance/wasmtime/releases/download/${ADAPTER_WASMTIME_TAG}/wasi_snapshot_preview1.reactor.wasm"
ADAPTER_REACTOR_SHA256="e352bf5b74aec62d8e7da7e0536ede4b3d1ccdb4d9a1767031f4d6f007d22e85"
ADAPTER_REACTOR_PATH="${ADAPTER_DIR}/wasi_snapshot_preview1.reactor.wasm"

ADAPTER_COMMAND_URL="https://github.com/bytecodealliance/wasmtime/releases/download/${ADAPTER_WASMTIME_TAG}/wasi_snapshot_preview1.command.wasm"
ADAPTER_COMMAND_SHA256="8ff2ea78d31179f12d6fb1d2cadc0ab83356cd049f362d5a8d94a4070c7a15bd"
ADAPTER_COMMAND_PATH="${ADAPTER_DIR}/wasi_snapshot_preview1.command.wasm"

# Back-compat: the S3-era variable names referenced the reactor by
# default. Keep them as aliases so any external script still pinning to
# them continues to resolve.
ADAPTER_URL="${ADAPTER_REACTOR_URL}"
ADAPTER_SHA256="${ADAPTER_REACTOR_SHA256}"
ADAPTER_PATH="${ADAPTER_REACTOR_PATH}"

echo "Provisioning component-model toolchain (pinned, --locked)…"
cargo install --locked --version "${WASM_TOOLS_VERSION}"        wasm-tools
cargo install --locked --version "${WASM_COMPONENT_LD_VERSION}" wasm-component-ld
cargo install --locked --version "${WAC_CLI_VERSION}"           wac-cli

fetch_adapter() {
  local kind="$1" url="$2" sha="$3" path="$4"
  echo "Fetch-pinning the preview1->preview2 ${kind} adapter…"
  if [ -f "${path}" ] && \
     [ "$(sha256sum "${path}" | cut -d' ' -f1)" = "${sha}" ]; then
    echo "  ${kind}: already present and verified"
    return 0
  fi
  curl -fsSL -o "${path}.tmp" "${url}"
  local got
  got="$(sha256sum "${path}.tmp" | cut -d' ' -f1)"
  if [ "${got}" != "${sha}" ]; then
    rm -f "${path}.tmp"
    echo "FATAL: ${kind} adapter checksum mismatch (expected ${sha}, got ${got})" >&2
    exit 1
  fi
  mv "${path}.tmp" "${path}"
  echo "  ${kind}: verified + pinned -> ${path}"
}

mkdir -p "${ADAPTER_DIR}"
fetch_adapter reactor "${ADAPTER_REACTOR_URL}" "${ADAPTER_REACTOR_SHA256}" "${ADAPTER_REACTOR_PATH}"
fetch_adapter command "${ADAPTER_COMMAND_URL}" "${ADAPTER_COMMAND_SHA256}" "${ADAPTER_COMMAND_PATH}"

echo
echo "Installed:"
wasm-tools --version
wasm-component-ld --version
wac --version
echo "reactor adapter: ${ADAPTER_WASMTIME_TAG} (${ADAPTER_REACTOR_SHA256})"
echo "command adapter: ${ADAPTER_WASMTIME_TAG} (${ADAPTER_COMMAND_SHA256})"
echo "OK — ADR-015 S3 (componentize on-ramp) + S6 (WIT export lifting) unblocked."
