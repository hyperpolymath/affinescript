#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Gate: tests/**/*.mjs must not mix up the two WebAssembly.instantiate
# overloads. Rationale, the exact failure mode it prevents, and the fix
# recipe live in tools/check-wasm-harness-idioms.mjs.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
exec node "$ROOT_DIR/tools/check-wasm-harness-idioms.mjs" "$ROOT_DIR"
