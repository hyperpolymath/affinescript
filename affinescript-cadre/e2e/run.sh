#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# End-to-end runtime test for the CadreRouter JS wrapper.

set -uo pipefail
cd "$(dirname "$0")"
REPO="$(cd ../.. && pwd)"
BIN="${AFFINESCRIPT_BIN:-$REPO/_build/default/bin/main.exe}"

if [ ! -x "$BIN" ]; then
  echo "SKIP: compiler not built ($BIN missing) — run dune build"
  exit 0
fi

command -v node >/dev/null 2>&1 || { echo "SKIP: node not on PATH"; exit 0; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Generate the Wasm router module
"$BIN" router-bridge -o "$TMP/router.wasm"

# Run the Node test
node test.mjs "$TMP/router.wasm"
