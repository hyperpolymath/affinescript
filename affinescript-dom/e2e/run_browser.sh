#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Manual browser host parity test (INT-11).
# Compiles the reconciler and serves browser_test.html locally.

set -uo pipefail
cd "$(dirname "$0")"
REPO="$(cd ../.. && pwd)"
BIN="${AFFINESCRIPT_BIN:-$REPO/_build/default/bin/main.exe}"

if [ ! -x "$BIN" ]; then
  echo "ERROR: compiler not built ($BIN missing) — run dune build first."
  exit 1
fi

echo "Compiling reconciler to WASM..."
cat ../src/dom.affine driver_main.affine > dom_drive.affine
"$BIN" compile dom_drive.affine -o dom_drive.wasm
rm dom_drive.affine

echo ""
echo "=========================================================="
echo "WASM compiled successfully. Starting local HTTP server..."
echo "Please open your browser to: http://localhost:8080/browser_test.html"
echo "Check the page to verify ALL ASSERTIONS PASS."
echo "Press Ctrl+C to stop the server and clean up."
echo "=========================================================="
echo ""

# Cleanup wasm on exit
trap 'rm -f dom_drive.wasm' EXIT

python3 -m http.server 8080
