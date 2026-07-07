#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# End-to-end proof that a Python-face-authored program runs: compile
# fixtures/python_face_runnable.pyaff *through the Python face* to core-WASM
# and assert main() === 5170 (fac(5)=120 + sum_to(100)=5050). Exercises the
# README's "faces are the product" claim as running code, not just a parse.
#
# Requires the compiler built (dune build → _build/default/bin/main.exe;
# override with AFFINESCRIPT_BIN) and node on PATH. Skips loudly (exit 0)
# if either is absent.
set -uo pipefail
cd "$(dirname "$0")"
REPO="$(cd ../.. && pwd)"
BIN="${AFFINESCRIPT_BIN:-$REPO/_build/default/bin/main.exe}"
SRC="fixtures/python_face_runnable.pyaff"
EXPECT=5170

[ -x "$BIN" ] || { echo "SKIP: compiler not built ($BIN) — run dune build"; exit 0; }
command -v node >/dev/null 2>&1 || { echo "SKIP: node not on PATH"; exit 0; }

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
WASM="$TMP/pf.wasm"

# The `# face: python` pragma on line 1 selects the face; no --face needed.
"$BIN" compile "$SRC" -o "$WASM" || { echo "FAIL: compile through Python face"; exit 1; }

node - "$WASM" "$EXPECT" <<'JS'
import { readFile } from "node:fs/promises";
const [wasm, expect] = [process.argv[2], Number(process.argv[3])];
const { instance } = await WebAssembly.instantiate(
  await readFile(wasm), { wasi_snapshot_preview1: { fd_write: () => 0 } });
const got = instance.exports.main();
console.log(`main() = ${got} (expected ${expect})`);
if (got !== expect) { console.error("FAIL: wrong result"); process.exit(1); }
console.log("PASS: Python-face program ran end-to-end (face → wasm → correct result)");
JS
