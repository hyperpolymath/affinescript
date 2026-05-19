#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# ADR-015 S3 — the componentize on-ramp. POST-codegen, codegen
# UNCHANGED: the compiler still emits a core `wasi_snapshot_preview1`
# module (the legacy default, ADR-015 reversible-in-progress); this
# wraps that core module into a WASI-0.2 (preview2) WASM *component*
# via the official preview1->preview2 reactor adapter (reactor, not
# command: AffineScript modules export functions, not a WASI `_start`).
#
# The `affinescript.ownership` custom section (the typed-wasm contract
# carrier, multi-producer ABI) MUST survive the wrap — asserted here.
#
# Usage:  tools/componentize.sh <core.wasm> <out.component.wasm>
# Adapter: tools/vendor/wasi_snapshot_preview1.reactor.wasm
#   (fetch-pinned + checksum-verified by provision-component-toolchain.sh;
#    override with AFFINESCRIPT_WASI_ADAPTER=/path).
set -euo pipefail

core="${1:?usage: componentize.sh <core.wasm> <out.component.wasm>}"
out="${2:?usage: componentize.sh <core.wasm> <out.component.wasm>}"
adapter="${AFFINESCRIPT_WASI_ADAPTER:-$(cd "$(dirname "$0")" && pwd)/vendor/wasi_snapshot_preview1.reactor.wasm}"

[ -f "$core" ]    || { echo "componentize: no core module: $core" >&2; exit 2; }
[ -f "$adapter" ] || { echo "componentize: adapter missing ($adapter) — run tools/provision-component-toolchain.sh" >&2; exit 2; }
command -v wasm-tools >/dev/null || { echo "componentize: wasm-tools not on PATH — run tools/provision-component-toolchain.sh" >&2; exit 2; }

# Count with `grep -c` (reads ALL input, so no early-close SIGPIPE that
# `set -o pipefail` would mis-report as a failure — the `grep -q`
# footgun). `|| true` neutralises grep's no-match exit 1.
section_count() {
  wasm-tools print "$1" 2>/dev/null | grep -c 'affinescript.ownership' || true
}

# Does the core carry the typed-wasm ownership section? (only assert
# survival if it was there to begin with.)
had_ownership=0
[ "$(section_count "$core")" -gt 0 ] && had_ownership=1

wasm-tools component new "$core" --adapt "wasi_snapshot_preview1=$adapter" -o "$out"
wasm-tools validate --features component-model "$out"

if [ "$had_ownership" = 1 ]; then
  if [ "$(section_count "$out")" -eq 0 ]; then
    echo "FATAL: affinescript.ownership section did NOT survive componentization" >&2
    rm -f "$out"
    exit 1
  fi
  echo "ownership section: preserved through componentization ✓"
fi

echo "componentized -> $out (valid WASI-0.2 component)"
