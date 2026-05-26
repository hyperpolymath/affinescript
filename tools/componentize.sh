#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# ADR-015 S3 + S6 — the componentize on-ramp. POST-codegen, the OCaml
# compiler still emits a core `wasi_snapshot_preview1` module (the
# legacy default; ADR-015 reversible-in-progress through S5). This
# wrapper turns that core module into a WASI-0.2 (preview2) WASM
# *component* via the official preview1->preview2 adapter family
# (fetch-pinned + checksum-verified by provision-component-toolchain.sh):
#
#   reactor (default, ADR-015 S3) — for AffineScript modules used as
#   libraries (host calls plain exports like `main`). The resulting
#   component instantiates but has no `wasi:cli/run`, so `wasmtime run`
#   cannot invoke it; the host loads it through its own WASI bindings.
#
#   command (--command, ADR-015 S6) — for AffineScript programs
#   invoked as WASI commands (`wasmtime run`, jco). Requires the core
#   module to export `_start : () -> ()`; the S6 codegen change in
#   `lib/codegen.ml` emits this shim automatically whenever the unit
#   exports a parameter-less `main`. The resulting component exports
#   `wasi:cli/run@0.2.x` per `wit/affinescript.wit`.
#
# The `typedwasm.ownership` custom section (the typed-wasm contract
# carrier, multi-producer ABI) MUST survive the wrap — asserted here
# in both modes.
#
# Usage:
#   tools/componentize.sh [--command|--reactor] <core.wasm> <out.component.wasm>
#
# Adapters: tools/vendor/wasi_snapshot_preview1.{reactor,command}.wasm
#   (fetch-pinned by provision-component-toolchain.sh; override with
#    AFFINESCRIPT_WASI_ADAPTER=/path to force a specific adapter file.)
set -euo pipefail

mode="reactor"
if [ "${1:-}" = "--command" ] || [ "${1:-}" = "--reactor" ]; then
  mode="${1#--}"
  shift
fi

core="${1:?usage: componentize.sh [--command|--reactor] <core.wasm> <out.component.wasm>}"
out="${2:?usage: componentize.sh [--command|--reactor] <core.wasm> <out.component.wasm>}"

adapter_dir="$(cd "$(dirname "$0")" && pwd)/vendor"
default_adapter="${adapter_dir}/wasi_snapshot_preview1.${mode}.wasm"
adapter="${AFFINESCRIPT_WASI_ADAPTER:-${default_adapter}}"

[ -f "$core" ]    || { echo "componentize: no core module: $core" >&2; exit 2; }
[ -f "$adapter" ] || { echo "componentize: ${mode} adapter missing ($adapter) — run tools/provision-component-toolchain.sh" >&2; exit 2; }
command -v wasm-tools >/dev/null || { echo "componentize: wasm-tools not on PATH — run tools/provision-component-toolchain.sh" >&2; exit 2; }

# Count with `grep -c` (reads ALL input, so no early-close SIGPIPE that
# `set -o pipefail` would mis-report as a failure — the `grep -q`
# footgun). `|| true` neutralises grep's no-match exit 1.
section_count() {
  wasm-tools print "$1" 2>/dev/null | grep -c 'typedwasm.ownership' || true
}

# Does the core carry the typed-wasm ownership section? (only assert
# survival if it was there to begin with.)
had_ownership=0
[ "$(section_count "$core")" -gt 0 ] && had_ownership=1

# Command mode requires `_start` in the core (the adapter lifts it into
# `wasi:cli/run`). Fail fast with a pointer to the codegen contract
# rather than letting wasm-tools complain a few lines deeper.
if [ "$mode" = "command" ]; then
  if ! wasm-tools print "$core" 2>/dev/null | grep -q '(export "_start"'; then
    echo "componentize --command: core module is missing the \`_start\` export." >&2
    echo "  AffineScript emits \`_start\` automatically when a parameter-less" >&2
    echo "  \`fn main()\` is present (ADR-015 S6 codegen, lib/codegen.ml)." >&2
    exit 2
  fi
fi

wasm-tools component new "$core" --adapt "wasi_snapshot_preview1=$adapter" -o "$out"
wasm-tools validate --features component-model "$out"

if [ "$had_ownership" = 1 ]; then
  if [ "$(section_count "$out")" -eq 0 ]; then
    echo "FATAL: typedwasm.ownership section did NOT survive componentization" >&2
    rm -f "$out"
    exit 1
  fi
  echo "ownership section: preserved through componentization ✓"
fi

# Command-mode contract: assert the result actually exports
# `wasi:cli/run` (catches a regression where the adapter wraps without
# lifting, or the wrong adapter file is pointed at).
if [ "$mode" = "command" ]; then
  if ! wasm-tools component wit "$out" 2>/dev/null | grep -q 'export wasi:cli/run'; then
    echo "FATAL: --command output does not export wasi:cli/run" >&2
    rm -f "$out"
    exit 1
  fi
  echo "wasi:cli/run: lifted into component world ✓"
fi

echo "componentized (${mode}) -> $out (valid WASI-0.2 component)"
