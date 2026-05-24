#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# ADR-015 S6b gated smoke (sockets on-ramp): compile an AffineScript
# program that calls the `net_shutdown` builtin, run it through
# `tools/componentize.sh --command`, and assert
#   (a) the core module imports `wasi_snapshot_preview1.sock_shutdown`,
#   (b) S6a's `_start` shim is still emitted,
#   (c) the command adapter accepts the socket import without surfacing
#       a host-side `wasi:sockets/*` requirement (it bridges
#       internally for sock_shutdown),
#   (d) `wasmtime run` invokes the component (exit 0 is the contract:
#       net_shutdown(stdin, RDWR) returns ENOTSOCK, the program drops
#       errno and returns 0), and
#   (e) the multi-import canonical-order indexing
#       (`ctx.wasi_func_indices`) doesn't collide when a non-S6 builtin
#       is also live in the same unit (combo regression).
#
# SKIPs cleanly (exit 0) when the component toolchain or wasmtime is
# not provisioned — opt-in, mirroring the S3 and S6a smokes.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! command -v wasm-tools >/dev/null \
   || [ ! -f tools/vendor/wasi_snapshot_preview1.command.wasm ]; then
  echo "SKIP: component toolchain / command adapter not provisioned (tools/provision-component-toolchain.sh)"
  exit 0
fi
if ! command -v wasmtime >/dev/null; then
  echo "SKIP: wasmtime not on PATH (real-host run is the S6b contract)"
  exit 0
fi

COMPILER="${AFFINESCRIPT:-$ROOT/_build/default/bin/main.exe}"
[ -x "$COMPILER" ] || COMPILER="dune exec affinescript --"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Primary fixture: net_shutdown only.
cat > "$work/sock.affine" <<'EOF'
fn main() -> Int {
  net_shutdown(0, 3);  // stdin, RDWR — returns ENOTSOCK; we drop it.
  return 0;
}
EOF

# Combo fixture: net_shutdown alongside clock_now_ms + env_count to
# exercise the canonical-order Effect_sites pre-scan + on-demand import
# assembly without index collisions.
cat > "$work/combo.affine" <<'EOF'
fn main() -> Int {
  let t = clock_now_ms(1);
  let n = env_count(());
  net_shutdown(0, 3);
  return t + n;
}
EOF

$COMPILER compile "$work/sock.affine"  -o "$work/sock.wasm"
$COMPILER compile "$work/combo.affine" -o "$work/combo.wasm"

# (a) sock_shutdown import is present
if ! wasm-tools print "$work/sock.wasm" 2>/dev/null \
     | grep -q '"wasi_snapshot_preview1" "sock_shutdown"'; then
  echo "FAIL: net_shutdown did not lower to wasi_snapshot_preview1.sock_shutdown"
  exit 1
fi

# (b) S6a _start shim is still emitted
if ! wasm-tools print "$work/sock.wasm" 2>/dev/null | grep -q '(export "_start"'; then
  echo "FAIL: S6a _start shim missing when S6b primitive is in use"
  exit 1
fi

# (c)+(d) componentize and invoke
tools/componentize.sh --command "$work/sock.wasm" "$work/sock.component.wasm"
if ! wasmtime run "$work/sock.component.wasm"; then
  echo "FAIL: wasmtime run rejected the S6b component"
  exit 1
fi

# (e) combo: canonical order must be fd_write < clock_time_get <
# environ_sizes_get < sock_shutdown (insertion order in
# lib/codegen.ml's optional_wasi list).
order="$(wasm-tools print "$work/combo.wasm" 2>/dev/null \
         | grep -oE '"(fd_write|clock_time_get|environ_sizes_get|sock_shutdown)"' \
         | head -4 | tr -d '"' | tr '\n' ' ')"
expected='fd_write clock_time_get environ_sizes_get sock_shutdown '
if [ "$order" != "$expected" ]; then
  echo "FAIL: combo import canonical order drifted"
  echo "  got     : $order"
  echo "  expected: $expected"
  exit 1
fi

tools/componentize.sh --command "$work/combo.wasm" "$work/combo.component.wasm" >/dev/null
if ! wasmtime run "$work/combo.component.wasm"; then
  echo "FAIL: combo (clock+env+sock) component rejected by wasmtime"
  exit 1
fi

echo "ADR-015 S6b sockets smoke: PASSED ✓"
