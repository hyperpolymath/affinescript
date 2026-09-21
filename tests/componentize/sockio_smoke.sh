#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# #487 gated smoke: compile a program that references net_recv /
# net_send / net_accept, wrap with --command, and assert the preview1
# sock_* imports are present. Real-host invoke of accept/recv against
# a non-socket fd is expected to return errno (the guest drops it).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! command -v wasm-tools >/dev/null \
   || [ ! -f tools/vendor/wasi_snapshot_preview1.command.wasm ]; then
  echo "SKIP: component toolchain / command adapter not provisioned"
  exit 0
fi
if ! command -v wasmtime >/dev/null; then
  echo "SKIP: wasmtime not on PATH"
  exit 0
fi

COMPILER="${AFFINESCRIPT:-$ROOT/_build/default/bin/main.exe}"
[ -x "$COMPILER" ] || COMPILER="dune exec affinescript --"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cat > "$work/sockio.affine" <<'EOF'
fn main() -> Int / { Net } {
  net_send(0, "x");
  let _s = net_recv(0, 8);
  net_accept(0);
  return 0;
}
EOF

$COMPILER compile "$work/sockio.affine" -o "$work/sockio.wasm"

for name in sock_recv sock_send sock_accept; do
  if ! wasm-tools print "$work/sockio.wasm" 2>/dev/null \
       | grep -q "\"wasi_snapshot_preview1\" \"$name\""; then
    echo "FAIL: net_* did not lower to wasi_snapshot_preview1.$name"
    exit 1
  fi
done

tools/componentize.sh --command "$work/sockio.wasm" "$work/sockio.component.wasm"
if ! wasmtime run "$work/sockio.component.wasm"; then
  echo "FAIL: wasmtime run rejected the #487 component"
  exit 1
fi

echo "ADR-015 #487 sockio smoke: PASSED ✓"
