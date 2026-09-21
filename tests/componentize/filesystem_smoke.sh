#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# ADR-015 S5 gated smoke (#485): compile an AffineScript program that
# round-trips a small file through file_open / file_fd_write /
# file_close / file_read, wrap it with tools/componentize.sh --command,
# and assert
#   (a) the core module imports path_open + fd_read + fd_close,
#   (b) wasmtime run --dir <preopen> exits 0,
#   (c) combo with clock + env + sock_shutdown keeps canonical order.
#
# SKIPs cleanly (exit 0) when the component toolchain or wasmtime is
# not provisioned — opt-in, mirroring the S3/S6 smokes.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! command -v wasm-tools >/dev/null \
   || [ ! -f tools/vendor/wasi_snapshot_preview1.command.wasm ]; then
  echo "SKIP: component toolchain / command adapter not provisioned (tools/provision-component-toolchain.sh)"
  exit 0
fi
if ! command -v wasmtime >/dev/null; then
  echo "SKIP: wasmtime not on PATH (real-host run is the S5 contract)"
  exit 0
fi

COMPILER="${AFFINESCRIPT:-$ROOT/_build/default/bin/main.exe}"
[ -x "$COMPILER" ] || COMPILER="dune exec affinescript --"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cat > "$work/fs.affine" <<'EOF'
fn main() -> Int / { File } {
  let fd = file_open("roundtrip.txt", 9);
  file_fd_write(fd, "ok\n");
  file_close(fd);
  let fd2 = file_open("roundtrip.txt", 0);
  let s = file_read(fd2, 8);
  file_close(fd2);
  return string_length(s);
}
EOF

cat > "$work/combo.affine" <<'EOF'
fn main() -> Int / { Time, Net, File } {
  let t = clock_now_ms(1);
  let n = env_count(());
  net_shutdown(0, 3);
  let fd = file_open("combo.txt", 1);
  file_close(fd);
  return 0;
}
EOF

$COMPILER compile "$work/fs.affine"    -o "$work/fs.wasm"
$COMPILER compile "$work/combo.affine" -o "$work/combo.wasm"

for name in path_open fd_read fd_close; do
  if ! wasm-tools print "$work/fs.wasm" 2>/dev/null \
       | grep -q "\"wasi_snapshot_preview1\" \"$name\""; then
    echo "FAIL: file_* did not lower to wasi_snapshot_preview1.$name"
    exit 1
  fi
done

tools/componentize.sh --command "$work/fs.wasm" "$work/fs.component.wasm"
# Map the host workdir onto the guest preopen (dirfd 3 / ".") so
# path_open("roundtrip.txt") is hermetic.
if ! wasmtime run --dir "$work::." "$work/fs.component.wasm"; then
  echo "FAIL: wasmtime run --dir rejected the S5 filesystem component"
  exit 1
fi

order="$(wasm-tools print "$work/combo.wasm" 2>/dev/null \
         | grep -oE '"(fd_write|clock_time_get|environ_sizes_get|sock_shutdown|path_open|fd_close)"' \
         | tr -d '"' | tr '\n' ' ')"
expected='fd_write clock_time_get environ_sizes_get sock_shutdown path_open fd_close '
if [ "$order" != "$expected" ]; then
  echo "FAIL: combo import canonical order drifted"
  echo "  got     : $order"
  echo "  expected: $expected"
  exit 1
fi

tools/componentize.sh --command "$work/combo.wasm" "$work/combo.component.wasm" >/dev/null
if ! wasmtime run --dir "$work::." "$work/combo.component.wasm"; then
  echo "FAIL: combo (clock+env+sock+fs) component rejected by wasmtime"
  exit 1
fi

echo "ADR-015 S5 filesystem smoke: PASSED ✓"
