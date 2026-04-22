#!/bin/bash
set -eu
ok() { echo "  [OK] $1"; }
fail() { echo "  [FAIL] $1"; exit 1; }
info() { echo "  [INFO] $1"; }
install_just() {
    if command -v just >/dev/null 2>&1; then ok "just installed"; return 0; fi
    info "Installing just..."
    curl -fsSL https://just.systems/install.sh | bash -s -- --to /usr/local/bin || fail "just install failed"
}
main() {
    echo "=== affinescript-vite Setup ==="
    install_just
    if [ ! -f "Justfile" ]; then fail "No Justfile"; fi
    if just --list | grep -q "^setup "; then just setup; else just doctor; fi
    echo "=== Setup Complete ==="
}
main
