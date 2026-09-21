#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Install a pinned tree-sitter CLI binary from GitHub Releases.
#
# Used by CI instead of `npm install -g --ignore-scripts tree-sitter-cli`:
# that package's binary is fetched in a postinstall script, so
# --ignore-scripts (Sonar S6505) leaves `tree-sitter` missing (ENOENT).
# A release tarball has no lifecycle scripts.
set -euo pipefail

VER="${TREE_SITTER_CLI_VERSION:-0.25.0}"
DEST="${TREE_SITTER_CLI_DEST:-/usr/local/bin/tree-sitter}"

arch="$(uname -m)"
case "$arch" in
  x86_64|amd64) ts_arch=x64 ;;
  aarch64|arm64) ts_arch=arm64 ;;
  *)
    echo "error: unsupported arch $arch" >&2
    exit 1
    ;;
esac

url="https://github.com/tree-sitter/tree-sitter/releases/download/v${VER}/tree-sitter-linux-${ts_arch}.gz"
tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
curl -fsSL "$url" | gunzip > "$tmp"
chmod +x "$tmp"

if [ -w "$(dirname "$DEST")" ]; then
  mv "$tmp" "$DEST"
  trap - EXIT
else
  sudo mv "$tmp" "$DEST"
  trap - EXIT
fi

"$DEST" --version
