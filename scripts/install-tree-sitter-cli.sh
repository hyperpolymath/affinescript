#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Install a pinned tree-sitter CLI binary from GitHub Releases.
#
# Used by CI instead of `npm install -g --ignore-scripts tree-sitter-cli`:
# that package's binary is fetched in a postinstall script, so
# --ignore-scripts (Sonar S6505) leaves `tree-sitter` missing (ENOENT).
set -euo pipefail

VER="${TREE_SITTER_CLI_VERSION:-0.25.0}"
# tree-sitter-linux-x64.gz from
# https://github.com/tree-sitter/tree-sitter/releases/tag/v0.25.0
SHA256="${TREE_SITTER_LINUX_X64_SHA256:-d7b68a7a79459c0c23e062f719fe90781ed284a4fb172756e217ca08ea86b8d3}"

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
workdir="${RUNNER_TEMP:-$(mktemp -d)}"
archive="${workdir}/tree-sitter-linux-${ts_arch}.gz"
bin_dir="${workdir}/tree-sitter-cli"
mkdir -p "$bin_dir"

curl --fail --location --retry 3 --proto "=https" \
  --output "$archive" \
  "$url"
printf '%s  %s\n' "$SHA256" "$archive" | sha256sum --check --strict
gunzip -c "$archive" > "${bin_dir}/tree-sitter"
chmod +x "${bin_dir}/tree-sitter"

if [[ -n "${GITHUB_PATH:-}" ]]; then
  printf '%s\n' "$bin_dir" >> "$GITHUB_PATH"
fi
export PATH="${bin_dir}:${PATH}"
tree-sitter --version
