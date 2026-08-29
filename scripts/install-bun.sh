#!/bin/bash
set -euo pipefail

# nosonar
# Intentional direct download because setup-bun is blocked by repo selected-action policy.

BUN_VERSION="${BUN_VERSION:-1.3.14}"
BUN_LINUX_X64_SHA256="${BUN_LINUX_X64_SHA256:-951ee2aee855f08595aeec6225226a298d3fea83a3dcd6465c09cbccdf7e848f}"

archive="$RUNNER_TEMP/bun-linux-x64.zip"
install_dir="$RUNNER_TEMP/bun-runtime"
curl --fail --location --retry 3 \
  --output "$archive" \
  "https://github.com/oven-sh/bun/releases/download/bun-v${BUN_VERSION}/bun-linux-x64.zip"
printf "%s  %s\n" "$BUN_LINUX_X64_SHA256" "$archive" | sha256sum --check --strict
unzip -q "$archive" -d "$install_dir"
printf "%s\n" "$install_dir/bun-linux-x64" >> "$GITHUB_PATH"
"$install_dir/bun-linux-x64/bun" --version
