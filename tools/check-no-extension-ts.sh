#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Regression guard for issue #35 Phase 3.
#
# The AffineScript VS Code extension (and any future face-extensions
# under faces/*/affinescript/editors/vscode/) is now authored in
# extension.affine and compiled via `affinescript compile -o
# extension.cjs`. The TypeScript source was deleted on 2026-05-03 once
# the Node-CJS codegen + Vscode bindings landed.
#
# This guard fails if any extension.ts reappears under editors/vscode/
# or faces/*/editors/vscode/. Run from the repo root.
#
# Wired into:
#   - editors/vscode/package.json scripts.guard
#   - just check (see justfile)
#   - CI (see .github/workflows/affinescript-canary.yml)

set -euo pipefail

cd "$(dirname "$0")/.."

bad=$(find editors/vscode/src faces/*/affinescript/editors/vscode/src \
        -maxdepth 1 -name '*.ts' 2>/dev/null || true)

if [ -n "$bad" ]; then
  echo "ERROR: TypeScript files have reappeared in vscode extension source:" >&2
  echo "$bad" >&2
  echo "" >&2
  echo "Issue #35 Phase 3 deleted these on 2026-05-03 — they cannot come" >&2
  echo "back without re-introducing the policy violation." >&2
  echo "" >&2
  echo "If you genuinely need a TS file, you must:" >&2
  echo "  1. Open a new exemption issue against hyperpolymath/standards" >&2
  echo "  2. Update .claude/CLAUDE.md TypeScript Exemptions table here" >&2
  echo "  3. Remove the path from this guard" >&2
  exit 1
fi

echo "OK: no extension.ts files in editors/vscode/src or face equivalents."
