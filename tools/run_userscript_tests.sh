#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Userscript target acceptance tests (thin wrapper).
#
# The checks live next to the fixtures they exercise — test/e2e/userscript/ —
# so that the README there, the planted negatives and the runner are read
# together. This wrapper exists for the repo-wide convention that CI and
# `just check` call tools/run_*_tests.sh.
#
# Usage: ./tools/run_userscript_tests.sh
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
exec "$ROOT_DIR/test/e2e/userscript/run.sh" "$@"
