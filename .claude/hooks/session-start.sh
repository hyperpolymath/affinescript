#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# SessionStart hook — provision the OCaml toolchain + tree-sitter grammar so
# Claude Code on the web can `dune build` / `dune runtest` this project
# without manual setup.
#
# Why apt and not opam: this repo's web environment reaches the Ubuntu archive
# and github.com, but opam.ocaml.org is blocked by the network policy (its
# index returns 403). Ubuntu's OCaml packages happen to satisfy the project's
# version pins (dune 3.14, cmdliner 1.2, alcotest 1.7, menhir 20231231,
# sedlex 3.2, ppx_deriving 5.2, ppxlib 0.32, yojson 2.1, js_of_ocaml 5.6), so
# apt is the reliable install path here. This exact set was verified to give a
# clean full `dune build` + `dune runtest`.
#
# Best-effort and idempotent: a failed step logs a warning to stderr but never
# aborts session start; already-installed steps are skipped. Synchronous, so
# the toolchain is guaranteed ready before the session begins.

set -uo pipefail

# Web sessions only — local dev environments manage their own toolchain.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

log()  { echo "[session-start] $*" >&2; }
warn() { echo "[session-start] WARNING: $*" >&2; }

REPO="${CLAUDE_PROJECT_DIR:-$(pwd)}"

# 1. OCaml toolchain via apt (skip if dune is already on PATH).
if command -v dune >/dev/null 2>&1; then
  log "OCaml toolchain present (dune $(dune --version 2>/dev/null)); skipping apt"
else
  log "installing OCaml toolchain via apt ..."
  export DEBIAN_FRONTEND=noninteractive
  apt-get update -qq || warn "apt-get update failed; install may be incomplete"
  apt-get install -y -qq \
    ocaml-nox ocaml-dune ocaml-findlib \
    libcmdliner-ocaml-dev libfmt-ocaml-dev libalcotest-ocaml-dev \
    menhir libmenhir-ocaml-dev libsedlex-ocaml-dev \
    libppx-deriving-ocaml-dev libppxlib-ocaml-dev libyojson-ocaml-dev \
    js-of-ocaml libjs-of-ocaml-dev \
    || warn "apt install of the OCaml toolchain failed; 'dune build' may not work"
fi

# 2. tree-sitter CLI (needed by the res-to-affine walker tests in
#    tools/res-to-affine/test/). Skip if already on PATH.
if command -v tree-sitter >/dev/null 2>&1; then
  log "tree-sitter present ($(tree-sitter --version 2>/dev/null)); skipping npm"
elif command -v npm >/dev/null 2>&1; then
  log "installing tree-sitter CLI ..."
  npm install -g tree-sitter-cli@^0.25.0 \
    || warn "tree-sitter CLI install failed; walker tests will auto-skip"
else
  warn "npm not found; cannot install tree-sitter CLI (walker tests will skip)"
fi

# 3. Pinned tree-sitter-rescript grammar — generates the parser the walker
#    shells out to. install.sh re-checks out the pinned commit; skip if the
#    generated parser is already present.
if [ -f "$REPO/tools/vendor/tree-sitter-rescript/src/parser.c" ]; then
  log "tree-sitter-rescript grammar already built"
elif command -v tree-sitter >/dev/null 2>&1; then
  log "building pinned tree-sitter-rescript grammar ..."
  ( cd "$REPO" && ./editors/tree-sitter-rescript/scripts/install.sh ) \
    || warn "grammar build failed; res-to-affine walker tests will auto-skip"
fi

log "ready. build: dune build | test: dune runtest"
exit 0
