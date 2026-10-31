#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Bootstrap a hermetic OCaml toolchain without opam.
#
# Why this exists: some build/verify environments (sandboxes, air-gapped CI
# runners, machines with no opam switch) have no OCaml at all, and the
# AffineScript compiler only needs a handful of findlib packages. This script
# builds the compiler + the pinned dependencies straight from upstream
# release tarballs into one prefix, so nothing touches a user-wide switch and
# nothing needs network access beyond GitHub.
#
# What it installs, in order, each step idempotent:
#
#   stage core   ocaml 4.14.2 (native), dune 3.16.1, ocamlfind 1.9.6
#   stage libs   yojson 2.1.2, fmt 0.10.0, cmdliner 1.3.0,
#                gen v1.1, menhir 20231231, sedlex 3.2,
#                ocaml-compiler-libs v0.12.4, stdlib-shims 0.3.0,
#                ppx_derivers 1.2.1, ppxlib 0.32.1, cppo 1.6.9,
#                ppx_deriving v5.2.1
#                (sexplib0 / ppx_sexp_conv are skipped on purpose: the repo does
#                not use them, and ppx_sexp_conv drags in janestreet `base`)
#   stage shims  findlib `seq`/`result` re-export shims (only needed when a
#                dependency asks for those package names on a stdlib that
#                already provides the modules)
#
# Usage:
#   ./tools/bootstrap-ocaml-toolchain.sh                     # core + libs
#   ./tools/bootstrap-ocaml-toolchain.sh --stage core         # just the compiler
#   ./tools/bootstrap-ocaml-toolchain.sh --prefix /opt/ocaml --jobs 8
#   ./tools/bootstrap-ocaml-toolchain.sh --verify            # build a smoke test
#
# Afterwards (the script prints the same block):
#   export PATH="$PREFIX/bin:$PATH"
#   export OCAMLPATH="$PREFIX/lib"
#   export CAML_LD_LIBRARY_PATH="$PREFIX/lib/stublibs"
#
# Notes learned the hard way, so they are encoded here rather than rediscovered:
#   * `dune build -p <pkg>` (release scoping) is a dead end in this layout —
#     build `@install` with `--profile release` instead.
#   * dune installs itself from its bootstrap: `_boot/dune.exe` is copied to
#     bin/dune, no `dune install` step (which is unreliable before dune is on
#     PATH and findlib is configured).
#   * fmt: v0.10.0 ships no dune-project (it is a topkg/ocamlbuild-era
#     tarball), so v0.11.0 is pinned here — still above the repo's `>= 0.9`
#     floor — and built with `--profile release` like everything else.
#   * sedlex 3.x generates its Unicode tables with python3 (gen_ucd.py), so
#     python3 must be present even though nothing else here is Python.
#   * `seq`/`result` are *stdlib re-export shims* on OCaml >= 4.14: a findlib
#     package name some dune files still ask for, not real libraries.

set -euo pipefail

PREFIX_DEFAULT="$HOME/.local/affinescript-toolchain"
PREFIX=""
JOBS="$( (nproc 2>/dev/null || echo 4) )"
STAGES="core libs shims"
VERIFY=0
WORK=""

usage() {
  sed -n '2,50p' "$0" | sed 's/^# \{0,1\}//'
  exit "${1:-0}"
}

while [ $# -gt 0 ]; do
  case "$1" in
    --prefix) PREFIX="${2:?--prefix needs a directory}"; shift 2 ;;
    --jobs|-j) JOBS="${2:?--jobs needs a number}"; shift 2 ;;
    --stage) STAGES="${2:?--stage needs a name}"; shift 2 ;;
    --work) WORK="${2:?--work needs a directory}"; shift 2 ;;
    --verify) VERIFY=1; shift ;;
    -h|--help) usage 0 ;;
    *) echo "unknown argument: $1" >&2; usage 1 ;;
  esac
done

PREFIX="${PREFIX:-$PREFIX_DEFAULT}"
WORK="${WORK:-$PREFIX/src}"
mkdir -p "$PREFIX" "$WORK"

log()  { printf '\n==> %s\n' "$*" >&2; }
skip() { printf '    (already satisfied: %s)\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

export PATH="$PREFIX/bin:$PATH"
export OCAMLPATH="$PREFIX/lib"
export CAML_LD_LIBRARY_PATH="$PREFIX/lib/stublibs"

has_stage() { case " $STAGES " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }

# ── preflight ───────────────────────────────────────────────────────────────
log "preflight"
for tool in curl tar make; do
  command -v "$tool" >/dev/null 2>&1 || die "$tool is required"
done
if command -v cc >/dev/null 2>&1 || command -v gcc >/dev/null 2>&1; then
  echo "    C compiler: $(command -v cc || command -v gcc)"
else
  die "a C compiler (cc or gcc) is required to build OCaml"
fi
echo "    prefix: $PREFIX"
echo "    jobs:   $JOBS"
echo "    stages: $STAGES"

# fetch <owner/repo> <tag> — extracts into $WORK/<repo>-<tag> and echoes it
fetch() {
  local slug="$1" tag="$2" name dir archive
  name="${slug##*/}"
  dir="$WORK/$name-$tag"
  archive="$WORK/$name-$tag.tar.gz"
  if [ -d "$dir" ]; then echo "$dir"; return 0; fi
  log "fetch $slug $tag"
  curl --fail --location --retry 3 --silent --show-error \
    --output "$archive" \
    "https://codeload.github.com/$slug/tar.gz/refs/tags/$tag" \
    || { rm -f "$archive"; die "cannot download $slug $tag (check the tag name)"; }
  rm -rf "$dir.partial"
  mkdir -p "$dir.partial"
  tar -xzf "$archive" -C "$dir.partial" --strip-components=1 \
    || { rm -rf "$dir.partial"; die "cannot extract $slug $tag"; }
  mv "$dir.partial" "$dir"
  echo "$dir"
}

# dune_build_install <dir> <package-name> [extra dune args...]
# Builds @install with the release profile and installs into $PREFIX.
dune_build_install() {
  local dir="$1" pkg="$2"; shift 2
  [ -n "$dir" ] && [ -d "$dir" ] || die "no source directory for $pkg (download failed?)"
  # remaining args: extra findlib package names to install (some upstream
  # tarballs ship several, e.g. menhir/menhirLib/menhirSdk).
  ( cd "$dir" && dune build --profile release @install && \
    dune install --prefix "$PREFIX" --libdir "$PREFIX/lib" "$pkg" "$@" ) && return 0
  # Some upstream packages install optional sub-packages whose build needs a
  # tool we do not have (cppo's `cppo_ocamlbuild` wants ocamlbuild). Fall back
  # to building just the package we actually need.
  printf '    @install failed for %s; retrying with %s.install only\n' "$pkg" "$pkg" >&2
  ( cd "$dir" && dune build --profile release "$pkg.install" && \
    dune install --prefix "$PREFIX" --libdir "$PREFIX/lib" "$pkg" "$@" ) \
    || die "failed to install $pkg (build log above)"
}

# ── stage: core ─────────────────────────────────────────────────────────────
if has_stage core; then
  if command -v ocamlopt >/dev/null 2>&1; then
    skip "ocaml ($(ocamlopt -version))"
  else
    d=$(fetch ocaml/ocaml 4.14.2)
    log "build ocaml 4.14.2 (this is the slow one: ~5-10 min)"
    ( cd "$d" && ./configure --prefix "$PREFIX" \
        --disable-ocamldoc --disable-debugger >/dev/null && \
      make -j"$JOBS" world.opt >/dev/null && make install >/dev/null ) \
      || die "OCaml build failed — re-run with the redirects removed to see the log"
    echo "    ocaml $(ocamlopt -version)"
  fi

  if command -v dune >/dev/null 2>&1; then
    skip "dune ($(dune --version))"
  else
    d=$(fetch ocaml/dune 3.16.1)
    log "bootstrap dune 3.16.1"
    # dune's bootstrap.ml takes -j/--verbose, not --release (3.16 has no such
    # flag); the generated _boot/dune.exe is the same binary either way.
    ( cd "$d" && ocaml boot/bootstrap.ml -j "$JOBS" >/dev/null ) \
      || die "dune bootstrap failed"
    install -m 0755 "$d/_boot/dune.exe" "$PREFIX/bin/dune"
    echo "    dune $(dune --version)"
  fi

  if command -v ocamlfind >/dev/null 2>&1; then
    skip "ocamlfind ($(ocamlfind printconf version 2>/dev/null || echo present))"
  else
    # Optional: dune generates its own META files, so a dune-only toolchain
    # consumes the packages below without findlib. ocamlfind is still handy
    # for `ocamlfind list` and for packages built by hand.
    d=$(fetch ocaml/ocamlfind findlib-1.9.6)
    log "build ocamlfind findlib-1.9.6 (optional)"
    if ( cd "$d" && ./configure -bindir "$PREFIX/bin" -sitelib "$PREFIX/lib" \
           -mandir "$PREFIX/man" -config "$PREFIX/etc/findlib.conf" >/dev/null && \
         make all >/dev/null && make opt >/dev/null && make install >/dev/null ); then
      echo "    ocamlfind installed"
    else
      echo "    WARNING: ocamlfind did not build; continuing (dune does not need it)" >&2
    fi
  fi
fi

# ── stage: shims ────────────────────────────────────────────────────────────
if has_stage shims; then
  for shim in seq result; do
    log "install the $shim findlib re-export shim"
    d="$WORK/shim-$shim"
    rm -rf "$d" "$PREFIX/lib/$shim"
    mkdir -p "$d"
    # These are re-exports, not reimplementations: on OCaml >= 4.14 the module
    # lives in the stdlib, but dune files written against the opam-era package
    # names still ask for `seq` / `result`, and some (ppx_deriving) name the
    # *type* through the package (`Result.result`), so a bare META is not
    # enough — the type has to exist.
    case "$shim" in
      seq) cat > "$d/seq.ml" <<'EOF'
(* findlib `seq` shim: OCaml >= 4.14 provides Seq in the stdlib. *)
include Stdlib.Seq
EOF
        ;;
      result) cat > "$d/result.ml" <<'EOF'
(* findlib `result` shim: OCaml >= 4.14 provides Result in the stdlib. The
   type is re-declared so packages naming it as `Result.result` resolve. *)
(* OCaml 4.14 spells the second constructor [Error]; 5.x-era code expects
   [Err], which is why packages carried a `result` dependency at all. *)
type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b

include Stdlib.Result
EOF
        ;;
    esac
    ( cd "$d" && \
      ocamlc -c "$shim.mli" 2>/dev/null || true; \
      ocamlc -c "$shim.ml" && ocamlopt -c "$shim.ml" && \
      ocamlc -a -o "$shim.cma" "$shim.cmo" && \
      ocamlopt -a -o "$shim.cmxa" "$shim.cmx" ) \
      || die "cannot build the $shim shim"
    mkdir -p "$PREFIX/lib/$shim"
    cp "$d/$shim".cmi "$d/$shim".cmt "$d/$shim".cmx "$d/$shim".cma "$d/$shim".cmxa \
       "$PREFIX/lib/$shim/" 2>/dev/null || true
    cp "$d"/*.o "$d"/*.a "$PREFIX/lib/$shim/" 2>/dev/null || true
    cat > "$PREFIX/lib/$shim/META" <<EOF
version = "0.3"
description = "$shim re-export shim (module provided by the stdlib)"
requires = ""
archive(byte) = "$shim.cma"
archive(native) = "$shim.cmxa"
EOF
  done
fi

# ── stage: libs ─────────────────────────────────────────────────────────────
if has_stage libs; then
  # Menhir must be on PATH before sedlex builds (it is a build-time dep).
  if [ -x "$PREFIX/bin/menhir" ]; then skip "menhir"; else
    dune_build_install "$(fetch LexiFi/menhir 20231231)" menhir menhirLib menhirSdk
  fi
  if [ -f "$PREFIX/lib/ocaml-compiler-libs/META" ]; then skip "ocaml-compiler-libs"; else
    # v0.17.0 is OCaml 5 only (Cmo_format.Compunit); 4.14 needs the v0.12 line.
    dune_build_install "$(fetch janestreet/ocaml-compiler-libs v0.12.4)" ocaml-compiler-libs
  fi
  if [ -f "$PREFIX/lib/stdlib-shims/META" ]; then skip "stdlib-shims"; else
    dune_build_install "$(fetch ocaml/stdlib-shims 0.3.0)" stdlib-shims
  fi
  if [ -f "$PREFIX/lib/sexplib0/META" ]; then skip "sexplib0"; else
    dune_build_install "$(fetch janestreet/sexplib0 v0.16.0)" sexplib0
  fi
  if [ -f "$PREFIX/lib/ppx_derivers/META" ]; then skip "ppx_derivers"; else
    dune_build_install "$(fetch ocaml-ppx/ppx_derivers 1.2.1)" ppx_derivers
  fi
  if [ -f "$PREFIX/lib/ppxlib/META" ]; then skip "ppxlib"; else
    dune_build_install "$(fetch ocaml-ppx/ppxlib 0.32.1)" ppxlib
  fi
  if [ -f "$PREFIX/lib/cppo/META" ]; then skip "cppo"; else
    # ppx_deriving's build runs cppo over its source template.
    dune_build_install "$(fetch ocaml-community/cppo v1.6.9)" cppo
  fi
  if [ -f "$PREFIX/lib/ppx_deriving/META" ]; then skip "ppx_deriving"; else
    dune_build_install "$(fetch ocaml-ppx/ppx_deriving v5.2.1)" ppx_deriving
  fi
  # ppx_sexp_conv is deliberately absent: the pinned version pulls in
  # janestreet `base` (and dune-configurator/csexp behind it) for a package
  # this repository does not use — there is no `[@@deriving sexp]` and no
  # Sexp reference anywhere in lib/ or bin/. Add it back here if that changes.
  if [ -f "$PREFIX/lib/yojson/META" ]; then skip "yojson"; else
    dune_build_install "$(fetch ocaml-community/yojson 2.1.2)" yojson
  fi
  if [ -f "$PREFIX/lib/fmt/META" ]; then skip "fmt"; else
    # fmt's release tarballs are topkg/ocamlbuild only (B0.ml + fmt.mllib, no
    # dune files anywhere). Rather than pull in ocamlbuild + topkg just to
    # compile three files, drop a minimal dune description on top: fmt is
    # stdlib-only, fmt.tty needs unix, and the module list is the mllib.
    d=$(fetch dbuenzli/fmt v0.11.0)
    log "build fmt 0.11.0 (dune shim over its topkg layout)"
    printf '(lang dune 3.0)\n' > "$d/dune-project"
    printf 'opam-version: "2.0"\nsynopsis: "OCaml Format pretty-printer combinators"\n' \
      > "$d/fmt.opam"
    cat > "$d/src/dune" <<'DUNE'
(library (name fmt) (public_name fmt) (modules fmt))
DUNE
    cat > "$d/src/tty/dune" <<'DUNE'
(library (name fmt_tty) (public_name fmt.tty) (modules fmt_tty)
 (libraries fmt unix))
DUNE
    # one package file so `dune install fmt fmt.tty` resolves both libraries
    # fmt and fmt.tty are one package: build fmt.install, install fmt.
    ( cd "$d" && dune build --profile release fmt.install && \
      dune install --prefix "$PREFIX" --libdir "$PREFIX/lib" fmt ) \
      || die "fmt build failed"
  fi
  if [ -f "$PREFIX/lib/cmdliner/META" ]; then skip "cmdliner"; else
    dune_build_install "$(fetch dbuenzli/cmdliner v1.3.0)" cmdliner
  fi
  if [ -f "$PREFIX/lib/gen/META" ]; then skip "gen"; else
    dune_build_install "$(fetch c-cube/gen v1.1)" gen
  fi
  if [ -f "$PREFIX/lib/sedlex/META" ]; then skip "sedlex"; else
    command -v python3 >/dev/null 2>&1 || die "sedlex needs python3 (gen_ucd.py)"
    dune_build_install "$(fetch ocaml-community/sedlex v3.2)" sedlex sedlex.ppx
  fi
fi

# ── summary ─────────────────────────────────────────────────────────────────
log "toolchain ready"
cat <<EOF
export PATH="$PREFIX/bin:\$PATH"
export OCAMLPATH="$PREFIX/lib"
export CAML_LD_LIBRARY_PATH="$PREFIX/lib/stublibs"
EOF
[ -x "$PREFIX/bin/dune" ] && echo "dune: $("$PREFIX/bin/dune" --version)"
[ -x "$PREFIX/bin/ocamlopt" ] && echo "ocaml: $("$PREFIX/bin/ocamlopt" -version)"

if [ "$VERIFY" = 1 ]; then
  log "verify: build the repo"
  repo_root="$(cd "$(dirname "$0")/.." && pwd)"
  ( cd "$repo_root" && dune build --profile release bin/main.exe ) \
    || die "the compiler did not build with this toolchain"
  echo "    _build/default/bin/main.exe: $("$repo_root/_build/default/bin/main.exe" --version 2>/dev/null || echo built)"
fi
