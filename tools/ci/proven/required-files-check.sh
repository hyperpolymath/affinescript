#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Vendored from hyperpolymath/proven-cicd-and-workflows
# actions/required-files-check/action.yml (run: block, verbatim) at
# 50a443f01b1b85e4957b893a6f1a0ed2e8a52459. See tools/ci/proven/VENDOR.adoc.
# The composite action took REPO_OWNER / REPO_NAME from the github context;
# here they default to the Actions environment.
REPO_OWNER="${REPO_OWNER:-${GITHUB_REPOSITORY_OWNER:-hyperpolymath}}"
REPO_NAME="${REPO_NAME:-${GITHUB_REPOSITORY:-hyperpolymath/affinescript}}"

# Accepted placements widened per owner ruling 2026-09-19: estate
# repos legitimately keep governance docs under docs/ (and GitHub
# renders under .github/) — rsr-template-repo itself was failing on
# layout, not on substance. Root forms still win (first hit).
# Template-repo mode: see below.
#
# Presence-only checking rewards filler. This gate previously demanded
# ARCHITECTURE.md / MAINTAINERS.adoc / GOVERNANCE.md and checked only
# that the paths existed — so the cheapest way to pass was to commit
# template boilerplate. That happened: an estate repo acquired an
# ARCHITECTURE.md describing a directory layout it does not have, a
# MAINTAINERS naming a different account as owner, and a mise.toml
# pinning `zig = "latest"` against that repo's own .tool-versions.
# All three would have passed. So: presence, THEN format, THEN substance.
#
# Format policy (estate):
#   .adoc  documentation (default)
#   .md    wiki content only — plus a transitional allowance for the
#          GitHub-mandated files, which are migrating to berrywiki format
#   .txt   licence texts
#   fixed  names GitHub or convention dictates (CODEOWNERS, funding.yml,
#          NOTICE, AUTHORS, MAINTAINERS) keep their form
# GitHub runs a composite step as `bash --noprofile --norc -e -o pipefail`.
# That `-e` comes from the harness, not from here, and `set -uo pipefail`
# does NOT clear it. It directly contradicts this gate's design: the whole
# script accumulates `fail=1` so it can report EVERY defect and exit once
# at the end. Under `-e` the first non-zero command kills the step instead,
# silently, before the line that would have explained itself.
# Measured 2026-09-22 on pons-asinorum: a comment-only CODEOWNERS made
# `grep -v ... | wc -l` exit 1, which killed the step one line ABOVE the
# message declaring that exact file valid. The count sites below are also
# individually guarded, so the contract survives someone re-adding `-e`.
set +e
set -uo pipefail
fail=0

# --- template-repo mode -------------------------------------------
# An un-instantiated template legitimately SHIPS {{OWNER}}/{{AUTHOR}}
# placeholders and self-references its own template name; judging it
# like an instantiated repo is a category error. Detection is explicit
# and auditable: the repo name says it is a template.
TEMPLATE_MODE=0
case "${REPO_NAME##*/}" in
  *-template|*-template-repo) TEMPLATE_MODE=1 ;;
esac
if [ "$TEMPLATE_MODE" -eq 1 ]; then
  echo "  template repo detected ($REPO_NAME): instantiation placeholders are the shipped product"
fi

# --- presence, accepting every policy-legal form -------------------
# "name:form1,form2,..." — first existing form wins.
declare -a required=(
  ".editorconfig:.editorconfig"
  ".gitignore:.gitignore"
  ".gitattributes:.gitattributes"
  "CODEOWNERS:CODEOWNERS,.github/CODEOWNERS,docs/CODEOWNERS"
  "GOVERNANCE:GOVERNANCE.adoc,GOVERNANCE.md,docs/GOVERNANCE.adoc,docs/GOVERNANCE.md,.github/GOVERNANCE.adoc,.github/GOVERNANCE.md"
  "ARCHITECTURE:ARCHITECTURE.adoc,ARCHITECTURE.md,docs/ARCHITECTURE.adoc,docs/ARCHITECTURE.md,docs/architecture/README.adoc,TOPOLOGY.adoc,TOPOLOGY.md"
  "MAINTAINERS:MAINTAINERS,MAINTAINERS.adoc,MAINTAINERS.md,docs/MAINTAINERS,docs/MAINTAINERS.adoc,docs/MAINTAINERS.md"
  "toolchain:.tool-versions,.mise.toml,mise.toml"
)

declare -A found=()
for spec in "${required[@]}"; do
  name="${spec%%:*}"; forms="${spec#*:}"
  hit=""
  IFS=',' read -ra cands <<< "$forms"
  for c in "${cands[@]}"; do [ -f "$c" ] && { hit="$c"; break; }; done
  if [ -z "$hit" ]; then
    echo "::error::Missing required file: $name (accepted: $forms)"
    fail=1
  else
    found["$name"]="$hit"
    echo "  found $name -> $hit"
  fi
done

# --- substance: a file that exists but says nothing is not a file --
PLACEHOLDERS_BASE='TODO: update|<PROJECT|YOUR_PROJECT|lorem ipsum|example\.com'
if [ "$TEMPLATE_MODE" -eq 1 ]; then
  # {{OWNER}}/{{AUTHOR}} are the shipped product. Only the RSR template
  # itself may retain its own name; other templates must not ship it.
  if [ "${REPO_NAME##*/}" = "rsr-template-repo" ]; then
    PLACEHOLDERS="$PLACEHOLDERS_BASE"
  else
    PLACEHOLDERS='rsr-template-repo|'"$PLACEHOLDERS_BASE"
  fi
else
  PLACEHOLDERS='\{\{|rsr-template-repo|'"$PLACEHOLDERS_BASE"
fi
for name in "${!found[@]}"; do
  f="${found[$name]}"

  # Config files are judged on whether they configure anything, not
  # on length. A three-line CODEOWNERS can be complete and correct;
  # demanding prose from it would be the same presence-theatre in
  # reverse.
  case "$name" in
    .editorconfig|.gitignore|.gitattributes|toolchain) continue ;;
    CODEOWNERS)
      # Rule 1: Solo-owned repos may have comment-only CODEOWNERS
      # Check if file has any functional (non-comment) lines
      functional_lines=$(grep -cvE '^\s*(#|//|;|$)' "$f" || true)  # || true: grep exits 1 on a comment-only file
      if [ "$functional_lines" -eq 0 ]; then
        # File is all comments - this is valid for solo-maintained repos per CODEOWNERS-POLICY.adoc Rule 1
        echo "  CODEOWNERS is comment-only (valid for solo-maintained repos per Rule 1)"
      elif [ "$TEMPLATE_MODE" -eq 1 ] && grep -qF '{{OWNER}}' "$f"; then
        echo "  CODEOWNERS carries {{OWNER}} instantiation placeholders — valid for a template repo"
      elif ! grep -qE '^[^#[:space:]]+.*(@[A-Za-z0-9_/-]+|[^[:space:]]+@[^[:space:]]+\.)' "$f"; then
        echo "::error::$f assigns no owner to any path."
        fail=1
      fi
      continue ;;
  esac

  # Documents are judged on substance.
  body=$(grep -cvE '^\s*(#|//|;|$)' "$f" || true)  # || true: grep exits 1 on an all-comment file
  if [ "$body" -lt 5 ]; then
    echo "::error::$f has $body substantive lines — a stub, not a document."
    fail=1
  fi
  if grep -qiE "$PLACEHOLDERS" "$f"; then
    echo "::error::$f still contains template placeholders:"
    grep -inE "$PLACEHOLDERS" "$f" | head -3
    fail=1
  fi
done

# ARCHITECTURE must describe THIS repository: at least one path it
# names has to exist. Catches a layout copied from a template.
if [ -n "${found[ARCHITECTURE]:-}" ]; then
  arch="${found[ARCHITECTURE]}"
  real=0
  while IFS= read -r p; do
    p="${p%/}"
    [ -e "$p" ] && { real=1; break; }
  done < <(grep -oE '\b[a-zA-Z0-9_.-]+/' "$arch" | sort -u)
  if [ "$real" -eq 0 ]; then
    echo "::error::$arch names no directory that exists in this repository."
    echo "::error::It appears to describe a different project's layout."
    fail=1
  fi
fi

# MAINTAINERS must not hand the repo to somebody else. A template that
# ships its author's handle is the commonest form of this.
if [ -n "${found[MAINTAINERS]:-}" ] && [ -n "${REPO_OWNER:-}" ]; then
  m="${found[MAINTAINERS]}"
  if [ "$TEMPLATE_MODE" -eq 1 ]; then
    if ! grep -qE '\{\{(OWNER|AUTHOR)\}\}' "$m"; then
      echo "::error::$m ships no owner-injection point ({{OWNER}}/{{AUTHOR}})."
      echo "::error::A template MAINTAINERS must let instantiation name the owner."
      fail=1
    fi
  elif ! grep -qiF "$REPO_OWNER" "$m"; then
    echo "::error::$m never mentions the repository owner ($REPO_OWNER)."
    echo "::error::Handles found: $(grep -oE '@[A-Za-z0-9_-]+' "$m" | sort -u | tr '\n' ' ')"
    echo "::error::This is the signature of an unedited template."
    fail=1
  fi
fi

if [ "$fail" -eq 1 ]; then
  echo "::error::Required files gate failed."
  exit 1
fi
echo "All required files present, correctly formatted, and about this repository."
