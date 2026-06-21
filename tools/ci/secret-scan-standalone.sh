#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Standalone secret scan.
#
# Replaces the external `hyperpolymath/standards` secret-scanner-reusable.yml
# so this repo's CI carries no cross-repo workflow dependency and needs no
# inherited secrets. Pure-shell, scans tracked files only, and uses a small
# set of HIGH-CONFIDENCE patterns chosen for a near-zero false-positive rate.
#
# This is a self-contained backstop, not a full entropy/credential scanner;
# CodeQL + Semgrep remain the deeper SAST layers. Exit non-zero on any hit.
set -uo pipefail

# High-confidence credential patterns (low false-positive). The PEM marker is
# assembled from fragments so this scanner does not itself trip credential
# scanners (no full marker literal appears anywhere in this file).
pem_b="-----BEG""IN"
pem_k="PRIV""ATE KEY-----"
patterns=(
  "${pem_b} [A-Z ]*${pem_k}"     # PEM / OpenSSH private keys
  'AKIA[0-9A-Z]{16}'             # AWS access key id
  'ASIA[0-9A-Z]{16}'             # AWS temporary access key id
  'gh[pousr]_[A-Za-z0-9]{36,}'   # GitHub personal/oauth/server tokens
  'github_pat_[A-Za-z0-9_]{40,}' # GitHub fine-grained PAT
  'xox[baprs]-[A-Za-z0-9-]{10,}' # Slack tokens
  'AIza[0-9A-Za-z_-]{35}'        # Google API key
)

# Tracked files only, excluding build/vendor output and this script itself
# (which necessarily contains the patterns).
mapfile -t files < <(git ls-files \
  | grep -vE '(^|/)(_build|node_modules|tools/vendor)/' \
  | grep -vxF 'tools/ci/secret-scan-standalone.sh')

hits=0
for pat in "${patterns[@]}"; do
  if [ "${#files[@]}" -gt 0 ]; then
    # `-e "$pat"` is required: several patterns begin with '-' (PEM markers),
    # which grep would otherwise parse as options (silently matching nothing).
    matches=$(printf '%s\0' "${files[@]}" | xargs -0 -r grep -InE -e "$pat" 2>/dev/null || true)
    if [ -n "$matches" ]; then
      printf '::error::potential secret (pattern: %s)\n' "$pat"
      printf '%s\n' "$matches" | sed 's/^/    /'
      hits=1
    fi
  fi
done

echo
if [ "$hits" -ne 0 ]; then echo "Secret scan: FAIL"; exit 1; fi
echo "Secret scan: PASS (no high-confidence secrets in tracked files)"
