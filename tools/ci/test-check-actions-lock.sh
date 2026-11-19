#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Regression tests for tools/ci/check-actions-lock.sh. Each case appends a
# job to a copy of the real workflows and asserts whether the guard passes.
# A reference in step context is always checked; only a job-level call of a
# workflow file (`jobs.<id>.uses: …/.github/workflows/x.yml@ref`) is skipped.
#
# Usage: tools/ci/test-check-actions-lock.sh   (from the repo root)
set -uo pipefail

guard=tools/ci/check-actions-lock.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0

# expect <pass|fail> <name> <yaml appended to ci.yml>
expect() {
  local want="$1" name="$2" yaml="$3" got
  rm -rf "$work/wf" && mkdir "$work/wf"
  cp .github/workflows/*.yml .github/workflows/actions.lock "$work/wf/"
  printf '%s\n' "$yaml" >> "$work/wf/ci.yml"
  if "$guard" "$work/wf" >/dev/null 2>&1; then got=pass; else got=fail; fi
  if [[ "$got" == "$want" ]]; then
    echo "ok   $name"
  else
    echo "FAIL $name (expected $want, got $got)"
    fails=$((fails + 1))
  fi
}

expect pass "real workflows" ""
expect fail "unlocked step action" '  t:
    runs-on: x
    steps:
      - uses: acme/build-action@v1'
expect fail "step action under a .github/workflows/*.yml directory" '  t:
    runs-on: x
    steps:
      - name: odd
        uses: acme/repo/.github/workflows/my-action.yml@v1'
expect fail "indentless step sequence" '  t:
    runs-on: x
    steps:
    - name: odd
      uses: acme/repo/.github/workflows/my-action.yml@v1'
expect fail "flow sequence on the steps: line" '  t:
    runs-on: x
    steps: [{uses: acme/repo/.github/workflows/f.yml@v2}]'
expect fail "flow mapping step" '  t:
    runs-on: x
    steps:
      - {uses: acme/build-action@v1, with: {a: b}}'
expect fail "quoted key, spaced colon" '  t:
    runs-on: x
    steps:
      - "uses" : acme/build-action@v1'
expect fail "ref containing a slash" '  t:
    runs-on: x
    steps:
      - uses: acme/build-action@release/v1'
expect fail "folded scalar fails closed" '  t:
    runs-on: x
    steps:
      - uses: >-
          acme/build-action@v1'
expect fail "job-level non-workflow path is still an action" '  t:
    uses: acme/repo/.github/workflows/my-action@v1'
expect pass "job-level reusable workflow call" '  t:
    uses: acme/repo/.github/workflows/x.yml@0123456789abcdef0123456789abcdef01234567'
expect pass "reusable call after an indentless steps block" '  a:
    runs-on: x
    steps:
    - run: echo
    timeout-minutes: 5
  b:
    uses: acme/repo/.github/workflows/x.yaml@v1'
expect pass "locked action with trailing spaces (plain and quoted)" '  t:
    runs-on: x
    steps:
      - uses: actions/checkout@v7.0.1
      - uses: "actions/checkout@v7.0.1"  '
expect fail "empty uses: value still fails closed" '  t:
    runs-on: x
    steps:
      - uses:   '
expect pass "local action and docker image" '  t:
    runs-on: x
    steps:
      - uses: ./tools/local-action
      - uses: docker://alpine:3'

if [[ "$fails" -ne 0 ]]; then
  echo "check-actions-lock self-test: $fails case(s) FAILED"
  exit 1
fi
echo "check-actions-lock self-test: OK"
