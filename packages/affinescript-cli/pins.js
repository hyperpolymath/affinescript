// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// ADR-019 / #260: the pin table. ONE compiler version + ONE sha256 per
// target, per shim release — the supply-chain rule (no floating fetch).
//
// Filled when a `v*` tag is cut: the `release.yml` (#260 S2) job
// publishes `affinescript-<target>` + `SHA256SUMS` to the GitHub
// Release. Copy each line's hex into the matching `sha256` below, set
// `version` to the tag, and bump THIS package's deno.json version in
// lockstep. Empty `sha256` ⇒ `resolveCompiler` refuses to run for that
// target (fail-closed: an unpinned binary is never executed).
//
// URL contract (must match release.yml asset names; do not rename
// without amending ADR-019): the per-target raw executable asset on the
// Release for `version`.

const REPO = "https://github.com/hyperpolymath/affinescript";

function assetUrl(version, target) {
  return `${REPO}/releases/download/${version}/affinescript-${target}`;
}

export const VERSION = "v0.1.0";

export const PINS = {
  version: VERSION,
  targets: {
    "linux-x64": {
      url: assetUrl(VERSION, "linux-x64"),
      sha256: "c1ce65308bace96669d2a178732cd5ee180845d85a5775e119a221b98fe2a5da",
    },
    "macos-x64": {
      // v0.1.0 macos-13 build was stuck in the runner queue at tag time;
      // left fail-closed (empty sha256 ⇒ resolveCompiler refuses) until
      // a follow-up release re-attempts the macos-x64 leg.
      url: assetUrl(VERSION, "macos-x64"),
      sha256: "",
    },
    "macos-arm64": {
      url: assetUrl(VERSION, "macos-arm64"),
      sha256: "2cac3ba54ae7778d31d1bd780d11b56a5cf78b5d5ee6c1d33edd3f8e753943d5",
    },
  },
};
