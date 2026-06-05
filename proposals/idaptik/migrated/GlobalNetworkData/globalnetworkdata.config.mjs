// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for GlobalNetworkData.affine. Oracle re-derives the
// hash-geometry predicates from the original GlobalNetworkData.res %raw source:
//
//   Short branch: len < 100 -> use full content (hash_is_short = 1)
//   Long branch:  len >= 100 -> length + slice(0,50) + slice(-50)
//     hash_head_len = min(len, 50)    (JS slice(0, 50) on a shorter string = content)
//     hash_tail_start = max(0, len - 50)  (JS slice(-50) = slice(len-50))
//   Negative len: treated as 0 (short branch, head=0, tail=0)

const SHORT_LIMIT = 100;
const SLICE_WIDTH = 50;

function oracleHashIsShort(len) {
  const n = len < 0 ? 0 : len;
  return n < SHORT_LIMIT ? 1 : 0;
}

function oracleHashHeadLen(len) {
  const n = len < 0 ? 0 : len;
  return n < SLICE_WIDTH ? n : SLICE_WIDTH;
}

function oracleHashTailStart(len) {
  const n = len < 0 ? 0 : len;
  const start = n - SLICE_WIDTH;
  return start < 0 ? 0 : start;
}

export default {
  affine: "GlobalNetworkData.affine",
  cases: [
    {
      name: "hash_is_short over len [-5..200]",
      export: "hash_is_short",
      args: [[-5, 200]],
      oracle: (len) => oracleHashIsShort(len) | 0,
    },
    {
      name: "hash_head_len over len [-5..200]",
      export: "hash_head_len",
      args: [[-5, 200]],
      oracle: (len) => oracleHashHeadLen(len) | 0,
    },
    {
      name: "hash_tail_start over len [-5..200]",
      export: "hash_tail_start",
      args: [[-5, 200]],
      oracle: (len) => oracleHashTailStart(len) | 0,
    },
  ],
};
