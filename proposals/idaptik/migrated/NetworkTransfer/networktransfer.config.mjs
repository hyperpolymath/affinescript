// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for NetworkTransfer.affine. Oracle re-derives the
// byte-transfer arithmetic from NetworkTransfer.res / NetworkTransferCoprocessor.res:
//
//   frame_bytes(rate_mbps, dt, bytes_per_mb) -> rate_mbps * bytes_per_mb * dt
//       Host passes bytes_per_mb=1000000.
//   step_transferred(transferred, dt, rate_mbps, total_bytes, bytes_per_mb) ->
//       min(transferred + frame_bytes, total_bytes). Host passes bytes_per_mb=1000000.
//   progress(transferred, total_bytes) -> transferred / total_bytes
//       NOTE: total_bytes=0 is excluded (division by zero; host guards this).
//   is_complete(transferred, total_bytes) -> transferred >= total_bytes ? 1 : 0
//
// NOTE: parity harness normalises to i32. Float kernels only match when the
// computed float, cast to i32, equals the oracle cast to i32. We use integer
// inputs so both sides compute exactly the same IEEE-754 result.

function oracleFrameBytes(rate_mbps, dt, bytes_per_mb) {
  return rate_mbps * bytes_per_mb * dt;
}
function oracleStepTransferred(transferred, dt, rate_mbps, total_bytes, bytes_per_mb) {
  const advanced = transferred + oracleFrameBytes(rate_mbps, dt, bytes_per_mb);
  return advanced > total_bytes ? total_bytes : advanced;
}
function oracleProgress(transferred, total_bytes) {
  // total_bytes=0 excluded from parity (division-by-zero; host guards)
  return transferred / total_bytes;
}
function oracleIsComplete(transferred, total_bytes) {
  return transferred >= total_bytes ? 1 : 0;
}

export default {
  affine: "NetworkTransfer.affine",
  cases: [
    {
      name: "frame_bytes: rate 0..5 (mbps integer), dt 0..3 (integer seconds), bytes_per_mb=1000000",
      export: "frame_bytes",
      args: [[0, 5], [0, 3], { values: [1000000] }],
      oracle: (r, d, bpm) => oracleFrameBytes(r, d, bpm) | 0,
    },
    {
      name: "step_transferred: transferred, dt=1, rate=1mbps, total, bytes_per_mb=1000000 (spot check)",
      export: "step_transferred",
      args: [
        { values: [0, 500000, 1000000, 1500000, 2000000] },
        { values: [1] },
        { values: [1] },
        { values: [0, 500000, 1000000, 1500000, 2000000] },
        { values: [1000000] },
      ],
      oracle: (t, d, r, tot, bpm) => oracleStepTransferred(t, d, r, tot, bpm) | 0,
    },
    {
      name: "progress: transferred 1..5, total 1..5 (total_bytes=0 excluded; host guards)",
      export: "progress",
      args: [
        { values: [1, 2, 3, 4, 5] },
        { values: [1, 2, 3, 4, 5] },
      ],
      oracle: (t, tot) => oracleProgress(t, tot) | 0,
    },
    {
      name: "is_complete: transferred 0..5, total 0..5",
      export: "is_complete",
      args: [
        { values: [0, 1, 2, 3, 4, 5] },
        { values: [0, 1, 2, 3, 4, 5] },
      ],
      oracle: (t, tot) => oracleIsComplete(t, tot) | 0,
    },
  ],
};
