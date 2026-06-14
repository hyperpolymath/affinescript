// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PlayTimeSplit.affine (idaptik save-slot play-time
// decomposition; scalar i32 ABI). The oracle re-derives the hour/minute split
// of SaveGame.formatPlayTime in plain JS, keyed by the whole-minute count, so a
// codegen regression in integer `/` or `%` surfaces as a differential mismatch.
//
// Brain contract: input is total whole minutes (the host has already done the
// Float `trunc(seconds / 60)`); output is the integer hours/minutes components
// and the caption-branch flag. A negative count is not a valid play-time and
// rejects with the out-of-band sentinel -1 (components) / 0 (flags).

// Independent re-implementation of the formatPlayTime integer core.
const valid = (m) => m >= 0;
// hours = total_minutes / 60, truncating toward zero (JS Math.trunc; for the
// non-negative in-band domain this equals plain integer division).
const hours = (m) => (valid(m) ? Math.trunc(m / 60) : -1);
// minutes = total_minutes % 60 (JS remainder; 0..59 for non-negative m).
const minutes = (m) => (valid(m) ? m % 60 : -1);
// caption branch: show hours iff hours > 0.
const hasHours = (m) => (valid(m) && Math.trunc(m / 60) > 0 ? 1 : 0);

export default {
  affine: "PlayTimeSplit.affine",
  cases: [
    {
      name: "minutes_per_hour()",
      export: "minutes_per_hour",
      args: [],
      oracle: () => 60,
    },
    {
      name: "play_time_minutes_valid over [-5..200]",
      export: "play_time_minutes_valid",
      args: [[-5, 200]],
      oracle: (m) => (valid(m) ? 1 : 0),
    },
    {
      // Sweep across several hours plus the negative-reject band. 605 minutes
      // exercises the 10h05m save-slot caption; -5..-1 exercises the sentinel.
      name: "play_time_hours over [-5..605]",
      export: "play_time_hours",
      args: [[-5, 605]],
      oracle: hours,
    },
    {
      name: "play_time_minutes over [-5..605]",
      export: "play_time_minutes",
      args: [[-5, 605]],
      oracle: minutes,
    },
    {
      name: "play_time_has_hours over [-5..605]",
      export: "play_time_has_hours",
      args: [[-5, 605]],
      oracle: hasHours,
    },
  ],
};
