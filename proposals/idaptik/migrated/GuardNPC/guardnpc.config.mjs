// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for GuardNPC.affine (idaptik guard-NPC decision brain;
// scalar i32 ABI, every export Int-in/Int-out). Each oracle is an INDEPENDENT JS
// reimplementation re-derived from src/app/enemies/GuardNPC.res semantics (NOT
// from the .affine), so a codegen or re-decomposition regression surfaces as a
// differential mismatch. Float operands are milli-units (round(value * 1000)),
// matching the GuardNPC.affine header contract.

// --- shared oracle helpers (re-derived from GuardNPC.res) -------------------

// guardState ordinal band: 18 valid codes 0..17, out-of-band clamps to 0 or 17.
const clampState = (c) => (c < 0 ? 0 : c > 17 ? 17 : c);
// guardRank ordinal band: 8 valid codes 0..7, out-of-band clamps to 0 or 7.
const clampRank = (c) => (c < 0 ? 0 : c > 7 ? 7 : c);

export default {
  affine: "GuardNPC.affine",
  cases: [
    // --- state taxonomy ----------------------------------------------------
    {
      name: "state_count()",
      export: "state_count",
      args: [],
      oracle: () => 18,
    },
    {
      name: "state_value over [-3..20]",
      export: "state_value",
      args: [[-3, 20]],
      oracle: (c) => clampState(c),
    },
    // --- rank taxonomy -----------------------------------------------------
    {
      name: "rank_count()",
      export: "rank_count",
      args: [],
      oracle: () => 8,
    },
    {
      name: "rank_value over [-3..11]",
      export: "rank_value",
      args: [[-3, 11]],
      oracle: (c) => clampRank(c),
    },
    // --- detection ladder --------------------------------------------------
    {
      // detectPlayer: effectiveRange = crouching ? range*0.6 : range.
      // milli, integer division (floor for the non-negative operands here).
      name: "effective_range_milli(range_milli {0,150000,220000,300000}, crouching {0,1})",
      export: "effective_range_milli",
      args: [{ values: [0, 150000, 220000, 300000] }, { values: [0, 1] }],
      oracle: (range_milli, crouching) =>
        crouching === 1 ? Math.trunc((range_milli * 600) / 1000) : range_milli,
    },
    {
      // detectPlayer after the cone hit: NotDetected(0)/Peripheral(1)/Full(2).
      // proximityFactor = 1 - distance/effRange; >0.6 == distance < effRange*0.4.
      name: "detection_class(distance_milli, effective_range_milli)",
      export: "detection_class",
      args: [
        { values: [0, 20000, 50000, 60000, 80000, 90000, 120000, 200000, 300000] },
        { values: [0, 90000, 150000, 220000] },
      ],
      oracle: (distance_milli, eff) => {
        if (eff <= 0) return 0;
        if (distance_milli > eff) return 0;
        if (distance_milli < Math.trunc((eff * 400) / 1000)) return 2;
        return 1;
      },
    },
    // --- per-rank suspicion thresholds / decay / pursuit -------------------
    {
      // detectPlayer Peripheral threshold: 0.85/0.65/0.5/0.6/0.7 by rank, milli.
      name: "peripheral_threshold_milli over rank [-1..8]",
      export: "peripheral_threshold_milli",
      args: [[-1, 8]],
      oracle: (rank) => {
        const r = clampRank(rank);
        if (r === 0) return 850;
        if (r === 1) return 650;
        if (r === 4) return 500;
        if (r === 5) return 600;
        return 700;
      },
    },
    {
      // detectPlayer NotDetected decay: 0.3/0.1/0.2 by rank, milli.
      name: "decay_rate_milli over rank [-1..8]",
      export: "decay_rate_milli",
      args: [[-1, 8]],
      oracle: (rank) => {
        const r = clampRank(rank);
        if (r === 0) return 300;
        if (r === 4) return 100;
        return 200;
      },
    },
    {
      // updatePatrol Investigating speedMult: 1.2/1.8/1.5 by rank, milli.
      name: "investigate_speed_mult_milli over rank [-1..8]",
      export: "investigate_speed_mult_milli",
      args: [[-1, 8]],
      oracle: (rank) => {
        const r = clampRank(rank);
        if (r === 0) return 1200;
        if (r === 4) return 1800;
        return 1500;
      },
    },
    {
      // updatePatrol Alerted speedMult: 1.5/2.0/2.5/1.8/1.5 by rank, milli.
      name: "alerted_speed_mult_milli over rank [-1..8]",
      export: "alerted_speed_mult_milli",
      args: [[-1, 8]],
      oracle: (rank) => {
        const r = clampRank(rank);
        if (r === 0) return 1500;
        if (r === 1) return 2000;
        if (r === 4) return 2500;
        if (r === 5) return 1800;
        return 1500;
      },
    },
    // --- anti-hacker psychology -------------------------------------------
    {
      // updateAntiHacker: courage = clamp(0,1, 0.3 +0.4(backup) -0.15(alert>=4)
      // -0.2(dist<120px)). milli; dist<120px == dist_milli<120000.
      name: "courage_milli(backup {0,1}, alert_level {0,3,4,5}, player_dist_milli)",
      export: "courage_milli",
      args: [
        { values: [0, 1] },
        { values: [0, 3, 4, 5] },
        { values: [0, 50000, 119999, 120000, 200000] },
      ],
      oracle: (backup, alert_level, player_dist_milli) => {
        let c = 300;
        if (backup === 1) c += 400;
        if (alert_level >= 4) c -= 150;
        if (player_dist_milli < 120000) c -= 200;
        if (c < 0) return 0;
        if (c > 1000) return 1000;
        return c;
      },
    },
    {
      // updateAntiHacker panic check: courage < panicThreshold (milli).
      name: "should_panic(courage_milli, panic_threshold_milli {250})",
      export: "should_panic",
      args: [{ values: [0, 100, 249, 250, 251, 500, 1000] }, { values: [250 ] }],
      oracle: (courage_milli, panic_threshold_milli) =>
        courage_milli < panic_threshold_milli ? 1 : 0,
    },
    {
      // detectPlayer FullDetection AntiHacker arm: panic(6) / callBackup(8) / hold(-1).
      name: "antihacker_full_detection_state(courage, threshold {250}, has_radio {0,1}, backup {0,1})",
      export: "antihacker_full_detection_state",
      args: [
        { values: [100, 250, 400] },
        { values: [250] },
        { values: [0, 1] },
        { values: [0, 1] },
      ],
      oracle: (courage, threshold, has_radio, backup) => {
        if (courage < threshold) return 6;
        if (has_radio === 1 && backup === 0) return 8;
        return -1;
      },
    },
    {
      // detectPlayer FullDetection switch (non-AntiHacker): Basic->Investigating(1),
      // AntiHacker->routed elsewhere(-1), Chief->Commanding(9), default Alerted(2).
      name: "full_detection_state_for_rank over rank [-1..8]",
      export: "full_detection_state_for_rank",
      args: [[-1, 8]],
      oracle: (rank) => {
        const r = clampRank(rank);
        if (r === 0) return 1;
        if (r === 2) return -1;
        if (r === 5) return 9;
        return 2;
      },
    },
    // --- assassin spawn + traps -------------------------------------------
    {
      // shouldSpawnAssassin: missionDifficulty >= 3 (Hard+).
      name: "should_spawn_assassin over difficulty [0..6]",
      export: "should_spawn_assassin",
      args: [[0, 6]],
      oracle: (difficulty) => (difficulty >= 3 ? 1 : 0),
    },
    {
      // updateAssassin SettingTrap: mod(trapCount, 5) selects trap type 0..4.
      name: "trap_kind_for over trap_count [0..12]",
      export: "trap_kind_for",
      args: [[0, 12]],
      oracle: (trap_count) => ((trap_count % 5) + 5) % 5,
    },
    {
      // checkPlayerTrapCollision: per-kind distance bands. milli; strict `<`.
      // 0 None, 1 Trip, 2 Lethal, 3 Spotted, 4 Darkness, 5 EMP.
      name: "trap_effect(trap_kind {0..4}, dist_milli, sprinting {0,1})",
      export: "trap_effect",
      args: [
        { values: [0, 1, 2, 3, 4] },
        { values: [0, 14999, 15000, 19999, 20000, 24999, 25000, 29999, 30000, 50000] },
        { values: [0, 1] },
      ],
      oracle: (trap_kind, dist_milli, sprinting) => {
        if (trap_kind === 0) return dist_milli < 20000 ? 1 : 0;
        if (trap_kind === 1) {
          if (dist_milli < 15000) return sprinting === 1 ? 2 : 3;
          return 0;
        }
        if (trap_kind === 2) return dist_milli < 30000 ? 4 : 0;
        if (trap_kind === 4) return dist_milli < 25000 ? 5 : 0;
        return 0; // LockedDoor (3) is environmental, never proximity-triggered
      },
    },
    // --- misc integer machinery -------------------------------------------
    {
      // updatePatrol / updateAssassin: mod(current+1, len); len<=0 -> 0.
      name: "next_waypoint(current {0..5}, len {0,1,3,4})",
      export: "next_waypoint",
      args: [{ values: [0, 1, 2, 3, 4, 5] }, { values: [0, 1, 3, 4] }],
      oracle: (current, len) => (len <= 0 ? 0 : (current + 1) % len),
    },
    {
      name: "increment_counter over [0..20]",
      export: "increment_counter",
      args: [[0, 20]],
      oracle: (count) => count + 1,
    },
  ],
};
