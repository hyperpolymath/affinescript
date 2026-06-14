// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for CombatResolve.affine (idaptik physical-combat
// resolution; scalar i32 ABI). The oracle re-derives the rank-keyed stomp /
// charge / knockdown bands and the body-block sign from Combat.res semantics in
// plain JS, so a codegen regression surfaces as a differential mismatch.
//
// Rank ordinals (shared with GuardNPC): 0 BasicGuard 1 SecurityGuard 2 AntiHacker
// 3 Sentinel 4 EliteGuard 5 SecurityChief 6 RivalHacker 7 Assassin. Out-of-band
// clamps to 0 (negative) or 7 (>7).

// Independent re-implementation: clamp a host integer to the 0..7 rank band.
const clampRank = (rank) => (rank < 0 ? 0 : rank > 7 ? 7 : rank);

// Combat.res update, HeadKO|BodyBounce arm: Assassin dodges, Sentinel armored,
// Chief/Elite -> elite knockdown, else normal knockdown.
const stompOutcome = (rank) => {
  const r = clampRank(rank);
  if (r === 7) return 3; // AssassinDodge
  if (r === 3) return 2; // ArmoredBounce
  if (r === 4 || r === 5) return 1; // EliteKnockdown
  return 0; // NormalKnockdown
};

// Combat.res update, Knockdown arm: Sentinel/Assassin immune, Chief/Elite ->
// elite knockdown, else normal knockdown.
const chargeOutcome = (rank) => {
  const r = clampRank(rank);
  if (r === 3 || r === 7) return 2; // Immune
  if (r === 4 || r === 5) return 1; // EliteKnockdown
  return 0; // NormalKnockdown
};

// Combat.Tuning: eliteKnockdownDuration 1.5s, normalKnockdownDuration 3.0s,
// crossing as milliseconds. Chief/Elite get the short duration.
const knockdownMs = (rank) => {
  const r = clampRank(rank);
  return r === 4 || r === 5 ? 1500 : 3000;
};

// Combat.res checkBodyBlock: player centre left of entity centre -> push left
// (-80), else push right (+80); strict `<` so equal centres push right.
const bodyBlockDir = (pc, ec) => (pc < ec ? -80 : 80);

export default {
  affine: "CombatResolve.affine",
  cases: [
    { name: "head_stomp_bounce()", export: "head_stomp_bounce", args: [], oracle: () => -180 },
    { name: "body_stomp_bounce()", export: "body_stomp_bounce", args: [], oracle: () => -250 },
    { name: "guard_stomp_bounce()", export: "guard_stomp_bounce", args: [], oracle: () => -200 },
    { name: "body_block_pushback()", export: "body_block_pushback", args: [], oracle: () => 80 },
    { name: "elite_knockdown_ms()", export: "elite_knockdown_ms", args: [], oracle: () => 1500 },
    { name: "normal_knockdown_ms()", export: "normal_knockdown_ms", args: [], oracle: () => 3000 },
    {
      name: "stomp_outcome over ranks [-2..9]",
      export: "stomp_outcome",
      args: [[-2, 9]],
      oracle: stompOutcome,
    },
    {
      name: "charge_outcome over ranks [-2..9]",
      export: "charge_outcome",
      args: [[-2, 9]],
      oracle: chargeOutcome,
    },
    {
      name: "knockdown_duration_ms over ranks [-2..9]",
      export: "knockdown_duration_ms",
      args: [[-2, 9]],
      oracle: knockdownMs,
    },
    {
      name: "body_block_direction over centres [0..6]x[0..6]",
      export: "body_block_direction",
      args: [[0, 6], [0, 6]],
      oracle: bodyBlockDir,
    },
  ],
};
