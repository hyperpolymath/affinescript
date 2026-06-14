// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VictoryScreen.affine (idaptik performance-grade
// brain; scalar i32 ABI). The oracle re-derives each function from the ORIGINAL
// VictoryScreen.res calculateGrade semantics (NOT from the .affine) so a codegen
// regression surfaces as a differential mismatch.
//
// From VictoryScreen.res calculateGrade (lines 70-107):
//   let score = ref(100)
//   score := score.contents - stats.alertLevelReached * 15
//   if stats.timeElapsedSec > 300.0 { score := score.contents - 10 }
//   if stats.timeElapsedSec > 600.0 { score := score.contents - 10 }
//   score := score.contents + stats.covertLinksDiscovered * 5
//   if stats.commandsExecuted > 100 { score := score.contents - 5 }
//   score := score.contents + stats.critSuccessCount * 3
//   score := score.contents - stats.critFailureCount * 5
//   if score >= 95 { S } else if >= 80 { A } else if >= 60 { B }
//   else if >= 40 { C } else { D }
// timeElapsedSec is a float in the .res; the host truncates to whole seconds
// before crossing, so the oracle takes an integer second count and the two
// threshold tests (> 300, > 600) coincide with the float ones on whole seconds.
// Grade variant S/A/B/C/D maps to ordinals 4/3/2/1/0 (descending switch order).

// Independent re-derivation of the score accumulator from the .res.
const timePenalty = (t) => (t > 300 ? 10 : 0) + (t > 600 ? 10 : 0);
const commandPenalty = (c) => (c > 100 ? 5 : 0);
const score = (alert, t, commands, covert, critSucc, critFail) =>
  100 - alert * 15 - timePenalty(t) + covert * 5 - commandPenalty(commands) +
  critSucc * 3 - critFail * 5;

// Independent re-derivation of the grade banding from the .res.
const grade = (s) => {
  if (s >= 95) return 4;
  if (s >= 80) return 3;
  if (s >= 60) return 2;
  if (s >= 40) return 1;
  return 0;
};
const gradeOfStats = (alert, t, commands, covert, critSucc, critFail) =>
  grade(score(alert, t, commands, covert, critSucc, critFail));

export default {
  affine: "VictoryScreen.affine",
  cases: [
    {
      name: "victory_base_score()",
      export: "victory_base_score",
      args: [],
      oracle: () => 100,
    },
    {
      name: "victory_time_penalty over t[0..900] step-swept",
      export: "victory_time_penalty",
      args: [[0, 900]],
      oracle: (t) => timePenalty(t),
    },
    {
      name: "victory_time_penalty boundary 299/300/301/600/601 + negative",
      export: "victory_time_penalty",
      args: [{ values: [-10, 0, 299, 300, 301, 599, 600, 601, 5000] }],
      oracle: (t) => timePenalty(t),
    },
    {
      name: "victory_command_penalty over c[0..200]",
      export: "victory_command_penalty",
      args: [[0, 200]],
      oracle: (c) => commandPenalty(c),
    },
    {
      name: "victory_command_penalty boundary 99/100/101 + negative",
      export: "victory_command_penalty",
      args: [{ values: [-5, 0, 99, 100, 101, 1000] }],
      oracle: (c) => commandPenalty(c),
    },
    {
      name: "victory_grade over score[-30..120]",
      export: "victory_grade",
      args: [[-30, 120]],
      oracle: (s) => grade(s),
    },
    {
      name: "victory_grade band edges 39/40/59/60/79/80/94/95",
      export: "victory_grade",
      args: [{ values: [39, 40, 59, 60, 79, 80, 94, 95, 96] }],
      oracle: (s) => grade(s),
    },
    {
      // Full score over a representative cross-product of the six stat axes.
      name: "victory_score over (alert,t,commands,covert,critSucc,critFail)",
      export: "victory_score",
      args: [
        { values: [0, 1, 2, 5] }, // alert level reached (0..5/5 in HUD, wider for safety)
        { values: [0, 250, 400, 700] }, // elapsed whole seconds straddling both thresholds
        { values: [0, 100, 150] }, // commands executed straddling the > 100 cut
        { values: [0, 3, 8] }, // covert links discovered
        { values: [0, 4 ] }, // critical successes
        { values: [0, 3 ] }, // critical failures
      ],
      oracle: (a, t, c, cv, cs, cf) => score(a, t, c, cv, cs, cf),
    },
    {
      // The composed grade-of-stats over the same axes.
      name: "victory_grade_of_stats over the six stat axes",
      export: "victory_grade_of_stats",
      args: [
        { values: [0, 1, 2, 5] },
        { values: [0, 250, 400, 700] },
        { values: [0, 100, 150] },
        { values: [0, 3, 8] },
        { values: [0, 4 ] },
        { values: [0, 3 ] },
      ],
      oracle: (a, t, c, cv, cs, cf) => gradeOfStats(a, t, c, cv, cs, cf),
    },
  ],
};
