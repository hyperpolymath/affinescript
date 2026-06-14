// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for MoletaireHunger.affine (idaptik companion hunger-
// classification kernel; scalar i32 ABI; hunger in milli-units). Each oracle
// re-derives the rule straight from the ReScript source / the host binding
// contract in plain JS, INDEPENDENTLY of the .affine, so a codegen regression
// surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine.
// Thresholds in milli-units: peckish/hungry 400, starving 800, objective-eat 900;
// display-only cuts 100 and 600.
//   behaviour(h,e)        = h>800 ? (e?2:3) : h>400 ? 1 : 0     (getBehaviour, res:241-253)
//   display_band(h)       = h<100?0 : h<400?1 : h<600?2 : h<800?3 : h<900?4 : 5  (res:256-270)
//   hunger_colour(h)      = h<400?G : h<600?YG : h<800?O : R      (hungerColor, res:273-283)
//   will_eat_objective(h) = h>900 ? 1 : 0                         (willEatObjective, res:293-294)
//   objective_at_risk(h)  = h>800 ? 1 : 0                         (objectiveAtRisk, res:298-299)

// Discrete hunger state; STRICT boundaries faithful to the .res `>` chain.
const behaviour = (h, e) => (h > 800 ? (e !== 0 ? 2 : 3) : h > 400 ? 1 : 0);
// HUD band index 0..5; STRICT-less-than cuts faithful to the .res `<` chain.
const displayBand = (h) =>
  h < 100 ? 0 : h < 400 ? 1 : h < 600 ? 2 : h < 800 ? 3 : h < 900 ? 4 : 5;
// Indicator colour (res hex, decimal here); STRICT-less-than cuts.
const hungerColour = (h) =>
  h < 400 ? 0x44ff44 : h < 600 ? 0xaaff44 : h < 800 ? 0xffaa44 : 0xff4444;
// Objective gets eaten above the 900 cut; STRICT-greater.
const willEatObjective = (h) => (h > 900 ? 1 : 0);
// Objective flagged at risk above the 800 cut; STRICT-greater.
const objectiveAtRisk = (h) => (h > 800 ? 1 : 0);

// Sweep hunger over the exact band edges + just-inside/just-outside each cut, plus
// the full 0..1000 milli-unit range and out-of-band negatives/overflow, so every
// branch and every boundary (99/100, 399/400, 599/600, 799/800, 899/900) fires.
const HUNGER = [
  -50, 0, 99, 100, 101, 399, 400, 401, 599, 600, 601, 799, 800, 801, 899, 900, 901, 1000, 1500,
];
// Edible-present flag over {0,1,2}: tests the 0 branch + the "any non-zero" truthiness.
const EDIBLE = [0, 1, 2];

export default {
  affine: "MoletaireHunger.affine",
  cases: [
    {
      name: "behaviour over hunger x edible-flag",
      export: "behaviour",
      args: [{ values: HUNGER }, { values: EDIBLE }],
      oracle: behaviour,
    },
    { name: "display_band over hunger band edges", export: "display_band", args: [{ values: HUNGER }], oracle: displayBand },
    { name: "hunger_colour over hunger band edges", export: "hunger_colour", args: [{ values: HUNGER }], oracle: hungerColour },
    { name: "will_eat_objective over hunger band edges", export: "will_eat_objective", args: [{ values: HUNGER }], oracle: willEatObjective },
    { name: "objective_at_risk over hunger band edges", export: "objective_at_risk", args: [{ values: HUNGER }], oracle: objectiveAtRisk },
  ],
};
