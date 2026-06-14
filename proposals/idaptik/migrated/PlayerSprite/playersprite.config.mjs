// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PlayerSprite.affine (idaptik player animation
// decisions; scalar i32 ABI). The oracle re-derives each kernel independently
// from the original PlayerSprite.res semantics (frameCount / animSpeed /
// stateToAnim switches and the facingScaleX call convention) so a codegen
// regression surfaces as a differential mismatch.
//
// Source semantics (PlayerSprite.res):
//   animationId ordinals (animOrdinal): Idle=0 Walk=1 Jump=2 Interact=3
//     Crouch=4 Stunned=5 ChargingJump=6 Sprint=7
//   frameCount:  Idle=4 Walk=8 Jump=4 Interact=4 Crouch=2 Stunned=4
//                ChargingJump=2 Sprint=8 ; off-domain -> 0
//   animSpeed:   Idle=.05 Walk=.15 Jump=.10 Interact=.12 Crouch=.04
//                Stunned=.20 ChargingJump=.08 Sprint=.20 (x1000 -> milli);
//                off-domain -> -1
//   stateToAnim (visualState ord -> animationId ord):
//                Idle(0)->Idle(0)  Walking(1)->Walk(1)  Sprinting(2)->Sprint(7)
//                Crouching(3)->Crouch(4)  Jumping(4)->Jump(2)
//                ChargingJump(5)->ChargingJump(6) ; off-domain -> -1
//   facingScaleX: call passes 0 (left)-> -1.0, else 1.0 (x1000 -> milli)

// Independent re-derivation of frameCount over animationId ordinals.
const frameCount = (a) => {
  switch (a) {
    case 0: return 4; // Idle
    case 1: return 8; // Walk
    case 2: return 4; // Jump
    case 3: return 4; // Interact
    case 4: return 2; // Crouch
    case 5: return 4; // Stunned
    case 6: return 2; // ChargingJump
    case 7: return 8; // Sprint
    default: return 0;
  }
};

// Independent re-derivation of animSpeed (as milli-units, x1000).
const animSpeedMilli = (a) => {
  const speeds = { 0: 50, 1: 150, 2: 100, 3: 120, 4: 40, 5: 200, 6: 80, 7: 200 };
  return Object.prototype.hasOwnProperty.call(speeds, a) ? speeds[a] : -1;
};

// Independent re-derivation of stateToAnim (visualState ord -> animationId ord).
const stateToAnim = (s) => {
  const map = { 0: 0, 1: 1, 2: 7, 3: 4, 4: 2, 5: 6 };
  return Object.prototype.hasOwnProperty.call(map, s) ? map[s] : -1;
};

// Independent re-derivation of facingScaleX (milli-units): 0 -> -1000 else 1000.
const facingScaleMilli = (isLeft) => (isLeft === 0 ? -1000 : 1000);

// Independent re-derivation of the validity guard for animationId.
const isValidAnim = (a) => (a >= 0 && a <= 7 ? 1 : 0);

export default {
  affine: "PlayerSprite.affine",
  cases: [
    {
      name: "frame_count over [-3..11]",
      export: "frame_count",
      args: [[-3, 11]],
      oracle: (a) => frameCount(a),
    },
    {
      name: "anim_speed_milli over [-3..11]",
      export: "anim_speed_milli",
      args: [[-3, 11]],
      oracle: (a) => animSpeedMilli(a),
    },
    {
      name: "state_to_anim over [-3..9]",
      export: "state_to_anim",
      args: [[-3, 9]],
      oracle: (s) => stateToAnim(s),
    },
    {
      name: "facing_scale_milli over [-2..3]",
      export: "facing_scale_milli",
      args: [[-2, 3]],
      oracle: (isLeft) => facingScaleMilli(isLeft),
    },
    {
      name: "is_valid_anim over [-3..11]",
      export: "is_valid_anim",
      args: [[-3, 11]],
      oracle: (a) => isValidAnim(a),
    },
  ],
};
