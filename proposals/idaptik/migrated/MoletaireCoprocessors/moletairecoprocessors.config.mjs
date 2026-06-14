// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for MoletaireCoprocessors.affine (idaptik companion
// coprocessor-effect kernel; scalar i32 ABI). Each oracle re-derives the rule
// straight from the ReScript source / the host binding contract in plain JS,
// INDEPENDENTLY of the .affine, so a codegen regression surfaces as a differential
// mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine:
//   level_value(l)               = clamp(l, 0, 3)                  (levelValue, res:41-47)
//   level_colour(l)              = palette[clamp(l,0,3)]           (levelColor, res:59-65)
//   next_level(l)                = (0<=l<=2) ? l+1 : -1            (upgrade, res:171-186)
//   audio_sound_count(l)         = {1:3,2:6,3:10}[l] ?? 0          (audioSoundCount, res:222-238)
//   sensor_range(l)              = {1:3,2:5,3:8}[l] ?? 1           (sensorRange, res:275-291)
//   can_mimic_voice(l)           = l === 3 ? 1 : 0                 (canMimicVoice, res:240-251)
//   can_detect_vault_weak_points = l === 3 ? 1 : 0                 (canDetectVaultWeakPoints, res:295-305)
//   can_carry_fragile(l)         = l >= 2 ? 1 : 0                  (canCarryFragile, res:370-371)
//   vibration_quality(l)         = (0<=l<=3) ? l : 0              (vibrationQuality+vibrationOfBand, res:319-346)

// Level magnitude: clamp onto the closed 0..3 band (Stock..Overclocked).
const levelValue = (l) => (l < 0 ? 0 : l > 3 ? 3 : l);
// Badge palette, keyed by the canonicalised level (res:59-65 hex, decimal here).
const PALETTE = [0x555555, 0x44aa44, 0x4488ff, 0xff8844];
const levelColour = (l) => PALETTE[levelValue(l)];
// Upgrade step: advance on the 0..2 band, out-of-band sentinel -1 at/above Overclocked.
const nextLevel = (l) => (l >= 0 && l <= 2 ? l + 1 : -1);
// AudioSynthesiser sound count; fails safe to Stock (0) off-band.
const audioSoundCount = (l) => (l === 1 ? 3 : l === 2 ? 6 : l === 3 ? 10 : 0);
// SignalProcessor sensor range; fails safe to Stock (1) off-band.
const sensorRange = (l) => (l === 1 ? 3 : l === 2 ? 5 : l === 3 ? 8 : 1);
// Voice mimicry: Overclocked-only equality gate.
const canMimicVoice = (l) => (l === 3 ? 1 : 0);
// Vault weak-point detection: Overclocked-only equality gate.
const canDetectVaultWeakPoints = (l) => (l === 3 ? 1 : 0);
// Fragile-carry: the one inequality gate -- Enhanced(2) or above.
const canCarryFragile = (l) => (l >= 2 ? 1 : 0);
// Vibration reading band == level band over 0..3; off-band -> NoData (0).
const vibrationQuality = (l) => (l >= 0 && l <= 3 ? l : 0);

// Sweep [-2..6] so both out-of-band guards (negative and above-Overclocked) and
// all four in-band levels (Stock/Basic/Enhanced/Overclocked) fire on every export.
const LEVEL = [-2, 6];

export default {
  affine: "MoletaireCoprocessors.affine",
  cases: [
    { name: "level_count()", export: "level_count", args: [], oracle: () => 4 },
    { name: "level_value over [-2..6]", export: "level_value", args: [LEVEL], oracle: levelValue },
    { name: "level_colour over [-2..6]", export: "level_colour", args: [LEVEL], oracle: levelColour },
    { name: "next_level over [-2..6]", export: "next_level", args: [LEVEL], oracle: nextLevel },
    { name: "audio_sound_count over [-2..6]", export: "audio_sound_count", args: [LEVEL], oracle: audioSoundCount },
    { name: "sensor_range over [-2..6]", export: "sensor_range", args: [LEVEL], oracle: sensorRange },
    { name: "can_mimic_voice over [-2..6]", export: "can_mimic_voice", args: [LEVEL], oracle: canMimicVoice },
    { name: "can_detect_vault_weak_points over [-2..6]", export: "can_detect_vault_weak_points", args: [LEVEL], oracle: canDetectVaultWeakPoints },
    { name: "can_carry_fragile over [-2..6]", export: "can_carry_fragile", args: [LEVEL], oracle: canCarryFragile },
    { name: "vibration_quality over [-2..6]", export: "vibration_quality", args: [LEVEL], oracle: vibrationQuality },
  ],
};
