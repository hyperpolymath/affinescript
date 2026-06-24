// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for JessicaLoadout.affine (idaptik loadout access
// arithmetic; scalar i32 ABI). The oracle re-derives each function from the
// original JessicaLoadout.res / JessicaLoadoutCoprocessorBridge.js semantics so
// a codegen regression surfaces as a differential mismatch.
//
// Weapon encoding (allWeapons order):
//   0 StunPistol  1 TacticalBaton  2 TranqRifle
//   3 BreachingShotgun  4 Railgun  5 EMPPistol
// Tool encoding (allTools order):
//   0 LockpickSet  1 GrapplingHook  2 MotionSensor  3 SignalJammer
//   4 TraumaKit    5 C4Charge       6 GhillieWrap   7 FieldSurgeryKit
// Consumable encoding (allConsumables order):
//   0 Flashbang  1 SmokeGrenade  2 ZipTies
//   3 AdrenalineShot  4 DecoyPhone  5 FibreTap
// Background encoding (JessicaBackground.all order):
//   0 Assault  1 Recon  2 Engineer  3 Signals  4 Medic  5 Logistics

// weapon_tier from JessicaLoadout.res getWeaponInfo tiers:
//   0 StunPistol T1=1, 1 TacticalBaton T1=1, 2 TranqRifle T2=2,
//   3 BreachingShotgun T2=2, 4 Railgun T3=3, 5 EMPPistol T3=3; off-domain 0
const weaponTier = (w) => {
  if (w === 0) return 1;
  if (w === 1) return 1;
  if (w === 2) return 2;
  if (w === 3) return 2;
  if (w === 4) return 3;
  if (w === 5) return 3;
  return 0;
};

// tool_tier from JessicaLoadout.res getToolInfo tiers:
//   0 LockpickSet T1=1, 1 GrapplingHook T1=1,
//   2 MotionSensor T2=2, 3 SignalJammer T2=2, 4 TraumaKit T2=2, 5 C4Charge T2=2,
//   6 GhillieWrap T3=3, 7 FieldSurgeryKit T3=3; off-domain 0
const toolTier = (t) => {
  if (t === 0) return 1;
  if (t === 1) return 1;
  if (t === 2) return 2;
  if (t === 3) return 2;
  if (t === 4) return 2;
  if (t === 5) return 2;
  if (t === 6) return 3;
  if (t === 7) return 3;
  return 0;
};

// consumable_uses from JessicaLoadout.res getConsumableInfo:
//   0 Flashbang=2, 1 SmokeGrenade=2, 2 ZipTies=3,
//   3 AdrenalineShot=1, 4 DecoyPhone=2, 5 FibreTap=1; off-domain 0
const consumableUses = (c) => {
  if (c === 0) return 2;
  if (c === 1) return 2;
  if (c === 2) return 3;
  if (c === 3) return 1;
  if (c === 4) return 2;
  if (c === 5) return 1;
  return 0;
};

// can_use_weapon from JessicaLoadout.res canUseWeapon:
//   T1 (0,1): always 1 regardless of background
//   2 TranqRifle: Assault(0) or Recon(1) => 1, else 0
//   3 BreachingShotgun: Assault(0) or Engineer(2) => 1, else 0
//   4 Railgun: Assault(0) => 1, else 0
//   5 EMPPistol: Engineer(2) or Signals(3) => 1, else 0
//   off-domain weapon or background: 0
const canUseWeapon = (w, bg) => {
  if (w === 0) return 1;
  if (w === 1) return 1;
  if (w === 2) return bg === 0 || bg === 1 ? 1 : 0;
  if (w === 3) return bg === 0 || bg === 2 ? 1 : 0;
  if (w === 4) return bg === 0 ? 1 : 0;
  if (w === 5) return bg === 2 || bg === 3 ? 1 : 0;
  return 0;
};

// can_use_tool from JessicaLoadout.res canUseTool:
//   T1 (0,1): always 1
//   2 MotionSensor: Recon(1)
//   3 SignalJammer: Signals(3)
//   4 TraumaKit: Medic(4)
//   5 C4Charge: Engineer(2)
//   6 GhillieWrap: Recon(1)
//   7 FieldSurgeryKit: Medic(4)
//   off-domain: 0
const canUseTool = (t, bg) => {
  if (t === 0) return 1;
  if (t === 1) return 1;
  if (t === 2) return bg === 1 ? 1 : 0;
  if (t === 3) return bg === 3 ? 1 : 0;
  if (t === 4) return bg === 4 ? 1 : 0;
  if (t === 5) return bg === 2 ? 1 : 0;
  if (t === 6) return bg === 1 ? 1 : 0;
  if (t === 7) return bg === 4 ? 1 : 0;
  return 0;
};

// use_consumable from JessicaLoadout.res useConsumable:
//   usesLeft > 0 => usesLeft - 1; else 0
const useConsumable = (usesLeft) => (usesLeft > 0 ? usesLeft - 1 : 0);

export default {
  affine: "JessicaLoadout.affine",
  cases: [
    {
      name: "weapon_tier over [-2..8]",
      export: "weapon_tier",
      args: [[-2, 8]],
      oracle: (w) => weaponTier(w),
    },
    {
      name: "tool_tier over [-2..10]",
      export: "tool_tier",
      args: [[-2, 10]],
      oracle: (t) => toolTier(t),
    },
    {
      name: "consumable_uses over [-2..8]",
      export: "consumable_uses",
      args: [[-2, 8]],
      oracle: (c) => consumableUses(c),
    },
    {
      name: "can_use_weapon over weapon[-2..8] × bg[-1..7]",
      export: "can_use_weapon",
      args: [[-2, 8], [-1, 7]],
      oracle: (w, bg) => canUseWeapon(w, bg),
    },
    {
      name: "can_use_tool over tool[-2..10] × bg[-1..7]",
      export: "can_use_tool",
      args: [[-2, 10], [-1, 7]],
      oracle: (t, bg) => canUseTool(t, bg),
    },
    {
      name: "use_consumable over [-3..10]",
      export: "use_consumable",
      args: [[-3, 10]],
      oracle: (u) => useConsumable(u),
    },
  ],
};
