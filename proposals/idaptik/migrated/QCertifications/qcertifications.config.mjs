// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for QCertifications.affine (idaptik cert-tier arithmetic;
// scalar i32 ABI). The oracle re-derives each function from the original
// QCertifications.res / QCertificationsCoprocessor.res semantics so a codegen
// regression surfaces as a differential mismatch.
//
// Source semantics (from QCertifications.res):
//   tierValue:  Practitioner=1 .. Sovereign=5; off-domain -> 0
//   tierColor:  fixed palette per tier (0..5); off-domain -> 0
//   nextTier:   1->2, 2->3, 3->4, 4->5, 5->0, off-domain->0
//   meetsTier:  tierValue(tier) >= tierValue(minTier) ? 1 : 0
//   isCyberwarUnlocked: expertCount >= 3 ? 1 : 0
//
// progress_total and progress_crosses take Float args; the scalar-i32 ABI cannot
// sweep float inputs via parity.mjs's range expander (all inputs are i32), so
// those two exports are omitted from the parity sweep per the ABI scope note in
// parity.mjs. The Int-only exports cover all arithmetic correctness.

// Re-derive tierValue from ReScript semantics (not from the .affine).
const tierValue = (t) => {
  if (t === 1) return 1;
  if (t === 2) return 2;
  if (t === 3) return 3;
  if (t === 4) return 4;
  if (t === 5) return 5;
  return 0;
};

// Re-derive tierColor from ReScript switch: the exact 0x-literal values from
// QCertifications.res:
//   Practitioner 0x888888 = 8947848
//   Professional 0x44aaff = 4500223
//   Expert       0xffaa44 = 16755268
//   Architect    0xff4488 = 16729224
//   Sovereign    0xff0000 = 16711680
// off-domain -> 0
const tierColor = (t) => {
  if (t === 1) return 8947848;
  if (t === 2) return 4500223;
  if (t === 3) return 16755268;
  if (t === 4) return 16729224;
  if (t === 5) return 16711680;
  return 0;
};

// Re-derive nextTier from ReScript addProgress nextTier switch:
//   Practitioner -> Professional (1->2), ..., Architect->Sovereign (4->5)
//   Sovereign -> None => 0; off-domain -> 0
const nextTier = (t) => {
  if (t === 1) return 2;
  if (t === 2) return 3;
  if (t === 3) return 4;
  if (t === 4) return 5;
  return 0; // Sovereign and off-domain
};

// Re-derive meetsTier from ReScript countAtTier:
//   tierValue(tier) >= tierValue(minTier) ? 1 : 0
const meetsTier = (tier, minTier) =>
  tierValue(tier) >= tierValue(minTier) ? 1 : 0;

// Re-derive isCyberwarUnlocked from ReScript canProgressCyberWar:
//   expertCount >= 3 => true
const isCyberwarUnlocked = (expertCount) => (expertCount >= 3 ? 1 : 0);

export default {
  affine: "QCertifications.affine",
  cases: [
    {
      name: "tier_value over [-2..7]",
      export: "tier_value",
      args: [[-2, 7]],
      oracle: (t) => tierValue(t),
    },
    {
      name: "tier_color over [-2..7]",
      export: "tier_color",
      args: [[-2, 7]],
      oracle: (t) => tierColor(t),
    },
    {
      name: "next_tier over [-2..7]",
      export: "next_tier",
      args: [[-2, 7]],
      oracle: (t) => nextTier(t),
    },
    {
      name: "meets_tier over tier×minTier [-1..6]×[-1..6]",
      export: "meets_tier",
      args: [[-1, 6], [-1, 6]],
      oracle: (tier, minTier) => meetsTier(tier, minTier),
    },
    {
      name: "is_cyberwar_unlocked over [0..6]",
      export: "is_cyberwar_unlocked",
      args: [[0, 6]],
      oracle: (ec) => isCyberwarUnlocked(ec),
    },
  ],
};
