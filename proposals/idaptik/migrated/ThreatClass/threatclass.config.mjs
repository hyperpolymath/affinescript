// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for ThreatClass.affine (idaptik detection-source threat
// classifier; scalar i32 ABI). The oracle re-derives the classifyThreat map from
// DualAlertBridge.res semantics in plain JS, keyed by the DetectionSystem.res
// variant ordinal, so a codegen regression surfaces as a differential mismatch.
//
// Source ordinals (DetectionSystem.res variant order):
//   0 GuardSight 1 GuardHearing 2 DogDetection 3 DogHearing 4 CameraMotion
//   5 CanaryTripped 6 PortScanDetected 7 CrackFailed 8 FirewallBlock
//   9 CovertLinkProbe 10 DistractionExpired 11 ContactDamage 12 ManualAlert
// Out-of-band clamps to 0 (negative) or 12 (>12).
//
// Threat band: 0 Physical, 1 Cyber, 2 Both.

const clampSource = (s) => (s < 0 ? 0 : s > 12 ? 12 : s);

// Independent re-implementation of DualAlertBridge.res classifyThreat, by source.
// Physical sources: guards (sight 0, hearing 1), dogs (detection 2, hearing 3),
//                   distraction-expired 10, contact-damage 11.
// Cyber sources:    canary 5, port-scan 6, crack-failed 7, firewall 8, link 9.
// Both sources:     camera-motion 4, manual-alert 12.
const PHYSICAL = new Set([0, 1, 2, 3, 10, 11]);
const CYBER = new Set([5, 6, 7, 8, 9]);
const BOTH = new Set([4, 12]);

const classifyThreat = (source) => {
  const s = clampSource(source);
  if (BOTH.has(s)) return 2;
  if (CYBER.has(s)) return 1;
  if (PHYSICAL.has(s)) return 0;
  return 0; // unreachable for 0..12, but defensive
};

// Physical alert iff Physical(0) or Both(2).
const raisesPhysical = (source) => (classifyThreat(source) === 1 ? 0 : 1);
// Cyber alert iff Cyber(1) or Both(2).
const raisesCyber = (source) => (classifyThreat(source) === 0 ? 0 : 1);

export default {
  affine: "ThreatClass.affine",
  cases: [
    { name: "source_count()", export: "source_count", args: [], oracle: () => 13 },
    {
      name: "source_value over [-3..15]",
      export: "source_value",
      args: [[-3, 15]],
      oracle: clampSource,
    },
    {
      name: "classify_threat over sources [-3..15]",
      export: "classify_threat",
      args: [[-3, 15]],
      oracle: classifyThreat,
    },
    {
      name: "raises_physical over sources [-3..15]",
      export: "raises_physical",
      args: [[-3, 15]],
      oracle: raisesPhysical,
    },
    {
      name: "raises_cyber over sources [-3..15]",
      export: "raises_cyber",
      args: [[-3, 15]],
      oracle: raisesCyber,
    },
  ],
};
