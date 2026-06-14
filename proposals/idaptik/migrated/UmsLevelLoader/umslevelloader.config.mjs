// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for UmsLevelLoader.affine (idaptik UMS level-decode
// validation core; scalar i32 ABI). Every oracle is an INDEPENDENT JS
// reimplementation derived from src/shared/UmsLevelLoader.res semantics (the
// itemKind/subtype decoder arms, the condition fallback-to-Good, the covert-link
// floor, the 200.0 patrol-radius default) -- NOT a copy of the .affine logic --
// so the differential sweep is a genuine cross-check. The .res frames these as
// string-keyed JSON decoding; here only the scalar ordinal result crosses, with
// patrol radius in milli-units (200.0 J-equivalent -> 200000).

const oIsValidKind = (k) => (k >= 0 && k <= 7 ? 1 : 0);
const oIsNullaryKind = (k) => (k === 7 ? 1 : 0);
const oRequiresCapacity = (k) => (k === 4 ? 1 : 0);

// Per-kind subtype band (independent table from the .res decoder switch arms).
const SUBTYPE_MAX = { 0: 5, 1: 3, 2: 5, 3: 3, 4: 1, 5: 3 };
const oSubtypeInBand = (kind, subtype) => {
  if (subtype < 0) return 0;
  if (!(kind in SUBTYPE_MAX)) return 0; // Keycard(6)/Radio(7)/out-of-band
  return subtype <= SUBTYPE_MAX[kind] ? 1 : 0;
};

const oIsValidCondition = (c) => (c >= 0 && c <= 4 ? 1 : 0);
const oNormaliseCondition = (c) => (c >= 0 && c <= 4 ? c : 1); // fallback Good
const oFloorCovertLinks = (n) => (n < 0 ? 0 : n);
const oResolvePatrolRadiusMilli = (present, valueMilli) => (present === 0 ? 200000 : valueMilli);

export default {
  affine: "UmsLevelLoader.affine",
  cases: [
    { name: "kind_count()", export: "kind_count", args: [], oracle: () => 8 },
    { name: "condition_count()", export: "condition_count", args: [], oracle: () => 5 },
    {
      name: "is_valid_kind over [-2..11]",
      export: "is_valid_kind",
      args: [[-2, 11]],
      oracle: oIsValidKind,
    },
    {
      name: "is_nullary_kind over [-2..11]",
      export: "is_nullary_kind",
      args: [[-2, 11]],
      oracle: oIsNullaryKind,
    },
    {
      name: "requires_capacity over [-2..11]",
      export: "requires_capacity",
      args: [[-2, 11]],
      oracle: oRequiresCapacity,
    },
    {
      name: "subtype_in_band over kind in [-1..9], subtype in [-1..7]",
      export: "subtype_in_band",
      args: [[-1, 9], [-1, 7]],
      oracle: oSubtypeInBand,
    },
    {
      name: "is_valid_condition over [-2..8]",
      export: "is_valid_condition",
      args: [[-2, 8]],
      oracle: oIsValidCondition,
    },
    {
      name: "normalise_condition over [-2..8] (out-of-band -> Good=1)",
      export: "normalise_condition",
      args: [[-2, 8]],
      oracle: oNormaliseCondition,
    },
    {
      name: "floor_covert_links over [-5..50]",
      export: "floor_covert_links",
      args: [[-5, 50]],
      oracle: oFloorCovertLinks,
    },
    {
      name: "resolve_patrol_radius_milli over present in {0,1}, value in {0,50000,200000,1000000}",
      export: "resolve_patrol_radius_milli",
      args: [{ values: [0, 1] }, { values: [0, 50000, 200000, 1000000] }],
      oracle: oResolvePatrolRadiusMilli,
    },
  ],
};
