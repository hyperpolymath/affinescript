// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for NetworkZones.affine (idaptik zone-category taxonomy +
// ISP-routing-class kernel; scalar i32 ABI). The oracle re-derives, from the
// original NetworkZones.res semantics, the 0..7 category band and the routing
// class that canRouteViaISP branches on, so a codegen regression surfaces as a
// differential mismatch.
//
// Original logic (ReScript, NetworkZones.res):
//   zoneCategory order: LAN=0 DMZ=1 Internal=2 IoT=3 Management=4 SCADA=5
//                       ISP=6 Service=7
//   canRouteViaISP(source, dest):
//     if dest.category == Service  -> reachable via any ISP tier or "public"
//     else if dest.category == ISP -> reachable via the tier hierarchy / public
//     else                         -> false   (not ISP routing)
//   => the routing CLASS is a pure function of dest.category:
//        Service -> endpoint-class (2), ISP -> member-class (1), else -> 0.

// Independent oracle: category validity over the closed 0..7 band.
const validCategory = (c) => c >= 0 && c <= 7;

// Independent oracle: the routing class canRouteViaISP selects on dest.category.
// Service (7) is the tier endpoint; ISP (6) is a tier member; every other valid
// category routes through neither branch (canRouteViaISP returns false); an
// out-of-band code is not a category at all.
function oracleRoutingClass(destCategory) {
  if (!validCategory(destCategory)) return -1;
  if (destCategory === 7) return 2; // Service
  if (destCategory === 6) return 1; // ISP
  return 0; // LAN/DMZ/Internal/IoT/Management/SCADA -> not ISP-routed
}

export default {
  affine: "NetworkZones.affine",
  cases: [
    { name: "category_count()", export: "category_count", args: [], oracle: () => 8 },
    { name: "lan_category()", export: "lan_category", args: [], oracle: () => 0 },
    { name: "isp_category()", export: "isp_category", args: [], oracle: () => 6 },
    { name: "service_category()", export: "service_category", args: [], oracle: () => 7 },
    {
      name: "is_valid_category over [-3..11]",
      export: "is_valid_category",
      args: [[-3, 11]],
      oracle: (c) => (validCategory(c) ? 1 : 0),
    },
    {
      name: "clamp_category over [-3..11]",
      export: "clamp_category",
      args: [[-3, 11]],
      oracle: (c) => (validCategory(c) ? c : -1),
    },
    {
      name: "routing_class over [-3..11]",
      export: "routing_class",
      args: [[-3, 11]],
      oracle: (c) => oracleRoutingClass(c),
    },
    {
      name: "is_isp_routable over [-3..11]",
      export: "is_isp_routable",
      args: [[-3, 11]],
      oracle: (c) => (oracleRoutingClass(c) > 0 ? 1 : 0),
    },
  ],
};
