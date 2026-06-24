// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for NetworkManager.affine. Oracle re-derives routing
// predicates from NetworkZones.res canAccessZones adjacency and
// NetworkManager.res routing logic.
//
// Zones 0..16 (ordinals mirror NetworkZones.allZones index in source):
//   0 downtown-lan, 1 rural-lan, 2 downtown-dmz, 3 downtown-internal,
//   4 downtown-dev, 5 downtown-security, 6 downtown-iot, 7 downtown-mgmt,
//   8 downtown-scada, 9 isp-tier3-rural, 10 isp-tier3-business,
//   11 isp-tier2-regional, 12 isp-tier1-backbone, 13 service-atlas,
//   14 service-nexus, 15 service-devhub, 16 public
//
// Adjacency from NetworkZones.res canAccessZones arrays (exact copy):
const EDGES = {
  0: [2,3,4,6,7,10,16],         // downtown-lan
  1: [2,9,16],                   // rural-lan
  2: [3,5,10,16],                // downtown-dmz
  3: [2,4,5,7],                  // downtown-internal
  4: [3,5,16],                   // downtown-dev
  5: [0,2,3,4,6,7,8],            // downtown-security
  6: [5],                        // downtown-iot
  7: [0,2,3,4,5,6,8],            // downtown-mgmt
  8: [],                         // downtown-scada (air-gapped)
  9: [11],                       // isp-tier3-rural
  10: [11],                      // isp-tier3-business
  11: [12],                      // isp-tier2-regional
  12: [13,14,15],                // isp-tier1-backbone
  13: [12,16],                   // service-atlas
  14: [12,16],                   // service-nexus
  15: [12,16],                   // service-devhub
  16: [],                        // public
};
const CAT = {
  0:0, 1:0,       // LAN
  2:1,            // DMZ
  3:2, 4:2, 5:2, // Internal
  6:3,            // IoT
  7:4,            // Mgmt
  8:5,            // SCADA
  9:6, 10:6, 11:6, 12:6, // ISP
  13:7, 14:7, 15:7, 16:7, // Service/Public
};
const CAT_ISP = 6;
const CAT_SERVICE = 7;
// public = zone 16 = CAT_SERVICE

function isZone(z) { return z >= 0 && z <= 16; }
function hasEdge(src, dst) {
  if (!isZone(src) || !isZone(dst)) return false;
  const edges = EDGES[src] || [];
  return edges.includes(dst);
}
function oracleSameSubnet(a, b) { return a === b ? 1 : 0; }
function oraclePackSubnet(o0, o1, o2) { return (o0 * 65536 + o1 * 256 + o2) | 0; }
function oracleCanZoneAccessZone(src, dst) {
  if (src === dst) return isZone(src) ? 1 : 0;
  if (!isZone(src) || !isZone(dst)) return 0;
  return hasEdge(src, dst) ? 1 : 0;
}
function reachesAnyISP(src) {
  return hasEdge(src, 10) || hasEdge(src, 9) || hasEdge(src, 11) || hasEdge(src, 12);
}
function oracleCanRouteViaISP(src, dst) {
  if (!isZone(src) || !isZone(dst)) return 0;
  const cat = CAT[dst];
  if (cat === CAT_SERVICE) {
    return (reachesAnyISP(src) || hasEdge(src, 16)) ? 1 : 0;
  }
  if (cat === CAT_ISP) {
    return (hasEdge(src, dst) || hasEdge(src, 10) || hasEdge(src, 9) || hasEdge(src, 16)) ? 1 : 0;
  }
  return 0;
}
function oracleCanIPAccessIP(src, dst, same_ip) {
  if (same_ip === 1) return 1;
  if (!isZone(src)) return 0;
  if (isZone(dst)) {
    if (oracleCanZoneAccessZone(src, dst) === 1) return 1;
    return oracleCanRouteViaISP(src, dst);
  }
  // dst is NO_ZONE (-1): src reaches public or tier-3 ISP
  if (hasEdge(src, 16) || hasEdge(src, 10) || hasEdge(src, 9)) return 1;
  return 0;
}
function oracleIsReachableFrom(src, dst, same_ip, dest_exists) {
  if (same_ip === 1) return 1;
  if (dest_exists === 0) {
    if (isZone(dst)) return 0;
    return oracleCanIPAccessIP(src, dst, 0);
  }
  return oracleCanIPAccessIP(src, dst, same_ip);
}
function oracleRoutePrefixHops(srcIsRouter, isExternal) {
  let hops = 0;
  if (srcIsRouter === 0) hops++;
  if (isExternal === 1) hops += 3;
  return hops;
}
function oracleRouteFinalLatency(isExternal) { return isExternal === 1 ? 32 : 2; }
function oracleRouteTotalHops(srcIsRouter, isExternal) {
  return oracleRoutePrefixHops(srcIsRouter, isExternal) + 1;
}

// Zone sweep: all valid zones 0..16 plus the NO_ZONE sentinel (-1)
const ALL_ZONES = { values: [-1, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] };

export default {
  affine: "NetworkManager.affine",
  cases: [
    {
      name: "same_subnet: all pairs prefix [0..3] x [0..3]",
      export: "same_subnet",
      args: [[0, 3], [0, 3]],
      oracle: (a, b) => oracleSameSubnet(a, b) | 0,
    },
    {
      name: "pack_subnet: octet triples (0..2 each, spot check)",
      export: "pack_subnet",
      args: [[0, 2], [0, 2], [0, 2]],
      oracle: (o0, o1, o2) => oraclePackSubnet(o0, o1, o2) | 0,
    },
    {
      name: "can_zone_access_zone: all zone pairs (incl sentinel)",
      export: "can_zone_access_zone",
      args: [ALL_ZONES, ALL_ZONES],
      oracle: (s, d) => oracleCanZoneAccessZone(s, d) | 0,
    },
    {
      name: "can_route_via_isp: all zone pairs",
      export: "can_route_via_isp",
      args: [ALL_ZONES, ALL_ZONES],
      oracle: (s, d) => oracleCanRouteViaISP(s, d) | 0,
    },
    {
      name: "can_ip_access_ip: zone pairs x same_ip 0/1",
      export: "can_ip_access_ip",
      args: [ALL_ZONES, ALL_ZONES, [0, 1]],
      oracle: (s, d, si) => oracleCanIPAccessIP(s, d, si) | 0,
    },
    {
      name: "is_reachable_from: zone pairs x same_ip x dest_exists",
      export: "is_reachable_from",
      args: [ALL_ZONES, ALL_ZONES, [0, 1], [0, 1]],
      oracle: (s, d, si, de) => oracleIsReachableFrom(s, d, si, de) | 0,
    },
    {
      name: "route_prefix_hops: src_is_router x is_external",
      export: "route_prefix_hops",
      args: [[0, 1], [0, 1]],
      oracle: (r, e) => oracleRoutePrefixHops(r, e) | 0,
    },
    {
      name: "route_final_latency: is_external 0/1",
      export: "route_final_latency",
      args: [[0, 1]],
      oracle: (e) => oracleRouteFinalLatency(e) | 0,
    },
    {
      name: "route_total_hops: src_is_router x is_external",
      export: "route_total_hops",
      args: [[0, 1], [0, 1]],
      oracle: (r, e) => oracleRouteTotalHops(r, e) | 0,
    },
  ],
};
