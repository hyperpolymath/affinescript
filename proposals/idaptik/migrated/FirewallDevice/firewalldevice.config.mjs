// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for FirewallDevice.affine. The oracle re-derives the ACL
// packet-filter decision INDEPENDENTLY from the original FirewallDevice.res, so a
// codegen regression surfaces as a differential mismatch. Source references:
//
//   checkPacket (FirewallDevice.res:176-207):
//     portMatch  = rule.port == 0 || rule.port == port
//     protoMatch = rule.protocol == "ANY" || rule.protocol == protocol
//     applies    = sourceMatch && destMatch && portMatch && protoMatch
//     no match   -> Deny (default deny)
//     Allow|Log  -> passedPackets++   (packet passed)
//     Deny       -> blockedAttempts++ (packet blocked)
//   actionToColor / aclAction order (FirewallDevice.res:31, 218-224):
//     Deny = 0, Allow = 1, Log = 2  (canonical band; -1 = out-of-band sentinel)
//   protocol codes (host-parsed): 255 = ANY wildcard, else a distinct per-proto code.

// --- independent oracle, mirroring the .res decision, not the .affine ---

const DENY = 0;
const ALLOW = 1;
const LOG = 2;
const PROTO_ANY = 255;

const oracleIsValidAction = (c) => (c >= 0 && c <= 2 ? 1 : 0);
const oracleClampAction = (c) => (oracleIsValidAction(c) === 1 ? c : -1);

// rule.port == 0 || rule.port == port
const oraclePortMatches = (rulePort, pktPort) =>
  rulePort === 0 || rulePort === pktPort ? 1 : 0;

// rule.protocol == "ANY" || rule.protocol == protocol  (ANY parsed to 255)
const oracleProtocolMatches = (ruleProto, pktProto) =>
  ruleProto === PROTO_ANY || ruleProto === pktProto ? 1 : 0;

// sourceMatch && destMatch && portMatch && protoMatch, each leg a strict-1 flag
const oracleRuleApplies = (s, d, p, q) =>
  s === 1 && d === 1 && p === 1 && q === 1 ? 1 : 0;

// Allow|Log -> passedPackets (1); Deny / anything else -> blockedAttempts (0)
const oracleVerdictPasses = (a) => (a === ALLOW || a === LOG ? 1 : 0);

export default {
  affine: "FirewallDevice.affine",
  cases: [
    { name: "action_deny()", export: "action_deny", args: [], oracle: () => DENY },
    { name: "action_allow()", export: "action_allow", args: [], oracle: () => ALLOW },
    { name: "action_log()", export: "action_log", args: [], oracle: () => LOG },
    { name: "action_count()", export: "action_count", args: [], oracle: () => 3 },
    { name: "proto_any()", export: "proto_any", args: [], oracle: () => PROTO_ANY },
    {
      name: "is_valid_action over [-3..5]",
      export: "is_valid_action",
      args: [[-3, 5]],
      oracle: (c) => oracleIsValidAction(c),
    },
    {
      name: "clamp_action over [-3..5]",
      export: "clamp_action",
      args: [[-3, 5]],
      oracle: (c) => oracleClampAction(c),
    },
    {
      name: "port_matches over rule[0..5] x pkt[0..5]",
      export: "port_matches",
      args: [[0, 5], [0, 5]],
      oracle: (rulePort, pktPort) => oraclePortMatches(rulePort, pktPort),
    },
    {
      name: "protocol_matches over {TCP,UDP,ICMP,ANY} x {TCP,UDP,ICMP,ANY}",
      export: "protocol_matches",
      args: [{ values: [0, 1, 2, 255] }, { values: [0, 1, 2, 255] }],
      oracle: (ruleProto, pktProto) => oracleProtocolMatches(ruleProto, pktProto),
    },
    {
      name: "rule_applies over src,dest,port,proto each [0..2]",
      export: "rule_applies",
      args: [[0, 2], [0, 2], [0, 2], [0, 2]],
      oracle: (s, d, p, q) => oracleRuleApplies(s, d, p, q),
    },
    { name: "default_action()", export: "default_action", args: [], oracle: () => DENY },
    {
      name: "verdict_passes_packet over [-2..4]",
      export: "verdict_passes_packet",
      args: [[-2, 4]],
      oracle: (a) => oracleVerdictPasses(a),
    },
  ],
};
