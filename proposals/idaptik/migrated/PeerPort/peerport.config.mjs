// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PeerPort.affine (idaptik peer-port arithmetic; scalar
// i32 ABI). The oracle re-derives the candidate-port arithmetic and the
// opposite-family base selection from PeerDetection.res INDEPENDENTLY in plain JS,
// so a codegen regression surfaces as a differential mismatch.

const validSlot = (s) => s >= 0 && s <= 5;
const candidatePort = (base, slot) => (validSlot(slot) ? base + slot * 10000 : -1);
// We seek the OPPOSITE family: browser(0)->8008 peer, gossamer(1)->8080 peer.
const peerBase = (isGossamer) => (isGossamer !== 0 ? 8080 : 8008);
const peerCandidate = (isGossamer, slot) => candidatePort(peerBase(isGossamer), slot);

export default {
  affine: "PeerPort.affine",
  cases: [
    { name: "candidate_slot_count()", export: "candidate_slot_count", args: [], oracle: () => 6 },
    {
      name: "is_valid_slot over [-3..9]",
      export: "is_valid_slot",
      args: [[-3, 9]],
      oracle: (s) => (validSlot(s) ? 1 : 0),
    },
    {
      // Sweep both real port families x slots incl. out-of-band slots.
      name: "candidate_port over {8080,8008} x [-2..7]",
      export: "candidate_port",
      args: [{ values: [8080, 8008] }, [-2, 7]],
      oracle: (base, slot) => candidatePort(base, slot),
    },
    {
      name: "peer_base_port over [0..3]",
      export: "peer_base_port",
      args: [[0, 3]],
      oracle: (g) => peerBase(g),
    },
    {
      name: "peer_candidate_port over [0..2] x [-2..7]",
      export: "peer_candidate_port",
      args: [[0, 2], [-2, 7]],
      oracle: (g, slot) => peerCandidate(g, slot),
    },
  ],
};
