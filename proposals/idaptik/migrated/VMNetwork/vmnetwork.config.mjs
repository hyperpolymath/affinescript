// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VMNetwork.affine (idaptik Tier-5 causal-ordering
// kernel; scalar i32 ABI). Each oracle re-derives the rule straight from the
// ReScript source / the host binding contract in plain JS, INDEPENDENTLY of the
// .affine, so a codegen regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine:
//   tick(t)            = t + 1                          (VMNetwork.res:70)
//   sync(l, r)         = max(l, r) + 1                  (VMNetwork.res:85)
//   merge_timestamp    = max(localTs, remoteTs)         (VMNetworkCoprocessor.res:46-47,
//                                                         "the later write")
//   merge_register     = remoteTs > localTs ? remote : local  (VMNetworkCoprocessor.res:49-50,
//                                                         last-writer-wins, strict-greater)
//   can_undo(c, u)     = (c == 0 && u == 0) ? 1 : 0     (VMNetwork.res:240-256;
//                                                         VMNetworkCoprocessor.res:52-53)
//   route_kind(p)      = (0<=p<=2) ? p : -1             (routePortMessage prefix branch,
//                                                         VMNetwork.res:150-188; CHANNEL
//                                                         {NET:0,COVERT:1,LOCAL:2})

// Lamport advance: strictly increment.
const tick = (t) => t + 1;
// Lamport receive merge: one past the later of the two clocks.
const sync = (l, r) => Math.max(l, r) + 1;
// Surviving write stamp of a reconciled register: the later write.
const mergeTimestamp = (localTs, remoteTs) => Math.max(localTs, remoteTs);
// Last-writer-wins value: take remote only on a STRICTLY greater stamp.
const mergeRegister = (localVal, localTs, remoteVal, remoteTs) =>
  remoteTs > localTs ? remoteVal : localVal;
// Causal-undo gate: may undo iff neither consumed nor undone. Any non-zero flag
// counts as set (truthy), mirroring the kernel's `!= 0` test.
const canUndo = (consumed, undone) => (consumed === 0 && undone === 0 ? 1 : 0);
// Routed-channel classifier: identity on the 0..2 band, out-of-band sentinel -1.
const routeKind = (p) => (p >= 0 && p <= 2 ? p : -1);

// Sweep ranges chosen so every branch fires:
//  - clock values incl. 0 and a tie (l == r) for sync's > branch;
//  - flags over {-1,0,1,2} so the `!= 0` truthiness + the 0 case both fire;
//  - channel ordinals over [-2..5] so both out-of-band guards and all three
//    in-band channels are exercised.
const FLAG = [-1, 0, 1, 2];
const CLOCK = [-1, 0, 1, 2, 7, 42];
const CHANNEL = [-2, -1, 0, 1, 2, 3, 5];

export default {
  affine: "VMNetwork.affine",
  cases: [
    {
      name: "tick over [-1..10]",
      export: "tick",
      args: [[-1, 10]],
      oracle: tick,
    },
    {
      name: "sync over clock x clock (incl. ties)",
      export: "sync",
      args: [{ values: CLOCK }, { values: CLOCK }],
      oracle: sync,
    },
    {
      name: "merge_timestamp over clock x clock",
      export: "merge_timestamp",
      args: [{ values: CLOCK }, { values: CLOCK }],
      oracle: mergeTimestamp,
    },
    {
      name: "merge_register: lww value over {val,ts} x {val,ts}",
      export: "merge_register",
      args: [{ values: [10, 20] }, { values: CLOCK }, { values: [30, 40] }, { values: CLOCK }],
      oracle: mergeRegister,
    },
    {
      name: "can_undo over flag x flag",
      export: "can_undo",
      args: [{ values: FLAG }, { values: FLAG }],
      oracle: canUndo,
    },
    {
      name: "route_kind over channel band + out-of-band",
      export: "route_kind",
      args: [{ values: CHANNEL }],
      oracle: routeKind,
    },
  ],
};
