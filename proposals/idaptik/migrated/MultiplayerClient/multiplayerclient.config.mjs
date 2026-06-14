// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for MultiplayerClient.affine (idaptik co-op multiplayer-
// session kernel; scalar i32 ABI). Each oracle re-derives the rule straight from
// the ReScript source / the host binding contract in plain JS, INDEPENDENTLY of the
// .affine, so a codegen regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine.
// multiplayerState {Offline0,InLobby1,InSession2};
// CONN_EVENT {SOCKET_CONNECTED0,SOCKET_DISCONNECTED1,JOIN_SESSION2,LEAVE_SESSION3};
// PLAYER_EVENT {PLAYER_JOINED0,PLAYER_LEFT1,SESSION_LEFT2}; role {Q0,Observer1}.
//   conn_transition(s,e)    = {0:1,1:0,2:2,3:1}[e] ?? s          (state assigns, res:173-409)
//   player_count_after(c,e) = c0=max(0,c); {0:c0+1,1:max(0,c0-1),2:0}[e] ?? c0  (coopPlayers dict)
//   role_from_string(f)     = f!==0 ? 1 : 0                      (roleFromString, res:77-82)
//   role_clamp(r)           = r===0 ? 0 : 1                      (binding roleClamp, res:68-69)
//   has_partner(c)          = c>0 ? 1 : 0                        (getCoopPartner, res:592-594)

// Connection/session machine: defined events set an absolute target; undefined
// event is a no-op (returns prior state). Mirrors the .res unconditional assigns.
const connTransition = (s, e) =>
  e === 0 ? 1 : e === 1 ? 0 : e === 2 ? 2 : e === 3 ? 1 : s;
// Player-list counter: join +1, left max(0,-1), session-left clear; floored at 0.
const playerCountAfter = (c, e) => {
  const c0 = c < 0 ? 0 : c;
  if (e === 0) return c0 + 1;
  if (e === 1) return c0 <= 0 ? 0 : c0 - 1;
  if (e === 2) return 0;
  return c0;
};
// Role decode from the "is observer" flag; any non-zero is Observer.
const roleFromString = (f) => (f !== 0 ? 1 : 0);
// Role clamp: Q is the single 0; anything else is Observer.
const roleClamp = (r) => (r === 0 ? 0 : 1);
// Partner present iff the co-op list is non-empty.
const hasPartner = (c) => (c > 0 ? 1 : 0);

// Sweep states/events/roles over their bands + out-of-band guards; counts incl.
// 0 (empty list) and the floor edge (a PLAYER_LEFT/has_partner at 0 and negatives).
const STATE = [-1, 0, 1, 2, 3];
const CONN_EVENT = [-1, 0, 1, 2, 3, 4];
const PLAYER_EVENT = [-1, 0, 1, 2, 3];
const ROLE = [-2, -1, 0, 1, 2, 5];
const COUNT = [-2, -1, 0, 1, 2, 5, 17];

export default {
  affine: "MultiplayerClient.affine",
  cases: [
    { name: "state_count()", export: "state_count", args: [], oracle: () => 3 },
    {
      name: "conn_transition over state x event (incl. out-of-band event)",
      export: "conn_transition",
      args: [{ values: STATE }, { values: CONN_EVENT }],
      oracle: connTransition,
    },
    {
      name: "player_count_after over count x event (incl. floor at 0)",
      export: "player_count_after",
      args: [{ values: COUNT }, { values: PLAYER_EVENT }],
      oracle: playerCountAfter,
    },
    { name: "role_from_string over flag {-2..5}", export: "role_from_string", args: [{ values: ROLE }], oracle: roleFromString },
    { name: "role_clamp over role band + out-of-band", export: "role_clamp", args: [{ values: ROLE }], oracle: roleClamp },
    { name: "has_partner over count {-2..17}", export: "has_partner", args: [{ values: COUNT }], oracle: hasPartner },
  ],
};
