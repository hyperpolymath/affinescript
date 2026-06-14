// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for LobbyManager.affine (idaptik multiplayer-lobby state
// kernel; SHAPE-A; scalar i32 ABI). Each oracle re-derives the rule straight from
// the ReScript source / the host binding contract in plain JS, INDEPENDENTLY of the
// .affine, so a codegen regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine.
// lobbyState {Disconnected0,Connecting1,Connected2,InRoom3,Ready4};
// connectionState {Disconnected0,Connecting1,Connected2}; role {Q0,Observer1};
// MAX_PLAYERS = 16.
//   state_of_connection(c) = (0<=c<=2) ? c : -1                  (setConnectionState, res:69-76)
//   enter_room_state(_)    = 3                                   (enterRoom, res:104-112)
//   leave_room_state(_)    = 2                                   (leaveRoom, res:115-124)
//   set_ready_state(_)     = 4                                   (setReady, res:161-163)
//   unset_ready_state(s,r) = s===4 ? (r?3:2) : s                 (unsetReady, res:166-175)
//   toggle_role(r)         = r===0 ? 1 : 0                       (toggleRole, res:89-96)
//   set_player_ready(m,i,r)= toggle bit i of m by r, no-op off-band  (setPlayerReady, res:147-158)
//   ready_count(m,n)       = popcount of low clamp(n,0,16) bits of m (allPlayersReady/binding)
//   all_players_ready(m,n) = n>0 && ready_count(m,n)===n         (allPlayersReady, res:190-193)
//   can_start_game(h,m,n)  = h ? all_players_ready(m,n) : 0      (canStartGame, res:197-199)
//   state_to_color(s)      = palette[s] ?? -1                    (stateToColor, res:213-221)

const MAX = 16;
const clampCount = (n) => (n < 0 ? 0 : n > MAX ? MAX : n);

const stateOfConnection = (c) => (c >= 0 && c <= 2 ? c : -1);
const enterRoomState = (_) => 3;
const leaveRoomState = (_) => 2;
const setReadyState = (_) => 4;
const unsetReadyState = (s, r) => (s === 4 ? (r !== 0 ? 3 : 2) : s);
const toggleRole = (r) => (r === 0 ? 1 : 0);

// Bit toggle on the packed mask; out-of-band index (>= MAX or < 0) is a no-op.
const validIndex = (i) => i >= 0 && i < MAX;
const setPlayerReady = (m, i, r) => {
  if (!validIndex(i)) return m;
  const bit = 2 ** i;
  const isSet = Math.floor(m / bit) % 2 === 1;
  if (r !== 0) return isSet ? m : m + bit;
  return isSet ? m - bit : m;
};
// Popcount of the low clamp(n,0,16) bits of m.
const readyCount = (m, n) => {
  const live = clampCount(n);
  let acc = 0;
  let bit = 1;
  for (let k = 0; k < live; k++) {
    if (Math.floor(m / bit) % 2 === 1) acc++;
    bit *= 2;
  }
  return acc;
};
const allPlayersReady = (m, n) => (n > 0 && readyCount(m, n) === n ? 1 : 0);
const canStartGame = (h, m, n) => (h !== 0 ? allPlayersReady(m, n) : 0);
const PALETTE = { 0: 0xff4444, 1: 0xffaa44, 2: 0x44ff44, 3: 0x4488ff, 4: 0x44ffaa };
const stateToColor = (s) => (s in PALETTE ? PALETTE[s] : -1);

// Sweep ranges. State/connection/role over their bands + out-of-band guards.
// Masks: a spread of bit patterns incl. 0, all-low-bits-set, and gaps. Counts:
// 0 (empty), small lobbies, and just past MAX to exercise the clamp.
const STATE = [-1, 0, 1, 2, 3, 4, 5, 7];
const CONN = [-1, 0, 1, 2, 3];
const ROLE = [-1, 0, 1, 2];
const FLAG = [0, 1];
const MASK = [0, 1, 2, 3, 5, 7, 8, 15, 31, 255];
const COUNT = [-1, 0, 1, 2, 3, 4, 8, 16, 17];
const INDEX = [-1, 0, 1, 2, 7, 15, 16, 20];

export default {
  affine: "LobbyManager.affine",
  cases: [
    { name: "state_of_connection over conn band + out-of-band", export: "state_of_connection", args: [{ values: CONN }], oracle: stateOfConnection },
    { name: "enter_room_state over state band", export: "enter_room_state", args: [{ values: STATE }], oracle: enterRoomState },
    { name: "leave_room_state over state band", export: "leave_room_state", args: [{ values: STATE }], oracle: leaveRoomState },
    { name: "set_ready_state over state band", export: "set_ready_state", args: [{ values: STATE }], oracle: setReadyState },
    { name: "unset_ready_state over state x has-room", export: "unset_ready_state", args: [{ values: STATE }, { values: FLAG }], oracle: unsetReadyState },
    { name: "toggle_role over role band + out-of-band", export: "toggle_role", args: [{ values: ROLE }], oracle: toggleRole },
    { name: "set_player_ready over mask x index x ready-flag", export: "set_player_ready", args: [{ values: MASK }, { values: INDEX }, { values: FLAG }], oracle: setPlayerReady },
    { name: "ready_count over mask x count", export: "ready_count", args: [{ values: MASK }, { values: COUNT }], oracle: readyCount },
    { name: "all_players_ready over mask x count", export: "all_players_ready", args: [{ values: MASK }, { values: COUNT }], oracle: allPlayersReady },
    { name: "can_start_game over host-flag x mask x count", export: "can_start_game", args: [{ values: FLAG }, { values: MASK }, { values: COUNT }], oracle: canStartGame },
    { name: "state_to_color over state band + out-of-band", export: "state_to_color", args: [{ values: STATE }], oracle: stateToColor },
  ],
};
