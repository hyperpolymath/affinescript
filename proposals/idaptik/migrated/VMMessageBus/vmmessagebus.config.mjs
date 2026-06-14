// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VMMessageBus.affine (idaptik VM message-routing
// kernel; scalar i32 ABI). Each oracle re-derives the rule straight from
// VMMessageBus.res in plain JS, INDEPENDENTLY of the .affine, so a codegen
// regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine:
//   messageTarget band: Console..DevicePort = 0..7   (res:86-94 declaration order)
//   coopEvent band:     VMExecuted..ChatSent = 0..6   (res:120-127 declaration order)
//   is_valid_target     = 0 <= t <= 7
//   clamp_target        = valid ? t : -1
//   relays_to_partner   = t in {CoopSync 3, CoopChat 4, CoopItem 5} ? 1 : 0
//                         (the co-op channels are the partner-relayed ones; the
//                          local/covert/device targets are not)
//   is_coop_channel     = 3 <= t <= 5 ? 1 : 0
//   is_covert_channel   = t == CovertLinkChannel(6) ? 1 : 0   (the startsWith "covert:" arm)
//   event_sends_to_client = (validEvent && kind != PortData(3)) ? 1 : 0
//                         (flushCoopOutbox res:136-161 sends every kind but PortData, res:148)

const validTarget = (t) => Number.isInteger(t) && t >= 0 && t <= 7;
const isValidTarget = (t) => (validTarget(t) ? 1 : 0);
const clampTarget = (t) => (validTarget(t) ? t : -1);
// Co-op channels (3,4,5) are the only targets relayed to the partner.
const relaysToPartner = (t) => (t === 3 || t === 4 || t === 5 ? 1 : 0);
const isCoopChannel = (t) => (t >= 3 && t <= 5 ? 1 : 0);
const isCovertChannel = (t) => (t === 6 ? 1 : 0);

const validEvent = (k) => Number.isInteger(k) && k >= 0 && k <= 6;
const isValidEvent = (k) => (validEvent(k) ? 1 : 0);
// flushCoopOutbox forwards every event kind to the client EXCEPT PortData(3).
const eventSendsToClient = (k) => (validEvent(k) && k !== 3 ? 1 : 0);

// Sweep the full messageTarget band plus out-of-band, and the full coopEvent
// band plus out-of-band, so every in-band arm and both boundary guards fire.
const TARGET = [-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
const EVENT = [-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 9];

export default {
  affine: "VMMessageBus.affine",
  cases: [
    {
      name: "message_target_count()",
      export: "message_target_count",
      args: [],
      oracle: () => 8,
    },
    {
      name: "is_valid_target over target band + out-of-band",
      export: "is_valid_target",
      args: [{ values: TARGET }],
      oracle: isValidTarget,
    },
    {
      name: "clamp_target over target band + out-of-band",
      export: "clamp_target",
      args: [{ values: TARGET }],
      oracle: clampTarget,
    },
    {
      name: "relays_to_partner over target band + out-of-band",
      export: "relays_to_partner",
      args: [{ values: TARGET }],
      oracle: relaysToPartner,
    },
    {
      name: "is_coop_channel over target band + out-of-band",
      export: "is_coop_channel",
      args: [{ values: TARGET }],
      oracle: isCoopChannel,
    },
    {
      name: "is_covert_channel over target band + out-of-band",
      export: "is_covert_channel",
      args: [{ values: TARGET }],
      oracle: isCovertChannel,
    },
    {
      name: "coop_event_count()",
      export: "coop_event_count",
      args: [],
      oracle: () => 7,
    },
    {
      name: "is_valid_event over event band + out-of-band",
      export: "is_valid_event",
      args: [{ values: EVENT }],
      oracle: isValidEvent,
    },
    {
      name: "event_sends_to_client over event band + out-of-band",
      export: "event_sends_to_client",
      args: [{ values: EVENT }],
      oracle: eventSendsToClient,
    },
  ],
};
