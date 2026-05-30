(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Canonical home for typed-wasm custom-section encoders.

    Wire encoders operate on pre-byte-encoded triples to keep this
    module free of any dependency on producer-side ownership types
    (avoiding a cycle with [Codegen]). Callers map their domain
    types to bytes before invoking.

    Forward-compatibility slot for typed-wasm proposal 0001 (issue
    hyperpolymath/typed-wasm#34): [Encode.regions] and
    [Encode.capabilities] land here when the proposal promotes to
    [accepted]. Sharing the internal LE writers (u8 / u32le / leb128)
    inside this module prevents the 3-sections × 2-copies = 6-encoder
    fan-out flagged by hyperpolymath/affinescript#444. *)

module Encode : sig
  (** v1 wire format for [typedwasm.ownership]:
        u32le  entry_count
        per entry:
          u32le  func_index
          u8     param_count
          u8*    param_kind  (one per param)
          u8     return_kind

      Caller responsibilities:
      - Map domain ownership type → byte before calling
        (see [Codegen.ownership_kind_byte]).
      - Empty input → [Bytes.empty]; caller omits the section.

      Producer-emit is v1 per ADR-021. Parser-side v2 support lives in
      [Tw_verify.parse_ownership_section_payload]; emit-side flip is
      cross-repo-coordinated, not local to this function. *)
  val ownership : (int * int list * int) list -> bytes
end
