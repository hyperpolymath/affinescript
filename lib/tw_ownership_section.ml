(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)
(*
 * tw_ownership_section.ml — the dedicated home for the
 * `typedwasm.ownership` Wasm custom section: kind encoding,
 * extraction from the AST, and binary serialisation.
 *
 * Extracted from lib/codegen.ml at 2026-05-24 per Tranche A3 of
 * docs/specs/TYPED-WASM-ROADMAP.adoc. Behaviour-preserving move.
 *
 * The format itself is specified in
 * docs/specs/TYPED-WASM-INTERFACE.adoc (v1, current) and
 * docs/specs/TYPED-WASM-ROADMAP.adoc §"Tranche B" (v2, the
 * ratified ADR-020 widening; landed via [Tw_verify] parse-support
 * first, with the producer-emit flip deferred to the coordinated
 * landing window per ADR-021 — see TYPED-WASM-COORDINATION-LEDGER.adoc
 * Q-001).
 *
 * Backwards compatibility: [Codegen] re-exports the same names so
 * `open Codegen` continues to expose [ownership_kind] and friends —
 * downstream callers ([Tw_verify], [Tw_interface], [Test_e2e]) need
 * no change.
 *)

open Ast

(** Ownership kind for typed-wasm schema annotations.
    Maps AffineScript ownership qualifiers to typed-wasm Level 7/10
    verification. *)
type ownership_kind =
  | Unrestricted  (** Plain value, no ownership constraint (Wasm i32/f64 etc.) *)
  | Linear        (** TyOwn / own — consumed exactly once (typed-wasm Level 10) *)
  | SharedBorrow  (** TyRef / ref — read-only aliasing safety (typed-wasm Level 7) *)
  | ExclBorrow    (** TyMut / mut — exclusive mutable aliasing safety (typed-wasm Level 7) *)

(** Extract ownership kind from a parameter declaration.
    Checks [p.p_ownership] first; falls back to the shape of [p.p_ty]. *)
let ownership_kind_of_param (p : param) : ownership_kind =
  match p.p_ownership with
  | Some Own -> Linear
  | Some Ref -> SharedBorrow
  | Some Mut -> ExclBorrow
  | None ->
    match p.p_ty with
    | TyOwn _ -> Linear
    | TyRef _ -> SharedBorrow
    | TyMut _ -> ExclBorrow
    | _ -> Unrestricted

(** Extract ownership kind from an optional return type expression *)
let ownership_kind_of_ret (ret : type_expr option) : ownership_kind =
  match ret with
  | Some (TyOwn _) -> Linear
  | Some (TyRef _) -> SharedBorrow
  | Some (TyMut _) -> ExclBorrow
  | _ -> Unrestricted

(** Encode an [ownership_kind] as a single byte (0..3). *)
let ownership_kind_byte = function
  | Unrestricted -> 0
  | Linear -> 1
  | SharedBorrow -> 2
  | ExclBorrow -> 3

(** Build the payload for the [typedwasm.ownership] Wasm custom section.

    v1 encoding (current emit; LE):
      u32  entry_count
      per entry:
        u32  func_index
        u8   param_count
        u8*  param_kind  (one per param, see kind encoding above)
        u8   return_kind

    Returns [Bytes.empty] when there are no annotations so the
    caller can omit the section entirely. *)
let build_section
    (annots : (int * ownership_kind list * ownership_kind) list) : bytes =
  if annots = [] then Bytes.empty
  else
    let buf = Buffer.create 64 in
    let write_u32_le n =
      Buffer.add_char buf (Char.chr  (n         land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr  8) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
    in
    let write_u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
    write_u32_le (List.length annots);
    List.iter (fun (func_idx, param_kinds, ret_kind) ->
      write_u32_le func_idx;
      write_u8 (List.length param_kinds);
      List.iter (fun k -> write_u8 (ownership_kind_byte k)) param_kinds;
      write_u8 (ownership_kind_byte ret_kind)
    ) annots;
    Buffer.to_bytes buf
