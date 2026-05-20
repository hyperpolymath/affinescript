(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Phase-1 text scanner for ReScript anti-patterns.

    Detects the six anti-patterns surfaced in idaptik Wave 3 pilot
    (see migration/main/LESSONS.md and PILOT.md upstream). This scanner
    is line/regex-based; it is intentionally cheap and gives best-effort
    location markers. Phase 2 replaces it with a tree-sitter AST walker
    using the vendored [editors/tree-sitter-rescript] grammar. The
    [Emitter] interface is stable across the two implementations.

    Patterns detected today:
    - {b side-effect import} : [let _ = Mod.foo] (ReScript module-load hack)
    - {b raw JS} : any line containing [%raw] (typed FFI required)
    - {b untyped exception} : [Promise.catch], [Js.Exn], [raise], [try]
    - {b mutable global} : top-level [ref] or [:=] assignment

    Deferred to Phase 2 (need real AST):
    - {b inline lambda callback record} : N>=3 [~handler: (...) =>] in a record
    - {b oversized function} : function body >50 LOC *)

type kind =
  | Side_effect_import
  | Raw_js
  | Untyped_exception
  | Mutable_global

val kind_to_label : kind -> string
(** Short tag used in emitted comment markers (e.g. ["side-effect-import"]). *)

val kind_to_guidance : kind -> string
(** One-line human guidance for the emitter to print alongside the marker. *)

type finding = {
  kind : kind;
  line : int;
  excerpt : string;
}

val scan : string -> finding list
(** [scan source] returns findings in source order. [source] is the
    full .res file contents. *)
