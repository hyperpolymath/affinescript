(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Skeleton emitter: given a source file and its scan findings, write
    a [.affine] stub with migration markers and the original source
    quoted at the bottom for human reference.

    The output is intentionally a {i skeleton}, not a transliteration.
    The human picks the decomposition; the tool surfaces what needs
    re-decomposing. See [tools/res-to-affine/README.md] for the
    rationale and the Phase 1 / 2 / 3 plan. *)

val module_name_of_path : string -> string
(** Derive an AffineScript module name from a path. [.../Config.res]
    yields ["Config"]; non-PascalCase basenames are capitalised. *)

val emit :
  module_name:string ->
  source_path:string ->
  source:string ->
  findings:Scanner.finding list ->
  string
(** Render the skeleton. The result is a complete file contents string. *)

val emit_translation :
  module_name:string ->
  source_path:string ->
  source:string ->
  findings:Scanner.finding list ->
  translated:(int * string) list ->
  string
(** Render the Phase-3 partial port: the same marker block as {!emit}, a
    proper [module Name;] header, then each translated structural type
    declaration ([translated] is the [(source_line, affinescript)] list
    from {!Walker.translate}, in source order), then a TODO note and the
    quoted original. Used for [--translate]; {!emit} is unchanged. *)

val emit_partial :
  module_name:string ->
  source_path:string ->
  source:string ->
  findings:Scanner.finding list ->
  translated:(int * string) list ->
  string
(** Render the #488 partial-port output: a banner stating the result does NOT
    type-check, the marker block, a [module Name;] header, the [fn] skeletons
    ([translated] from {!Walker.translate_partial}), then a TODO note and the
    quoted original. Used for [--partial]. *)
