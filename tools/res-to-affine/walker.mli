(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Phase-2 AST walker for ReScript anti-patterns.

    Replaces the Phase-1 [Scanner] line-regex pipeline with a real
    tree-sitter-driven walker over the vendored
    [editors/tree-sitter-rescript/] grammar. The two pipelines share
    [Scanner.kind] and [Scanner.finding] so the emitter is unchanged.

    Phase 2b (#322) ported [Side_effect_import]. Phase 2c (this file's
    current state) ports the remaining three — [Raw_js],
    [Untyped_exception], [Mutable_global] — to AST detection,
    reaching parity with [Scanner.scan] on the synthetic
    [test/fixtures/sample.res] corpus.

    The walker's discovery of [Side_effect_import] and
    [Mutable_global] is strictly stronger than the regex's: it
    requires the anti-pattern to live at *module top level*. The
    column-0 false-positive class that [#319] band-aided is
    eliminated structurally rather than by regex anchor. *)

val scan :
  grammar_dir:string ->
  path:string ->
  source:string ->
  Scanner.finding list
(** [scan ~grammar_dir ~path ~source] invokes [tree-sitter parse] on
    [path] using the generated parser at [grammar_dir/src/parser.c]
    (the directory produced by [editors/tree-sitter-rescript/scripts/
    install.sh]), walks the resulting AST, and returns
    [Scanner.finding]s for every detected anti-pattern.

    Raises [Failure] if the [tree-sitter] CLI is missing, the parse
    fails, or the output cannot be parsed. The caller is responsible
    for catching this and falling back / surfacing a user error.

    [source] is used only to slice excerpt strings out for findings;
    it must be the same content the file at [path] holds. *)

val default_grammar_dir : string
(** Default location of the generated grammar relative to the current
    working directory: ["tools/vendor/tree-sitter-rescript"]. Matches
    the output path of [editors/tree-sitter-rescript/scripts/install.sh]. *)
