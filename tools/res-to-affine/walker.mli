(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Phase-2 AST walker for ReScript anti-patterns.

    Replaces the Phase-1 [Scanner] line-regex pipeline with a real
    tree-sitter-driven walker over the vendored
    [editors/tree-sitter-rescript/] grammar. The two pipelines share
    [Scanner.kind] and [Scanner.finding] so the emitter is unchanged.

    Phase 2b ported a single anti-pattern: [Side_effect_import].
    Phase 2c (this revision) extends the walker to:

    - the remaining three Phase-1 kinds — [Raw_js],
      [Untyped_exception], [Mutable_global] — via AST shapes
      ([extension_expression], [try_expression]/[call_expression]
      [raise]/member-expression [Promise.catch]/[Js.Exn],
      [let_declaration] with [ref] body and [mutation_expression]);
    - the two kinds that were explicitly deferred from Phase 1
      because they need real AST — [Inline_callback_record]
      (≥3 inline function values in a single record or call) and
      [Oversized_function] (function spans >50 source rows);

    The walker's discovery is strictly stronger than the regex's
    on the three ported kinds: it requires structural module-top-
    level placement for [Mutable_global], it tells [try {…}] apart
    from the identifier [try] in scanner-only false-positive
    contexts, and it does not double-fire on the same line for
    structurally-nested matches. *)

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

val translate :
  grammar_dir:string ->
  path:string ->
  source:string ->
  (int * string) list
(** [translate ~grammar_dir ~path ~source] parses [path] and returns the
    Phase-3 (slice 1) translations of its module-top-level, fully-structural
    type declarations — primitive aliases and simple sum types — as
    [(source_line, affinescript)] pairs in source order.

    Conservative by construction: declarations that use type parameters,
    qualified paths, record bodies, non-primitive references, GADT return
    annotations, or variant spreads are skipped (absent from the result),
    never guessed — so the result is always compilable AffineScript and a
    skipped form is recovered from the marker block / quoted source the
    emitter prints. Raises [Failure] under the same conditions as {!scan}. *)

val default_grammar_dir : string
(** Default location of the generated grammar relative to the current
    working directory: ["tools/vendor/tree-sitter-rescript"]. Matches
    the output path of [editors/tree-sitter-rescript/scripts/install.sh]. *)
