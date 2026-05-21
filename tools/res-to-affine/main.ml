(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** [res-to-affine] CLI — ReScript-to-AffineScript migration assistant.

    Reads a [.res] file, scans it for the six anti-patterns surfaced in
    the idaptik Wave 3 pilot, and emits an [.affine] skeleton with
    migration markers. The original source is quoted at the bottom of
    the output so the human migrating the file has it side-by-side.

    Phase 1 (this binary) uses a text scanner. Phase 2 swaps the
    [Scanner] implementation for a tree-sitter AST walker reading the
    vendored grammar at [editors/tree-sitter-rescript/]. See the
    tool README for the full plan. *)

open Res_to_affine

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

type engine = Scanner_engine | Walker_engine

let engine_label = function
  | Scanner_engine -> "scanner"
  | Walker_engine  -> "walker"

let run engine grammar_dir input output_opt =
  if not (Sys.file_exists input) then begin
    Format.eprintf "res-to-affine: input not found: %s@." input;
    exit 2
  end;
  let source  = read_file input in
  let findings =
    match engine with
    | Scanner_engine -> Scanner.scan source
    | Walker_engine  ->
        (try Walker.scan ~grammar_dir ~path:input ~source with
         | Failure msg ->
             Format.eprintf "res-to-affine: %s@." msg;
             Format.eprintf
               "res-to-affine: falling back to scanner engine for %s@."
               input;
             Scanner.scan source)
  in
  let module_name = Emitter.module_name_of_path input in
  let out =
    Emitter.emit
      ~module_name
      ~source_path:input
      ~source
      ~findings
  in
  match output_opt with
  | None ->
      print_string out
  | Some path ->
      write_file path out;
      Format.printf
        "res-to-affine: %d finding%s [%s] → %s@."
        (List.length findings)
        (if List.length findings = 1 then "" else "s")
        (engine_label engine)
        path

(* ---- cmdliner wiring ---- *)

let input_arg =
  let doc = "ReScript source file to migrate." in
  Cmdliner.Arg.(
    required & pos 0 (some non_dir_file) None &
    info [] ~docv:"INPUT.res" ~doc)

let output_arg =
  let doc = "Write the skeleton to FILE instead of stdout." in
  Cmdliner.Arg.(
    value & opt (some string) None &
    info ["o"; "output"] ~docv:"FILE" ~doc)

let engine_arg =
  let doc =
    "Detection engine: 'scanner' (default, line-regex, Phase 1) or \
     'walker' (tree-sitter AST, Phase 2). The walker requires the \
     vendored grammar to be built — see `just install-grammar`. \
     Falls back to 'scanner' if the grammar is missing or \
     tree-sitter parse fails."
  in
  Cmdliner.Arg.(
    value & opt
      (enum ["scanner", Scanner_engine; "walker", Walker_engine])
      Scanner_engine &
    info ["engine"] ~docv:"ENGINE" ~doc)

let grammar_dir_arg =
  let doc =
    "Path to the generated tree-sitter-rescript grammar directory \
     (the output of `just install-grammar`). Only consulted when \
     `--engine=walker`. Defaults to `tools/vendor/tree-sitter-rescript`."
  in
  Cmdliner.Arg.(
    value & opt string Walker.default_grammar_dir &
    info ["grammar-dir"] ~docv:"DIR" ~doc)

let cmd =
  let doc = "Emit an AffineScript skeleton from a ReScript source file." in
  let info = Cmdliner.Cmd.info "res-to-affine" ~version:"0.1.0" ~doc in
  let term =
    Cmdliner.Term.(
      const run $ engine_arg $ grammar_dir_arg $ input_arg $ output_arg)
  in
  Cmdliner.Cmd.v info term

let () = exit (Cmdliner.Cmd.eval cmd)
