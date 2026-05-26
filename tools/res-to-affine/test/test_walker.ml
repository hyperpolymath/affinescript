(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** End-to-end tests for the tree-sitter walker (#57 Phase 2b).

    These tests shell out to the [tree-sitter] CLI; they are
    automatically skipped when the CLI is not on PATH so a fresh
    clone can still run [dune runtest] without bootstrapping the
    grammar. CI installs tree-sitter and runs them as a gate.

    To run locally: install tree-sitter (`cargo install
    tree-sitter-cli`), then `just install-grammar`, then `dune
    runtest tools/res-to-affine/`. *)

open Res_to_affine

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let tree_sitter_available () =
  Sys.command "command -v tree-sitter > /dev/null 2>&1" = 0

(* Find the repo root by walking up from the test's runtime cwd looking
   for `dune-project`. Dune sandboxes tests under
   `_build/default/.../test/`, so the natural "up three levels" arithmetic
   gives a build-tree path where the source-tree
   `tools/vendor/tree-sitter-rescript` does not exist. *)
let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None  (* hit filesystem root *)
    else find_repo_root parent

let repo_root () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some s when s <> "" -> s
  | _ ->
      (match find_repo_root (Sys.getcwd ()) with
       | Some s -> s
       | None -> Sys.getcwd ())

let grammar_dir () =
  Filename.concat (repo_root ()) "tools/vendor/tree-sitter-rescript"

let grammar_built () =
  Sys.file_exists (Filename.concat (grammar_dir ()) "src/parser.c")

let skip_unless_ready () =
  if not (tree_sitter_available ()) then begin
    Printf.printf
      "  [skip] tree-sitter CLI not on PATH; install via `cargo install \
       tree-sitter-cli`@\n";
    Alcotest.skip ()
  end;
  if not (grammar_built ()) then begin
    Printf.printf
      "  [skip] grammar not built; run `just install-grammar`@\n";
    Alcotest.skip ()
  end

let fixture = "fixtures/sample.res"

let test_walker_finds_side_effect_import () =
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  let has_kind k =
    List.exists (fun (f : Scanner.finding) -> f.kind = k) findings
  in
  Alcotest.(check bool)
    "walker reports side-effect-import on sample.res"
    true (has_kind Scanner.Side_effect_import)

let test_walker_only_module_toplevel () =
  (* The walker's promised improvement over the regex scanner: it
     should NOT flag `let _ = chained.call()` inside a function body
     as a side-effect-import. We can't synthesise that case without
     running tree-sitter, so this test is gated and uses the existing
     fixture, which has the regex-scanner-matching shape at module
     top level — the walker should match it. The negative case lives
     in Phase 2c's expanded corpus. *)
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  let side_effect_lines =
    List.filter_map
      (fun (f : Scanner.finding) ->
        if f.kind = Scanner.Side_effect_import then Some f.line else None)
      findings
  in
  (* sample.res line 8: `let _ = Pixi.Sound.register` — the only
     top-level side-effect import. *)
  Alcotest.(check (list int))
    "walker reports the line-8 import and only that one"
    [8] side_effect_lines

let lines_for_kind ~k findings =
  List.filter_map
    (fun (f : Scanner.finding) ->
      if f.kind = k then Some f.line else None)
    findings

let test_walker_raw_js () =
  (* sample.res line 11: `let host = %raw(\`globalThis.location.host\`)` *)
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  Alcotest.(check (list int))
    "walker reports raw-js at line 11"
    [11]
    (lines_for_kind ~k:Scanner.Raw_js findings)

let test_walker_untyped_exception () =
  (* sample.res:
       line 19  try { ... }
       line 22  | Js.Exn.Error(_) => None     (Js.Exn in pattern position)
       line 28  api->Promise.catch(...)        *)
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  Alcotest.(check (list int))
    "walker reports untyped-exception at lines 19, 22, 28"
    [19; 22; 28]
    (lines_for_kind ~k:Scanner.Untyped_exception findings)

let test_walker_mutable_global () =
  (* sample.res line 15: `currentUser := Some("alice")` — top-level
     assignment via the := operator. The declaration on line 14
     (`let currentUser = ref(None)`) is intentionally not flagged in
     parity-port scope; it is a Phase-2 follow-up in CORPUS-RUN.md. *)
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  Alcotest.(check (list int))
    "walker reports mutable-global at line 15"
    [15]
    (lines_for_kind ~k:Scanner.Mutable_global findings)

let test_walker_parity_with_scanner () =
  (* The headline parity property of Phase 2c: walker and scanner
     produce the same set of (kind, line) pairs on the synthetic
     fixture. Both are expected to find 6 findings — see CORPUS-RUN.md
     for the 491-file estate-scale numbers behind this design. *)
  skip_unless_ready ();
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  let walker_findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  let scanner_findings = Scanner.scan source in
  let summarise findings =
    List.map
      (fun (f : Scanner.finding) -> (Scanner.kind_to_label f.kind, f.line))
      findings
    |> List.sort compare
  in
  Alcotest.(check (list (pair string int)))
    "walker and scanner emit the same (kind, line) pairs on sample.res"
    (summarise scanner_findings)
    (summarise walker_findings)

(* ---- s-exp parser sanity (NOT gated; pure OCaml) ---------------------------

   The walker subprocess is shelled out in the gated tests above. The
   s-exp parser itself is pure-OCaml and can be exercised directly,
   but it lives behind walker.ml's module boundary. We do not unit-
   test it here to avoid widening the .mli for test-only access; the
   gated end-to-end tests above exercise it through real
   tree-sitter output. *)

let () =
  Alcotest.run "res-to-affine-walker"
    [
      ( "walker",
        [
          Alcotest.test_case "side-effect-import found on sample.res"
            `Quick test_walker_finds_side_effect_import;
          Alcotest.test_case "module-toplevel-only, correct line"
            `Quick test_walker_only_module_toplevel;
          Alcotest.test_case "raw-js detected on sample.res"
            `Quick test_walker_raw_js;
          Alcotest.test_case "untyped-exception detected at all expected lines"
            `Quick test_walker_untyped_exception;
          Alcotest.test_case "mutable-global detected at column-0 line"
            `Quick test_walker_mutable_global;
          Alcotest.test_case "walker / scanner parity on the synthetic fixture"
            `Quick test_walker_parity_with_scanner;
        ] );
    ]
