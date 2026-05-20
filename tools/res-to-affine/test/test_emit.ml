(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Snapshot tests for the res-to-affine emitter.

    Each fixture pair lives under [test/fixtures/<name>.res] and
    [test/expected/<name>.affine]. To regenerate after intentional
    changes:

    {[
      dune exec tools/res-to-affine/main.exe -- \
        tools/res-to-affine/test/fixtures/<name>.res \
        > tools/res-to-affine/test/expected/<name>.affine
    ]}

    Tree-sitter is NOT required by these tests; the Phase-1 scanner is
    pure-OCaml. Phase 2 will add a separate suite gated on the
    [editors/tree-sitter-rescript] install. *)

open Res_to_affine

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let render_fixture fixture_path =
  let source = read_file fixture_path in
  let findings = Scanner.scan source in
  Emitter.emit
    ~module_name:(Emitter.module_name_of_path fixture_path)
    ~source_path:fixture_path
    ~source
    ~findings

let check_snapshot name =
  let fixture  = Printf.sprintf "fixtures/%s.res"   name in
  let expected = Printf.sprintf "expected/%s.affine" name in
  let got = render_fixture fixture in
  let want = read_file expected in
  Alcotest.(check string)
    (Printf.sprintf "%s snapshot" name)
    want got

let test_sample () =
  check_snapshot "sample"

let test_finding_kinds () =
  let source = read_file "fixtures/sample.res" in
  let kinds =
    Scanner.scan source
    |> List.map (fun (f : Scanner.finding) -> Scanner.kind_to_label f.kind)
    |> List.sort_uniq compare
  in
  Alcotest.(check (list string))
    "all four Phase-1 kinds detected"
    [ "mutable-global"; "raw-js"; "side-effect-import"; "untyped-exception" ]
    kinds

let test_module_name () =
  Alcotest.(check string) "PascalCase basename" "Config"
    (Emitter.module_name_of_path "/path/to/Config.res");
  Alcotest.(check string) "lowercase basename is capitalised" "Webhook"
    (Emitter.module_name_of_path "webhook.res")

let () =
  Alcotest.run "res-to-affine"
    [
      ( "snapshot",
        [ Alcotest.test_case "sample.res → sample.affine" `Quick test_sample ] );
      ( "scanner",
        [ Alcotest.test_case "all kinds detected" `Quick test_finding_kinds ] );
      ( "emitter",
        [ Alcotest.test_case "module name derivation" `Quick test_module_name ] );
    ]
