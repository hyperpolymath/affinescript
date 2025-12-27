(** Golden tests for parser/AST output *)

open Affinescript

(** Directory containing golden test files *)
let golden_dir = "test/golden"

(** Read file contents *)
let read_file path =
  let chan = open_in path in
  let content = really_input_string chan (in_channel_length chan) in
  close_in chan;
  content

(** Check if file exists *)
let file_exists path =
  try
    let _ = Unix.stat path in
    true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

(** Normalize AST output for comparison (remove span details) *)
let normalize_ast ast_str =
  (* Remove span information for stable comparison *)
  let re_span = Str.regexp {|{ Span.file = "[^"]*"; start_pos = [^}]*; end_pos = [^}]* }|} in
  let without_spans = Str.global_replace re_span "<span>" ast_str in
  (* Normalize whitespace *)
  let re_ws = Str.regexp "[ \t\n\r]+" in
  Str.global_replace re_ws " " without_spans

(** Parse a file and return the AST as a string *)
let parse_to_string path =
  try
    let ast = Parse_driver.parse_file path in
    let ast_str = Ast.show_program ast in
    Ok ast_str
  with
  | Parse_driver.Parse_error (msg, span) ->
      Error (Printf.sprintf "Parse error at %s: %s" (Span.show span) msg)
  | e ->
      Error (Printf.sprintf "Error: %s" (Printexc.to_string e))

(** Run a single golden test *)
let run_golden_test ~source_path ~expected_path () =
  match parse_to_string source_path with
  | Error msg ->
      Alcotest.fail (Printf.sprintf "Failed to parse %s: %s" source_path msg)
  | Ok actual ->
      if not (file_exists expected_path) then
        Alcotest.fail (Printf.sprintf "Expected file not found: %s\nActual output:\n%s" expected_path actual)
      else
        let expected = read_file expected_path in
        let norm_actual = normalize_ast actual in
        let norm_expected = normalize_ast expected in
        if norm_actual <> norm_expected then begin
          Printf.eprintf "\n=== EXPECTED ===\n%s\n" expected;
          Printf.eprintf "\n=== ACTUAL ===\n%s\n" actual;
          Alcotest.fail "AST output does not match expected"
        end

(** Create a test case from a .as file *)
let test_case_of_file filename =
  let base = Filename.chop_extension filename in
  let source_path = Filename.concat golden_dir filename in
  let expected_path = Filename.concat golden_dir (base ^ ".expected") in
  Alcotest.test_case base `Quick (run_golden_test ~source_path ~expected_path)

(** Discover all .as files in golden directory *)
let discover_tests () =
  if not (Sys.file_exists golden_dir) then
    []
  else
    let files = Sys.readdir golden_dir in
    files
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".as")
    |> List.sort String.compare
    |> List.map test_case_of_file

(** Run parser on examples directory and check they all parse *)
let examples_dir = "examples"

let run_example_parse_test filename () =
  let path = Filename.concat examples_dir filename in
  match parse_to_string path with
  | Error msg ->
      Alcotest.fail (Printf.sprintf "Failed to parse %s: %s" path msg)
  | Ok _ast ->
      (* Just check it parses successfully *)
      ()

(** Discover all examples and create parse tests *)
let discover_example_tests () =
  if not (Sys.file_exists examples_dir) then
    []
  else
    let files = Sys.readdir examples_dir in
    files
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".as")
    |> List.sort String.compare
    |> List.map (fun f ->
        let base = Filename.chop_extension f in
        Alcotest.test_case ("parse " ^ base) `Quick (run_example_parse_test f))

(** All golden tests *)
let tests = discover_tests ()

(** All example parse tests *)
let example_tests = discover_example_tests ()
