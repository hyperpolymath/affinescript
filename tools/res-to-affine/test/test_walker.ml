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

(* Substring search — OCaml's [String] only has char membership. *)
let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  if nn = 0 then true
  else
    let rec loop i =
      if i + nn > hn then false
      else if String.sub haystack i nn = needle then true
      else loop (i + 1)
    in
    loop 0

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

(* ---- Phase 2c parity: scanner-detected kinds via walker -------------------

   The synthetic [fixtures/sample.res] exercises all four Phase-1 kinds.
   After Phase 2c, the walker must report each of them too (lines per
   the fixture comments). *)

let scan_sample () =
  let source = read_file fixture in
  let path = Filename.concat (Sys.getcwd ()) fixture in
  Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source

let lines_for_kind findings k =
  List.filter_map
    (fun (f : Scanner.finding) ->
      if f.kind = k then Some f.line else None)
    findings

let test_walker_finds_raw_js () =
  skip_unless_ready ();
  let findings = scan_sample () in
  (* sample.res line 11: `let host = %raw(`globalThis.location.host`)`. *)
  Alcotest.(check (list int))
    "walker reports raw-js on line 11"
    [11] (lines_for_kind findings Scanner.Raw_js)

let test_walker_finds_mutable_global () =
  skip_unless_ready ();
  let findings = scan_sample () in
  let got = lines_for_kind findings Scanner.Mutable_global in
  (* sample.res line 14: `let currentUser = ref(None)` — top-level
     ref-binding; line 15: `currentUser := Some("alice")` — top-level
     mutation. Both are mutable-global. *)
  Alcotest.(check bool)
    "walker reports mutable-global on lines 14 and 15"
    true (List.mem 14 got && List.mem 15 got)

let test_walker_finds_untyped_exception () =
  skip_unless_ready ();
  let findings = scan_sample () in
  let got = lines_for_kind findings Scanner.Untyped_exception in
  (* sample.res has untyped-exception flavours at: line 19 (`try {`),
     line 22 (`Js.Exn.Error(_)`), line 28 (`Promise.catch(...)`). *)
  let want_subset = [19; 22; 28] in
  let missing = List.filter (fun l -> not (List.mem l got)) want_subset in
  Alcotest.(check (list int))
    "walker reports untyped-exception on lines 19, 22, 28"
    [] missing

(* ---- Phase 2c new kinds: inline-callback-record + oversized-function ------

   Synthetic fixture [phase2c.res] specifically exercises the two
   anti-patterns deferred from Phase 1 entirely. These are walker-only
   by construction; the scanner does not detect them. *)

let phase2c_fixture = "fixtures/phase2c.res"

let test_walker_finds_inline_callback_record () =
  skip_unless_ready ();
  let source = read_file phase2c_fixture in
  let path = Filename.concat (Sys.getcwd ()) phase2c_fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  let got = lines_for_kind findings Scanner.Inline_callback_record in
  Alcotest.(check bool)
    "walker reports inline-callback-record at least once on phase2c.res"
    true (got <> [])

let test_walker_finds_oversized_function () =
  skip_unless_ready ();
  let source = read_file phase2c_fixture in
  let path = Filename.concat (Sys.getcwd ()) phase2c_fixture in
  let findings =
    Walker.scan ~grammar_dir:(grammar_dir ()) ~path ~source
  in
  let got = lines_for_kind findings Scanner.Oversized_function in
  Alcotest.(check bool)
    "walker reports oversized-function at least once on phase2c.res"
    true (got <> [])

(* ---- s-exp parser sanity (NOT gated; pure OCaml) ---------------------------

   The walker subprocess is shelled out in the gated tests above. The
   s-exp parser itself is pure-OCaml and can be exercised directly,
   but it lives behind walker.ml's module boundary. We do not unit-
   test it here to avoid widening the .mli for test-only access; the
   gated end-to-end tests above exercise it through real
   tree-sitter output. *)

(* ---- Phase 3 slice 1: structural type-declaration translation -------------

   [fixtures/phase3.res] holds three translatable structural type decls
   (a primitive alias and two simple sum types) plus forms that slice 1
   must skip (a generic, a qualified-path alias, and a let/switch). These
   gated tests assert the translator's output structurally. *)

let phase3_fixture = "fixtures/phase3.res"

let translate_phase3 () =
  let source = read_file phase3_fixture in
  let path = Filename.concat (Sys.getcwd ()) phase3_fixture in
  Walker.translate ~grammar_dir:(grammar_dir ()) ~path ~source

let translate_phase3_blob () =
  String.concat "\n" (List.map snd (translate_phase3 ()))

let test_translate_count () =
  skip_unless_ready ();
  (* userId, color, shape, and (slice 2) the generic box — 4. theirMap
     (qualified) and the let/switch stay skipped. *)
  Alcotest.(check int)
    "four structural type decls are translated"
    4 (List.length (translate_phase3 ()))

let test_translate_generic_sum () =
  skip_unless_ready ();
  let blob = translate_phase3_blob () in
  Alcotest.(check bool)
    "generic sum -> type Box[A] = | Box(A)"
    true (contains blob "type Box[A] =" && contains blob "| Box(A)")

let test_translate_alias () =
  skip_unless_ready ();
  Alcotest.(check bool)
    "primitive alias -> capitalised TyCon + Int"
    true (contains (translate_phase3_blob ()) "type UserId = Int")

let test_translate_nullary_sum () =
  skip_unless_ready ();
  let blob = translate_phase3_blob () in
  let ok =
    contains blob "type Color =" && contains blob "| Red"
    && contains blob "| Green" && contains blob "| Blue"
  in
  Alcotest.(check bool) "nullary sum -> leading-pipe variant form" true ok

let test_translate_payload_sum () =
  skip_unless_ready ();
  let blob = translate_phase3_blob () in
  let ok =
    contains blob "type Shape =" && contains blob "| Circle(Float)"
    && contains blob "| Rect(Int, Int)"
  in
  Alcotest.(check bool) "primitive-payload sum -> mapped param types" true ok

let test_translate_skips_non_structural () =
  skip_unless_ready ();
  let blob = translate_phase3_blob () in
  (* the qualified Belt.Map.t and the let/switch must stay absent — the tool
     never guesses them; and no raw ReScript type-var ['a] leaks through. *)
  let leaked =
    contains blob "switch" || contains blob "area"
    || contains blob "Belt" || contains blob "'a"
  in
  Alcotest.(check bool) "qualified / non-type forms skipped, no raw type-var"
    false leaked

(* ---- Phase 3 slice 2: records (-> struct) and generics --------------------

   [fixtures/phase3b.res] holds a record (-> struct), a generic record
   (-> struct with type params), a generic alias, and two records that must
   be skipped (mutable + optional fields). *)

let phase3b_fixture = "fixtures/phase3b.res"

let translate_phase3b () =
  let source = read_file phase3b_fixture in
  let path = Filename.concat (Sys.getcwd ()) phase3b_fixture in
  Walker.translate ~grammar_dir:(grammar_dir ()) ~path ~source

let translate_phase3b_blob () =
  String.concat "\n" (List.map snd (translate_phase3b ()))

let test_translate_b_count () =
  skip_unless_ready ();
  (* point, box, id translate; counter (mutable) and config (optional) skip. *)
  Alcotest.(check int)
    "three of five record/generic decls translate"
    3 (List.length (translate_phase3b ()))

let test_translate_record () =
  skip_unless_ready ();
  let blob = translate_phase3b_blob () in
  let ok =
    contains blob "struct Point {" && contains blob "x: Int"
    && contains blob "y: Int"
  in
  Alcotest.(check bool) "record -> struct with mapped field types" true ok

let test_translate_generic_record () =
  skip_unless_ready ();
  let blob = translate_phase3b_blob () in
  let ok = contains blob "struct Box[A] {" && contains blob "value: A" in
  Alcotest.(check bool) "generic record -> struct with type params" true ok

let test_translate_generic_alias () =
  skip_unless_ready ();
  Alcotest.(check bool)
    "generic alias -> type Id[A] = A"
    true (contains (translate_phase3b_blob ()) "type Id[A] = A")

let test_translate_b_skips () =
  skip_unless_ready ();
  let blob = translate_phase3b_blob () in
  (* mutable + optional records must be skipped, never silently flattened. *)
  let leaked =
    contains blob "mutable" || contains blob "count"
    || contains blob "verbose" || contains blob "?"
  in
  Alcotest.(check bool) "mutable / optional records skipped" false leaked

(* ---- Phase 3 slice 3: `let <id> = <literal>` -> module-level `const` ------

   [fixtures/phase3c.res] holds literal lets (int/float/string/bool) that
   become `const`, plus a call-bodied let, a ref (mutable-global), and a
   destructuring let that must all be skipped. *)

let phase3c_fixture = "fixtures/phase3c.res"

let translate_phase3c () =
  let source = read_file phase3c_fixture in
  let path = Filename.concat (Sys.getcwd ()) phase3c_fixture in
  Walker.translate ~grammar_dir:(grammar_dir ()) ~path ~source

let translate_phase3c_blob () =
  String.concat "\n" (List.map snd (translate_phase3c ()))

let test_translate_c_count () =
  skip_unless_ready ();
  (* answer, pi, greeting, enabled, disabled -> 5; now/counter/(a,b) skip. *)
  Alcotest.(check int)
    "five literal let-bindings translate to const"
    5 (List.length (translate_phase3c ()))

let test_translate_const_int_float () =
  skip_unless_ready ();
  let blob = translate_phase3c_blob () in
  let ok =
    contains blob "const answer: Int = 42;"
    && contains blob "const pi: Float = 3.14;"
  in
  Alcotest.(check bool) "int + float literal -> typed const" true ok

let test_translate_const_string_bool () =
  skip_unless_ready ();
  let blob = translate_phase3c_blob () in
  let ok =
    contains blob "const greeting: String = \"hi\";"
    && contains blob "const enabled: Bool = true;"
    && contains blob "const disabled: Bool = false;"
  in
  Alcotest.(check bool) "string + bool literal -> typed const" true ok

let test_translate_c_skips () =
  skip_unless_ready ();
  let blob = translate_phase3c_blob () in
  (* call / ref / destructuring bindings must never become a const. *)
  let leaked =
    contains blob "Date" || contains blob "ref(" || contains blob "const now"
    || contains blob "const counter" || contains blob "const a:"
  in
  Alcotest.(check bool) "non-literal / ref / destructuring lets skipped"
    false leaked

(* ---- #488 partial-port: fn skeletons + switch->match ----------------------

   [fixtures/partial1.res] holds five module-top-level functions exercising
   switch->match, variant + nullary patterns, int/float/concat operators, a
   member-call, and a pipe-first form that must become a TODO hole. The output
   is a partial port (does not type-check); these assert its structure. *)

let partial_fixture = "fixtures/partial1.res"

let translate_partial1 () =
  let source = read_file partial_fixture in
  let path = Filename.concat (Sys.getcwd ()) partial_fixture in
  Walker.translate_partial ~grammar_dir:(grammar_dir ()) ~path ~source

let partial1_blob () =
  String.concat "\n" (List.map snd (translate_partial1 ()))

let test_partial_count () =
  skip_unless_ready ();
  Alcotest.(check int)
    "five module-top-level functions -> fn skeletons"
    5 (List.length (translate_partial1 ()))

let test_partial_switch_to_match () =
  skip_unless_ready ();
  let blob = partial1_blob () in
  let ok =
    contains blob "fn classify(x: _) -> _" && contains blob "match x {"
    && contains blob "Some(n) => n + 1" && contains blob "None => 0"
  in
  Alcotest.(check bool) "switch -> match with translated arms + patterns" true ok

let test_partial_float_op_normalised () =
  skip_unless_ready ();
  let blob = partial1_blob () in
  Alcotest.(check bool) "float op normalised; multi-param skeleton"
    true
    (contains blob "fn area(w: _, h: _) -> _"
    && contains blob "w * h"
    && not (contains blob "*."))

let test_partial_concat_and_call () =
  skip_unless_ready ();
  let blob = partial1_blob () in
  Alcotest.(check bool) "string concat + member-call translated"
    true
    (contains blob "\"hi \" ++ name" && contains blob "Js.log(msg)")

let test_partial_todo_hole () =
  skip_unless_ready ();
  let blob = partial1_blob () in
  Alcotest.(check bool) "untranslatable form becomes a () /* TODO */ hole"
    true (contains blob "() /* TODO:")

let () =
  Alcotest.run "res-to-affine-walker"
    [
      ( "walker-side-effect-import",
        [
          Alcotest.test_case "side-effect-import found on sample.res"
            `Quick test_walker_finds_side_effect_import;
          Alcotest.test_case "module-toplevel-only, correct line"
            `Quick test_walker_only_module_toplevel;
        ] );
      ( "walker-phase2c-parity",
        [
          Alcotest.test_case "raw-js found on sample.res line 11"
            `Quick test_walker_finds_raw_js;
          Alcotest.test_case "mutable-global found on sample.res lines 14+15"
            `Quick test_walker_finds_mutable_global;
          Alcotest.test_case
            "untyped-exception found on sample.res lines 19/22/28"
            `Quick test_walker_finds_untyped_exception;
        ] );
      ( "walker-phase2c-new-kinds",
        [
          Alcotest.test_case "inline-callback-record found on phase2c.res"
            `Quick test_walker_finds_inline_callback_record;
          Alcotest.test_case "oversized-function found on phase2c.res"
            `Quick test_walker_finds_oversized_function;
        ] );
      ( "walker-phase3-translate",
        [
          Alcotest.test_case "four structural type decls translated"
            `Quick test_translate_count;
          Alcotest.test_case "primitive alias -> type UserId = Int"
            `Quick test_translate_alias;
          Alcotest.test_case "nullary sum -> leading-pipe variants"
            `Quick test_translate_nullary_sum;
          Alcotest.test_case "primitive-payload sum -> mapped params"
            `Quick test_translate_payload_sum;
          Alcotest.test_case "generic sum -> type Box[A] = | Box(A)"
            `Quick test_translate_generic_sum;
          Alcotest.test_case "qualified / non-type forms skipped"
            `Quick test_translate_skips_non_structural;
        ] );
      ( "walker-phase3b-records-generics",
        [
          Alcotest.test_case "three of five record/generic decls translate"
            `Quick test_translate_b_count;
          Alcotest.test_case "record -> struct"
            `Quick test_translate_record;
          Alcotest.test_case "generic record -> struct[A]"
            `Quick test_translate_generic_record;
          Alcotest.test_case "generic alias -> type Id[A] = A"
            `Quick test_translate_generic_alias;
          Alcotest.test_case "mutable / optional records skipped"
            `Quick test_translate_b_skips;
        ] );
      ( "walker-phase3c-let-const",
        [
          Alcotest.test_case "five literal lets -> const"
            `Quick test_translate_c_count;
          Alcotest.test_case "int + float -> typed const"
            `Quick test_translate_const_int_float;
          Alcotest.test_case "string + bool -> typed const"
            `Quick test_translate_const_string_bool;
          Alcotest.test_case "call / ref / destructuring lets skipped"
            `Quick test_translate_c_skips;
        ] );
      ( "walker-488-partial",
        [
          Alcotest.test_case "five functions -> fn skeletons"
            `Quick test_partial_count;
          Alcotest.test_case "switch -> match + patterns + arm bodies"
            `Quick test_partial_switch_to_match;
          Alcotest.test_case "float op normalised + multi-param skeleton"
            `Quick test_partial_float_op_normalised;
          Alcotest.test_case "concat + member-call translated"
            `Quick test_partial_concat_and_call;
          Alcotest.test_case "untranslatable form -> TODO hole"
            `Quick test_partial_todo_hole;
        ] );
    ]
