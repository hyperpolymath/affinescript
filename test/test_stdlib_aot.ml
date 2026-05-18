(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025 hyperpolymath *)

(** STAGE-A closers (affinescript#128):

    - #136: stdlib-wide AOT compile-smoke gate — every [stdlib/*.affine]
      driven through resolve -> typecheck -> borrow -> codegen
      (Deno-ESM), so the AOT path cannot silently rot again.
    - #137: a multi-module integration program that [use]s several
      stdlib modules together in one compiled unit, proving cross-module
      resolution/typecheck/codegen works as a coherent set. *)

open Affinescript

(* Locate the stdlib directory from the test's runtime cwd
   (`_build/default/test`). The `(source_tree ../stdlib)` dep
   materialises it; fall back to a couple of plausible roots. *)
let stdlib_dir =
  let candidates =
    (match Sys.getenv_opt "AFFINESCRIPT_STDLIB" with
     | Some d -> [ d ] | None -> [])
    @ [ "../stdlib"; "stdlib"; "../../stdlib" ]
  in
  match
    List.find_opt
      (fun d -> Sys.file_exists (Filename.concat d "prelude.affine"))
      candidates
  with
  | Some d -> d
  | None -> failwith "test_stdlib_aot: cannot locate stdlib/ (no prelude.affine)"

let loader () =
  let cfg =
    { (Module_loader.default_config ()) with
      Module_loader.stdlib_path = stdlib_dir }
  in
  Module_loader.create cfg

(** Full AOT pipeline: resolve -> typecheck -> borrow -> Deno-ESM codegen.
    Returns [Ok js] (the emitted ES-module source) or [Error stage-msg]. *)
let pipeline_to_deno (prog : Ast.program) : (string, string) result =
  let ld = loader () in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, sp) ->
    Error (Printf.sprintf "resolve: %s @ %s"
             (Resolve.show_resolve_error e) (Span.show sp))
  | Ok (rctx, itc) ->
    (match
       Typecheck.check_program
         ~import_types:itc.Typecheck.name_types rctx.symbols prog
     with
     | Error e ->
       Error (Printf.sprintf "typecheck: %s" (Typecheck.format_type_error e))
     | Ok _ ->
       (match Borrow.check_program rctx.symbols prog with
        | Error e ->
          Error (Printf.sprintf "borrow: %s" (Borrow.format_borrow_error e))
        | Ok () ->
          let flat = Module_loader.flatten_imports ld prog in
          (match Codegen_deno.codegen_deno flat rctx.symbols with
           | Error e -> Error (Printf.sprintf "deno-codegen: %s" e)
           | Ok js -> Ok js)))

let parse_file_safe path =
  try Ok (Parse_driver.parse_file path)
  with
  | Parse_driver.Parse_error (msg, span) ->
    Error (Printf.sprintf "parse: %s @ %s" msg (Span.show span))
  | e -> Error (Printf.sprintf "parse: %s" (Printexc.to_string e))

(* ---- #136: stdlib-wide AOT compile-smoke gate -------------------------- *)

let stdlib_files () =
  Sys.readdir stdlib_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".affine")
  |> List.sort compare
  |> List.map (fun f -> Filename.concat stdlib_dir f)

let check_one path () =
  let result =
    match parse_file_safe path with
    | Error m -> Error m
    | Ok prog -> pipeline_to_deno prog
  in
  match result with
  | Ok js ->
    Alcotest.(check bool)
      (Printf.sprintf "%s emits a non-empty ES module" (Filename.basename path))
      true (String.length js > 0)
  | Error m ->
    Alcotest.failf "AOT pipeline failed for %s: %s"
      (Filename.basename path) m

let aot_smoke_tests =
  List.map
    (fun path ->
       Alcotest.test_case
         (Printf.sprintf "AOT %s" (Filename.basename path))
         `Quick (check_one path))
    (stdlib_files ())

(* ---- #137: multi-module integration ----------------------------------- *)

(* One compiled program that pulls in several stdlib modules together and
   actually uses a symbol from each, exercising cross-module
   resolution/typecheck/codegen as a coherent set (not file-by-file). *)
let integration_src = {|
use prelude::{ Option, Some, None };
use string::{ split, join };
use option::{ unwrap_or };
use collections::{ reverse };

fn pipeline(csv: String) -> String {
  let parts = split(csv, ",");
  let flipped = reverse(parts);
  join(flipped, "-")
}

fn first_or(xs: [String], dflt: String) -> String {
  let head: Option<String> = if len(xs) > 0 { Some(xs[0]) } else { None };
  unwrap_or(head, dflt)
}
|}

let test_multi_module_integration () =
  match Parse_driver.parse_string ~file:"<integration>" integration_src with
  | exception e ->
    Alcotest.failf "integration parse raised: %s" (Printexc.to_string e)
  | prog ->
    (match pipeline_to_deno prog with
     | Ok js ->
       Alcotest.(check bool)
         "multi-module program compiles to a non-empty ES module"
         true (String.length js > 0)
     | Error m ->
       Alcotest.failf "multi-module integration failed: %s" m)

let integration_tests =
  [ Alcotest.test_case "string+option+collections together" `Quick
      test_multi_module_integration ]

let tests =
  [ ("STAGE-A AOT smoke (#136)", aot_smoke_tests);
    ("STAGE-A multi-module integration (#137)", integration_tests) ]
