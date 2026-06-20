(* SPDX-License-Identifier: MPL-2.0 *)
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

(** Full AOT pipeline to the core-Wasm backend: resolve -> typecheck ->
    borrow -> [Codegen.generate_module] (loader-aware). Mirrors
    [pipeline_to_deno] but targets the backend whose cross-module constructor
    linking (#138) the test below regression-locks. The Wasm path feeds the
    original (un-flattened) [prog] to codegen and resolves imported decls
    natively via [Codegen.gen_imports]. *)
let pipeline_to_wasm (prog : Ast.program) : (Wasm.wasm_module, string) result =
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
          let optimized = Opt.fold_constants_program prog in
          (match Codegen.generate_module ~loader:ld optimized with
           | Error e ->
             Error (Printf.sprintf "wasm-codegen: %s"
                      (Codegen.show_codegen_error e))
           | Ok m -> Ok m)))

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

(* ---- #138: cross-module constructor linking on the core-Wasm backend ----

   A consumer that imports prelude's Option/Result and APPLIES their
   constructors must reach a runnable artifact. The front-end resolves
   `Some`/`None`/`Ok`/`Err` through the module path, so `check` passes; before
   the #138 codegen fix [Codegen.gen_imports] dropped the imported TYPE decls,
   so the Wasm backend had no variant tag for `Some` and raised
   [UnboundVariable "...Some"] at codegen. This feeds the imported-constructor
   decl shape to all three [variant_tags] consumers in [Codegen]: construction
   with an argument (`Some(x)`, `Ok`/`Err`), the zero-arg form (`None`), and
   constructor patterns (`match`). It is the Wasm counterpart to the #137
   Deno-path integration above. *)
let imported_ctors_src = {|
module xmod_ctors;
use prelude::{ Option, Some, None, Result, Ok, Err };

pub fn wrap(x: Int) -> Option<Int> { Some(x) }
pub fn empty() -> Option<Int> { None }

pub fn unwrap_or(o: Option<Int>, d: Int) -> Int {
  match o {
    Some(v) => v,
    None => d,
  }
}

pub fn divide(a: Int, b: Int) -> Result<Int, Int> {
  if b == 0 { Err(0) } else { Ok(a / b) }
}
|}

let test_imported_constructors_wasm () =
  match Parse_driver.parse_string ~file:"<xmod_ctors>" imported_ctors_src with
  | exception e ->
    Alcotest.failf "imported-ctors parse raised: %s" (Printexc.to_string e)
  | prog ->
    (match pipeline_to_wasm prog with
     | Ok m ->
       let names =
         List.map (fun (e : Wasm.export) -> e.Wasm.e_name) m.Wasm.exports in
       List.iter
         (fun fn ->
            Alcotest.(check bool)
              (Printf.sprintf "Wasm module exports %s" fn)
              true (List.mem fn names))
         [ "wrap"; "empty"; "unwrap_or"; "divide" ]
     | Error m ->
       Alcotest.failf
         "imported prelude constructors must codegen to Wasm (#138): %s" m)

let xmod_constructor_tests =
  [ Alcotest.test_case "imported Option/Result constructors -> Wasm" `Quick
      test_imported_constructors_wasm ]

let tests =
  [ ("STAGE-A AOT smoke (#136)", aot_smoke_tests);
    ("STAGE-A multi-module integration (#137)", integration_tests);
    ("cross-module constructor linking, Wasm (#138)", xmod_constructor_tests) ]
