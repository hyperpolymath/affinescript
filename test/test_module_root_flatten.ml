(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)
(** Regressions found by checking and running a real multi-module program
    (metadatastician/kyq, the estate's KYAML processor) with the compiler:

    - a file in a package sub-directory could not import its siblings by
      their full module path, because modules were looked up from the
      file's own directory;
    - a file found for [A.B] that declares [module A;] was accepted as
      [A.B], so a non-existent submodule imported [A]'s exports;
    - a missing transitive dependency was reported as the importer's own
      import being undefined;
    - flattening carried an imported function before the private helpers it
      calls (and carried no helpers at all for glob and plain imports), so
      the interpreter could not find them;
    - the interpreter had no [++] on arrays and no [==] on chars, records,
      tuples or variants, all of which the checker accepts;
    - the Bun-ESM backend compiled a [return] in a match arm of a [let]
      initializer into a return from the match's IIFE. *)

open Affinescript

let fresh_dir tag =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "as_root_%s_%d" tag (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir

let write_file dir rel body =
  let path = Filename.concat dir rel in
  let parent = Filename.dirname path in
  (try Unix.mkdir parent 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out path in
  output_string oc body;
  close_out oc

let loader_in dir =
  Module_loader.create
    { (Module_loader.default_config ()) with current_dir = dir }

(* ---- module_root_dir ---------------------------------------------------- *)

let root_cases () =
  let check name expected file_dir path =
    Alcotest.(check string) name expected
      (Module_loader.module_root_dir ~file_dir path)
  in
  check "A.B in .../A is rooted above A" "/p/src" "/p/src/Kyaml"
    (Some [ "Kyaml"; "Ijson" ]);
  check "A.B.C in .../A/B is rooted above A" "/p/src" "/p/src/Kyaml/Lint"
    (Some [ "Kyaml"; "Lint"; "RequiredFields" ]);
  check "one-segment name keeps the file's directory" "/p/src" "/p/src"
    (Some [ "Main" ]);
  check "no declaration keeps the file's directory" "/p/src/x" "/p/src/x" None;
  check "a layout that does not match keeps the file's directory"
    "/p/tests" "/p/tests" (Some [ "Kyaml"; "Parser"; "Test" ])

(* ---- declared path must match the requested path ------------------------ *)

let decl_mismatch_rejected () =
  let dir = fresh_dir "mismatch" in
  write_file dir "P.affine" "module P;\npub fn p() -> Int { 1 }\n";
  (match Module_loader.load_module (loader_in dir) [ "P"; "Nope" ] with
   | Error (Module_loader.ModuleDeclMismatch (want, got, _)) ->
     Alcotest.(check (list string)) "requested" [ "P"; "Nope" ] want;
     Alcotest.(check (list string)) "declared" [ "P" ] got
   | Error e ->
     Alcotest.failf "expected ModuleDeclMismatch, got %s"
       (Module_loader.show_load_error e)
   | Ok _ -> Alcotest.fail "P.Nope must not load from P.affine");
  match Module_loader.load_module (loader_in dir) [ "P" ] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "P itself must load: %s"
                 (Module_loader.show_load_error e)

(* ---- a missing dependency is named, not blamed on the importer ---------- *)

let missing_dependency_named () =
  let dir = fresh_dir "missingdep" in
  write_file dir "Q.affine"
    "module Q;\nuse NoSuchModule::*;\npub fn q() -> Int { 1 }\n";
  let prog =
    Parse_driver.parse_string ~file:"<importer>"
      "module Importer;\nuse Q::*;\npub fn main() -> Int { q() }\n"
  in
  match Resolve.resolve_program_with_loader prog (loader_in dir) with
  | Ok _ -> Alcotest.fail "a missing dependency must not resolve"
  | Error (Resolve.UndefinedModule id, _) ->
    Alcotest.failf "the error blames `%s`, not the missing NoSuchModule"
      id.name
  | Error (e, _) ->
    let msg = Resolve.show_resolve_error e in
    Alcotest.(check bool) ("names NoSuchModule: " ^ msg) true
      (Str.string_match (Str.regexp ".*NoSuchModule") msg 0)

(* ---- flattening carries helpers, in declaration order ------------------- *)

let helper_module =
  "module P.M;\n\
   fn helper(s: String) -> Int { 7 }\n\
   pub fn api(s: String) -> Int {\n\
  \  let x = helper(s);\n\
  \  x\n\
   }\n"

let eval_main dir src =
  let prog = Parse_driver.parse_string ~file:"<importer>" src in
  let ld = loader_in dir in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok _ ->
    let flat = Module_loader.flatten_imports ld prog in
    (match Interp.eval_program flat with
     | Error e -> Alcotest.failf "eval: %s" (Value.show_eval_error e)
     | Ok env ->
       (match Value.lookup_env "main" env with
        | Error _ -> Alcotest.fail "no main"
        | Ok main_fn ->
          (match Interp.apply_function main_fn [] with
           | Ok v -> v
           | Error e -> Alcotest.failf "run: %s" (Value.show_eval_error e))))

let flatten_carries_helpers import_line () =
  let dir = fresh_dir "flatten" in
  write_file dir "P/M.affine" helper_module;
  let src =
    "module D;\n" ^ import_line ^ "\npub fn main() -> Int { api(\"a\") }\n"
  in
  match eval_main dir src with
  | Value.VInt 7 -> ()
  | v -> Alcotest.failf "expected 7, got %s" (Value.show_value v)

(* ---- interpreter binary operators the checker accepts ------------------- *)

let binop_cases () =
  let ok name expected got =
    match got with
    | Ok v -> Alcotest.(check bool) name true (Value.value_eq v expected)
    | Error e -> Alcotest.failf "%s: %s" name (Value.show_eval_error e)
  in
  let open Value in
  ok "array ++ array"
    (VArray [| VInt 1; VInt 2 |])
    (Interp.eval_binop Ast.OpConcat (VArray [| VInt 1 |]) (VArray [| VInt 2 |]));
  ok "char == char" (VBool true) (Interp.eval_binop Ast.OpEq (VChar 'a') (VChar 'a'));
  ok "char != char" (VBool true) (Interp.eval_binop Ast.OpNe (VChar 'a') (VChar 'b'));
  ok "char < char" (VBool true) (Interp.eval_binop Ast.OpLt (VChar 'a') (VChar 'b'));
  ok "tuple == tuple" (VBool false)
    (Interp.eval_binop Ast.OpEq (VTuple [ VInt 1; VInt 2 ]) (VTuple [ VInt 1; VInt 3 ]));
  ok "variant == variant" (VBool true)
    (Interp.eval_binop Ast.OpEq (VVariant ("A", Some (VInt 1)))
       (VVariant ("A", Some (VInt 1))))

(* ---- Bun-ESM: return in a match arm of a let initializer ---------------- *)

let return_in_arm_src = {|
module retarm;

pub type R = Good(Int) | Bad(Int)

pub fn f(r: R) -> Int {
  let v = match r {
    Good(n) => n,
    Bad(e) => return e + 100
  };
  v + 1
}

pub fn tail(r: R) -> Int {
  let k = 1;
  match r { Good(n) => n + k, Bad(e) => return e + 200 }
}

pub fn lam(r: R) -> Int {
  let g = |x: Int| -> Int { match r { Good(n) => n + x, Bad(e) => return e + 300 } };
  g(0) + 1
}
|}

let return_in_operand_src = {|
module retop;

pub type R = Good(Int) | Bad(Int)

pub fn f(r: R) -> Int {
  let v = 1 + match r { Good(n) => n, Bad(e) => return e };
  v
}
|}

let node_available () = Sys.command "node --version > /dev/null 2>&1" = 0

let run_node js =
  let dir = fresh_dir "retarm" in
  let file = Filename.concat dir "retarm.mjs" in
  let oc = open_out file in
  output_string oc js;
  close_out oc;
  let ic = Unix.open_process_in ("node " ^ Filename.quote file ^ " 2>&1") in
  let out = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  String.trim out

let return_in_match_arm () =
  if not (node_available ()) then
    Alcotest.fail
      "node is not on PATH. This test is a gate, not an optional extra: \
       it runs the emitted Bun-ESM module (CI installs node 20).";
  let prog = Parse_driver.parse_string ~file:"<retarm>" return_in_arm_src in
  let ld = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    let flat = Module_loader.flatten_imports ld prog in
    (match Codegen_deno.codegen_deno flat rctx.symbols with
     | Error e -> Alcotest.failf "deno codegen: %s" e
     | Ok js ->
       let out =
         run_node
           (js ^ "\nconsole.log(f(Good(1)), f(Bad(5)), tail(Good(1)), \
                   tail(Bad(5)), lam(Good(1)), lam(Bad(5)));\n") in
       (* f: Good(1) binds v = 1 and gives 2; Bad(5) returns 105 from f (the
          old IIFE lowering bound 105 to v and printed 106).  tail and lam:
          the early return leaves the function, or the lambda. *)
       Alcotest.(check string) "let, tail and lambda positions"
         "2 105 2 205 2 306" out)

let return_in_operand_refused () =
  let prog = Parse_driver.parse_string ~file:"<retop>" return_in_operand_src in
  let ld = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_deno prog rctx.symbols with
     | Ok _ ->
       Alcotest.fail
         "a return in a match arm used as an operand must be refused, not \
          compiled to a return from an IIFE"
     | Error msg ->
       Alcotest.(check bool) ("refusal names the cause: " ^ msg) true
         (Str.string_match (Str.regexp ".*return. inside a match arm") msg 0))

let tests =
  [
    Alcotest.test_case "module_root_dir" `Quick root_cases;
    Alcotest.test_case "declared path must match" `Quick decl_mismatch_rejected;
    Alcotest.test_case "missing dependency is named" `Quick missing_dependency_named;
    Alcotest.test_case "list import carries private helper" `Quick
      (flatten_carries_helpers "use P.M::{api};");
    Alcotest.test_case "glob import carries private helper" `Quick
      (flatten_carries_helpers "use P.M::*;");
    Alcotest.test_case "interp binops the checker accepts" `Quick binop_cases;
    Alcotest.test_case "Bun-ESM: return in a let-initializer match arm" `Quick
      return_in_match_arm;
    Alcotest.test_case "Bun-ESM: return in an operand match is refused" `Quick
      return_in_operand_refused;
  ]
