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

(* ---- Review round 1 (affinescript#788) ---------------------------------- *)

let normalise_cases () =
  Alcotest.(check string) "a `.` segment is dropped" "/p/src/Kyaml"
    (Module_loader.normalise_dir "/p/src/./Kyaml");
  Alcotest.(check string) "a `..` segment is resolved" "/p/src"
    (Module_loader.normalise_dir "/p/src/Kyaml/..");
  Alcotest.(check string) "the root is found through a `.` segment" "/p/src"
    (Module_loader.module_root_dir ~file_dir:"/p/src/./Kyaml"
       (Some [ "Kyaml"; "Ijson" ]))

(* Rooting a nested module at its package root must not stop a short
   sibling import from resolving (#642). *)
let sibling_import_from_nested () =
  let dir = fresh_dir "sibling" in
  write_file dir "A/Helper.affine" "module Helper;\npub fn h() -> Int { 3 }\n";
  write_file dir "A/Main.affine"
    "module A.Main;\nuse Helper::{h};\npub fn main() -> Int { h() }\n";
  let config =
    Module_loader.config_for_file
      ~file_path:(Filename.concat dir "A/Main.affine") (Some [ "A"; "Main" ])
  in
  Alcotest.(check string) "rooted at the package root"
    (Module_loader.normalise_dir dir) config.current_dir;
  match Module_loader.load_module (Module_loader.create config) [ "Helper" ] with
  | Ok _ -> ()
  | Error e ->
    Alcotest.failf "sibling Helper must load: %s" (Module_loader.show_load_error e)

(* A parent file near the source must not shadow the exact file later on
   the search path. *)
let exact_file_beats_parent () =
  let near = fresh_dir "near" and far = fresh_dir "far" in
  write_file near "P.affine" "module P;\npub fn p() -> Int { 1 }\n";
  write_file far "P/M.affine" "module P.M;\npub fn m() -> Int { 2 }\n";
  let loader =
    Module_loader.create
      { (Module_loader.default_config ()) with
        current_dir = near; search_paths = [ far ] }
  in
  match Module_loader.load_module loader [ "P"; "M" ] with
  | Ok lm ->
    Alcotest.(check string) "loaded far/P/M.affine"
      (Filename.concat far "P/M.affine") lm.mod_file
  | Error e ->
    Alcotest.failf "P.M must load from the search path: %s"
      (Module_loader.show_load_error e)

let failed_dependency_is_not_a_cycle () =
  let dir = fresh_dir "marker" in
  write_file dir "Q.affine" "module Q;\nuse NoSuch::*;\npub fn q() -> Int { 1 }\n";
  let loader = loader_in dir in
  ignore (Module_loader.load_module loader [ "Q" ]);
  match Module_loader.load_module loader [ "Q" ] with
  | Error (Module_loader.ModuleCycle _) ->
    Alcotest.fail "a failed load left Q marked as loading (reported as a cycle)"
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Q cannot load without NoSuch"

let eval_int dir src expected =
  match eval_main dir src with
  | Value.VInt n -> Alcotest.(check int) "result" expected n
  | v -> Alcotest.failf "expected %d, got %s" expected (Value.show_value v)

(* `api` calls `helper` by its own name even when the importer renames it. *)
let alias_keeps_module_binding () =
  let dir = fresh_dir "alias" in
  write_file dir "P/N.affine"
    "module P.N;\npub fn helper() -> Int { 4 }\n\
     pub fn api() -> Int { helper() + 1 }\n";
  eval_int dir
    "module D;\nuse P.N::{helper as h, api};\n\
     pub fn main() -> Int { api() * 10 + h() }\n"
    54

let two_modules_same_private_helper () =
  let dir = fresh_dir "collide" in
  write_file dir "Q/A.affine"
    "module Q.A;\nfn helper() -> Int { 1 }\npub fn a() -> Int { helper() }\n";
  write_file dir "Q/B.affine"
    "module Q.B;\nfn helper() -> Int { 2 }\npub fn b() -> Int { helper() }\n";
  (* Each module calls its own helper. *)
  eval_int dir
    "module D;\nuse Q.A::*;\nuse Q.B::*;\n\
     pub fn main() -> Int { a() * 10 + b() }\n"
    12;
  (* The importer's own `helper` neither replaces nor is replaced by Q.A's. *)
  eval_int dir
    "module D;\nuse Q.A::{a};\nfn helper() -> Int { 99 }\n\
     pub fn main() -> Int { a() + helper() }\n"
    100

(* Qualifying a module's private names leaves a local of the same name
   alone. *)
let qualification_respects_shadowing () =
  let dir = fresh_dir "shadow" in
  write_file dir "Q/C.affine"
    "module Q.C;\nfn helper() -> Int { 1 }\n\
     pub fn c(k: Int) -> Int { let helper = 50; helper + k }\n";
  eval_int dir "module D;\nuse Q.C::{c};\npub fn main() -> Int { c(1) }\n" 51

let diamond_import () =
  let dir = fresh_dir "diamond" in
  write_file dir "P/M.affine" helper_module;
  write_file dir "J.affine"
    "module J;\nuse P.M::{api};\npub fn j() -> Int { api(\"x\") + 1 }\n";
  eval_int dir
    "module D;\nuse P.M::{api};\nuse J::{j};\n\
     pub fn main() -> Int { api(\"a\") + j() }\n"
    15

let main_only_from_root () =
  let parse src = Parse_driver.parse_string ~file:"<main>" src in
  Alcotest.(check bool) "a root with main" true
    (Module_loader.declares_main (parse "module R;\npub fn main() -> Int { 0 }\n"));
  Alcotest.(check bool) "a root without main" false
    (Module_loader.declares_main (parse "module R;\npub fn other() -> Int { 0 }\n"))

let id name = { Ast.name; span = Span.dummy }

let free_vars_see_records () =
  let e =
    Ast.ExprRecord
      { er_fields = [ (id "k", None); (id "f", Some (Ast.ExprVar (id "v"))) ];
        er_spread = Some (Ast.ExprVar (id "base")) }
  in
  let free = Ast.find_free_vars [] e in
  List.iter (fun n ->
      Alcotest.(check bool) (n ^ " is free") true (List.mem n free))
    [ "k"; "v"; "base" ];
  Alcotest.(check bool) "a bound shorthand is not free" false
    (List.mem "k" (Ast.find_free_vars [ "k" ] e))

let contains_return_sees_aggregates () =
  let ret = Ast.ExprReturn None in
  List.iter (fun (what, e) ->
      Alcotest.(check bool) what true (Ast.expr_contains_return e))
    [ ("tuple", Ast.ExprTuple [ Ast.ExprVar (id "x"); ret ]);
      ("array", Ast.ExprArray [ ret ]);
      ("record", Ast.ExprRecord { er_fields = [ (id "f", Some ret) ]; er_spread = None });
      ("index", Ast.ExprIndex (Ast.ExprArray [ ret ], Ast.ExprVar (id "i"))) ]

let compile_deno src =
  let prog = Parse_driver.parse_string ~file:"<deno>" src in
  let ld = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Error ("resolve: " ^ Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    Codegen_deno.codegen_deno (Module_loader.flatten_imports ld prog) rctx.symbols

let guards_wildcards_and_temporaries () =
  if not (node_available ()) then
    Alcotest.fail "node is not on PATH (this test runs the emitted module)";
  let src = {|
module guards;
pub type R = Good(Int) | Bad(Int)
pub fn f(r: R) -> Int {
  let v = match r { Good(n) if n > 10 => n, Good(n) => n + 100, Bad(e) => return e };
  v
}
pub fn g(r: R) -> Int {
  let _ = match r { Good(n) => n, Bad(e) => return e + 1000 };
  7
}
pub fn h() -> Int {
  let __let_tmp_1 = 5;
  let w = match Good(1) { Good(n) => n, Bad(e) => return e };
  __let_tmp_1 + w
}
|} in
  match compile_deno src with
  | Error e -> Alcotest.failf "deno codegen: %s" e
  | Ok js ->
    (* f: the guard reads its own binding (Good(5) fails the guard and takes
       the next arm); g: `let _` keeps the early return; h: a source name
       that looks like a temporary does not collide with one. *)
    Alcotest.(check string) "f f f g g h" "105 20 3 1001 7 6"
      (run_node (js ^ "\nconsole.log(f(Good(5)), f(Good(20)), f(Bad(3)), \
                       g(Bad(1)), g(Good(1)), h());\n"))

let return_in_condition_refused () =
  let prelude =
    "module refuse;\npub type R = Good(Int) | Bad(Int)\n\
     fn id(x: Int) -> Int { x }\nfn yes(x: Int) -> Bool { true }\n"
  in
  List.iter (fun (what, body) ->
      match compile_deno (prelude ^ body) with
      | Ok _ -> Alcotest.failf "%s: a return there must be refused" what
      | Error msg ->
        Alcotest.(check bool) (what ^ ": " ^ msg) true
          (Str.string_match (Str.regexp ".*return. inside") msg 0))
    [ ("if condition",
       "pub fn f(r: R) -> Int { let v = if yes(return 1) { 2 } else { 3 }; v }\n");
      ("scrutinee",
       "pub fn f(r: R) -> Int { let v = match id(return 1) { 0 => 2, _ => 3 }; v }\n");
      ("guard",
       "pub fn f(r: R) -> Int { let v = match r { Good(n) if yes(return 9) => n, _ => 3 }; v }\n") ]

(* A guard is compiled against the integer tracking it runs under, not the
   one the arm's body leaves behind: the body's `let n = 0.5` must not turn
   the guard's `n / 2` into float division (CodeRabbit on #797). *)
let guard_ignores_body_int_tracking () =
  if not (node_available ()) then
    Alcotest.fail "node is not on PATH (this test runs the emitted module)";
  let src = {|
module guardint;
pub fn k(n: Int) -> Int {
  let _ = match n { m if n / 2 == 2 => { let n = 0.5; 0 }, _ => return 9 };
  1
}
|} in
  match compile_deno src with
  | Error e -> Alcotest.failf "deno codegen: %s" e
  | Ok js ->
    (* 5 / 2 truncates to 2: the guard holds, so k(5) falls through to 1. *)
    Alcotest.(check string) "k(5) k(9)" "1 9"
      (run_node (js ^ "\nconsole.log(k(5), k(9));\n"))

(* A `return` the statement lowering cannot keep (inside an array, so inside
   an IIFE) is refused rather than dropped. *)
let return_in_array_arm_refused () =
  let src =
    "module arr;\n\
     pub fn f(b: Bool) -> Int {\n\
    \  let _ = match b { true => [{ return 7; 0 }], false => return 9 };\n\
    \  11\n}\n"
  in
  match compile_deno src with
  | Ok _ -> Alcotest.fail "a return inside an array arm must be refused"
  | Error msg ->
    Alcotest.(check bool) msg true
      (Str.string_match (Str.regexp ".*return. inside") msg 0)

(* An original a module carries for its own callers, but which the importer
   binds only under an alias, does not occupy the importer's namespace: two
   modules' `helper`s stay apart (CodeRabbit on #797). *)
let aliased_originals_stay_apart () =
  let dir = fresh_dir "aliasapart" in
  write_file dir "R/A.affine"
    "module R.A;\npub fn helper() -> Int { 1 }\npub fn api() -> Int { helper() }\n";
  write_file dir "R/B.affine" "module R.B;\npub fn helper() -> Int { 2 }\n";
  eval_int dir
    "module D;\nuse R.A::{helper as ah, api};\nuse R.B::{helper as bh};\n\
     pub fn main() -> Int { api() * 100 + ah() * 10 + bh() }\n"
    112

(* Module paths that join to the same text (`S_T.U`, `S.T_U`) still give
   their private helpers different internal names. *)
let internal_names_are_injective () =
  Alcotest.(check bool) "distinct" true
    (Module_loader.qualified_name [ "S_T"; "U" ] "h"
     <> Module_loader.qualified_name [ "S"; "T_U" ] "h");
  let dir = fresh_dir "inject" in
  write_file dir "S_T/U.affine"
    "module S_T.U;\nfn h() -> Int { 1 }\npub fn u1() -> Int { h() }\n";
  write_file dir "S/T_U.affine"
    "module S.T_U;\nfn h() -> Int { 2 }\npub fn u2() -> Int { h() }\n";
  eval_int dir
    "module D;\nuse S_T.U::{u1};\nuse S.T_U::{u2};\n\
     pub fn main() -> Int { u1() * 10 + u2() }\n"
    12

(* Aliasing a mutable module binding would make a second, separate copy: it
   is a structured flattening error, which the CLI reports as a diagnostic. *)
let mutable_alias_refused () =
  let dir = fresh_dir "mutalias" in
  write_file dir "V/M.affine"
    "module V.M;\npub const mut counter: Int = 0;\n\
     pub fn get() -> Int { counter }\n";
  let src =
    "module D;\nuse V.M::{counter as c, get};\npub fn main() -> Int { get() }\n"
  in
  let prog = Parse_driver.parse_string ~file:"<importer>" src in
  let ld = loader_in dir in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok _ ->
    (match Module_loader.flatten_imports ld prog with
     | _ -> Alcotest.fail "aliasing a mutable binding must be refused"
     | exception Module_loader.Flatten_error
                   (Module_loader.MutableAlias ("counter", "c", [ "V"; "M" ])) ->
       ())

(* ---- #801: one flattened namespace, collision-free ---------------------- *)

(* Two modules each define a function of one name; each module's code must
   call its own.  [label] is public in both, as in #801's reproduction, then
   private in both (same failure, reported in the issue too). *)
let label_modules dir vis =
  write_file dir "Lib/A.affine"
    (Printf.sprintf "module Lib.A;\n%sfn label() -> Int { 1 }\n\
                     pub fn a_label() -> Int { label() }\n" vis);
  write_file dir "Lib/B.affine"
    (Printf.sprintf "module Lib.B;\n%sfn label() -> Int { 2 }\n\
                     pub fn b_label() -> Int { label() }\n" vis)

let label_main =
  "module Main;\nuse Lib.A::{a_label};\nuse Lib.B::{b_label};\n\
   pub fn main() -> Int { a_label() * 10 + b_label() }\n"

(* Compile [src] (with the modules under [dir]) to Bun-ESM and run it under
   node with [expr] printed. *)
let run_flat_js dir src expr =
  let prog = Parse_driver.parse_string ~file:"<main>" src in
  let ld = loader_in dir in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_bun (Module_loader.flatten_imports ld prog)
             rctx.symbols with
     | Error e -> Alcotest.failf "Bun-ESM codegen: %s" e
     | Ok js ->
       (* Drop the auto-call of main: the test prints what it needs. *)
       let js =
         String.concat "\n"
           (List.filter (fun l -> l <> "await main();")
              (String.split_on_char '\n' js))
       in
       run_node (js ^ "\nconsole.log(" ^ expr ^ ");\n"))

let test_801_same_name_in_two_modules () =
  List.iter (fun vis ->
      let dir = fresh_dir ("i801" ^ String.trim vis) in
      label_modules dir vis;
      eval_int dir label_main 12;
      if node_available () then
        Alcotest.(check string) ("Bun-ESM, " ^ vis ^ "label") "12"
          (run_flat_js dir label_main "main()"))
    [ "pub "; "" ]

(* The shape #801's reproduction has on main after #797: the colliding
   functions reach the root only through other modules' bodies.  X uses
   A's [f] and Y uses B's; the old flattener kept one [f] for both. *)
let test_801_transitive_public_and_const () =
  let dir = fresh_dir "t801" in
  write_file dir "Lib/A.affine"
    "module Lib.A;\npub fn f() -> Int { 1 }\npub const K: Int = 100;\n";
  write_file dir "Lib/B.affine"
    "module Lib.B;\npub fn f() -> Int { 2 }\npub const K: Int = 200;\n";
  write_file dir "Lib/X.affine"
    "module Lib.X;\nuse Lib.A::{f, K};\npub fn via_a() -> Int { f() + K }\n";
  write_file dir "Lib/Y.affine"
    "module Lib.Y;\nuse Lib.B::*;\npub fn via_b() -> Int { f() + K }\n";
  let src =
    "module Main;\nuse Lib.X::{via_a};\nuse Lib.Y::{via_b};\n\
     pub fn main() -> Int { via_a() * 1000 + via_b() }\n"
  in
  eval_int dir src 101202;
  if node_available () then
    Alcotest.(check string) "Bun-ESM" "101202" (run_flat_js dir src "main()")

(* Externs keep their host names: an alias is rewritten to the extern's own
   name, never carried as an extern of the alias's name. *)
let test_801_externs_keep_host_names () =
  let dir = fresh_dir "x801" in
  write_file dir "H.affine"
    "module H;\npub extern fn host_thing(x: Int) -> Int;\n\
     pub fn twice(x: Int) -> Int { host_thing(host_thing(x)) }\n";
  let src =
    "module Main;\nuse H::{host_thing as ht, twice};\n\
     pub fn main() -> Int { ht(1) + twice(2) }\n"
  in
  let prog = Parse_driver.parse_string ~file:"<main>" src in
  let ld = loader_in dir in
  (match Resolve.resolve_program_with_loader prog ld with
   | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
   | Ok _ -> ());
  let flat = Module_loader.flatten_imports ld prog in
  let externs, others =
    List.partition Module_loader.is_extern flat.Ast.prog_decls in
  Alcotest.(check (list string)) "one extern, under its host name"
    [ "host_thing" ] (List.filter_map Module_loader.value_name externs);
  Alcotest.(check bool) "no declaration named after the alias" false
    (List.mem "ht" (List.filter_map Module_loader.value_name others));
  let main_refs =
    List.concat_map (function
        | Ast.TopFn fd when fd.fd_name.name = "main" ->
          Module_loader.top_level_free_names (Ast.TopFn fd)
        | _ -> []) flat.prog_decls
  in
  Alcotest.(check bool) "main calls the extern by its host name" true
    (List.mem "host_thing" main_refs && not (List.mem "ht" main_refs))

(* The backstop: a flattened program in which two different bindings share a
   name is refused, whatever produced it. *)
let test_801_planted_collision_refused () =
  let fn_decl src =
    match (Parse_driver.parse_string ~file:"<p>" src).prog_decls with
    | [ d ] -> d
    | _ -> Alcotest.fail "expected one declaration"
  in
  let a = fn_decl "fn label() -> Int { 1 }" in
  let b = fn_decl "fn label() -> Int { 2 }" in
  (match
     Module_loader.check_unique
       [ { origin = [ "Lib"; "A" ]; decl = a }; { origin = [ "Lib"; "B" ]; decl = b } ]
   with
   | () -> Alcotest.fail "two different `label`s must be refused"
   | exception Module_loader.Flatten_error
                 (Module_loader.ImportCollision ("label", _, _)) -> ());
  (* A function with an extern's name: a backend would lower calls to it as
     host calls.  The root's own `host_thing` meets the extern H carries. *)
  let dir = fresh_dir "p801" in
  write_file dir "H.affine"
    "module H;\nextern fn host_thing(x: Int) -> Int;\n\
     pub fn twice(x: Int) -> Int { host_thing(host_thing(x)) }\n";
  let src =
    "module Main;\nuse H::{twice};\nfn host_thing(x: Int) -> Int { x }\n\
     pub fn main() -> Int { twice(host_thing(2)) }\n"
  in
  let prog = Parse_driver.parse_string ~file:"<main>" src in
  let ld = loader_in dir in
  (match Resolve.resolve_program_with_loader prog ld with
   | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
   | Ok _ -> ());
  (match Module_loader.flatten_imports ld prog with
   | _ -> Alcotest.fail "a function named like a carried extern must be refused"
   | exception Module_loader.Flatten_error
                 (Module_loader.ImportCollision ("host_thing", _, _)) -> ());
  (* The JS emitters refuse a duplicate name even when handed one directly
     (Bun keeps the last `function` of a name without complaint). *)
  let dup = { (Parse_driver.parse_string ~file:"<d>" "") with
              Ast.prog_decls = [ a; b ] } in
  let refused what = function
    | Ok _ -> Alcotest.failf "%s emitted two `label`s" what
    | Error msg ->
      Alcotest.(check bool) (what ^ ": " ^ msg) true
        (Str.string_match (Str.regexp ".*`label`.*#801") msg 0)
  in
  refused "Bun-ESM" (Codegen_deno.codegen_bun dup (Symbol.create ()));
  refused "JS" (Js_codegen.codegen_js dup (Symbol.create ()))

(* Enum constructors keep their names.  Two enums' identical constructors are
   one runtime value and are emitted once; constructors of one name and
   different shapes are refused. *)
let test_801_enum_constructors () =
  let dir = fresh_dir "e801" in
  write_file dir "Lib/A.affine"
    "module Lib.A;\npub enum Col { Red, Blue }\n\
     pub fn a_label() -> Int { match Red { Red => 1, Blue => 3 } }\n";
  write_file dir "Lib/B.affine"
    "module Lib.B;\npub enum Paint { Blue, Red }\n\
     pub fn b_label() -> Int { match Blue { Red => 4, Blue => 2 } }\n";
  if node_available () then
    Alcotest.(check string) "identical constructors shared" "12"
      (run_flat_js dir label_main "main()");
  write_file dir "Lib/B.affine"
    "module Lib.B;\npub enum Paint { Blue(Int), Red }\n\
     pub fn b_label() -> Int { match Red { Red => 2, Blue(n) => n } }\n";
  let prog = Parse_driver.parse_string ~file:"<main>" label_main in
  let ld = loader_in dir in
  match Resolve.resolve_program_with_loader prog ld with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_bun (Module_loader.flatten_imports ld prog)
             rctx.symbols with
     | Ok _ -> Alcotest.fail "`Blue` and `Blue(Int)` must not share a binding"
     | Error msg ->
       Alcotest.(check bool) msg true
         (Str.string_match (Str.regexp ".*`Blue`.*#801") msg 0))

(* Trait impl methods are emitted as free functions under their own names
   (trait dispatch is not lowered on the non-Wasm backends), so two impls of
   one trait gave two `function eq`s and Bun silently kept the second.  They
   are refused until impl methods get their own names. *)
let test_801_impl_methods_refused () =
  let src = {|
module impls;
trait Same { fn same(ref self, ref other: Self) -> Bool; }
impl Same for Int { pub fn same(ref self, ref other: Int) -> Bool { self == other } }
impl Same for Bool { pub fn same(ref self, ref other: Bool) -> Bool { self == other } }
|} in
  match compile_deno src with
  | Ok _ -> Alcotest.fail "two impls' `same` must not both be emitted as `same`"
  | Error msg ->
    Alcotest.(check bool) msg true
      (Str.string_match (Str.regexp ".*`same`.*#801") msg 0)

let test_801_impl_reads_imported_mutable () =
  (* #801 review follow-ups (CodeRabbit on #802).  The comments sit inside
     the bodies: the soundness ledger pins the preceding test up to the next
     top-level `let`.  A root impl method that reads an imported mutable
     constant refers to the qualified storage, which is carried. *)
  let dir = fresh_dir "im801" in
  write_file dir "V/M.affine"
    "module V.M;\npub const mut counter: Int = 7;\n";
  let src =
    "module D;\nuse V.M::{counter};\n\
     trait Peek { fn peek(ref self) -> Int; }\n\
     impl Peek for Int { pub fn peek(ref self) -> Int { counter } }\n\
     pub fn main() -> Int { 1 }\n"
  in
  let prog = Parse_driver.parse_string ~file:"<main>" src in
  let ld = loader_in dir in
  (match Resolve.resolve_program_with_loader prog ld with
   | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
   | Ok _ -> ());
  let flat = Module_loader.flatten_imports ld prog in
  let q = Module_loader.qualified_name [ "V"; "M" ] "counter" in
  let impl_refs =
    List.concat_map (function
        | Ast.TopImpl _ as d -> Module_loader.top_level_free_names d
        | _ -> []) flat.prog_decls
  in
  Alcotest.(check bool) "the method reads the qualified storage" true
    (List.mem q impl_refs && not (List.mem "counter" impl_refs));
  Alcotest.(check bool) "the storage is carried" true
    (List.mem q (List.filter_map Module_loader.value_name flat.prog_decls))

(* The root's copy of an imported immutable array is the module's value, so
   reference equality between the two holds, as it did before #801. *)
let test_801_immutable_import_one_value () =
  let dir = fresh_dir "iv801" in
  write_file dir "A.affine"
    "module A;\npub const K: [Int] = [1, 2];\n\
     pub fn original() -> [Int] { K }\n";
  let src =
    "module Main;\nuse A::{K, original};\n\
     pub fn main() -> Bool { original() == K }\n"
  in
  let prog = Parse_driver.parse_string ~file:"<main>" src in
  let ld = loader_in dir in
  (match Resolve.resolve_program_with_loader prog ld with
   | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
   | Ok _ -> ());
  let flat = Module_loader.flatten_imports ld prog in
  let q = Module_loader.qualified_name [ "A" ] "K" in
  (match List.find_opt (function
       | Ast.TopConst c -> c.tc_name.name = "K" | _ -> false) flat.prog_decls with
   | Some (Ast.TopConst { tc_value = Ast.ExprVar v; _ }) ->
     Alcotest.(check string) "K refers to the module's K" q v.name
   | _ -> Alcotest.fail "expected `K` bound to the module's declaration");
  if node_available () then
    Alcotest.(check string) "Bun-ESM: one runtime value" "true"
      (run_flat_js dir src "main()")

(* `__mod_` is reserved: a source binder of that form could capture a
   rewritten reference. *)
let test_801_reserved_prefix () =
  match
    Parse_driver.parse_string ~file:"<r>"
      "fn f() -> Int { let __mod_1A__K = 2; __mod_1A__K }\n"
  with
  | _ -> Alcotest.fail "an identifier starting with `__mod_` must be refused"
  | exception e ->
    let msg = Printexc.to_string e in
    Alcotest.(check bool) msg true
      (Str.string_match (Str.regexp ".*__mod_.*reserved") msg 0)

(* JS: a top-level binding may not take a name the prelude declares. *)
let test_801_js_prelude_names () =
  let prog = Parse_driver.parse_string ~file:"<j>"
      "fn println(s: String) -> Int { 1 }\n" in
  match Js_codegen.codegen_js prog (Symbol.create ()) with
  | Ok _ -> Alcotest.fail "a second `println` must be refused"
  | Error msg ->
    Alcotest.(check bool) msg true
      (Str.string_match (Str.regexp ".*`println`.*#801") msg 0)

let test_801_js_mangled_ctor_clash () =
  (* `class` is a JS reserved word, emitted as `class_`; a constructor
     `class_` beside it must be refused, not silently merged. *)
  let prog = Parse_driver.parse_string ~file:"<j>"
      "enum E { class, class_ }\nfn main() -> Int { 0 }\n" in
  match Js_codegen.codegen_js prog (Symbol.create ()) with
  | Ok _ -> Alcotest.fail "`class` and `class_` must not share one binding"
  | Error msg ->
    Alcotest.(check bool) msg true
      (Str.string_match (Str.regexp ".*`class_`.*#801") msg 0)

let test_801_preamble_ctor_arity () =
  (* A variant named like a runtime constructor shares the runtime's binding
     only at the runtime's arity; a nullary `Some` is refused on both JS
     backends rather than becoming the unary factory. *)
  let bad = "enum Choice { Some, Other }\nfn main() -> Int { 0 }\n" in
  let ok = "enum Maybe { Some(Int), None }\n\
            fn main() -> Int { match Some(2) { Some(n) => n, None => 0 } }\n" in
  let refused = Str.regexp ".*`Some` of `Choice`.*#801" in
  (match compile_deno bad with
   | Ok _ -> Alcotest.fail "Bun-ESM: a nullary `Some` must be refused"
   | Error m -> Alcotest.(check bool) m true (Str.string_match refused m 0));
  (match Js_codegen.codegen_js (Parse_driver.parse_string ~file:"<j>" bad)
           (Symbol.create ()) with
   | Ok _ -> Alcotest.fail "JS: a nullary `Some` must be refused"
   | Error m -> Alcotest.(check bool) m true (Str.string_match refused m 0));
  (match compile_deno ok with
   | Ok _ -> ()
   | Error e -> Alcotest.failf "Bun-ESM: a unary `Some` is the runtime's: %s" e)

let test_801_escaped_extern_clash () =
  (* An extern `class` is called as `class_`; a root fn `class_` would
     receive that call, so Bun-ESM refuses it. *)
  match compile_deno
          "extern fn class(x: Int) -> Int;\nfn class_(x: Int) -> Int { x }\n\
           fn main() -> Int { class(1) }\n" with
  | Ok _ -> Alcotest.fail "a root `class_` beside an extern `class` must be refused"
  | Error m ->
    Alcotest.(check bool) m true (Str.string_match (Str.regexp ".*`class_`.*#801") m 0)

let test_801_js_prelude_names_complete () =
  (* Every lower-case name the JS prelude declares at top level, including
     the later declarators of a `let a = …, b = …;` line, is reserved. *)
  let first = Str.regexp "^\\(const\\|let\\|var\\|function\\) +\\([a-z_][A-Za-z0-9_]*\\)" in
  let later = Str.regexp ", *\\([a-z_][A-Za-z0-9_]*\\) *=" in
  let names =
    String.split_on_char '\n' Js_codegen.prelude
    |> List.concat_map (fun l ->
        if not (Str.string_match first l 0) then []
        else
          let rec more pos acc =
            match Str.search_forward later l pos with
            | p -> more (p + 1) (Str.matched_group 1 l :: acc)
            | exception Not_found -> List.rev acc
          in
          let n0 = Str.matched_group 2 l in
          n0 :: (if String.starts_with ~prefix:"function" l then [] else more 0 []))
  in
  Alcotest.(check bool) "prelude declares __as_stdin_off" true
    (List.mem "__as_stdin_off" names);
  List.iter (fun n ->
      let prog = Parse_driver.parse_string ~file:"<j>"
          (Printf.sprintf "fn %s() -> Int { 1 }\n" n) in
      match Js_codegen.codegen_js prog (Symbol.create ()) with
      | Ok _ -> Alcotest.failf "a root `%s` must be refused beside the prelude's" n
      | Error _ -> ())
    names

(* Bun-ESM: a constructor a private and a public enum share is exported,
   whichever enum comes first. *)
let test_801_shared_constructor_exported () =
  match compile_deno
          "module pe;\nenum Hidden { Red, Green }\npub enum Shown { Red, Blue }\n" with
  | Error e -> Alcotest.failf "Bun-ESM: %s" e
  | Ok js ->
    Alcotest.(check bool) "Red is exported once" true
      (Str.string_match (Str.regexp "\\(.\\|\n\\)*export const Red = ") js 0);
    Alcotest.(check bool) "Green stays private" false
      (Str.string_match (Str.regexp "\\(.\\|\n\\)*export const Green") js 0)

let tests =
  [
    Alcotest.test_case "#801: impl method reads an imported mutable" `Quick
      test_801_impl_reads_imported_mutable;
    Alcotest.test_case "#801: an immutable import is one runtime value" `Quick
      test_801_immutable_import_one_value;
    Alcotest.test_case "#801: the __mod_ prefix is reserved" `Quick
      test_801_reserved_prefix;
    Alcotest.test_case "#801: JS refuses prelude names" `Quick
      test_801_js_prelude_names;
    Alcotest.test_case "#801: JS reserves every prelude name" `Quick
      test_801_js_prelude_names_complete;
    Alcotest.test_case "#801: JS refuses a mangled constructor clash" `Quick
      test_801_js_mangled_ctor_clash;
    Alcotest.test_case "#801: runtime constructor names keep their arity" `Quick
      test_801_preamble_ctor_arity;
    Alcotest.test_case "#801: Bun-ESM refuses an escaped extern clash" `Quick
      test_801_escaped_extern_clash;
    Alcotest.test_case "#801: a shared constructor stays exported" `Quick
      test_801_shared_constructor_exported;
    Alcotest.test_case "#801: same-named functions in two modules" `Quick
      test_801_same_name_in_two_modules;
    Alcotest.test_case "#801: transitive public functions and constants" `Quick
      test_801_transitive_public_and_const;
    Alcotest.test_case "#801: externs keep their host names" `Quick
      test_801_externs_keep_host_names;
    Alcotest.test_case "#801: a planted collision is refused" `Quick
      test_801_planted_collision_refused;
    Alcotest.test_case "#801: enum constructors" `Quick test_801_enum_constructors;
    Alcotest.test_case "#801: impl methods sharing a name are refused" `Quick
      test_801_impl_methods_refused;
    Alcotest.test_case "Bun-ESM: a guard ignores the body's int tracking" `Quick
      guard_ignores_body_int_tracking;
    Alcotest.test_case "Bun-ESM: return inside an array arm refused" `Quick
      return_in_array_arm_refused;
    Alcotest.test_case "aliased originals stay apart" `Quick
      aliased_originals_stay_apart;
    Alcotest.test_case "internal names are injective" `Quick
      internal_names_are_injective;
    Alcotest.test_case "aliasing a mutable binding is refused" `Quick
      mutable_alias_refused;
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
    Alcotest.test_case "simple import carries private helper" `Quick
      (flatten_carries_helpers "use P.M;");
    Alcotest.test_case "normalise_dir" `Quick normalise_cases;
    Alcotest.test_case "sibling import from a nested module" `Quick
      sibling_import_from_nested;
    Alcotest.test_case "exact file beats parent file" `Quick exact_file_beats_parent;
    Alcotest.test_case "failed dependency is not a cycle" `Quick
      failed_dependency_is_not_a_cycle;
    Alcotest.test_case "alias keeps the module binding" `Quick
      alias_keeps_module_binding;
    Alcotest.test_case "same-named private helpers stay distinct" `Quick
      two_modules_same_private_helper;
    Alcotest.test_case "qualification respects shadowing" `Quick
      qualification_respects_shadowing;
    Alcotest.test_case "diamond import" `Quick diamond_import;
    Alcotest.test_case "main only from the root" `Quick main_only_from_root;
    Alcotest.test_case "free vars see record shorthand and spread" `Quick
      free_vars_see_records;
    Alcotest.test_case "contains_return sees aggregates" `Quick
      contains_return_sees_aggregates;
    Alcotest.test_case "Bun-ESM: guards, let _, temporaries" `Quick
      guards_wildcards_and_temporaries;
    Alcotest.test_case "Bun-ESM: return in condition, scrutinee, guard refused" `Quick
      return_in_condition_refused;
  ]
