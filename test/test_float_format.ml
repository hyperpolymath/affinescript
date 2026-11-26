(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** #643: float_to_string prints the same text on every backend: the
    shortest round-trip digits in ECMAScript layout (Bun's String(x)).
    The full WASM-vs-Bun sweep is tests/codegen/test_float_to_string.mjs;
    these pin the shared table, the interpreter and the module assembly. *)

open Affinescript

(* (value, what String(x) prints in Bun) *)
let table = [
  (0.0, "0"); (-0.0, "0"); (Float.nan, "NaN");
  (Float.infinity, "Infinity"); (Float.neg_infinity, "-Infinity");
  (3.0, "3"); (-1.5, "-1.5"); (0.1, "0.1"); (0.1 +. 0.2, "0.30000000000000004");
  (1.0 /. 3.0, "0.3333333333333333"); (100.0, "100");
  (1e20, "100000000000000000000"); (1e21, "1e+21"); (1.5e21, "1.5e+21");
  (1e-6, "0.000001"); (1.5e-6, "0.0000015"); (1e-7, "1e-7"); (1.5e-7, "1.5e-7");
  (5e-324, "5e-324"); (2.2250738585072014e-308, "2.2250738585072014e-308");
  (1.7976931348623157e308, "1.7976931348623157e+308");
  (9007199254740992.0, "9007199254740992");
  (267522186230338.625, "267522186230338.62");  (* exact tie: even digit *)
  (12345.678, "12345.678"); (123e-20, "1.23e-18");
]

let test_table () =
  List.iter (fun (x, want) ->
    Alcotest.(check string) want want (Float_format.to_string x)) table

let interp_string src name =
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp: %s" (Value.show_eval_error e)
  | Ok env ->
    match Value.lookup_env name env with
    | Ok (Value.VString s) -> s
    | Ok _ -> Alcotest.failf "%s is not a string" name
    | Error e -> Alcotest.failf "lookup: %s" (Value.show_eval_error e)

(* The interpreter used OCaml's string_of_float ("3.", 12 digits). *)
let test_interp () =
  Alcotest.(check string) "whole float" "3"
    (interp_string "const s: String = float_to_string(3.0);" "s");
  Alcotest.(check string) "17 digits" "0.30000000000000004"
    (interp_string "const s: String = float_to_string(0.1 + 0.2);" "s");
  Alcotest.(check string) "exponent" "1e+21"
    (interp_string "const s: String = float_to_string(1.0e21);" "s")

let wasm src =
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Codegen.generate_module (Opt.fold_constants_program prog) with
  | Ok m -> m
  | Error e -> Alcotest.failf "codegen: %s" (Codegen.show_codegen_error e)

let calls_index n body =
  let rec go = function
    | Wasm.Call i -> i = n
    | Wasm.Block (_, b) | Wasm.Loop (_, b) -> List.exists go b
    | Wasm.If (_, a, b) -> List.exists go a || List.exists go b
    | _ -> false in
  List.exists go body

(* Used to fail with UnboundVariable. The printer is appended after the
   user's functions, and the call site points at it. *)
let test_wasm_assembly () =
  let plain = wasm "pub fn f(x: Float) -> Float { return x; }" in
  let m = wasm "pub fn f(x: Float) -> String { return float_to_string(x); }" in
  Alcotest.(check int) "printer functions appended"
    (List.length plain.funcs + List.length (Wasm_dtoa.funcs ~base:0 ~heap:0
       ~intern:(fun _ -> 0)))
    (List.length m.funcs);
  let imports = List.length (List.filter (fun (i : Wasm.import) ->
    match i.i_desc with Wasm.ImportFunc _ -> true | _ -> false) m.imports) in
  let user_f = List.hd m.funcs in
  Alcotest.(check bool) "call site patched to the printer" true
    (calls_index (imports + List.length plain.funcs) user_f.f_body);
  List.iter (fun (f : Wasm.func) ->
    Alcotest.(check bool) "no sentinel left" false
      (Wasm_dtoa.body_calls_sentinel f.f_body)) m.funcs

let tests = [
  Alcotest.test_case "shared table matches String(x)" `Quick test_table;
  Alcotest.test_case "interpreter uses the shared format" `Quick test_interp;
  Alcotest.test_case "core WASM lowers float_to_string" `Quick test_wasm_assembly;
]
