(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** #478 — JS-text backend (`lib/js_codegen.ml`) must emit `Math.trunc(a/b)`
    for `Int / Int` and keep plain `/` for `Float / Float`. The Deno-ESM
    backend already does this via the inline classifier in
    `lib/codegen_deno.ml`; this test pins the same behaviour for the
    `--js` / `*.js`-suffix code path that goes through
    [Js_codegen.codegen_js].

    Strategy: compile a tiny snippet through the full frontend
    (parse -> resolve -> typecheck -> js_codegen) and grep the output
    text for [Math.trunc]. Coverage:

    1. Int/Int via params:                idiv(a:Int, b:Int) -> a / b
    2. Int param / int literal:           half(n:Int)        -> n / 2
    3. Int-returning fn call:             quot(a,b)          -> abs_i(a*b) / 4
    4. let-bound int:                     let x = 7 in x / 2
    5. Float/Float must NOT truncate:     fdiv(a:Float, b:Float) -> a / b

    A regression failure looks like `idiv(255, 16) === 15.9375` at the
    JS layer; assertions below catch the upstream miss in OCaml-land
    before the JS ever runs. *)

open Affinescript

let compile_js (src : string) : (string, string) result =
  let open Result in
  let ( let* ) = bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_int_div_js>" src)
    with
    | Parse_driver.Parse_error (m, sp) ->
      Error (Printf.sprintf "Parse error at %s: %s" (Span.show sp) m)
    | e -> Error (Printf.sprintf "Unexpected: %s" (Printexc.to_string e))
  in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  let* resolve_ctx =
    match Resolve.resolve_program_with_loader prog loader with
    | Ok (rc, _) -> Ok rc
    | Error (e, _) -> Error ("Resolution error: " ^ Resolve.show_resolve_error e)
  in
  let* () =
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Ok _ -> Ok ()
    | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)
  in
  Js_codegen.codegen_js prog resolve_ctx.symbols

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let count_occurrences ~needle s =
  let nl = String.length needle and sl = String.length s in
  if nl = 0 then 0 else
  let rec go i n =
    if i + nl > sl then n
    else if String.sub s i nl = needle then go (i + 1) (n + 1)
    else go (i + 1) n
  in
  go 0 0

(* 1. Two Int params, divide -> truncating. *)
let int_params_truncate () =
  let src = "pub fn idiv(a: Int, b: Int) -> Int { a / b }\n" in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    Alcotest.(check bool) "Math.trunc emitted for Int/Int param divide"
      true (contains ~needle:"Math.trunc" js)

(* 2. Int param / int literal -> truncating. *)
let int_param_over_literal_truncates () =
  let src = "pub fn half(n: Int) -> Int { n / 2 }\n" in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    Alcotest.(check bool) "Math.trunc emitted for n / 2"
      true (contains ~needle:"Math.trunc" js)

(* 3. Two Float params, divide -> NO truncation (regression guard). *)
let float_params_no_trunc () =
  let src = "pub fn fdiv(a: Float, b: Float) -> Float { a / b }\n" in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    Alcotest.(check bool) "no Math.trunc for Float/Float"
      false (contains ~needle:"Math.trunc" js)

(* 4. let-bound int divided by int literal -> truncating. *)
let let_bound_int_truncates () =
  let src =
    "pub fn binom_step(n: Int) -> Int {\n\
    \  let x = n + 1;\n\
    \  x / 2\n\
     }\n"
  in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    Alcotest.(check bool) "Math.trunc emitted for let-bound Int / 2"
      true (contains ~needle:"Math.trunc" js)

(* 5. Int-returning fn call as operand -> truncating. The forward call
   in a single program must be recognised because we register all
   Int-returning fns before emitting any body. *)
let int_returning_call_truncates () =
  let src =
    "pub fn abs_i(x: Int) -> Int { if x < 0 { -x } else { x } }\n\
     pub fn lcm_like(a: Int, b: Int) -> Int { abs_i(a * b) / 4 }\n"
  in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    Alcotest.(check bool) "Math.trunc emitted for Int-returning call / 4"
      true (contains ~needle:"Math.trunc" js)

(* 6. Mixed program: float-div and int-div coexist. Verifies the
   classifier is per-function and doesn't accidentally apply to the
   float path. *)
let mixed_int_and_float_in_same_program () =
  let src =
    "pub fn idiv(a: Int, b: Int) -> Int { a / b }\n\
     pub fn fdiv(a: Float, b: Float) -> Float { a / b }\n"
  in
  match compile_js src with
  | Error e -> Alcotest.fail ("compile: " ^ e)
  | Ok js ->
    (* Exactly one Math.trunc — the int path, not the float one. *)
    let n = count_occurrences ~needle:"Math.trunc" js in
    Alcotest.(check int) "exactly one Math.trunc in mixed program" 1 n

let tests = [
  Alcotest.test_case "Int/Int params truncate"          `Quick int_params_truncate;
  Alcotest.test_case "Int param / literal truncates"    `Quick int_param_over_literal_truncates;
  Alcotest.test_case "Float/Float does NOT truncate"    `Quick float_params_no_trunc;
  Alcotest.test_case "let-bound Int truncates"          `Quick let_bound_int_truncates;
  Alcotest.test_case "Int-returning call truncates"     `Quick int_returning_call_truncates;
  Alcotest.test_case "mixed Int/Float — only Int truncates" `Quick mixed_int_and_float_in_same_program;
]
