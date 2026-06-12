(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** #547 — slash-effect-row `fn(...) -> T / { E }` in type-expression
    position.

    Until this PR the `fn(...) -> T / { E }` syntax accepted in fn-decl
    return-type position parse-errored in record-field and type-alias
    positions, forcing every effectful function-typed record field to
    fall back to the older `-{E}->` arrow form. This test module pins
    down the parser slice that closes the asymmetry. *)

open Affinescript

let parse_ok (src : string) : (unit, string) result =
  try
    let _prog = Parse_driver.parse_string ~file:"<test_slash_effect_row>" src in
    Ok ()
  with
  | Parse_driver.Parse_error (m, sp) ->
    Error (Printf.sprintf "Parse error at %s: %s" (Span.show sp) m)
  | e -> Error (Printf.sprintf "Unexpected: %s" (Printexc.to_string e))

(* Slash-row in a type alias — the headline case from the issue body. *)
let alias_braced_single_effect () =
  let src = "pub type AsyncCall = fn(Int) -> Int / { Async };\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Slash-row in a record field — the load-bearing case (effectful
   handler stored in a record). *)
let record_field_braced_effect () =
  let src = "pub type H = { call: fn(Int) -> Int / { Async } }\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Single-effect short form (no braces). *)
let record_field_single_effect_short () =
  let src = "pub type H = { call: fn(Int) -> Int / Async }\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Multi-effect braced row — Frontier Guide `/{IO, Async}` shape. *)
let multi_effect_row () =
  let src = "pub type Multi = fn(Int, String) -> Bool / { IO, Async };\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Arrow-as-type form (no `fn(...)` wrapper) with slash row. *)
let arrow_type_with_braced_row () =
  let src = "pub type R = Int -> Int / { Mut };\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Zero-arg fn with effect row. *)
let zero_arg_fn_with_effect () =
  let src = "pub type R = fn() -> () / { IO };\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Regression: existing `-{E}->` arrow form still parses. *)
let arrow_form_still_works () =
  let src = "pub type H = { call: Int -{Async}-> Int }\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Regression: plain (no effect) fn type still parses. *)
let plain_fn_type_still_works () =
  let src = "pub type H = { call: fn(Int) -> Int }\n" in
  match parse_ok src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

let tests = [
  Alcotest.test_case "type alias with `/{Effect}` braced single"  `Quick alias_braced_single_effect;
  Alcotest.test_case "record field with `/{Effect}` braced"      `Quick record_field_braced_effect;
  Alcotest.test_case "record field with `/Effect` single short"  `Quick record_field_single_effect_short;
  Alcotest.test_case "multi-effect `/{IO, Async}`"               `Quick multi_effect_row;
  Alcotest.test_case "arrow-as-type with braced row"             `Quick arrow_type_with_braced_row;
  Alcotest.test_case "zero-arg fn with effect"                   `Quick zero_arg_fn_with_effect;
  Alcotest.test_case "regression: `-{E}->` arrow form"           `Quick arrow_form_still_works;
  Alcotest.test_case "regression: plain fn type"                 `Quick plain_fn_type_still_works;
]
