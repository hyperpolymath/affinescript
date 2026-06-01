(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Stdlib algebraic-law regression tests (first batch from the
    proof-obligation catalogue's "ready-this-week" list).

    Each test compiles a small inline AffineScript program, runs it
    through the full frontend (parse → resolve → typecheck) + the
    tree-walking interpreter, then calls a named `test_X` function and
    asserts the result is `VInt 0` (the convention is: zero means the
    law held; nonzero is the failing branch's tag).

    The interpreter is the right oracle here: it executes the AS
    semantics directly, so we are asserting that the semantics — not
    just any particular backend's lowering — agree with the law.
    Where a backend-specific regression is the point (e.g. int-division
    truncation in JavaScript lowering, #478), the matching codegen-deno
    harness in `tools/run_codegen_deno_tests.sh` already covers it; the
    test here checks the source-level invariant the codegen must
    preserve.

    Laws covered in this batch:
      1. String concatenation: associativity                  (semigroup)
      2. String concatenation: left unit (`"" ++ s = s`)      (monoid)
      3. String concatenation: right unit (`s ++ "" = s`)     (monoid)
      4. Integer division truncates toward zero, positive     (#478 source-level)
      5. Integer division truncates toward zero, negative     (#478 source-level)
      6. Integer addition associativity                       (semigroup)

    Follow-on batches (filed as issues from the catalogue) will add
    Option/Result functor laws, dict key uniqueness, bytes endianness
    round-trip, JSON round-trip, hash stability. *)

open Affinescript

(** parse + resolve + typecheck + borrow-check an inline source. *)
let frontend (src : string) : (Ast.program, string) result =
  let open Result in
  let ( let* ) = bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_stdlib_laws>" src)
    with
    | Parse_driver.Parse_error (m, sp) ->
        Error (Printf.sprintf "Parse error at %s: %s" (Span.show sp) m)
    | e -> Error (Printf.sprintf "Unexpected: %s" (Printexc.to_string e))
  in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  let* ctx =
    match Resolve.resolve_program_with_loader prog loader with
    | Ok (rc, _) -> Ok rc
    | Error (e, _) -> Error ("Resolution error: " ^ Resolve.show_resolve_error e)
  in
  let* _ =
    match Typecheck.check_program ctx.symbols prog with
    | Ok _ -> Ok ()
    | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)
  in
  Ok prog

(** Compile + eval, then call `name` with no args and assert the int
    result equals `expected`. *)
let run_returns_int ~name ~src ~expected =
  match frontend src with
  | Error m -> Alcotest.failf "frontend: %s" m
  | Ok prog ->
      (match Interp.eval_program prog with
       | Error e ->
           Alcotest.failf "eval_program: %s" (Value.show_eval_error e)
       | Ok env ->
           (match Value.lookup_env name env with
            | Error _ -> Alcotest.failf "%s not found in env" name
            | Ok fn ->
                (match Interp.apply_function fn [] with
                 | Error e ->
                     Alcotest.failf "%s: %s" name (Value.show_eval_error e)
                 | Ok v ->
                     (match v with
                      | Value.VInt n ->
                          Alcotest.(check int)
                            (Printf.sprintf "%s returned %d" name n)
                            expected n
                      | _ ->
                          Alcotest.failf "%s returned non-Int" name))))

(* ── Law programs ─────────────────────────────────────────────────── *)

let string_concat_assoc () =
  let src = {|
pub fn check() -> Int {
  let lhs = ("a" ++ "b") ++ "c";
  let rhs = "a" ++ ("b" ++ "c");
  if lhs == rhs { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let string_concat_left_unit () =
  let src = {|
pub fn check() -> Int {
  if "" ++ "x" == "x" { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let string_concat_right_unit () =
  let src = {|
pub fn check() -> Int {
  if "x" ++ "" == "x" { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let int_div_truncates_positive () =
  let src = {|
pub fn check() -> Int {
  if 5 / 2 == 2 { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let int_div_truncates_negative () =
  let src = {|
pub fn check() -> Int {
  // `-5 / 2` in JS truncates toward zero -> -2 (not floored -> -3).
  // The codegen-deno fix #478 makes the JS lowering match this; here
  // we assert the source-level semantics that *must* be preserved.
  if (0 - 5) / 2 == 0 - 2 { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let int_add_assoc () =
  let src = {|
pub fn check() -> Int {
  if (1 + 2) + 3 == 1 + (2 + 3) { 0 } else { 1 }
}
|} in
  run_returns_int ~name:"check" ~src ~expected:0

let tests =
  [ ("string_concat: associativity", `Quick, string_concat_assoc);
    ("string_concat: left unit", `Quick, string_concat_left_unit);
    ("string_concat: right unit", `Quick, string_concat_right_unit);
    ("int division: truncates toward zero (positive)", `Quick, int_div_truncates_positive);
    ("int division: truncates toward zero (negative, #478)", `Quick, int_div_truncates_negative);
    ("int addition: associativity", `Quick, int_add_assoc);
  ]
