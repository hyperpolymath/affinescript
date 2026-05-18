(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** Effect-row v1 inference tests (issue #59).

    Covers the one concrete effect source wired for tracking-only v1:
    a catch-less [try] / the [?] desugar performs [Partial], and that is
    checked against an explicitly declared effect row. An undeclared row
    stays permissive. *)

open Affinescript

(** parse -> resolve -> typecheck an inline source string.
    [Ok ()] = frontend accepted; [Error msg] = first stage error. *)
let frontend (src : string) : (unit, string) result =
  let open Result in
  let ( let* ) = bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_effects>" src)
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
  match Typecheck.check_program resolve_ctx.symbols prog with
  | Ok _ -> Ok ()
  | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* A catch-less `?` inside a function whose declared row includes
   Partial is accepted. *)
let declared_partial_ok () =
  match frontend "fn f(r: Int) -> Int /{Partial} { r? }" with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok, got: %s" m

(* The same body in a function declaring only /{IO} must be rejected
   with the issue-#59 effect-mismatch error naming Partial. *)
let declared_missing_partial_rejected () =
  match frontend "fn g(r: Int) -> Int /{IO} { r? }" with
  | Ok () -> Alcotest.fail "expected an effect-mismatch error, got Ok"
  | Error m ->
    Alcotest.(check bool) "mentions Partial" true (contains ~needle:"Partial" m);
    Alcotest.(check bool) "is the #59 mismatch" true (contains ~needle:"#59" m)

(* No declared row → tracking-only v1 stays permissive (no error even
   though the body performs Partial). *)
let undeclared_row_permissive () =
  match frontend "fn h(r: Int) -> Int { r? }" with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected permissive Ok, got: %s" m

(* A handled `try` (catch arm present) discharges the failure and adds
   no Partial, so a /{IO}-only row is fine. *)
let handled_try_no_partial () =
  match
    frontend
      "fn k(r: Int) -> Int /{IO} { try { r } catch { _ => 0 } }"
  with
  | Ok () -> ()
  | Error m -> Alcotest.failf "handled try should add no effect, got: %s" m

let tests =
  [
    Alcotest.test_case "declared /{Partial} + ? accepted" `Quick
      declared_partial_ok;
    Alcotest.test_case "declared /{IO} + ? rejected (#59)" `Quick
      declared_missing_partial_rejected;
    Alcotest.test_case "undeclared row stays permissive" `Quick
      undeclared_row_permissive;
    Alcotest.test_case "handled try adds no Partial" `Quick
      handled_try_no_partial;
  ]
