(* SPDX-License-Identifier: MPL-2.0 *)
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

(** parse -> resolve -> typecheck -> BORROW-check an inline source.
    [Ok ()] = borrow-checker accepted; [Error msg] = first failing stage
    ([Error] from [Borrow] is prefixed "Borrow error:"). Used by the
    #225 PR3b affine-capture regression: the borrow checker runs on the
    straight-line AST *before* the WasmGC CPS transform (verified
    pipeline order in bin/main.ml), so the exactly-once obligation on a
    local that the transform later captures into a continuation env is
    enforced here, by the existing checker, with no transform-aware
    machinery. *)
let frontend_borrow (src : string) : (unit, string) result =
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
  let* () =
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Ok _ -> Ok ()
    | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)
  in
  match Borrow.check_program resolve_ctx.symbols prog with
  | Ok () -> Ok ()
  | Error e -> Error ("Borrow error: " ^ Borrow.format_borrow_error e)

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

(* #225 PR3b — affine-capture obligation (ADR-013 obligation 1).

   The WasmGC CPS transform captures a local that is live across the
   async split into the continuation environment. Soundness ("a linear/
   owned capture is consumed exactly once; double-resumption is
   impossible") is discharged BY COMPOSITION, not by transform-aware
   static analysis:

     (a) the borrow checker runs on the straight-line AST *before* the
         codegen transform (pipeline order in bin/main.ml), so a double
         consume of an owned local across the async let is rejected
         there, exactly as in any synchronous body; and
     (b) the PR2 once-resumption trap guarantees the continuation runs
         at most once, so the transformed code performs each consume the
         same number of times the checker already verified.

   These two tests pin (a): the capture-shaped body is borrow-checked
   with the existing machinery, double-use rejected, single-use OK. *)

let async_capture_shape body =
  Printf.sprintf
    "extern type Resource;\n\
     extern fn mkRes() -> Resource;\n\
     extern fn consume(own x: Resource) -> Int;\n\
     extern fn http_request_thenable(url: String, method: String, \
       body: String) -> Int /{Net, Async};\n\
     fn launch() -> Int /{Net, Async} {\n%s\n}" body

(* An owned local captured across the async split and consumed TWICE is
   a use-after-move — rejected by the existing borrow checker on the
   pre-transform AST (no transform-aware rule needed). *)
let async_capture_double_use_rejected () =
  let src =
    async_capture_shape
      "  let x = mkRes();\n\
      \  let r = http_request_thenable(\"u\", \"GET\", \"\");\n\
      \  let a = consume(x);\n\
      \  consume(x)"
  in
  match frontend_borrow src with
  | Ok () ->
    Alcotest.fail "expected a use-after-move borrow error, got Ok"
  | Error m ->
    Alcotest.(check bool) "is a borrow error" true
      (contains ~needle:"Borrow error:" m);
    Alcotest.(check bool) "names a moved value" true
      (contains ~needle:"moved" m)

(* The same capture shape with the owned local consumed exactly ONCE in
   the continuation is accepted — confirms the rule is not over-
   rejecting the legitimate captured-once case the transform relies on. *)
let async_capture_single_use_ok () =
  let src =
    async_capture_shape
      "  let x = mkRes();\n\
      \  let r = http_request_thenable(\"u\", \"GET\", \"\");\n\
      \  consume(x)"
  in
  match frontend_borrow src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok (captured-once), got: %s" m

let tests =
  [
    Alcotest.test_case "declared /{Partial} + ? accepted" `Quick
      declared_partial_ok;
    Alcotest.test_case "PR3b: owned capture double-use rejected" `Quick
      async_capture_double_use_rejected;
    Alcotest.test_case "PR3b: owned capture single-use accepted" `Quick
      async_capture_single_use_ok;
    Alcotest.test_case "declared /{IO} + ? rejected (#59)" `Quick
      declared_missing_partial_rejected;
    Alcotest.test_case "undeclared row stays permissive" `Quick
      undeclared_row_permissive;
    Alcotest.test_case "handled try adds no Partial" `Quick
      handled_try_no_partial;
  ]
