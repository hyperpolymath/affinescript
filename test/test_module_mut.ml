(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** #548 — module-level mutable binding (`const mut`).

    The parser accepts `const mut <name>: T = init;` as a peer of the
    existing immutable `const <name>: T = init;` at the top-level
    declaration position. The AST carries the mutability on
    [TopConst.tc_mut]; the borrow checker seeds module-level muts into
    each function's [mutable_bindings] so writes from inside function
    bodies are accepted; the Deno-ESM codegen emits a JS `let` (vs
    `const`) so the binding is mutable at runtime.

    These tests pin down the surface-syntax behaviour. A separate
    end-to-end harness exercises the JS execution (`deno run` of the
    Deno-ESM output increments a counter through three call sites and
    confirms the final value is 3). *)

open Affinescript

(** parse -> resolve -> typecheck -> borrow-check an inline source string. *)
let frontend (src : string) : (unit, string) result =
  let open Result in
  let ( let* ) = bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_module_mut>" src)
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
  | Error _ -> Error "Borrow error"

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* Bare `const mut <name>: T = init;` at module scope parses, type-checks
   and borrow-checks. This is the parser slice the issue's first probe
   pinned down (was: parse error at col 1 of line 1 on `let mut`). *)
let bare_const_mut_parses () =
  let src = "const mut counter: Int = 0;\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* `pub` modifier in front of `const mut` is the export form; same parser
   slice. *)
let pub_const_mut_parses () =
  let src = "pub const mut counter: Int = 0;\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Writing to a `const mut` from inside a function body is the load-bearing
   case for the issue's coprocessor-handle / counter-singleton patterns.
   The borrow checker must accept the assignment because the module-mut
   binding was seeded into each function's [mutable_bindings] at the
   program-level entry. *)
let write_to_module_mut_from_fn_accepted () =
  let src =
    "pub const mut counter: Int = 0;\n\
     pub fn bump() -> Int { counter = counter + 1; counter }\n"
  in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* The structural shape from the issue body: a registry-style module-mut
   holding an Option-like value, a function that flips its state.
   The frontend used here doesn't seed the prelude, so the Option /
   Some / None of the issue body are reproduced as a local sum type;
   that's a faithful structural test of the [Resolve+TopConst+Borrow]
   path, just without crossing the stdlib boundary. *)
let coprocessor_handle_pattern_accepted () =
  let src =
    "extern type Bridge;\n\
     pub type Handle = NotReady | Ready(Bridge);\n\
     pub const mut coprocessor: Handle = NotReady;\n\
     pub fn set_coprocessor(b: Bridge) -> Int {\n\
     \  coprocessor = Ready(b);\n\
     \  0\n\
     }\n"
  in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

(* Regression: a plain `const <name>: T = init;` (no `mut`) still parses
   and is JS-immutable. We can't observe JS-level immutability from the
   frontend alone, but we can confirm:
     (a) the parser still accepts the immutable form;
     (b) writes to the immutable form are REJECTED by borrow check,
         exactly as they were before this PR.
   (b) is the negative case that pins the asymmetry between `const` and
   `const mut`. *)
let plain_const_still_works () =
  let src = "const PI: Float = 3.14159;\npub fn area(r: Float) -> Float { PI * r * r }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.fail ("expected ok, got: " ^ m)

let write_to_plain_const_rejected () =
  let src =
    "const counter: Int = 0;\n\
     pub fn bump() -> Int { counter = counter + 1; counter }\n"
  in
  match frontend src with
  | Ok () -> Alcotest.fail "expected borrow error on write to plain `const`"
  | Error m ->
    Alcotest.(check bool) "is a borrow error (not parse/type)" true
      (contains ~needle:"Borrow error" m)

let tests = [
  Alcotest.test_case "bare `const mut` parses" `Quick bare_const_mut_parses;
  Alcotest.test_case "`pub const mut` parses" `Quick pub_const_mut_parses;
  Alcotest.test_case "write to module mut from fn accepted" `Quick
    write_to_module_mut_from_fn_accepted;
  Alcotest.test_case "issue-body smoke: coprocessor handle pattern" `Quick
    coprocessor_handle_pattern_accepted;
  Alcotest.test_case "regression: plain `const` still parses" `Quick
    plain_const_still_works;
  Alcotest.test_case "regression: write to plain `const` rejected" `Quick
    write_to_plain_const_rejected;
]
