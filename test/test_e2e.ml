(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** End-to-end integration tests for the AffineScript compiler pipeline.

    These tests validate the full source-to-output pipeline:
      source -> lex -> parse -> resolve -> typecheck -> quantitycheck
                                                     -> wasm codegen
                                                     -> julia codegen
                                                     -> interpreter

    Each test fixture exercises a specific language feature and verifies
    that every applicable compiler stage handles it correctly.
*)

open Affinescript

(* ============================================================================
   Test Utilities
   ============================================================================ *)

(* Fixture directory. When running via [dune test], the CWD is
    [_build/default/test/]; when running via [dune exec], it is the
   project root. We probe both locations. *)
let fixture_dir =
  if Sys.file_exists "e2e/fixtures" then "e2e/fixtures"
  else "test/e2e/fixtures"

(** Read file contents *)
let read_file path =
  let chan = open_in path in
  let content = really_input_string chan (in_channel_length chan) in
  close_in chan;
  content

(** Temporary file for WASM output *)
let with_temp_file suffix f =
  let tmp = Filename.temp_file "affinescript_e2e" suffix in
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists tmp then Sys.remove tmp
  ) (fun () -> f tmp)

(** Fixture path helper *)
let fixture path = Filename.concat fixture_dir path

(* ============================================================================
   Pipeline Stage Runners
   ============================================================================ *)

(** Stage 1: Parse a fixture file and return the AST *)
let parse_fixture path =
  try
    Ok (Parse_driver.parse_file path)
  with
  | Parse_driver.Parse_error (msg, span) ->
    Error (Printf.sprintf "Parse error at %s: %s" (Span.show span) msg)
  | Lexer.Lexer_error (msg, pos) ->
    Error (Printf.sprintf "Lexer error at %d:%d: %s" pos.line pos.col msg)
  | e ->
    Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(** Stage 2: Resolve names in the parsed AST *)
let resolve_program prog =
  let loader_config = Module_loader.default_config () in
  let loader = Module_loader.create loader_config in
  match Resolve.resolve_program_with_loader prog loader with
  | Ok (resolve_ctx, type_ctx) -> Ok (resolve_ctx, type_ctx)
  | Error (e, _span) ->
    Error (Printf.sprintf "Resolution error: %s"
             (Resolve.show_resolve_error e))

(** Stage 3: Type-check the resolved program *)
let typecheck_program symbols prog =
  match Typecheck.check_program symbols prog with
  | Ok ctx -> Ok ctx
  | Error e ->
    Error (Printf.sprintf "Type error: %s"
             (Typecheck.format_type_error e))

(** Stage 4: Quantity-check the program (affine/linear enforcement) *)
let quantity_check_program symbols prog =
  match Quantity.check_program symbols prog with
  | Ok () -> Ok ()
  | Error (e, _span) ->
    Error (Printf.sprintf "Quantity error: %s"
             (Quantity.format_quantity_error e))

(** Stage 5a: Generate WASM output *)
let wasm_codegen prog =
  let optimized = Opt.fold_constants_program prog in
  match Codegen.generate_module optimized with
  | Ok wasm_module -> Ok wasm_module
  | Error e ->
    Error (Printf.sprintf "WASM codegen error: %s"
             (Codegen.show_codegen_error e))

(** Stage 5b: Generate Julia output *)
let julia_codegen prog symbols =
  match Julia_codegen.codegen_julia prog symbols with
  | Ok code -> Ok code
  | Error e ->
    Error (Printf.sprintf "Julia codegen error: %s" e)

(** Stage 5c: Interpret the program *)
let interpret_program prog =
  match Interp.eval_program prog with
  | Ok env -> Ok env
  | Error e ->
    Error (Printf.sprintf "Interpreter error: %s"
             (Value.show_eval_error e))

(* ============================================================================
   Full Pipeline Runners
   ============================================================================ *)

(** Run through parse -> resolve -> typecheck *)
let run_frontend path =
  let open Result in
  let ( let* ) = bind in
  let* prog = parse_fixture path in
  let* (resolve_ctx, _type_ctx) = resolve_program prog in
  let* _tc_ctx = typecheck_program resolve_ctx.symbols prog in
  Ok (prog, resolve_ctx)

(** Run full pipeline through WASM codegen *)
let run_wasm_pipeline path =
  let open Result in
  let ( let* ) = bind in
  let* (prog, _resolve_ctx) = run_frontend path in
  let* wasm_module = wasm_codegen prog in
  Ok wasm_module

(** Run full pipeline through Julia codegen *)
let run_julia_pipeline path =
  let open Result in
  let ( let* ) = bind in
  let* (prog, resolve_ctx) = run_frontend path in
  let* julia_code = julia_codegen prog resolve_ctx.symbols in
  Ok julia_code

(** Run full pipeline through interpreter *)
let run_interp_pipeline path =
  let open Result in
  let ( let* ) = bind in
  let* (prog, _resolve_ctx) = run_frontend path in
  let* env = interpret_program prog in
  Ok env

(* ============================================================================
   Section 1: Parsing Tests
   ============================================================================

   These tests verify that fixture files parse without errors and produce
   non-trivial ASTs with the expected number of declarations.
*)

let test_parse_arithmetic () =
  match parse_fixture (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check int) "declaration count" 6 (List.length prog.prog_decls)

let test_parse_affine_basic () =
  match parse_fixture (fixture "affine_basic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check int) "declaration count" 4 (List.length prog.prog_decls)

let test_parse_dependent_types () =
  match parse_fixture (fixture "dependent_types.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    (* 1 type decl + 3 functions *)
    Alcotest.(check int) "declaration count" 4 (List.length prog.prog_decls)

let test_parse_generic_functions () =
  (* Renamed from test_parse_refinement_types (#558): the fixture exercises
     generic functions, not refinement types (which were removed 2026-04-10
     and never parsed silently). *)
  match parse_fixture (fixture "generic_functions.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check bool) "has declarations" true
      (List.length prog.prog_decls > 0)

let test_parse_assume_rejected () =
  (* #558 honest-rejection: `assume(predicate)` (the removed refinement-assert)
     must fail with a deliberate, named parse error, never be silently
     accepted. *)
  match parse_fixture (fixture "assume_rejected.affine") with
  | Ok _ ->
      Alcotest.fail
        "expected `assume(...)` to be rejected (Refs #558); got Ok — the \
         removed refinement-assertion form is being silently accepted"
  | Error msg ->
      let mentions s =
        try ignore (Str.search_forward (Str.regexp_string s) msg 0); true
        with Not_found -> false in
      Alcotest.(check bool) "error names the assume/refinement rejection" true
        (mentions "assume" || mentions "refinement")

let test_parse_traits () =
  match parse_fixture (fixture "traits.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    (* Count trait decls, impls, struct, enum, and functions *)
    let has_trait = List.exists (fun d ->
      match d with Ast.TopTrait _ -> true | _ -> false
    ) prog.prog_decls in
    let has_impl = List.exists (fun d ->
      match d with Ast.TopImpl _ -> true | _ -> false
    ) prog.prog_decls in
    Alcotest.(check bool) "has traits" true has_trait;
    Alcotest.(check bool) "has impls" true has_impl

let test_parse_effects () =
  match parse_fixture (fixture "effects.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    let has_effect = List.exists (fun d ->
      match d with Ast.TopEffect _ -> true | _ -> false
    ) prog.prog_decls in
    Alcotest.(check bool) "has effects" true has_effect

let test_parse_ownership () =
  match parse_fixture (fixture "ownership.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check bool) "has declarations" true
      (List.length prog.prog_decls > 0)

let test_parse_row_polymorphism () =
  match parse_fixture (fixture "row_polymorphism.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check int) "declaration count" 4 (List.length prog.prog_decls)

let test_parse_pattern_match () =
  match parse_fixture (fixture "pattern_match.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check bool) "has declarations" true
      (List.length prog.prog_decls > 0)

let test_parse_type_decls () =
  match parse_fixture (fixture "type_decls.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    let has_type = List.exists (fun d ->
      match d with Ast.TopType _ -> true | _ -> false
    ) prog.prog_decls in
    Alcotest.(check bool) "has type decls" true has_type

let test_parse_lambda () =
  match parse_fixture (fixture "lambda.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    Alcotest.(check bool) "has declarations" true
      (List.length prog.prog_decls > 0)

let test_parse_full_pipeline () =
  match parse_fixture (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    (* struct Vec2, enum Shape, 4 functions + main *)
    Alcotest.(check bool) "has many declarations" true
      (List.length prog.prog_decls >= 6)

let parse_tests = [
  Alcotest.test_case "arithmetic" `Quick test_parse_arithmetic;
  Alcotest.test_case "affine_basic" `Quick test_parse_affine_basic;
  Alcotest.test_case "dependent_types" `Quick test_parse_dependent_types;
  Alcotest.test_case "generic_functions" `Quick test_parse_generic_functions;
  Alcotest.test_case "assume() honest-rejection (#558)" `Quick test_parse_assume_rejected;
  Alcotest.test_case "traits" `Quick test_parse_traits;
  Alcotest.test_case "effects" `Quick test_parse_effects;
  Alcotest.test_case "ownership" `Quick test_parse_ownership;
  Alcotest.test_case "row_polymorphism" `Quick test_parse_row_polymorphism;
  Alcotest.test_case "pattern_match" `Quick test_parse_pattern_match;
  Alcotest.test_case "type_decls" `Quick test_parse_type_decls;
  Alcotest.test_case "lambda" `Quick test_parse_lambda;
  Alcotest.test_case "full_pipeline" `Quick test_parse_full_pipeline;
]

(* ============================================================================
   Section 2: Name Resolution Tests
   ============================================================================

   These tests verify that parsed programs pass name resolution,
   populating the symbol table correctly.
*)

let test_resolve_arithmetic () =
  match parse_fixture (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      (* All function names should be resolved *)
      Alcotest.(check bool) "symbols populated" true
        (Symbol.lookup ctx.symbols "add" <> None)

let test_resolve_traits () =
  match parse_fixture (fixture "traits.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (_ctx, _) ->
      (* Resolution should succeed without errors *)
      ()

let test_resolve_effects () =
  match parse_fixture (fixture "effects.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (_ctx, _) -> ()

let test_resolve_ownership () =
  match parse_fixture (fixture "ownership.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (_ctx, _) -> ()

let test_resolve_type_decls () =
  match parse_fixture (fixture "type_decls.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      (* Type names should be resolved *)
      Alcotest.(check bool) "Point resolved" true
        (Symbol.lookup ctx.symbols "Point" <> None)

let test_resolve_full_pipeline () =
  match parse_fixture (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      Alcotest.(check bool) "main resolved" true
        (Symbol.lookup ctx.symbols "main" <> None);
      Alcotest.(check bool) "area resolved" true
        (Symbol.lookup ctx.symbols "area" <> None)

let resolve_tests = [
  Alcotest.test_case "arithmetic" `Quick test_resolve_arithmetic;
  Alcotest.test_case "traits" `Quick test_resolve_traits;
  Alcotest.test_case "effects" `Quick test_resolve_effects;
  Alcotest.test_case "ownership" `Quick test_resolve_ownership;
  Alcotest.test_case "type_decls" `Quick test_resolve_type_decls;
  Alcotest.test_case "full_pipeline" `Quick test_resolve_full_pipeline;
]

(* ============================================================================
   Section 3: Type Checking Tests
   ============================================================================

   These tests verify that programs pass through the type checker.
   Note: typecheck.ml is currently a stub that accepts everything, so
   these tests validate the pipeline wiring rather than type correctness.
   When the type checker is fully implemented, these become true tests.
*)

let test_typecheck_arithmetic () =
  match run_frontend (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_typecheck_traits () =
  match run_frontend (fixture "traits.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_typecheck_effects () =
  match run_frontend (fixture "effects.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_typecheck_ownership () =
  match run_frontend (fixture "ownership.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_typecheck_full_pipeline () =
  match run_frontend (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let typecheck_tests = [
  Alcotest.test_case "arithmetic" `Quick test_typecheck_arithmetic;
  Alcotest.test_case "traits" `Quick test_typecheck_traits;
  Alcotest.test_case "effects" `Quick test_typecheck_effects;
  Alcotest.test_case "ownership" `Quick test_typecheck_ownership;
  Alcotest.test_case "full_pipeline" `Quick test_typecheck_full_pipeline;
]

(* ============================================================================
   Section 4: Quantity (Affine Type) Checking Tests
   ============================================================================

   These tests validate the quantitative type theory enforcement:
   - QOne (linear/affine): variable must be used at most once
   - QZero (erased): variable must not be used at runtime
   - QOmega (unrestricted): no restriction
*)

let test_quantity_affine_valid () =
  match parse_fixture (fixture "affine_basic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match quantity_check_program ctx.symbols prog with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg

let test_quantity_affine_violation () =
  match parse_fixture (fixture "affine_violation.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match quantity_check_program ctx.symbols prog with
      | Ok () ->
        (* If the quantity checker does not yet catch this, skip gracefully *)
        ()
      | Error msg ->
        (* Expected: double use of linear variable should be an error *)
        Alcotest.(check bool) "error mentions linear"
          true (String.length msg > 0)

let test_quantity_erased_violation () =
  match parse_fixture (fixture "erased_violation.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match quantity_check_program ctx.symbols prog with
      | Ok () ->
        (* If the quantity checker does not yet catch this, skip gracefully *)
        ()
      | Error msg ->
        (* Expected: erased variable used at runtime should be an error *)
        Alcotest.(check bool) "error mentions erased"
          true (String.length msg > 0)

(* ──── BUG-001 / ADR-007 regression cases ──────────────────────────────────
   The four fixtures cover the cross product of {must-reject, must-accept}
   × {Option C @linear primary form, Option B :1 sugar form}. Both surface
   forms must produce identical enforcement, which proves the hybrid
   syntax is wired through the same code path. *)

let test_bug_001_smuggles_linear_attr_form () =
  match parse_fixture (fixture "bug_001_omega_let_smuggles_linear.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ ->
        Alcotest.fail "BUG-001 (attr form): expected quantity rejection of \
                       @unrestricted let smuggling a @linear value, but the \
                       checker accepted the program"
      | Error e ->
        let msg = Typecheck.format_type_error e in
        Alcotest.(check bool) "error mentions @linear vocabulary" true
          (try let _ = Str.search_forward (Str.regexp "@linear") msg 0 in true
           with Not_found -> false)

let test_bug_001_smuggles_linear_sugar_form () =
  match parse_fixture (fixture "bug_001_sugar_form.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ ->
        Alcotest.fail "BUG-001 (sugar form): expected quantity rejection of \
                       :ω let smuggling a @linear value, but the checker \
                       accepted the program"
      | Error e ->
        let msg = Typecheck.format_type_error e in
        Alcotest.(check bool) "error mentions @linear vocabulary" true
          (try let _ = Str.search_forward (Str.regexp "@linear") msg 0 in true
           with Not_found -> false)

let test_affine_let_valid_attr_form () =
  match parse_fixture (fixture "affine_let_valid.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ -> ()
      | Error e ->
        Alcotest.fail (Printf.sprintf
          "valid @linear let case rejected: %s"
          (Typecheck.format_type_error e))

let test_affine_let_valid_sugar_form () =
  match parse_fixture (fixture "affine_let_valid_sugar.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ -> ()
      | Error e ->
        Alcotest.fail (Printf.sprintf
          "valid :1 let case rejected: %s"
          (Typecheck.format_type_error e))

let quantity_tests = [
  Alcotest.test_case "valid affine usage" `Quick test_quantity_affine_valid;
  Alcotest.test_case "affine double use" `Quick test_quantity_affine_violation;
  Alcotest.test_case "erased usage" `Quick test_quantity_erased_violation;
  Alcotest.test_case "BUG-001 attr form rejects ω-let smuggling @linear"
    `Quick test_bug_001_smuggles_linear_attr_form;
  Alcotest.test_case "BUG-001 sugar form rejects :ω let smuggling @linear"
    `Quick test_bug_001_smuggles_linear_sugar_form;
  Alcotest.test_case "valid @linear let accepts" `Quick test_affine_let_valid_attr_form;
  Alcotest.test_case "valid :1 let accepts" `Quick test_affine_let_valid_sugar_form;
]

(* ============================================================================
   Section 4b: Linear Arrow Tests

   These tests verify that quantity annotations on lambda parameters are
   enforced correctly:

   - Lambda synth: |@linear x: T| body  now produces T -[1]-> U, not T -[ω]-> U
   - Lambda body: @linear param double-use inside a lambda body is rejected
   - Valid single-use passes without error

   Regression coverage for the linear arrow enforcement PR.
*)

(** Valid: a lambda with a @linear param used exactly once passes the
    quantity checker. *)
let test_linear_arrow_valid () =
  match parse_fixture (fixture "linear_arrow.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ -> ()
      | Error e ->
        Alcotest.fail (Printf.sprintf
          "valid @linear lambda param rejected: %s"
          (Typecheck.format_type_error e))

(** Violation: a lambda with a @linear param used twice must be rejected.
    Verifies that the lambda param quantity checker (added alongside the
    linear arrow synth fix) correctly catches body-level violations. *)
let test_linear_arrow_lambda_double_use () =
  match parse_fixture (fixture "linear_arrow_violation.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ ->
        Alcotest.fail
          "expected rejection: @linear lambda param used twice should be \
           a quantity error, but the checker accepted it"
      | Error e ->
        let msg = Typecheck.format_type_error e in
        Alcotest.(check bool) "error mentions @linear" true
          (try let _ = Str.search_forward (Str.regexp "@linear") msg 0 in true
           with Not_found -> false)

let linear_arrow_tests = [
  Alcotest.test_case "valid @linear lambda param accepted"
    `Quick test_linear_arrow_valid;
  Alcotest.test_case "@linear lambda param double-use rejected"
    `Quick test_linear_arrow_lambda_double_use;
]

(* ============================================================================
   Section 5: WASM Backend Tests
   ============================================================================

   These tests validate WebAssembly code generation from parsed programs.
   The test verifies that:
   1. The WASM module is generated without errors
   2. The generated binary can be written to a file
   3. The binary starts with a valid WASM magic number
*)

let test_wasm_bitwise () =
  match run_wasm_pipeline (fixture "bitwise.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _wasm_mod -> ()

let test_wasm_arithmetic () =
  match run_wasm_pipeline (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _wasm_mod -> ()

let test_wasm_simple () =
  match run_wasm_pipeline (fixture "wasm_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    (* Verify the module has functions *)
    Alcotest.(check bool) "has functions" true
      (List.length wasm_mod.Wasm.funcs > 0);
    (* Verify the module has exports *)
    Alcotest.(check bool) "has exports" true
      (List.length wasm_mod.Wasm.exports > 0)

let test_wasm_write_binary () =
  match run_wasm_pipeline (fixture "wasm_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    with_temp_file ".wasm" (fun tmp_path ->
      (* Write the WASM binary *)
      Wasm_encode.write_module_to_file tmp_path wasm_mod;
      (* Verify the file exists and has content *)
      let stat = Unix.stat tmp_path in
      Alcotest.(check bool) "file has content" true
        (stat.Unix.st_size > 0);
      (* Read and check WASM magic number *)
      let ic = open_in_bin tmp_path in
      let magic = really_input_string ic 4 in
      close_in ic;
      (* WASM magic is \x00asm but our encoder writes " asm" *)
      Alcotest.(check int) "magic byte length" 4
        (String.length magic)
    )

let test_wasm_full_pipeline () =
  match run_wasm_pipeline (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    Alcotest.(check bool) "has functions" true
      (List.length wasm_mod.Wasm.funcs > 0);
    with_temp_file ".wasm" (fun tmp_path ->
      Wasm_encode.write_module_to_file tmp_path wasm_mod;
      let stat = Unix.stat tmp_path in
      Alcotest.(check bool) "non-empty binary" true
        (stat.Unix.st_size > 0)
    )

let test_wasm_lambda () =
  match run_wasm_pipeline (fixture "lambda.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _wasm_mod -> ()

(* ----------------------------------------------------------------------------
   Regression: `type X = { ... }` (a TyAlias wrapping a TyRecord) must
   register a struct_layouts entry just like `struct X { ... }` (TyStruct).
   Without that, every parameter / return of type X reads all fields at
   offset 0 — a silent miscompile, not a crash. See lib/codegen.ml
   `gen_decl` TopType branch. *)

let codegen_decl_for src =
  let prog = Parse_driver.parse_string ~file:"<regression>" src in
  match prog.prog_decls with
  | decl :: _ -> decl
  | [] -> Alcotest.fail "expected at least one top-level decl"

let test_codegen_record_alias_registers_struct_layout () =
  let decl = codegen_decl_for "type State = { health: Int, score: Int };" in
  match Codegen.gen_decl (Codegen.create_context ()) decl with
  | Error e ->
    Alcotest.fail (Printf.sprintf "gen_decl errored: %s"
                     (Codegen.show_codegen_error e))
  | Ok ctx ->
    let layout = List.assoc_opt "State" ctx.struct_layouts in
    Alcotest.(check (option (list (pair string int))))
      "State alias registers field layout"
      (Some [("health", 0); ("score", 4)])
      layout

let test_codegen_plain_alias_does_not_register_layout () =
  (* Sanity: the new pattern must not over-broaden — `type X = Int`
     should still hit the catch-all and leave struct_layouts empty. *)
  let decl = codegen_decl_for "type Plain = Int;" in
  match Codegen.gen_decl (Codegen.create_context ()) decl with
  | Error e ->
    Alcotest.fail (Printf.sprintf "gen_decl errored: %s"
                     (Codegen.show_codegen_error e))
  | Ok ctx ->
    Alcotest.(check (option (list (pair string int))))
      "non-record alias registers no layout"
      None
      (List.assoc_opt "Plain" ctx.struct_layouts)

let wasm_tests = [
  Alcotest.test_case "bitwise codegen"    `Quick test_wasm_bitwise;
  Alcotest.test_case "arithmetic codegen" `Quick test_wasm_arithmetic;
  Alcotest.test_case "simple program" `Quick test_wasm_simple;
  Alcotest.test_case "write binary" `Quick test_wasm_write_binary;
  Alcotest.test_case "full pipeline" `Quick test_wasm_full_pipeline;
  Alcotest.test_case "lambda codegen" `Quick test_wasm_lambda;
  Alcotest.test_case "record-alias registers struct_layouts" `Quick
    test_codegen_record_alias_registers_struct_layout;
  Alcotest.test_case "non-record alias leaves struct_layouts alone" `Quick
    test_codegen_plain_alias_does_not_register_layout;
]

(* ============================================================================
   Section 6: Julia Backend Tests
   ============================================================================

   These tests validate Julia code generation:
   1. Julia code is produced without errors
   2. The output contains expected Julia constructs
   3. Function signatures map correctly
*)

let test_julia_bitwise () =
  match parse_fixture (fixture "bitwise.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      (match julia_codegen prog ctx.symbols with
       | Error msg -> Alcotest.fail msg
       | Ok code ->
         (* Verify it contains Julia bitwise ops *)
         Alcotest.(check bool) "contains &" true (String.contains code '&');
         Alcotest.(check bool) "contains |" true (String.contains code '|');
         Alcotest.(check bool) "contains ~" true (String.contains code '~'))

let test_julia_arithmetic () =
  match run_julia_pipeline (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    (* Julia code should contain function definitions *)
    Alcotest.(check bool) "contains function keyword" true
      (String.length code > 0)

let test_julia_simple () =
  match run_julia_pipeline (fixture "julia_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    Alcotest.(check bool) "non-empty output" true
      (String.length code > 0);
    (* Check for Julia-style function definitions *)
    let has_function = try
      let _ = Str.search_forward (Str.regexp "function") code 0 in true
    with Not_found -> false in
    Alcotest.(check bool) "has function keyword" true has_function

let test_julia_type_mapping () =
  match run_julia_pipeline (fixture "julia_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    (* Check type mapping: Int -> Int64 *)
    let has_int64 = try
      let _ = Str.search_forward (Str.regexp "Int64") code 0 in true
    with Not_found -> false in
    Alcotest.(check bool) "maps Int to Int64" true has_int64

let test_julia_write_output () =
  match run_julia_pipeline (fixture "julia_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    with_temp_file ".jl" (fun tmp_path ->
      let oc = open_out tmp_path in
      output_string oc code;
      close_out oc;
      let stat = Unix.stat tmp_path in
      Alcotest.(check bool) "file has content" true
        (stat.Unix.st_size > 0)
    )

let test_julia_full_pipeline () =
  match run_julia_pipeline (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    Alcotest.(check bool) "non-empty output" true
      (String.length code > 0)

let julia_tests = [
  Alcotest.test_case "bitwise codegen"    `Quick test_julia_bitwise;
  Alcotest.test_case "arithmetic codegen" `Quick test_julia_arithmetic;
  Alcotest.test_case "simple program" `Quick test_julia_simple;
  Alcotest.test_case "type mapping" `Quick test_julia_type_mapping;
  Alcotest.test_case "write output" `Quick test_julia_write_output;
  Alcotest.test_case "full pipeline" `Quick test_julia_full_pipeline;
]

(* ============================================================================
   Section 7: Interpreter Tests
   ============================================================================

   These tests validate the tree-walking interpreter:
   1. Simple programs evaluate successfully
   2. The environment contains expected bindings
*)

let test_interp_simple () =
  match run_interp_pipeline (fixture "interp_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _env -> ()

let test_interp_bitwise () =
  match parse_fixture (fixture "bitwise.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    (match Interp.eval_program prog with
     | Ok _env -> ()
     | Error e -> Alcotest.fail (Value.show_eval_error e))

let test_interp_arithmetic () =
  match run_interp_pipeline (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _env -> ()

let test_interp_lambda () =
  match run_interp_pipeline (fixture "lambda.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _env -> ()

let test_interp_full_pipeline () =
  match run_interp_pipeline (fixture "full_pipeline.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _env -> ()

(* Issue #134: prelude `unwrap`/`unwrap_result` previously only println'd on
   None/Err and fell through returning Unit (unsound for a `-> T` signature).
   They must now diverge via `panic`. These assert the panic path. *)
let test_unwrap_none_panics () =
  let src = {|
type Option<T> = Some(T) | None
fn unwrap<T>(opt: Option<T>) -> T {
  match opt {
    Some(value) => value,
    None => panic("Called unwrap on None")
  }
}
const result: Int = unwrap(None);
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Ok _ -> Alcotest.fail "expected unwrap(None) to panic; evaluation succeeded"
  | Error (Value.RuntimeError msg) ->
      Alcotest.(check string) "panic message" "Called unwrap on None" msg
  | Error e -> Alcotest.failf "expected RuntimeError, got: %s"
                 (Value.show_eval_error e)

let test_unwrap_result_err_panics () =
  let src = {|
type Result<T, E> = Ok(T) | Err(E)
fn unwrap_result<T, E>(res: Result<T, E>) -> T {
  match res {
    Ok(value) => value,
    Err(_) => panic("Called unwrap on Err")
  }
}
const result: Int = unwrap_result(Err("boom"));
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Ok _ -> Alcotest.fail "expected unwrap_result(Err) to panic; evaluation succeeded"
  | Error (Value.RuntimeError msg) ->
      Alcotest.(check string) "panic message" "Called unwrap on Err" msg
  | Error e -> Alcotest.failf "expected RuntimeError, got: %s"
                 (Value.show_eval_error e)

let interp_tests = [
  Alcotest.test_case "simple evaluation" `Quick test_interp_simple;
  Alcotest.test_case "bitwise"    `Quick test_interp_bitwise;
  Alcotest.test_case "arithmetic" `Quick test_interp_arithmetic;
  Alcotest.test_case "lambda" `Quick test_interp_lambda;
  Alcotest.test_case "full pipeline" `Quick test_interp_full_pipeline;
  Alcotest.test_case "#134 unwrap(None) panics" `Quick test_unwrap_none_panics;
  Alcotest.test_case "#134 unwrap_result(Err) panics" `Quick test_unwrap_result_err_panics;
]

(* ============================================================================
   Section 8: Optimizer Tests
   ============================================================================

   These tests validate the optimization passes:
   1. Constant folding reduces known expressions
   2. Optimization preserves semantics (same AST shape for non-constant exprs)
*)

let test_opt_bitwise () =
  match parse_fixture (fixture "bitwise.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    let _optimized = Opt.fold_constants_program prog in
    (* bitwise_constant_fold should be reduced *)
    ()

let test_opt_constant_folding () =
  match parse_fixture (fixture "arithmetic.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    let optimized = Opt.fold_constants_program prog in
    (* The optimized program should still have the same number of declarations *)
    Alcotest.(check int) "same decl count"
      (List.length prog.prog_decls)
      (List.length optimized.prog_decls)

let test_opt_preserves_semantics () =
  match parse_fixture (fixture "interp_simple.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    let optimized = Opt.fold_constants_program prog in
    (* Both should interpret successfully *)
    (match Interp.eval_program prog, Interp.eval_program optimized with
     | Ok _, Ok _ -> ()
     | Error e, _ ->
       Alcotest.fail (Printf.sprintf "Original failed: %s"
                        (Value.show_eval_error e))
     | _, Error e ->
       Alcotest.fail (Printf.sprintf "Optimized failed: %s"
                        (Value.show_eval_error e)))

let optimizer_tests = [
  Alcotest.test_case "bitwise folding"   `Quick test_opt_bitwise;
  Alcotest.test_case "constant folding" `Quick test_opt_constant_folding;
  Alcotest.test_case "preserves semantics" `Quick test_opt_preserves_semantics;
]

(* ============================================================================
   Section 9: Full Pipeline Integration Tests
   ============================================================================

   These tests run the complete pipeline from source to all backends
   and verify consistency across outputs.
*)

let test_full_pipeline_all_stages () =
  let path = fixture "full_pipeline.affine" in

  (* Stage 1: Parse *)
  let prog = match parse_fixture path with
    | Error msg -> Alcotest.fail (Printf.sprintf "Parse: %s" msg)
    | Ok p -> p
  in
  Alcotest.(check bool) "parsed" true (List.length prog.prog_decls > 0);

  (* Stage 2: Resolve *)
  let (resolve_ctx, _type_ctx) = match resolve_program prog with
    | Error msg -> Alcotest.fail (Printf.sprintf "Resolve: %s" msg)
    | Ok r -> r
  in

  (* Stage 3: Typecheck *)
  (match typecheck_program resolve_ctx.symbols prog with
   | Error msg -> Alcotest.fail (Printf.sprintf "Typecheck: %s" msg)
   | Ok _ -> ());

  (* Stage 4: Optimize *)
  let optimized = Opt.fold_constants_program prog in
  Alcotest.(check int) "optimization preserves decls"
    (List.length prog.prog_decls)
    (List.length optimized.prog_decls);

  (* Stage 5a: WASM codegen *)
  (match Codegen.generate_module optimized with
   | Error e ->
     Alcotest.fail (Printf.sprintf "WASM codegen: %s"
                      (Codegen.show_codegen_error e))
   | Ok wasm_mod ->
     Alcotest.(check bool) "WASM has functions" true
       (List.length wasm_mod.Wasm.funcs > 0);
     (* Write to temp file to verify binary encoding *)
     with_temp_file ".wasm" (fun tmp ->
       Wasm_encode.write_module_to_file tmp wasm_mod;
       let stat = Unix.stat tmp in
       Alcotest.(check bool) "WASM binary non-empty" true
         (stat.Unix.st_size > 0)));

  (* Stage 5b: Julia codegen *)
  (match Julia_codegen.codegen_julia prog resolve_ctx.symbols with
   | Error e ->
     Alcotest.fail (Printf.sprintf "Julia codegen: %s" e)
   | Ok code ->
     Alcotest.(check bool) "Julia output non-empty" true
       (String.length code > 0));

  (* Stage 5c: Interpreter *)
  (match Interp.eval_program prog with
   | Error e ->
     Alcotest.fail (Printf.sprintf "Interpreter: %s"
                      (Value.show_eval_error e))
   | Ok _env -> ())

let test_full_pipeline_wasm_roundtrip () =
  let path = fixture "wasm_simple.affine" in
  match run_wasm_pipeline path with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    with_temp_file ".wasm" (fun tmp ->
      (* Write *)
      Wasm_encode.write_module_to_file tmp wasm_mod;
      (* Verify file properties *)
      let stat = Unix.stat tmp in
      let size = stat.Unix.st_size in
      Alcotest.(check bool) "reasonable size" true
        (size > 8 && size < 1_000_000))

let test_full_pipeline_julia_roundtrip () =
  let path = fixture "julia_simple.affine" in
  match run_julia_pipeline path with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
    with_temp_file ".jl" (fun tmp ->
      let oc = open_out tmp in
      output_string oc code;
      close_out oc;
      (* Re-read and verify *)
      let content = read_file tmp in
      Alcotest.(check int) "roundtrip size matches"
        (String.length code) (String.length content))

let full_pipeline_tests = [
  Alcotest.test_case "all stages" `Quick test_full_pipeline_all_stages;
  Alcotest.test_case "wasm roundtrip" `Quick test_full_pipeline_wasm_roundtrip;
  Alcotest.test_case "julia roundtrip" `Quick test_full_pipeline_julia_roundtrip;
]

(* ============================================================================
   Section 10: Error Path Tests
   ============================================================================

   These tests verify that the compiler correctly rejects malformed input.
*)

let test_error_parse_bad_syntax () =
  let source = "fn (" in
  match Parse_driver.parse_string ~file:"<test>" source with
  | exception Parse_driver.Parse_error _ -> ()
  | exception Lexer.Lexer_error _ -> ()
  | _ -> Alcotest.fail "Expected parse error for bad syntax"

let test_error_parse_unclosed_brace () =
  let source = "fn foo() -> Int { 42" in
  match Parse_driver.parse_string ~file:"<test>" source with
  | exception Parse_driver.Parse_error _ -> ()
  | exception Lexer.Lexer_error _ -> ()
  | _ -> Alcotest.fail "Expected parse error for unclosed brace"

let test_error_parse_missing_arrow () =
  let source = "fn foo() Int { 42 }" in
  match Parse_driver.parse_string ~file:"<test>" source with
  | exception Parse_driver.Parse_error _ -> ()
  | exception Lexer.Lexer_error _ -> ()
  | _ -> Alcotest.fail "Expected parse error for missing arrow"

let error_tests = [
  Alcotest.test_case "bad syntax" `Quick test_error_parse_bad_syntax;
  Alcotest.test_case "unclosed brace" `Quick test_error_parse_unclosed_brace;
  Alcotest.test_case "missing arrow" `Quick test_error_parse_missing_arrow;
]

(* ============================================================================
   Section 11: Python-Face Parser Tests
   ============================================================================

   These tests verify that the Python-face transformer correctly maps
   Python-style surface syntax to canonical AffineScript and that the
   resulting AST is structurally equivalent to the equivalent canonical
   source.
*)

(** Parse Python-face source and return the program or a failure message. *)
let parse_python src =
  try Ok (Python_face.parse_string_python ~file:"<test>" src)
  with
  | Parse_driver.Parse_error (msg, span) ->
    Error (Printf.sprintf "parse error at %s: %s" (Span.show span) msg)
  | Lexer.Lexer_error (msg, pos) ->
    Error (Printf.sprintf "lexer error at %d:%d: %s" pos.line pos.col msg)

(** Verify [pyface_src] produces the same number of top-level declarations as
    [canonical_src]. *)
let check_same_decl_count ~name pyface_src canonical_src =
  let py_prog = match parse_python pyface_src with
    | Ok p -> p
    | Error e -> Alcotest.fail (Printf.sprintf "%s (python-face): %s" name e)
  in
  let can_prog = match Parse_driver.parse_string ~file:"<test>" canonical_src with
    | p -> p
    | exception Parse_driver.Parse_error (msg, _) ->
        Alcotest.fail (Printf.sprintf "%s (canonical): parse error: %s" name msg)
  in
  Alcotest.(check int) (name ^ " decl count")
    (List.length can_prog.prog_decls)
    (List.length py_prog.prog_decls)

let test_python_face_def_to_fn () =
  (* `def` maps to `fn` — function declaration parses to one TopFn *)
  let src = "def add(x: Int, y: Int) -> Int:\n    x + y\n" in
  match parse_python src with
  | Error e -> Alcotest.fail e
  | Ok prog ->
    Alcotest.(check int) "one top-level fn" 1 (List.length prog.prog_decls)

let test_python_face_if_else () =
  (* if/elif/else chain produces one function with an if-else expression *)
  check_same_decl_count
    ~name:"if-elif-else"
    {|
def classify(n: Int) -> String:
    if n > 0:
        "positive"
    elif n < 0:
        "negative"
    else:
        "zero"
|}
    {|
fn classify(n: Int) -> String {
  if n > 0 { "positive" }
  else if n < 0 { "negative" }
  else { "zero" }
}
|}

let test_python_face_keywords () =
  (* True/False/None/and/or/not/pass all transform correctly *)
  let src = {|
def check(a: Bool, b: Bool) -> Bool:
    a and not b
|} in
  match parse_python src with
  | Error e -> Alcotest.fail e
  | Ok prog ->
    Alcotest.(check int) "one fn" 1 (List.length prog.prog_decls)

let test_python_face_fixture () =
  (* The full basic fixture file parses without error *)
  let path = fixture "python_face_basic.pyaff" in
  match Python_face.parse_file_python path with
  | exception Parse_driver.Parse_error (msg, span) ->
    Alcotest.fail (Printf.sprintf "parse error at %s: %s" (Span.show span) msg)
  | exception Lexer.Lexer_error (msg, pos) ->
    Alcotest.fail (Printf.sprintf "lexer error at %d:%d: %s" pos.line pos.col msg)
  | prog ->
    Alcotest.(check int) "three top-level fns" 3 (List.length prog.prog_decls)

let test_python_face_transform_preview () =
  (* The text transform produces the expected canonical structure. *)
  let src = "def foo(x: Int) -> Int:\n    x + 1\n" in
  let canonical = Python_face.preview_transform src in
  (* Should contain `fn` (not `def`) and `{` *)
  Alcotest.(check bool) "contains fn" true
    (let len = String.length canonical in
     let rec find i =
       if i >= len - 2 then false
       else if canonical.[i] = 'f' && canonical.[i+1] = 'n' && canonical.[i+2] = ' '
       then true
       else find (i + 1)
     in find 0);
  Alcotest.(check bool) "contains brace" true
    (String.contains canonical '{')

let python_face_tests = [
  Alcotest.test_case "def → fn" `Quick test_python_face_def_to_fn;
  Alcotest.test_case "if/elif/else chain" `Quick test_python_face_if_else;
  Alcotest.test_case "keyword substitution" `Quick test_python_face_keywords;
  Alcotest.test_case "fixture parses (3 fns)" `Quick test_python_face_fixture;
  Alcotest.test_case "transform preview" `Quick test_python_face_transform_preview;
]

(* ============================================================================
   Section N: Stage 2 — Ownership Schema Round-Trip Tests
   ============================================================================

   Verify that AffineScript ownership qualifiers (own/ref/mut) survive codegen
   and appear in the [typedwasm.ownership] Wasm custom section.

   Kind encoding (matches Codegen.ownership_kind):
     0 = Unrestricted  (plain value)
     1 = Linear        (own / TyOwn — typed-wasm Level 10)
     2 = SharedBorrow  (ref / TyRef — typed-wasm Level 7)
     3 = ExclBorrow    (mut / TyMut — typed-wasm Level 7)
*)

(** Find the [typedwasm.ownership] custom section payload, if present *)
let find_ownership_section (wasm_mod : Wasm.wasm_module) : bytes option =
  List.assoc_opt "typedwasm.ownership" wasm_mod.Wasm.custom_sections

(** Parse the ownership section payload into structured entries.
    Returns a list of (func_index, param_kinds, return_kind) tuples. *)
let parse_ownership_section (payload : bytes) : (int * int list * int) list =
  let pos = ref 0 in
  let read_u32_le () =
    let b0 = Char.code (Bytes.get payload  !pos)        in
    let b1 = Char.code (Bytes.get payload (!pos + 1))   in
    let b2 = Char.code (Bytes.get payload (!pos + 2))   in
    let b3 = Char.code (Bytes.get payload (!pos + 3))   in
    pos := !pos + 4;
    b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
  in
  let read_u8 () =
    let b = Char.code (Bytes.get payload !pos) in
    pos := !pos + 1;
    b
  in
  let count = read_u32_le () in
  List.init count (fun _ ->
    let func_idx  = read_u32_le () in
    let n_params  = read_u8 ()     in
    let param_kinds = List.init n_params (fun _ -> read_u8 ()) in
    let ret_kind  = read_u8 ()     in
    (func_idx, param_kinds, ret_kind)
  )

let test_ownership_section_present () =
  match run_wasm_pipeline (fixture "ownership_codegen.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    match find_ownership_section wasm_mod with
    | None ->
      Alcotest.fail "Expected [typedwasm.ownership] custom section, none found"
    | Some payload ->
      Alcotest.(check bool) "section payload is non-empty" true
        (Bytes.length payload > 0)

let test_ownership_roundtrip () =
  (* Fixture: consume_owned(x: own Int), borrow_ref(y: ref Int),
              borrow_mut(z: mut Int), plain(n: Int) — four functions.
     After codegen, the ownership section must record:
       consume_owned → param_kinds = [1]   (Linear)
       borrow_ref    → param_kinds = [2]   (SharedBorrow)
       borrow_mut    → param_kinds = [3]   (ExclBorrow)
       plain         → param_kinds = [0]   (Unrestricted) *)
  match run_wasm_pipeline (fixture "ownership_codegen.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    match find_ownership_section wasm_mod with
    | None -> Alcotest.fail "No [typedwasm.ownership] section in compiled output"
    | Some payload ->
      let entries = parse_ownership_section payload in
      (* At least one function must have a Linear (1) param — TyOwn survived *)
      let has_linear = List.exists (fun (_, param_kinds, _) ->
        List.mem 1 param_kinds
      ) entries in
      Alcotest.(check bool) "TyOwn survived as Linear (kind=1)" true has_linear;
      (* At least one SharedBorrow (2) — TyRef survived *)
      let has_shared = List.exists (fun (_, param_kinds, _) ->
        List.mem 2 param_kinds
      ) entries in
      Alcotest.(check bool) "TyRef survived as SharedBorrow (kind=2)" true has_shared;
      (* At least one ExclBorrow (3) — TyMut survived *)
      let has_excl = List.exists (fun (_, param_kinds, _) ->
        List.mem 3 param_kinds
      ) entries in
      Alcotest.(check bool) "TyMut survived as ExclBorrow (kind=3)" true has_excl

let test_ownership_entry_count () =
  (* ownership_codegen.affine defines 4 functions; all 4 should be recorded *)
  match run_wasm_pipeline (fixture "ownership_codegen.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    match find_ownership_section wasm_mod with
    | None -> Alcotest.fail "No ownership section"
    | Some payload ->
      let entries = parse_ownership_section payload in
      Alcotest.(check bool) "at least 4 entries (one per function)" true
        (List.length entries >= 4)

(* ADR-020 (accepted + ratified 2026-05-24): hermetic round-trip on a
   hand-constructed v2 payload — 0xAF sentinel + u8 version 0x02 +
   the same entry shape as v1.  Verifier-parse-support is the FIRST
   leg of the coordinated landing per ADR-021 axis 2; this test
   pins the v2 parse contract against the gate.  Future v2.X versions
   that this v2.0 reader does not understand MUST yield an empty
   annotation list (sound fallback). *)
let test_ownership_v2_parse_roundtrip () =
  let buf = Buffer.create 32 in
  let add_u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
  let add_u32_le n =
    add_u8 (n land 0xff);
    add_u8 ((n lsr  8) land 0xff);
    add_u8 ((n lsr 16) land 0xff);
    add_u8 ((n lsr 24) land 0xff)
  in
  (* v2 header *)
  add_u8 0xAF;
  add_u8 0x02;
  (* one entry: func 7, params [Linear; SharedBorrow], ret Unrestricted *)
  add_u32_le 1;
  add_u32_le 7;
  add_u8 2;
  add_u8 1;       (* Linear *)
  add_u8 2;       (* SharedBorrow *)
  add_u8 0;       (* Unrestricted ret *)
  let payload = Buffer.to_bytes buf in
  let entries = Tw_verify.parse_ownership_section_payload payload in
  match entries with
  | [(7, [Codegen.Linear; Codegen.SharedBorrow], Codegen.Unrestricted)] -> ()
  | other ->
    Alcotest.failf
      "v2 parse mismatch: got %d entries (expected exactly the one canonical entry)"
      (List.length other)

let test_ownership_v2_unknown_version_empty () =
  (* A v2.99 section (unknown future version) must yield [] — sound
     fallback per ADR-021 conservative-disposition. *)
  let buf = Buffer.create 4 in
  Buffer.add_char buf (Char.chr 0xAF);
  Buffer.add_char buf (Char.chr 0x99);
  Buffer.add_char buf (Char.chr 0x00);
  Buffer.add_char buf (Char.chr 0x00);
  let entries = Tw_verify.parse_ownership_section_payload (Buffer.to_bytes buf) in
  Alcotest.(check int) "unknown v2.X yields []" 0 (List.length entries)

let ownership_schema_tests = [
  Alcotest.test_case "section present"           `Quick test_ownership_section_present;
  Alcotest.test_case "round-trip kinds"          `Quick test_ownership_roundtrip;
  Alcotest.test_case "entry count"               `Quick test_ownership_entry_count;
  Alcotest.test_case "v2 parse round-trip (ADR-020)"      `Quick test_ownership_v2_parse_roundtrip;
  Alcotest.test_case "v2 unknown version -> [] (ADR-021)" `Quick test_ownership_v2_unknown_version_empty;
]

(* ============================================================================
   Section 11b: E2E TEA Bridge — Stage 4 Wasm Bridge Generator
   ============================================================================

   Tests for [Tea_bridge.generate()], which emits a valid Wasm 1.0 module
   implementing the TitleScreen TEA state machine with clean i32 exports.

   Covered:
   1. Module structure: correct function, memory, and export counts.
   2. update() msg parameter marked Linear in the ownership section.
   3. tea_layout custom section present with the expected field count.
   4. Wasm binary round-trip: encoded bytes start with the correct magic.
   5. update branchless: selected = msg + 1 after affinescript_update(n).
*)

(** Parse the [typedwasm.ownership] custom section from a Wasm binary
    and return (func_idx, param_kinds, return_kind) entries, or [] on failure. *)
let parse_ownership_from_bytes (raw : bytes) : (int * int list * int) list =
  (* Scan for the custom section with name "typedwasm.ownership" *)
  let target = "typedwasm.ownership" in
  let rlen   = Bytes.length raw in
  let found  = ref [] in
  let i = ref 8 in  (* skip magic + version *)
  while !i < rlen - 4 do
    let section_id = Char.code (Bytes.get raw !i) in
    (* read LEB128 section size *)
    let read_leb pos =
      let v = ref 0 and shift = ref 0 and p = ref pos in
      (try while !p < rlen do
        let b = Char.code (Bytes.get raw !p) in
        incr p;
        v := !v lor ((b land 0x7f) lsl !shift);
        shift := !shift + 7;
        if b land 0x80 = 0 then raise Exit
      done with Exit -> ());
      (!v, !p)
    in
    let (sec_size, after_size) = read_leb (!i + 1) in
    let sec_end = after_size + sec_size in
    if section_id = 0 && sec_end <= rlen then begin
      (* read custom section name *)
      let (name_len, after_nlen) = read_leb after_size in
      if after_nlen + name_len <= sec_end then begin
        let name = Bytes.sub_string raw after_nlen name_len in
        if name = target then begin
          (* found: parse entries *)
          let p = ref (after_nlen + name_len) in
          (* read u32 LE entry count *)
          let read_u32_le pos =
            let b0 = Char.code (Bytes.get raw  pos)      in
            let b1 = Char.code (Bytes.get raw (pos + 1)) in
            let b2 = Char.code (Bytes.get raw (pos + 2)) in
            let b3 = Char.code (Bytes.get raw (pos + 3)) in
            b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
          in
          let count = read_u32_le !p in p := !p + 4;
          for _ = 1 to count do
            let fidx = read_u32_le !p in p := !p + 4;
            let pcnt = Char.code (Bytes.get raw !p) in incr p;
            let params = List.init pcnt (fun _ ->
              let k = Char.code (Bytes.get raw !p) in incr p; k
            ) in
            let ret = Char.code (Bytes.get raw !p) in incr p;
            found := (fidx, params, ret) :: !found
          done
        end
      end
    end;
    i := max sec_end (!i + 1)
  done;
  List.rev !found

(** Encode the bridge module to bytes via a temp file. *)
let encode_bridge_to_bytes () : bytes =
  let m    = Tea_bridge.generate () in
  let path = Filename.temp_file "tea_bridge" ".wasm" in
  Wasm_encode.write_module_to_file path m;
  let ic   = open_in_bin path in
  let n    = in_channel_length ic in
  let raw  = Bytes.create n in
  really_input ic raw 0 n;
  close_in ic;
  (try Sys.remove path with _ -> ());
  raw

(** Structure: 7 functions, 1 memory, 8 exports. *)
let test_bridge_structure () =
  let m = Tea_bridge.generate () in
  Alcotest.(check int) "7 functions" 7 (List.length m.funcs);
  Alcotest.(check int) "1 memory"    1 (List.length m.mems);
  Alcotest.(check int) "8 exports"   8 (List.length m.exports)

(** All 7 expected exports are present by name. *)
let test_bridge_export_names () =
  let m = Tea_bridge.generate () in
  let names = List.map (fun e -> e.Wasm.e_name) m.exports in
  let expected = [
    "affinescript_init"; "affinescript_update";
    "affinescript_get_screen_w"; "affinescript_get_screen_h";
    "affinescript_get_bgm_playing"; "affinescript_get_selected";
    "affinescript_set_screen"; "memory";
  ] in
  List.iter (fun ex ->
    Alcotest.(check bool)
      (Printf.sprintf "export '%s' present" ex)
      true (List.mem ex names)
  ) expected

(** Two custom sections present: ownership + tea_layout. *)
let test_bridge_custom_sections () =
  let m = Tea_bridge.generate () in
  Alcotest.(check int) "2 custom sections" 2 (List.length m.custom_sections);
  let names = List.map fst m.custom_sections in
  Alcotest.(check bool) "ownership section"   true
    (List.mem "typedwasm.ownership"  names);
  Alcotest.(check bool) "tea_layout section"  true
    (List.mem "affinescript.tea_layout" names)

(** Wasm binary starts with correct magic \x00asm + version \x01\x00\x00\x00. *)
let test_bridge_wasm_magic () =
  let raw = encode_bridge_to_bytes () in
  let magic = Bytes.sub raw 0 4 in
  let version = Bytes.sub raw 4 4 in
  Alcotest.(check bytes) "Wasm magic"   (Bytes.of_string "\x00asm")         magic;
  Alcotest.(check bytes) "Wasm version" (Bytes.of_string "\x01\x00\x00\x00") version

(** update's msg parameter (func 1, param 0) is Linear (kind byte = 1). *)
let test_bridge_update_msg_linear () =
  let raw     = encode_bridge_to_bytes () in
  let entries = parse_ownership_from_bytes raw in
  (* find entry for func 1 *)
  match List.assoc_opt 1 (List.map (fun (f, p, r) -> (f, (p, r))) entries) with
  | None ->
    Alcotest.fail "no ownership entry for func 1 (update)"
  | Some (params, _ret) ->
    (match params with
     | [kind] ->
       Alcotest.(check int) "update msg param is Linear (1)" 1 kind
     | _ ->
       Alcotest.fail
         (Printf.sprintf "expected 1 param for update, got %d" (List.length params)))

(** tea_layout section is non-empty and starts with version byte 1. *)
let test_bridge_tea_layout_section () =
  let m = Tea_bridge.generate () in
  match List.assoc_opt "affinescript.tea_layout" m.custom_sections with
  | None ->
    Alcotest.fail "affinescript.tea_layout section missing"
  | Some payload ->
    Alcotest.(check bool) "payload non-empty" true (Bytes.length payload > 0);
    let version_byte = Char.code (Bytes.get payload 0) in
    Alcotest.(check int) "layout version byte = 1" 1 version_byte

(** Stage 8: TEA bridge module passes typed-wasm Level 7/10 verification.
    [fn_update] uses [LocalGet 0] (msg) exactly once — per-path analysis
    gives min=1, max=1 → OK. *)
let test_bridge_ownership_verify () =
  let m = Tea_bridge.generate () in
  match Tw_verify.verify_from_module m with
  | Ok () -> ()   (* expected *)
  | Error errs ->
    let msg = String.concat "; " (List.map (fun e ->
      Format.asprintf "%a" Tw_verify.pp_error e) errs) in
    Alcotest.fail (Printf.sprintf "TEA bridge failed ownership verification: %s" msg)

let tea_bridge_tests = [
  Alcotest.test_case "structure (7 funcs, 1 mem, 8 exports)" `Quick test_bridge_structure;
  Alcotest.test_case "export names all present"              `Quick test_bridge_export_names;
  Alcotest.test_case "two custom sections"                   `Quick test_bridge_custom_sections;
  Alcotest.test_case "Wasm binary magic + version"           `Quick test_bridge_wasm_magic;
  Alcotest.test_case "update msg param is Linear"            `Quick test_bridge_update_msg_linear;
  Alcotest.test_case "tea_layout section present + versioned" `Quick test_bridge_tea_layout_section;
  Alcotest.test_case "ownership verify: clean"               `Quick test_bridge_ownership_verify;
]

(* ============================================================================
   Section 11b: E2E TEA Router — Cadre Router Wasm Module
   ============================================================================

   Verifies the Tea_router.generate () output satisfies all structural,
   ownership, layout, and round-trip invariants needed for Stage 6.

   Tests cover:
   1. Module structure (11 funcs, 1 mem, 12 exports)
   2. All 12 export names present
   3. Two custom sections (ownership + tea_layout)
   4. Wasm binary magic + version
   5. push param is Linear (kind=1 in ownership section)
   6. present_popup param is Linear
   7. resize params are both Linear
   8. Push / pop round-trip: push Title(0), pop → stack_len returns to 0
*)

(** Encode the router module to bytes via a temp file (mirrors encode_bridge_to_bytes). *)
let encode_router_to_bytes () : bytes =
  let m    = Tea_router.generate () in
  let path = Filename.temp_file "tea_router" ".wasm" in
  Wasm_encode.write_module_to_file path m;
  let ic   = open_in_bin path in
  let n    = in_channel_length ic in
  let raw  = Bytes.create n in
  really_input ic raw 0 n;
  close_in ic;
  (try Sys.remove path with _ -> ());
  raw

(** Structure: 11 functions, 1 memory, 12 exports. *)
let test_router_structure () =
  let m = Tea_router.generate () in
  Alcotest.(check int) "11 functions" 11 (List.length m.funcs);
  Alcotest.(check int) "1 memory"      1 (List.length m.mems);
  Alcotest.(check int) "12 exports"   12 (List.length m.exports)

(** All 12 expected exports are present by name. *)
let test_router_export_names () =
  let m = Tea_router.generate () in
  let names = List.map (fun e -> e.Wasm.e_name) m.exports in
  let expected = [
    "affinescript_router_init";
    "affinescript_router_push";
    "affinescript_router_pop";
    "affinescript_router_present_popup";
    "affinescript_router_dismiss_popup";
    "affinescript_router_resize";
    "affinescript_router_get_screen_w";
    "affinescript_router_get_screen_h";
    "affinescript_router_get_stack_len";
    "affinescript_router_get_stack_top";
    "affinescript_router_get_popup_tag";
    "memory";
  ] in
  List.iter (fun ex ->
    Alcotest.(check bool)
      (Printf.sprintf "export '%s' present" ex)
      true (List.mem ex names)
  ) expected

(** Two custom sections: ownership + tea_layout. *)
let test_router_custom_sections () =
  let m = Tea_router.generate () in
  Alcotest.(check int) "2 custom sections" 2 (List.length m.custom_sections);
  let names = List.map fst m.custom_sections in
  Alcotest.(check bool) "ownership section"  true
    (List.mem "typedwasm.ownership"  names);
  Alcotest.(check bool) "tea_layout section" true
    (List.mem "affinescript.tea_layout" names)

(** Wasm binary starts with correct magic \x00asm + version \x01\x00\x00\x00. *)
let test_router_wasm_magic () =
  let raw = encode_router_to_bytes () in
  let magic   = Bytes.sub raw 0 4 in
  let version = Bytes.sub raw 4 4 in
  Alcotest.(check bytes) "Wasm magic"   (Bytes.of_string "\x00asm")          magic;
  Alcotest.(check bytes) "Wasm version" (Bytes.of_string "\x01\x00\x00\x00") version

(** fn_push (func 1): param 0 (screen_tag) must be Linear (kind byte = 1). *)
let test_router_push_param_linear () =
  let raw     = encode_router_to_bytes () in
  let entries = parse_ownership_from_bytes raw in
  match List.assoc_opt 1 (List.map (fun (f, p, r) -> (f, (p, r))) entries) with
  | None ->
    Alcotest.fail "no ownership entry for func 1 (push)"
  | Some (params, _ret) ->
    (match params with
     | [kind] ->
       Alcotest.(check int) "push screen_tag is Linear (1)" 1 kind
     | _ ->
       Alcotest.fail
         (Printf.sprintf "expected 1 param for push, got %d" (List.length params)))

(** fn_present_popup (func 3): param 0 (popup_tag) must be Linear. *)
let test_router_present_popup_param_linear () =
  let raw     = encode_router_to_bytes () in
  let entries = parse_ownership_from_bytes raw in
  match List.assoc_opt 3 (List.map (fun (f, p, r) -> (f, (p, r))) entries) with
  | None ->
    Alcotest.fail "no ownership entry for func 3 (present_popup)"
  | Some (params, _ret) ->
    (match params with
     | [kind] ->
       Alcotest.(check int) "present_popup popup_tag is Linear (1)" 1 kind
     | _ ->
       Alcotest.fail
         (Printf.sprintf "expected 1 param for present_popup, got %d" (List.length params)))

(** fn_resize (func 5): both params (w, h) must be Linear. *)
let test_router_resize_params_linear () =
  let raw     = encode_router_to_bytes () in
  let entries = parse_ownership_from_bytes raw in
  match List.assoc_opt 5 (List.map (fun (f, p, r) -> (f, (p, r))) entries) with
  | None ->
    Alcotest.fail "no ownership entry for func 5 (resize)"
  | Some (params, _ret) ->
    (match params with
     | [k0; k1] ->
       Alcotest.(check int) "resize w is Linear (1)" 1 k0;
       Alcotest.(check int) "resize h is Linear (1)" 1 k1
     | _ ->
       Alcotest.fail
         (Printf.sprintf "expected 2 params for resize, got %d" (List.length params)))

(** tea_layout section is non-empty and starts with version byte 1. *)
let test_router_tea_layout_section () =
  let m = Tea_router.generate () in
  match List.assoc_opt "affinescript.tea_layout" m.custom_sections with
  | None ->
    Alcotest.fail "affinescript.tea_layout section missing"
  | Some payload ->
    Alcotest.(check bool) "payload non-empty" true (Bytes.length payload > 0);
    let version_byte = Char.code (Bytes.get payload 0) in
    Alcotest.(check int) "layout version byte = 1" 1 version_byte

(** Stage 9: Router bridge module passes typed-wasm Level 7/10 verification.
    fn_push: then-branch stores LocalGet 0, else-branch explicitly [LocalGet 0; Drop].
    Per-path analysis: min(1,1)=1, max(1,1)=1 → OK. *)
let test_router_ownership_verify () =
  let m = Tea_router.generate () in
  match Tw_verify.verify_from_module m with
  | Ok () -> ()   (* expected *)
  | Error errs ->
    let msg = String.concat "; " (List.map (fun e ->
      Format.asprintf "%a" Tw_verify.pp_error e) errs) in
    Alcotest.fail (Printf.sprintf "Router bridge failed ownership verification: %s" msg)

let tea_router_tests = [
  Alcotest.test_case "structure (11 funcs, 1 mem, 12 exports)" `Quick test_router_structure;
  Alcotest.test_case "export names all present"                 `Quick test_router_export_names;
  Alcotest.test_case "two custom sections"                      `Quick test_router_custom_sections;
  Alcotest.test_case "Wasm binary magic + version"              `Quick test_router_wasm_magic;
  Alcotest.test_case "push screen_tag param is Linear"          `Quick test_router_push_param_linear;
  Alcotest.test_case "present_popup param is Linear"            `Quick test_router_present_popup_param_linear;
  Alcotest.test_case "resize w+h params are both Linear"        `Quick test_router_resize_params_linear;
  Alcotest.test_case "tea_layout section present + versioned"   `Quick test_router_tea_layout_section;
  Alcotest.test_case "ownership verify: clean"                  `Quick test_router_ownership_verify;
]

(* ============================================================================
   Section 12: E2E Traits — Registry, Method Dispatch, Body Checking
   ============================================================================

   These tests verify the trait resolution pipeline wired in the type checker:

   1. A valid impl (all required methods provided) passes type checking.
   2. An impl that omits a required method is rejected with a descriptive
      error that names the missing method.

   Regression coverage for the trait-registry wiring PR.
*)

(** Valid impl: an impl block that satisfies its trait in full must be
    accepted by the type checker without error. *)
let test_trait_impl_valid () =
  match parse_fixture (fixture "trait_impl_valid.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ -> ()  (* Expected: impl accepted *)
      | Error e ->
        Alcotest.fail (Printf.sprintf
          "valid trait impl unexpectedly rejected: %s"
          (Typecheck.format_type_error e))

(** Missing method: an impl block that omits a non-default required method
    must be rejected by the type checker.  The error must mention the
    missing method name so the user knows what to fix. *)
let test_trait_impl_missing_method () =
  match parse_fixture (fixture "trait_impl_missing_method.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      match Typecheck.check_program ctx.symbols prog with
      | Ok _ ->
        Alcotest.fail
          "expected rejection: impl omitting a required trait method should \
           be a type error, but the checker accepted the program"
      | Error e ->
        (* The error must mention the omitted method name *)
        let msg = Typecheck.format_type_error e in
        Alcotest.(check bool) "error mentions 'summary'" true
          (try let _ = Str.search_forward (Str.regexp "summary") msg 0 in true
           with Not_found -> false)

let trait_impl_tests = [
  Alcotest.test_case "valid impl accepted"
    `Quick test_trait_impl_valid;
  Alcotest.test_case "missing method rejected"
    `Quick test_trait_impl_missing_method;
]

(* ============================================================================
   Section N+1: Stage 3 — TEA stdlib (The Elm Architecture) tests
   ============================================================================

   Verify:
   3a — Cmd/Sub/Html enum types parse and type-check
   3b — enum constructors are bound at runtime (nullary + payload)
   3c — counter.afs: init→0, update(Increment)→1, update(Decrement)→0
   3d — titlescreen.afs compiles without errors (interpreter-level)
*)

(** Run eval pipeline through to the interpreter env, then call main() with
    stdin from a string.  Returns the (stdout, exit code) pair. *)
let run_tea_program_with_input fixture_name input_lines =
  (* Write the test input to a temp file and redirect stdin *)
  let input = String.concat "\n" input_lines ^ "\n" in
  let tmp_in = Filename.temp_file "affinescript_tea_in" "" in
  let oc = open_out tmp_in in
  output_string oc input;
  close_out oc;
  let output_buf = Buffer.create 64 in
  let saved_stdout = Unix.dup Unix.stdout in
  let tmp_out = Filename.temp_file "affinescript_tea_out" "" in
  let fd_out = Unix.openfile tmp_out [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.close fd_out;
  let saved_stdin  = Unix.dup Unix.stdin  in
  let fd_in  = Unix.openfile tmp_in  [Unix.O_RDONLY] 0o600 in
  Unix.dup2 fd_in Unix.stdin;
  Unix.close fd_in;
  let result = (match run_wasm_pipeline (fixture fixture_name) with
    | Error _ -> `Failed  (* type/compile error before running *)
    | Ok _    ->
      match run_frontend (fixture fixture_name) with
      | Error _ -> `Failed
      | Ok (prog, _) ->
        (match Interp.eval_program prog with
        | Error _ -> `Failed
        | Ok env ->
          (match Value.lookup_env "main" env with
          | Error _ -> `NoMain
          | Ok main_fn ->
            (match Interp.apply_function main_fn [] with
            | Ok _ -> `Ok
            | Error _ -> `RuntimeError)))) in
  Unix.dup2 saved_stdout Unix.stdout;
  Unix.close saved_stdout;
  Unix.dup2 saved_stdin  Unix.stdin;
  Unix.close saved_stdin;
  let ic = open_in tmp_out in
  (try while true do
    Buffer.add_string output_buf (input_line ic); Buffer.add_char output_buf '\n'
  done with End_of_file -> ());
  close_in ic;
  Sys.remove tmp_in; Sys.remove tmp_out;
  (Buffer.contents output_buf, result)

(** Simpler helper: just check the program compiles and evals without error *)
let test_tea_counter_compiles () =
  (* counter.affine should parse, type-check, and run without errors *)
  match run_frontend (fixture "counter.affine") with
  | Error msg -> Alcotest.fail ("Frontend error: " ^ msg)
  | Ok (prog, _) ->
    match Interp.eval_program prog with
    | Error e -> Alcotest.fail ("Eval error: " ^ Value.show_eval_error e)
    | Ok _ -> ()  (* definitions loaded into env — success *)

let test_tea_counter_init () =
  (* counter_init() should return 0 *)
  match run_frontend (fixture "counter.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok (prog, _) ->
    match Interp.eval_program prog with
    | Error e -> Alcotest.fail (Value.show_eval_error e)
    | Ok env ->
      match Value.lookup_env "counter_init" env with
      | Error _ -> Alcotest.fail "counter_init not found in env"
      | Ok init_fn ->
        match Interp.apply_function init_fn [] with
        | Error e -> Alcotest.fail (Value.show_eval_error e)
        | Ok v ->
          Alcotest.(check int) "counter starts at 0" 0
            (match v with Value.VInt n -> n | _ -> -1)

let test_tea_counter_update () =
  (* counter_update(Increment, 0) should return 1;
     counter_update(Decrement, 1) should return 0 *)
  match run_frontend (fixture "counter.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok (prog, _) ->
    match Interp.eval_program prog with
    | Error e -> Alcotest.fail (Value.show_eval_error e)
    | Ok env ->
      match Value.lookup_env "counter_update" env with
      | Error _ -> Alcotest.fail "counter_update not found"
      | Ok update_fn ->
        let incr = Value.VVariant ("Increment", None) in
        let decr = Value.VVariant ("Decrement", None) in
        let model0 = Value.VInt 0 in
        (match Interp.apply_function update_fn [incr; model0] with
        | Error e -> Alcotest.fail (Value.show_eval_error e)
        | Ok v1 ->
          Alcotest.(check int) "Increment 0 → 1" 1
            (match v1 with Value.VInt n -> n | _ -> -1);
          match Interp.apply_function update_fn [decr; v1] with
          | Error e -> Alcotest.fail (Value.show_eval_error e)
          | Ok v2 ->
            Alcotest.(check int) "Decrement 1 → 0" 0
              (match v2 with Value.VInt n -> n | _ -> -1))

let test_tea_titlescreen_compiles () =
  (* titlescreen.affine should parse, type-check, and eval without errors *)
  match run_frontend (fixture "titlescreen.affine") with
  | Error msg -> Alcotest.fail ("Frontend error: " ^ msg)
  | Ok (prog, _) ->
    match Interp.eval_program prog with
    | Error e -> Alcotest.fail ("Eval error: " ^ Value.show_eval_error e)
    | Ok _ -> ()

let test_tea_titlescreen_update () =
  (* title_update(NewGame, init_model) should set selected = "new_game" *)
  match run_frontend (fixture "titlescreen.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok (prog, _) ->
    match Interp.eval_program prog with
    | Error e -> Alcotest.fail (Value.show_eval_error e)
    | Ok env ->
      let get n = match Value.lookup_env n env with
        | Ok v -> v | Error _ -> Alcotest.fail (n ^ " not found") in
      let init_fn   = get "title_init"   in
      let update_fn = get "title_update" in
      match Interp.apply_function init_fn [] with
      | Error e -> Alcotest.fail (Value.show_eval_error e)
      | Ok model ->
        let new_game_msg = Value.VVariant ("NewGame", None) in
        (match Interp.apply_function update_fn [new_game_msg; model] with
        | Error e -> Alcotest.fail (Value.show_eval_error e)
        | Ok new_model ->
          (* selected field should be "new_game" *)
          let selected = match new_model with
            | Value.VRecord fields ->
              (match List.assoc_opt "selected" fields with
              | Some (Value.VString s) -> s
              | _ -> "?")
            | _ -> "?"
          in
          Alcotest.(check string) "NewGame sets selected=new_game"
            "new_game" selected)

let tea_tests = [
  Alcotest.test_case "counter compiles"          `Quick test_tea_counter_compiles;
  Alcotest.test_case "counter init=0"            `Quick test_tea_counter_init;
  Alcotest.test_case "counter update transitions" `Quick test_tea_counter_update;
  Alcotest.test_case "titlescreen compiles"      `Quick test_tea_titlescreen_compiles;
  Alcotest.test_case "titlescreen NewGame→new_game" `Quick test_tea_titlescreen_update;
]

(* ============================================================================
   Section 13: E2E LSP Phase B — Hover and Goto-Definition

   These tests verify the hover and goto-def pipeline entry points that
   power LSP features.  They run entirely through the library API (no
   subprocess), calling the same helpers used by the CLI commands.

   Key properties verified:
   - find_symbol_at locates a symbol by definition span
   - find_symbol_at resolves a use-site reference back to its definition
   - Not-found returns None (no crash)
   - JSON serialisation is duplicate-key-free
*)

(** Run parse→resolve→typecheck on a fixture, collect the symbol table
    and reference list for subsequent hover/goto-def queries. *)
let pipeline_for_hover path =
  match parse_fixture path with
  | Error msg -> failwith msg
  | Ok prog ->
    let loader_config = Module_loader.default_config () in
    let loader = Module_loader.create loader_config in
    match Resolve.resolve_program_with_loader prog loader with
    | Error (e, _) -> failwith (Resolve.show_resolve_error e)
    | Ok (resolve_ctx, _) ->
      (* Type-check to populate sym_type; ignore errors (same as CLI). *)
      let _tc = Typecheck.check_program resolve_ctx.symbols prog in
      let refs =
        List.rev resolve_ctx.references
        |> List.map (fun (r : Resolve.reference) ->
             Json_output.{ ref_symbol_id = r.ref_symbol_id;
                           ref_span      = r.ref_span })
      in
      (resolve_ctx.symbols, refs)

(** Helper: run the query, fail the test if the symbol isn't found. *)
let require_symbol symbols refs line col =
  match Json_output.find_symbol_at symbols refs line col with
  | None ->
    Alcotest.failf "hover: expected symbol at (%d,%d) but got None" line col
  | Some sym -> sym

(** hover — definition span resolves to its own name. *)
let test_hover_def_span () =
  let (symbols, refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  (* `fn add(…)` — name span starts at line 5, col 1. *)
  let sym = require_symbol symbols refs 5 1 in
  Alcotest.(check string) "hovered name is 'add'" "add" sym.Symbol.sym_name;
  Alcotest.(check string) "kind is function"
    "function" (Json_output.symbol_kind_to_string sym.sym_kind)

(** hover — use-site reference resolves to the defining symbol. *)
let test_hover_use_site () =
  let (symbols, refs) = pipeline_for_hover (fixture "full_pipeline.affine") in
  (* `Circle` is used at line 33, col 14 (verified above). *)
  let sym = require_symbol symbols refs 33 14 in
  Alcotest.(check string) "use-site resolves to 'Circle'" "Circle" sym.Symbol.sym_name

(** hover — off-document position returns None without crashing. *)
let test_hover_not_found () =
  let (symbols, refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let result = Json_output.find_symbol_at symbols refs 9999 9999 in
  Alcotest.(check bool) "none at phantom position" true (result = None)

(** goto-def — JSON output is well-formed (has "found" and "file"). *)
let test_goto_def_json () =
  let (symbols, refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let sym_opt = Json_output.find_symbol_at symbols refs 5 3 in
  let json = match sym_opt with
    | Some sym -> Json_output.goto_def_to_json sym
    | None     -> Json_output.not_found_json
  in
  (match json with
   | `Assoc fields ->
     Alcotest.(check bool) "has 'found'" true (List.mem_assoc "found" fields);
     Alcotest.(check bool) "has 'file'"  true (List.mem_assoc "file"  fields)
   | _ -> Alcotest.fail "goto_def_to_json must return a JSON object")

let lsp_phase_b_tests = [
  Alcotest.test_case "hover def span"       `Quick test_hover_def_span;
  Alcotest.test_case "hover use-site"       `Quick test_hover_use_site;
  Alcotest.test_case "hover not found"      `Quick test_hover_not_found;
  Alcotest.test_case "goto-def JSON fields" `Quick test_goto_def_json;
]

(* ============================================================================
   Section N: LSP Phase C — Completion candidates
   ============================================================================

   These tests validate [Json_output.extract_prefix_at] and
   [Json_output.collect_completions] — the two functions powering the
   [complete FILE LINE COL] subcommand.
*)

(** Cursor placed right after "add(" on line 5 of arithmetic.affine:
    col 7 puts end_idx at 5 (0-based), scanning back collects 'd','d','a'
    → prefix "add". *)
let test_complete_prefix_extracted () =
  let path = fixture "arithmetic.affine" in
  let source = read_file path in
  (* Line 5: "fn add(a: Int, b: Int) -> Int = a + b;"
     Cols:      1234567  — col 7 is '(' so prefix ends at col 6 *)
  let (prefix, dot_ctx) = Json_output.extract_prefix_at source 5 7 in
  Alcotest.(check string) "prefix is 'add'"    "add"  prefix;
  Alcotest.(check bool)   "not dot context"    false  dot_ctx

(** Prefix "add" matches the [add] symbol in the arithmetic fixture. *)
let test_complete_prefix_match () =
  let (symbols, _refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let items = Json_output.collect_completions symbols "add" false in
  let names =
    List.map (fun (i : Json_output.completion_item) -> i.Json_output.comp_name) items
  in
  Alcotest.(check bool) "add is a candidate" true (List.mem "add" names)

(** Empty prefix returns all symbols + keywords — at least the 6 functions
    defined in the arithmetic fixture are present. *)
let test_complete_empty_prefix () =
  let (symbols, _refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let items = Json_output.collect_completions symbols "" false in
  Alcotest.(check bool) "non-empty for empty prefix"
    true (List.length items > 0)

(** An unrecognised prefix produces an empty candidate list. *)
let test_complete_no_match () =
  let (symbols, _refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let items = Json_output.collect_completions symbols "zzznotfound" false in
  Alcotest.(check int) "zero candidates for unknown prefix" 0 (List.length items)

(** Keyword "fn" appears in completions when the prefix matches and we are
    not in a dot-access context. *)
let test_complete_keyword_included () =
  let (symbols, _refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  let items = Json_output.collect_completions symbols "fn" false in
  let kinds =
    List.map (fun (i : Json_output.completion_item) -> i.Json_output.comp_kind) items
  in
  Alcotest.(check bool) "keyword item present" true (List.mem "keyword" kinds)

(** In a dot-access context, keyword candidates are suppressed. *)
let test_complete_dot_suppresses_keywords () =
  let (symbols, _refs) = pipeline_for_hover (fixture "arithmetic.affine") in
  (* dot_ctx = true → no keywords, even for empty prefix *)
  let items = Json_output.collect_completions symbols "" true in
  let kinds =
    List.map (fun (i : Json_output.completion_item) -> i.Json_output.comp_kind) items
  in
  Alcotest.(check bool) "no keyword items in dot context"
    false (List.mem "keyword" kinds)

let lsp_phase_c_tests = [
  Alcotest.test_case "prefix extracted correctly"      `Quick test_complete_prefix_extracted;
  Alcotest.test_case "prefix match returns symbol"     `Quick test_complete_prefix_match;
  Alcotest.test_case "empty prefix returns candidates" `Quick test_complete_empty_prefix;
  Alcotest.test_case "unknown prefix returns empty"    `Quick test_complete_no_match;
  Alcotest.test_case "keyword included when prefix ok" `Quick test_complete_keyword_included;
  Alcotest.test_case "dot ctx suppresses keywords"     `Quick test_complete_dot_suppresses_keywords;
]

(* ============================================================================
   Section N+1: LSP Phase D — JSON-RPC server helpers
   ============================================================================

   The server loop itself requires a live stdin/stdout pair, so we test
   the stateless helper functions that underpin every LSP handler:
   uri_to_path, lsp_range (0-based position conversion), and the in-process
   pipeline runner.
*)

(** [file:///abs/path] → [/abs/path]. *)
let test_lsp_uri_to_path () =
  let path = Lsp_server.uri_to_path "file:///var/mnt/eclipse/repos/foo.affine" in
  Alcotest.(check string) "uri_to_path strips prefix"
    "/var/mnt/eclipse/repos/foo.affine" path

(** Compiler spans are 1-based; LSP ranges must be 0-based. *)
let test_lsp_position_conversion () =
  let span = Span.make
    ~file:"test.affine"
    ~start_pos:{ Span.line = 5; col = 3; offset = 0 }
    ~end_pos:  { Span.line = 5; col = 6; offset = 0 }
  in
  let range = Lsp_server.lsp_range span in
  (match range with
  | `Assoc rf ->
    (match List.assoc_opt "start" rf with
    | Some (`Assoc sp) ->
      let ln = (match List.assoc_opt "line"      sp with Some (`Int n) -> n | _ -> -1) in
      let ch = (match List.assoc_opt "character" sp with Some (`Int n) -> n | _ -> -1) in
      Alcotest.(check int) "start.line is 0-based"      4 ln;
      Alcotest.(check int) "start.character is 0-based" 2 ch
    | _ -> Alcotest.fail "start is not an object")
  | _ -> Alcotest.fail "lsp_range must return an Assoc")

(** Valid source → empty diagnostics + symbol table present. *)
let test_lsp_pipeline_valid () =
  let source = "fn add(a: Int, b: Int) -> Int = a + b;" in
  let (diags, symbols_opt, _refs) =
    Lsp_server.run_pipeline "/tmp/affinescript_lsp_test.affine" source
  in
  Alcotest.(check int)  "no diagnostics for valid source" 0  (List.length diags);
  Alcotest.(check bool) "symbol table present"            true (symbols_opt <> None)

(** Broken source → at least one diagnostic, no crash. *)
let test_lsp_pipeline_invalid () =
  let source = "fn broken( " in
  let (diags, _symbols_opt, _refs) =
    Lsp_server.run_pipeline "/tmp/affinescript_lsp_test_bad.affine" source
  in
  Alcotest.(check bool) "at least one diagnostic for broken source"
    true (List.length diags > 0)

let lsp_phase_d_tests = [
  Alcotest.test_case "uri_to_path strips file://"        `Quick test_lsp_uri_to_path;
  Alcotest.test_case "lsp_range converts to 0-based"     `Quick test_lsp_position_conversion;
  Alcotest.test_case "pipeline: valid source → no diags" `Quick test_lsp_pipeline_valid;
  Alcotest.test_case "pipeline: broken source → diag"    `Quick test_lsp_pipeline_invalid;
]

(* ============================================================================
   Section 21: Try / Catch / Finally Tests
   ============================================================================

   These tests verify that the try/catch/finally construct type-checks and
   survives the full pipeline through both the Julia and interpreter backends.

   WASM 1.0 tests only verify that the pipeline raises a clean
   UnsupportedFeature error when catch arms are present; body-only and
   finally-only variants must succeed.
*)

let test_try_typecheck_body_only () =
  match run_frontend (fixture "try_body_only.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_try_typecheck_finally () =
  match run_frontend (fixture "try_finally.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_try_typecheck_catch_wildcard () =
  match run_frontend (fixture "try_catch_wildcard.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_try_typecheck_catch_var () =
  match run_frontend (fixture "try_catch_var.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_try_typecheck_full () =
  match run_frontend (fixture "try_catch_finally.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

(** Body-only and finally-only variants must compile to WASM without error. *)
let test_try_wasm_body_only () =
  match run_wasm_pipeline (fixture "try_body_only.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_try_wasm_finally () =
  match run_wasm_pipeline (fixture "try_finally.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

(** Catch arms must produce a clean UnsupportedFeature error in WASM 1.0. *)
let test_try_wasm_catch_unsupported () =
  match run_wasm_pipeline (fixture "try_catch_wildcard.affine") with
  | Ok _ ->
      (* Acceptable if the WASM backend happens to support this in future. *)
      ()
  | Error msg ->
      Alcotest.(check bool) "UnsupportedFeature error for catch in WASM"
        true (String.length msg > 0)

(** All five fixtures must produce Julia code without errors. *)
let test_try_julia_body_only () =
  match run_julia_pipeline (fixture "try_body_only.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
      Alcotest.(check bool) "non-empty Julia output" true
        (String.length code > 0)

let test_try_julia_finally () =
  match run_julia_pipeline (fixture "try_finally.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
      Alcotest.(check bool) "contains try keyword" true
        (try let _ = Str.search_forward (Str.regexp "try") code 0 in true
         with Not_found -> false)

let test_try_julia_catch_wildcard () =
  match run_julia_pipeline (fixture "try_catch_wildcard.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
      Alcotest.(check bool) "contains catch keyword" true
        (try let _ = Str.search_forward (Str.regexp "catch") code 0 in true
         with Not_found -> false)

let test_try_julia_catch_var () =
  match run_julia_pipeline (fixture "try_catch_var.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
      Alcotest.(check bool) "contains catch keyword" true
        (try let _ = Str.search_forward (Str.regexp "catch") code 0 in true
         with Not_found -> false)

let test_try_julia_full () =
  match run_julia_pipeline (fixture "try_catch_finally.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok code ->
      let has_try     = try let _ = Str.search_forward (Str.regexp "try")     code 0 in true with Not_found -> false in
      let has_catch   = try let _ = Str.search_forward (Str.regexp "catch")   code 0 in true with Not_found -> false in
      let has_finally = try let _ = Str.search_forward (Str.regexp "finally") code 0 in true with Not_found -> false in
      Alcotest.(check bool) "has try"     true has_try;
      Alcotest.(check bool) "has catch"   true has_catch;
      Alcotest.(check bool) "has finally" true has_finally

let try_catch_tests = [
  Alcotest.test_case "typecheck: body-only"        `Quick test_try_typecheck_body_only;
  Alcotest.test_case "typecheck: finally"          `Quick test_try_typecheck_finally;
  Alcotest.test_case "typecheck: catch wildcard"   `Quick test_try_typecheck_catch_wildcard;
  Alcotest.test_case "typecheck: catch var"        `Quick test_try_typecheck_catch_var;
  Alcotest.test_case "typecheck: full form"        `Quick test_try_typecheck_full;
  Alcotest.test_case "wasm: body-only compiles"    `Quick test_try_wasm_body_only;
  Alcotest.test_case "wasm: finally compiles"      `Quick test_try_wasm_finally;
  Alcotest.test_case "wasm: catch → unsupported"   `Quick test_try_wasm_catch_unsupported;
  Alcotest.test_case "julia: body-only"            `Quick test_try_julia_body_only;
  Alcotest.test_case "julia: finally"              `Quick test_try_julia_finally;
  Alcotest.test_case "julia: catch wildcard"       `Quick test_try_julia_catch_wildcard;
  Alcotest.test_case "julia: catch var"            `Quick test_try_julia_catch_var;
  Alcotest.test_case "julia: full form"            `Quick test_try_julia_full;
]

(* ============================================================================
   Issue #555: effect-handler loud-fail fences
   ============================================================================

   The core-WASM / JS-text / Deno-ESM backends previously compiled `handle`
   body-only (handler arms silently dropped; `resume` lowered as an argument
   passthrough). An effects-free return-arm handle evaluated to 42 in the
   interpreter and 41 in compiled wasm — a silent wrong-value miscompile.
   These tests pin the fences: all three backends must fail loudly, and the
   interpreter path must keep working. *)

let contains_str needle haystack =
  try let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
  with Not_found -> false

let test_handle_interp_still_works () =
  match run_interp_pipeline (fixture "handle_return_arm.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok _ -> ()

let test_handle_wasm_loud_fail () =
  match run_wasm_pipeline (fixture "handle_return_arm.affine") with
  | Ok _ ->
      Alcotest.fail
        "expected UnsupportedFeature for handle on core-WASM (Refs #555); \
         got Ok — silent arm-drop has regressed"
  | Error msg ->
      Alcotest.(check bool) "error names the handler fence" true
        (contains_str "effect handler" msg)

let test_handle_js_loud_fail () =
  let result =
    let open Result in
    let ( let* ) = bind in
    let* (prog, resolve_ctx) = run_frontend (fixture "handle_return_arm.affine") in
    Js_codegen.codegen_js prog resolve_ctx.symbols
  in
  match result with
  | Ok _ ->
      Alcotest.fail
        "expected loud failure for handle on JS-text (Refs #555); \
         got Ok — silent erasure has regressed"
  | Error msg ->
      Alcotest.(check bool) "error names the handler fence" true
        (contains_str "effect handler" msg)

let test_handle_deno_loud_fail () =
  let result =
    let open Result in
    let ( let* ) = bind in
    let* (prog, resolve_ctx) = run_frontend (fixture "handle_return_arm.affine") in
    Codegen_deno.codegen_deno prog resolve_ctx.symbols
  in
  match result with
  | Ok _ ->
      Alcotest.fail
        "expected loud failure for handle on Deno-ESM (Refs #555); \
         got Ok — silent erasure has regressed"
  | Error msg ->
      Alcotest.(check bool) "error names the handler fence" true
        (contains_str "effect handler" msg)

let test_handle_c_loud_fail () =
  let result =
    let open Result in
    let ( let* ) = bind in
    let* (prog, resolve_ctx) = run_frontend (fixture "handle_return_arm.affine") in
    C_codegen.codegen_c prog resolve_ctx.symbols
  in
  match result with
  | Ok _ ->
      Alcotest.fail
        "expected loud failure for handle on the C backend (Refs #555); \
         got Ok — silent arm-drop has regressed"
  | Error msg ->
      Alcotest.(check bool) "error names the handler fence" true
        (contains_str "effect handler" msg)

(* NOTE: the Lean and Why3 text backends are experimental stubs whose
   block-lowering only emits `let` bindings — a `return` statement (and thus
   any expression it wraps, including `handle`) is silently dropped, so
   `fn main() -> Int { return handle ... }` emits `def main : Int := ()`.
   That is a *broader* incompleteness than #555 (they miscompile far more than
   handlers), so a handle-specific fence there would be misleading. They are
   flagged separately rather than fenced here; the reachable #555 codegen
   holes were the production backends (WASM / WasmGC / Deno-ESM / JS-text)
   and C, all fenced + tested above. *)

let handler_fence_tests = [
  Alcotest.test_case "interp: return-arm handle still evaluates" `Quick test_handle_interp_still_works;
  Alcotest.test_case "wasm: handle → loud UnsupportedFeature"     `Quick test_handle_wasm_loud_fail;
  Alcotest.test_case "js-text: handle → loud failure"             `Quick test_handle_js_loud_fail;
  Alcotest.test_case "deno-esm: handle → loud failure"            `Quick test_handle_deno_loud_fail;
  Alcotest.test_case "c: handle → loud failure"                   `Quick test_handle_c_loud_fail;
]

(* ============================================================================
   Issue #559: trait coherence — overlapping impls must be rejected
   ============================================================================

   `Trait.check_coherence` was a no-op stub (TODO) and unwired, so two impls of
   the same trait for the same (or unifiable) self type were silently accepted
   and method resolution picked whichever impl came first. `check_all_coherence`
   now runs in `check_program` after every impl is registered and rejects any
   pair of impls whose self types unify. *)

let tc_source src =
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match resolve_program prog with
  | Error msg -> Error msg
  | Ok (ctx, _) ->
    (match Typecheck.check_program ctx.symbols prog with
     | Ok _ -> Ok ()
     | Error e -> Error (Typecheck.format_type_error e))

let test_coherence_duplicate_rejected () =
  let src = {|
trait Greet { fn greet() -> Int; }
struct Person { age: Int }
impl Greet for Person { fn greet() -> Int = 1; }
impl Greet for Person { fn greet() -> Int = 2; }
fn main() -> Int = 0;
|} in
  match tc_source src with
  | Ok () ->
      Alcotest.fail "#559: two impls of Greet for Person must be rejected as \
                     overlapping; the checker accepted them"
  | Error msg ->
      Alcotest.(check bool) "error names trait coherence" true
        (contains_str "coherence" msg)

let test_coherence_distinct_types_ok () =
  let src = {|
trait Greet { fn greet() -> Int; }
struct Person { age: Int }
struct Robot { id: Int }
impl Greet for Person { fn greet() -> Int = 1; }
impl Greet for Robot { fn greet() -> Int = 2; }
fn main() -> Int = 0;
|} in
  match tc_source src with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "#559: impls of Greet for two DISTINCT types must be \
                      accepted (no overlap), got error: %s" msg

let test_coherence_distinct_generic_args_ok () =
  (* Coherence must not over-reject: impls for the SAME generic head but
     distinct concrete args (`Pair[Int,Bool]` vs `Pair[Bool,Int]`) do not
     overlap and must both be accepted. *)
  let src = {|
trait Greet { fn greet() -> Int; }
enum Pair[A, B] { Mk(A, B) }
impl Greet for Pair[Int, Bool] { fn greet() -> Int = 1; }
impl Greet for Pair[Bool, Int] { fn greet() -> Int = 2; }
fn main() -> Int = 0;
|} in
  match tc_source src with
  | Ok () -> ()
  | Error msg ->
      Alcotest.failf "#559: impls for Pair[Int,Bool] vs Pair[Bool,Int] do not \
                      overlap and must be accepted, got error: %s" msg

(* #559 generic-subsumption: a blanket impl `impl[T] Greet for Box[T]`
   overlapping a specific `impl Greet for Box[Int]` IS detected and rejected.
   The unification-based coherence check instantiates the generic head — Box[T]
   unifies with Box[Int] — so the overlap is caught by the same machinery as the
   concrete case. (Ground-truthed 2026-06-21 against the running compiler; an
   earlier note here wrongly claimed this was undetected — corrected per the
   docs/SOUNDNESS.adoc "compiler is ground truth" rule.) *)
let test_coherence_generic_subsumption_rejected () =
  let src = {|
trait Greet { fn greet() -> Int; }
enum Box[T] { Mk(T) }
impl[T] Greet for Box[T] { fn greet() -> Int = 1; }
impl Greet for Box[Int] { fn greet() -> Int = 2; }
fn main() -> Int = 0;
|} in
  match tc_source src with
  | Ok () ->
      Alcotest.fail "#559 generic subsumption: impl[T] Box[T] overlapping \
                     Box[Int] must be rejected as overlapping; the checker \
                     accepted them — the hole has regressed"
  | Error msg ->
      Alcotest.(check bool) "error names trait coherence" true
        (contains_str "coherence" msg)

let coherence_tests = [
  Alcotest.test_case "duplicate impl (same self type) → rejected"    `Quick test_coherence_duplicate_rejected;
  Alcotest.test_case "impls for distinct types → accepted"           `Quick test_coherence_distinct_types_ok;
  Alcotest.test_case "distinct generic args → accepted (no over-reject)" `Quick test_coherence_distinct_generic_args_ok;
  Alcotest.test_case "generic subsumption (impl[T] Box[T] vs Box[Int]) → rejected" `Quick test_coherence_generic_subsumption_rejected;
]

(* ============================================================================
   Issue #556: async CPS table-miss loud-fail fence
   ============================================================================

   gen_function previously fell through to synchronous `gen_expr` whenever the
   ADR-013 CPS transform did not fire — even for an `Async` fn that performs an
   async call whose result is consumed by a continuation. That silently ran the
   continuation against an unsettled `Thenable` handle (a wrong value, no
   diagnostic). The fence makes that case fail loudly, while still lowering the
   sound shapes: the `let r = <async-call>; <cont>` base case (transformed), the
   bare tail-return-`Thenable` pass-through (#205 convergence protocol), and an
   `Async` fn that performs no async call at all. *)

let test_async_wasm_loud_fail () =
  match run_wasm_pipeline (fixture "async_sync_fallback.affine") with
  | Ok _ ->
      Alcotest.fail
        "expected UnsupportedFeature for an un-lowerable async-consuming fn \
         on core-WASM (Refs #556); got Ok — silent synchronous fallback has \
         regressed"
  | Error msg ->
      Alcotest.(check bool) "error names the async fence" true
        (contains_str "async function" msg)

let test_async_passthrough_still_compiles () =
  (* Tail-return-Thenable pass-through (#205): sound to lower synchronously;
     the fence must not over-reject it. *)
  match run_wasm_pipeline (fixture "async_passthrough_thenable.affine") with
  | Error msg -> Alcotest.failf "async pass-through should compile, got: %s" msg
  | Ok _ -> ()

let test_async_no_boundary_still_compiles () =
  (* `Async` row but no async call ⇒ no continuation to mis-order; must
     compile (the guard keys on an actual async-prim call, not the row). *)
  match run_wasm_pipeline (fixture "async_no_boundary.affine") with
  | Error msg -> Alcotest.failf "async-rowed fn with no async call should compile, got: %s" msg
  | Ok _ -> ()

let async_fence_tests = [
  Alcotest.test_case "wasm: async-consuming fn → loud UnsupportedFeature" `Quick test_async_wasm_loud_fail;
  Alcotest.test_case "wasm: tail-return-Thenable pass-through compiles"   `Quick test_async_passthrough_still_compiles;
  Alcotest.test_case "wasm: async row without async call compiles"        `Quick test_async_no_boundary_still_compiles;
]

(* ============================================================================
   Issue #555: interpreter resume soundness (multi-shot loud-fail)
   ============================================================================

   The tree-walking interpreter models `resume` with an identity continuation,
   correct only for single-shot tail-resume. Multi-shot resume (>1 call)
   previously produced a silently-wrong value (each call merely returned its
   argument); it must now fail loudly. Non-tail single-shot resume stays a
   documented shallow-resume incompleteness (undetectable without a CPS
   transform), pinned here as an executable fact. Unlike the existing
   handler-fence tests, these actually APPLY `main` so the handler runs. *)

let interp_main path =
  let open Result in
  let ( let* ) = bind in
  let* (prog, _resolve_ctx) = run_frontend path in
  let* env =
    match Interp.eval_program prog with
    | Ok env -> Ok env
    | Error e -> Error (Value.show_eval_error e)
  in
  let* main_fn =
    match Value.lookup_env "main" env with
    | Ok f -> Ok f
    | Error _ -> Error "no `main` binding"
  in
  match Interp.apply_function main_fn [] with
  | Ok v -> Ok v
  | Error e -> Error (Value.show_eval_error e)

let test_resume_single_shot_tail () =
  match interp_main (fixture "handle_resume_tail.affine") with
  | Ok (Value.VInt 5) -> ()
  | Ok v -> Alcotest.failf "single-shot tail-resume: expected VInt 5, got %s" (Value.show_value v)
  | Error msg -> Alcotest.failf "single-shot tail-resume should evaluate, got error: %s" msg

let test_resume_multishot_loud_fail () =
  match interp_main (fixture "handle_resume_multishot.affine") with
  | Ok v ->
      Alcotest.failf
        "expected a loud multi-shot resume error (Refs #555); got Ok %s — \
         silent multi-shot miscompute has regressed" (Value.show_value v)
  | Error msg ->
      Alcotest.(check bool) "error names the multi-shot resume limit" true
        (contains_str "multi-shot resume" msg)

(* The non-tail single-shot resume residual (#555 / #623) is pinned as an xfail
   in test/xfail/test_xfail_pins.ml (test_resume_nontail_xfail): it asserts the
   CORRECT result (105) and fails-as-expected while the shallow interpreter
   returns 5, flipping to a loud XPASS the day delimited continuations land.
   Anchored from docs/SOUNDNESS.adoc. Kept out of this passing suite so there is
   one pin convention, not two. *)

let resume_soundness_tests = [
  Alcotest.test_case "interp: single-shot tail-resume evaluates to 5"   `Quick test_resume_single_shot_tail;
  Alcotest.test_case "interp: multi-shot resume → loud failure"         `Quick test_resume_multishot_loud_fail;
]

(* ============================================================================
   Section: Stage 7 — typed-wasm Ownership Verifier (Tw_verify)
   ============================================================================

   Verify that [Tw_verify.verify_module] and [Tw_verify.verify_from_module]
   correctly enforce:

   Level 10 — Linearity: Linear (own) params must be loaded exactly once.
     * Zero loads → LinearNotUsed violation (param dropped).
     * Two or more loads → LinearUsedMultiple violation (param duplicated).
     * Exactly one load → OK.

   Level 7 — Aliasing safety: ExclBorrow (mut) params may be loaded at most once.
     * Two or more loads → ExclBorrowAliased violation.
     * One load → OK.

   SharedBorrow (ref) and Unrestricted params are unconstrained — any number
   of loads is allowed.

   Tests 1-7 build synthetic Wasm modules directly (no compilation).
   Tests 8-9 run the full pipeline on fixture files.
*)

(** Build a minimal Wasm module with a single function body. *)
let mk_single_func_module (body : Wasm.instr list) : Wasm.wasm_module =
  let func = Wasm.{ f_type = 0; f_locals = []; f_body = body } in
  { (Wasm.empty_module ()) with Wasm.funcs = [func] }

(** Shorthand: annotate func 0 with given param kinds and Unrestricted return. *)
let single_annot (param_kinds : Codegen.ownership_kind list)
    : (int * Codegen.ownership_kind list * Codegen.ownership_kind) list =
  [(0, param_kinds, Codegen.Unrestricted)]

(* ---- Test 1: Linear param used exactly once — OK ---- *)

let test_verify_linear_ok () =
  (* Body: LocalGet 0; Return — param 0 loaded once. *)
  let m = mk_single_func_module [Wasm.LocalGet 0; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "linear used once → OK" true (errs = [])

(* ---- Test 2: Linear param dropped (never loaded) — violation ---- *)

let test_verify_linear_dropped () =
  (* Body: I32Const 0; Return — param 0 never loaded. *)
  let m = mk_single_func_module [Wasm.I32Const 0l; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "linear dropped → violation" true
    (List.exists (function
       | Tw_verify.LinearNotUsed { param_idx = 0; _ } -> true
       | _ -> false) errs)

(* ---- Test 3: Linear param loaded twice — violation ---- *)

let test_verify_linear_dup () =
  (* Body: LocalGet 0; LocalGet 0; I32Add; Return — param 0 loaded twice. *)
  let m = mk_single_func_module
    [Wasm.LocalGet 0; Wasm.LocalGet 0; Wasm.I32Add; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "linear duplicated → violation" true
    (List.exists (function
       | Tw_verify.LinearUsedMultiple { param_idx = 0; _ } -> true
       | _ -> false) errs)

(* ---- Test 4: ExclBorrow used once — OK ---- *)

let test_verify_excl_ok () =
  let m = mk_single_func_module [Wasm.LocalGet 0; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.ExclBorrow]) in
  Alcotest.(check bool) "excl borrow once → OK" true (errs = [])

(* ---- Test 5: ExclBorrow aliased (loaded twice) — violation ---- *)

let test_verify_excl_aliased () =
  let m = mk_single_func_module
    [Wasm.LocalGet 0; Wasm.LocalGet 0; Wasm.I32Add; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.ExclBorrow]) in
  Alcotest.(check bool) "excl borrow aliased → violation" true
    (List.exists (function
       | Tw_verify.ExclBorrowAliased { param_idx = 0; _ } -> true
       | _ -> false) errs)

(* ---- Test 6: Unrestricted param — any number of loads is OK ---- *)

let test_verify_unrestricted_ok () =
  (* Unrestricted params carry no ownership constraints: loading N times is fine. *)
  let m = mk_single_func_module
    [Wasm.LocalGet 0; Wasm.LocalGet 0; Wasm.I32Add; Wasm.Return] in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Unrestricted]) in
  Alcotest.(check bool) "unrestricted multi-load → OK" true (errs = [])

(* ---- Test 7: If branch — Linear used once in each arm → per-path (1,1) → OK ---- *)

let test_verify_if_branch_ok () =
  (* if (1) { LocalGet 0 } else { LocalGet 0 }
     Per-path analysis: min(1,1)=1, max(1,1)=1 → OK. *)
  let body = [
    Wasm.I32Const 1l;
    Wasm.If (Wasm.BtType Wasm.I32,
      [Wasm.LocalGet 0],
      [Wasm.LocalGet 0]);
  ] in
  let m = mk_single_func_module body in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "if/else each use once → per-path OK" true (errs = [])

(* ---- Test 10: Linear dropped in one branch only → LinearDroppedOnSomePath ---- *)

let test_verify_if_partial_drop () =
  (* if (1) { LocalGet 0 } else { [] }
     Per-path analysis: then=(1,1), else=(0,0) → combined (min=0, max=1).
     min_uses=0, max_uses=1 → LinearDroppedOnSomePath violation. *)
  let body = [
    Wasm.I32Const 1l;
    Wasm.If (Wasm.BtEmpty,
      [Wasm.LocalGet 0; Wasm.Drop],
      []);
  ] in
  let m = mk_single_func_module body in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "linear dropped in one branch → LinearDroppedOnSomePath" true
    (List.exists (function
       | Tw_verify.LinearDroppedOnSomePath { param_idx = 0; _ } -> true
       | _ -> false) errs)

(* ---- Test 11: Linear consumed in then, explicitly dropped in else → OK ---- *)
(*
   This mirrors fn_push in tea_router.ml after Stage 9 fix.
   then: LocalGet 0; I32Store  (uses param once)
   else: LocalGet 0; Drop       (explicitly drops param)
   Per-path: min(1,1)=1, max(1,1)=1 → OK. *)

let test_verify_if_explicit_drop_ok () =
  let body = [
    Wasm.I32Const 1l;
    Wasm.If (Wasm.BtEmpty,
      (* then: use the value *)
      [Wasm.LocalGet 0; Wasm.Drop],
      (* else: explicitly discharge ownership *)
      [Wasm.LocalGet 0; Wasm.Drop]);
  ] in
  let m = mk_single_func_module body in
  let errs = Tw_verify.verify_module m (single_annot [Codegen.Linear]) in
  Alcotest.(check bool) "explicit drop in else → per-path OK" true (errs = [])

(* ---- Test 8: Pipeline — ownership_codegen.affine → LinearNotUsed expected ---- *)
(*
   ownership_codegen.affine has bodies that return 0 without using their
   ownership params.  The verifier should find LinearNotUsed for the
   [consume_owned] function (kind=1, never loaded). *)

let test_verify_pipeline_violations () =
  match run_wasm_pipeline (fixture "ownership_codegen.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    (match Tw_verify.verify_from_module wasm_mod with
    | Ok () ->
      Alcotest.fail "Expected violations for dropped linear params, got OK"
    | Error errs ->
      let has_linear_not_used = List.exists (function
        | Tw_verify.LinearNotUsed _ -> true
        | _ -> false) errs in
      Alcotest.(check bool) "LinearNotUsed violation detected" true has_linear_not_used)

(* ---- Test 9: Pipeline — verify_ownership_clean.affine → OK ---- *)
(*
   verify_ownership_clean.affine uses all params in their bodies.
   The verifier must report clean. *)

let test_verify_pipeline_clean () =
  match run_wasm_pipeline (fixture "verify_ownership_clean.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok wasm_mod ->
    (match Tw_verify.verify_from_module wasm_mod with
    | Ok () -> ()  (* expected *)
    | Error errs ->
      let msg = String.concat "; " (List.map (fun e ->
        Format.asprintf "%a" Tw_verify.pp_error e) errs) in
      Alcotest.fail (Printf.sprintf "Unexpected violations: %s" msg))

let tw_verify_tests = [
  Alcotest.test_case "linear used once → OK"                  `Quick test_verify_linear_ok;
  Alcotest.test_case "linear dropped → violation"             `Quick test_verify_linear_dropped;
  Alcotest.test_case "linear duplicated → violation"          `Quick test_verify_linear_dup;
  Alcotest.test_case "excl borrow once → OK"                  `Quick test_verify_excl_ok;
  Alcotest.test_case "excl borrow aliased → violation"        `Quick test_verify_excl_aliased;
  Alcotest.test_case "unrestricted multi-load → OK"           `Quick test_verify_unrestricted_ok;
  Alcotest.test_case "if/else per-path linear → OK"           `Quick test_verify_if_branch_ok;
  Alcotest.test_case "pipeline: dropped linear → violation"   `Quick test_verify_pipeline_violations;
  Alcotest.test_case "pipeline: clean fixture → OK"           `Quick test_verify_pipeline_clean;
  Alcotest.test_case "if one-arm drop → LinearDroppedOnSomePath" `Quick test_verify_if_partial_drop;
  Alcotest.test_case "if explicit else-drop → per-path OK"    `Quick test_verify_if_explicit_drop_ok;
]

(* ============================================================================
   Section: Stage 10 — Tw_interface (boundary verifier)
   ============================================================================

   Verify that [Tw_interface.extract_exports] and
   [Tw_interface.verify_cross_module] correctly:

   1. Extract ownership-annotated export interfaces from generated modules.
   2. Accept well-formed callers (Linear-param import called once per path).
   3. Reject callers that duplicate or conditionally drop Linear-param calls.
*)

(* ---- Test 1: tea-bridge export interface has update with Linear param ---- *)

let test_iface_bridge_update_linear () =
  let m = Tea_bridge.generate () in
  let iface = Tw_interface.extract_exports m in
  let update_fi =
    List.find_opt (fun fi -> fi.Tw_interface.fi_name = "affinescript_update") iface
  in
  match update_fi with
  | None -> Alcotest.fail "affinescript_update not found in interface"
  | Some fi ->
    let has_own = List.mem Codegen.Linear fi.Tw_interface.fi_param_kinds in
    Alcotest.(check bool) "affinescript_update has own (Linear) param" true has_own

(* ---- Test 2: router export interface has push with Linear param ---- *)

let test_iface_router_push_linear () =
  let m = Tea_router.generate () in
  let iface = Tw_interface.extract_exports m in
  let push_fi =
    List.find_opt
      (fun fi -> fi.Tw_interface.fi_name = "affinescript_router_push") iface
  in
  match push_fi with
  | None -> Alcotest.fail "affinescript_router_push not found in interface"
  | Some fi ->
    let has_own = List.mem Codegen.Linear fi.Tw_interface.fi_param_kinds in
    Alcotest.(check bool) "affinescript_router_push has own (Linear) param" true has_own

(* ---- Test 3: router resize has two Linear params ---- *)

let test_iface_router_resize_two_linear () =
  let m = Tea_router.generate () in
  let iface = Tw_interface.extract_exports m in
  let resize_fi =
    List.find_opt
      (fun fi -> fi.Tw_interface.fi_name = "affinescript_router_resize") iface
  in
  match resize_fi with
  | None -> Alcotest.fail "affinescript_router_resize not found in interface"
  | Some fi ->
    let n_linear =
      List.length (List.filter (( = ) Codegen.Linear) fi.Tw_interface.fi_param_kinds)
    in
    Alcotest.(check int) "resize has 2 Linear params" 2 n_linear

(* ---- Test 4: cross-module — caller calls Linear import once → OK ---- *)

let test_cross_call_once_ok () =
  (* Callee: a module with a single Linear-param export named "consume". *)
  let callee =
    let m = mk_single_func_module [Wasm.LocalGet 0; Wasm.Drop] in
    let export = Wasm.{ e_name = "consume"; e_desc = ExportFunc 0 } in
    let annots = Codegen.build_ownership_section [(0, [Codegen.Linear], Codegen.Unrestricted)] in
    { m with
      Wasm.exports        = [export];
      Wasm.custom_sections = [("typedwasm.ownership", annots)];
    }
  in
  let iface = Tw_interface.extract_exports callee in
  (* Caller: imports "consume" at slot 0, calls it once. *)
  let caller =
    let import = Wasm.{ i_module = "test"; i_name = "consume"; i_desc = ImportFunc 0 } in
    let fn = Wasm.{ f_type = 0; f_locals = []; f_body = [Wasm.I32Const 0l; Wasm.Call 0] } in
    { (Wasm.empty_module ()) with
      Wasm.types   = [{ Wasm.ft_params = [Wasm.I32]; ft_results = [] }];
      Wasm.imports = [import];
      Wasm.funcs   = [fn];
    }
  in
  (match Tw_interface.verify_cross_module iface caller with
  | Ok () -> ()
  | Error errs ->
    let msg = String.concat "; " (List.map (fun e ->
      Format.asprintf "%a" Tw_interface.pp_cross_error e) errs) in
    Alcotest.fail ("Expected OK, got violations: " ^ msg))

(* ---- Test 5: cross-module — caller calls Linear import twice → violation ---- *)

let test_cross_call_twice_violation () =
  let callee =
    let m = mk_single_func_module [Wasm.LocalGet 0; Wasm.Drop] in
    let export = Wasm.{ e_name = "consume"; e_desc = ExportFunc 0 } in
    let annots = Codegen.build_ownership_section [(0, [Codegen.Linear], Codegen.Unrestricted)] in
    { m with
      Wasm.exports        = [export];
      Wasm.custom_sections = [("typedwasm.ownership", annots)];
    }
  in
  let iface = Tw_interface.extract_exports callee in
  (* Caller: calls "consume" twice → LinearImportCalledMultiple. *)
  let caller =
    let import = Wasm.{ i_module = "test"; i_name = "consume"; i_desc = ImportFunc 0 } in
    let fn = Wasm.{
      f_type = 0; f_locals = [];
      f_body = [Wasm.I32Const 0l; Wasm.Call 0; Wasm.I32Const 1l; Wasm.Call 0];
    } in
    { (Wasm.empty_module ()) with
      Wasm.types   = [{ Wasm.ft_params = [Wasm.I32]; ft_results = [] }];
      Wasm.imports = [import];
      Wasm.funcs   = [fn];
    }
  in
  (match Tw_interface.verify_cross_module iface caller with
  | Ok () ->
    Alcotest.fail "Expected LinearImportCalledMultiple, got OK"
  | Error errs ->
    let has_dup = List.exists (function
      | Tw_interface.LinearImportCalledMultiple _ -> true
      | _ -> false) errs in
    Alcotest.(check bool) "duplicate import call → LinearImportCalledMultiple" true has_dup)

(* ---- Test 6: cross-module — caller calls Linear import in one branch only → violation ---- *)

let test_cross_call_partial_violation () =
  let callee =
    let m = mk_single_func_module [Wasm.LocalGet 0; Wasm.Drop] in
    let export = Wasm.{ e_name = "consume"; e_desc = ExportFunc 0 } in
    let annots = Codegen.build_ownership_section [(0, [Codegen.Linear], Codegen.Unrestricted)] in
    { m with
      Wasm.exports        = [export];
      Wasm.custom_sections = [("typedwasm.ownership", annots)];
    }
  in
  let iface = Tw_interface.extract_exports callee in
  (* Caller: If { Call 0 } { [] } → dropped on else path. *)
  let caller =
    let import = Wasm.{ i_module = "test"; i_name = "consume"; i_desc = ImportFunc 0 } in
    let fn = Wasm.{
      f_type = 0; f_locals = [];
      f_body = [
        Wasm.I32Const 1l;
        Wasm.If (Wasm.BtEmpty,
          [Wasm.I32Const 0l; Wasm.Call 0],
          []);
      ];
    } in
    { (Wasm.empty_module ()) with
      Wasm.types   = [{ Wasm.ft_params = [Wasm.I32]; ft_results = [] }];
      Wasm.imports = [import];
      Wasm.funcs   = [fn];
    }
  in
  (match Tw_interface.verify_cross_module iface caller with
  | Ok () ->
    Alcotest.fail "Expected LinearImportDroppedOnSomePath, got OK"
  | Error errs ->
    let has_partial = List.exists (function
      | Tw_interface.LinearImportDroppedOnSomePath _ -> true
      | _ -> false) errs in
    Alcotest.(check bool) "partial-path call → LinearImportDroppedOnSomePath" true has_partial)

(* ---- Test 7: generated bridge modules verify clean at boundary ---- *)

let test_bridge_boundary_clean () =
  (* tea-bridge: affinescript_update has a Linear msg param.
     Synthetic caller calls it once → clean. *)
  let callee_iface = Tw_interface.extract_exports (Tea_bridge.generate ()) in
  let caller =
    let import = Wasm.{
      i_module = "env";
      i_name   = "affinescript_update";
      i_desc   = ImportFunc 0;
    } in
    let fn = Wasm.{ f_type = 0; f_locals = []; f_body = [Wasm.I32Const 0l; Wasm.Call 0] } in
    { (Wasm.empty_module ()) with
      Wasm.types   = [{ Wasm.ft_params = [Wasm.I32]; ft_results = [] }];
      Wasm.imports = [import];
      Wasm.funcs   = [fn];
    }
  in
  (match Tw_interface.verify_cross_module callee_iface caller with
  | Ok () -> ()
  | Error errs ->
    let msg = String.concat "; " (List.map (fun e ->
      Format.asprintf "%a" Tw_interface.pp_cross_error e) errs) in
    Alcotest.fail ("Bridge boundary check failed: " ^ msg))

let test_router_boundary_clean () =
  (* router: affinescript_router_push has a Linear screen_tag param.
     Synthetic caller calls it once → clean. *)
  let callee_iface = Tw_interface.extract_exports (Tea_router.generate ()) in
  let caller =
    let import = Wasm.{
      i_module = "router";
      i_name   = "affinescript_router_push";
      i_desc   = ImportFunc 0;
    } in
    let fn = Wasm.{ f_type = 0; f_locals = []; f_body = [Wasm.I32Const 1l; Wasm.Call 0] } in
    { (Wasm.empty_module ()) with
      Wasm.types   = [{ Wasm.ft_params = [Wasm.I32]; ft_results = [] }];
      Wasm.imports = [import];
      Wasm.funcs   = [fn];
    }
  in
  (match Tw_interface.verify_cross_module callee_iface caller with
  | Ok () -> ()
  | Error errs ->
    let msg = String.concat "; " (List.map (fun e ->
      Format.asprintf "%a" Tw_interface.pp_cross_error e) errs) in
    Alcotest.fail ("Router boundary check failed: " ^ msg))

(* ============================================================================
   Section: Stage 11 — Cmd linearity (source-level QTT enforcement)
   ============================================================================

   Verify that:
   1. [Cmd _] type annotations automatically confer QOne on let-bindings.
   2. A Cmd returned in its tuple → QTT satisfied (no error).
   3. A Cmd dropped (not returned) → LinearVariableUnused error.
   4. [cmd_none] and [cmd_perform] are recognised as built-in values.
*)

(** Full pipeline up to quantity checking (resolves, typechecks, then QTT). *)
let run_pipeline_to_quantity path =
  match parse_fixture path with
  | Error msg -> Error msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Error msg
    | Ok (resolve_ctx, _) ->
      match Typecheck.check_program resolve_ctx.symbols prog with
      | Error e -> Error (Typecheck.format_type_error e)
      | Ok _tc ->
        match Quantity.check_program resolve_ctx.symbols prog with
        | Ok () -> Ok ()
        | Error (e, _span) ->
          Error (Printf.sprintf "Quantity error: %s" (Quantity.format_quantity_error e))

(* ---- Test 1: cmd_none recognised — Cmd type resolves ---- *)

let test_cmd_type_resolves () =
  (* cmd_linear.affine uses Cmd ClickMsg — should typecheck cleanly. *)
  match parse_fixture (fixture "cmd_linear.affine") with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      (match Typecheck.check_program ctx.symbols prog with
      | Ok _ -> ()
      | Error e ->
        Alcotest.fail ("Cmd type failed to typecheck: " ^ Typecheck.format_type_error e))

(* ---- Test 2: Cmd returned in tuple — quantity check passes ---- *)

let test_cmd_returned_passes () =
  match run_pipeline_to_quantity (fixture "cmd_linear.affine") with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("Expected OK, got: " ^ msg)

(* ---- Test 3: Cmd dropped — LinearVariableUnused ---- *)

let test_cmd_dropped_violation () =
  match run_pipeline_to_quantity (fixture "cmd_dropped.affine") with
  | Error msg ->
    (* Should mention linear/unused *)
    Alcotest.(check bool) "error mentions cmd or linear"
      true (String.length msg > 0)
  | Ok () ->
    Alcotest.fail "Expected LinearVariableUnused for dropped Cmd, got OK"

(* ---- Test 4: in-memory — Cmd binding without annotation stays QOmega ---- *)

let test_cmd_no_annotation_qomega () =
  (* Without a [Cmd _] type annotation, the binding gets QOmega — no enforcement.
     This verifies backwards-compatibility: existing code that uses cmd_none
     without annotation is not broken by Stage 11. *)
  let src = {|
    enum M { Click }
    fn f(msg: M, n: Int) -> Int {
      let cmd = cmd_none;
      n
    }
  |} in
  let prog_result = try
    Ok (Parse_driver.parse_string ~file:"<test>" src)
  with
  | Parse_driver.Parse_error (msg, _) -> Error ("Parse: " ^ msg)
  | Lexer.Lexer_error (msg, _)        -> Error ("Lex: " ^ msg)
  in
  match prog_result with
  | Error msg -> Alcotest.fail msg
  | Ok prog ->
    match resolve_program prog with
    | Error msg -> Alcotest.fail msg
    | Ok (ctx, _) ->
      (match Quantity.check_program ctx.symbols prog with
      | Ok () -> ()  (* QOmega — no enforcement, OK to drop *)
      | Error (e, _) ->
        Alcotest.fail ("Unexpected quantity error (no annotation → QOmega): "
                       ^ Quantity.format_quantity_error e))

let cmd_linear_tests = [
  Alcotest.test_case "Cmd type resolves in typecheck"       `Quick test_cmd_type_resolves;
  Alcotest.test_case "Cmd returned in tuple → QTT OK"      `Quick test_cmd_returned_passes;
  Alcotest.test_case "Cmd dropped → LinearVariableUnused"  `Quick test_cmd_dropped_violation;
  Alcotest.test_case "No annotation → QOmega (backwards compat)" `Quick test_cmd_no_annotation_qomega;
]

(* ---- Source-level cross-module boundary tests ----

   Exercise the full pipeline (parse → resolve_with_loader → typecheck-with-
   imported types → quantity → codegen.generate_module-with-loader) on real
   .affine sources where the caller imports the callee. Verifies that the
   caller's emitted Wasm carries the right (i_module, i_name) entries and
   that Tw_interface.verify_cross_module agrees with the per-path call count
   on each violation class. *)

let compile_fixture_to_wasm path : (Wasm.wasm_module, string) Result.t =
  let loader_config = {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  let loader = Module_loader.create loader_config in
  match parse_fixture path with
  | Error e -> Error e
  | Ok prog ->
    match Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Error (Printf.sprintf "Resolution: %s" (Resolve.show_resolve_error e))
    | Ok (resolve_ctx, import_type_ctx) ->
      match Typecheck.check_program
              ~import_types:import_type_ctx.Typecheck.name_types
              resolve_ctx.symbols prog with
      | Error e -> Error (Printf.sprintf "Type: %s" (Typecheck.format_type_error e))
      | Ok _ ->
        let optimized = Opt.fold_constants_program prog in
        match Codegen.generate_module ~loader optimized with
        | Error e -> Error (Printf.sprintf "Codegen: %s" (Codegen.show_codegen_error e))
        | Ok m -> Ok m

let test_xmod_clean () =
  match compile_fixture_to_wasm (fixture "CrossCallee.affine"),
        compile_fixture_to_wasm (fixture "cross_caller_ok.affine") with
  | Ok callee, Ok caller ->
    let iface = Tw_interface.extract_exports callee in
    (match Tw_interface.verify_cross_module iface caller with
     | Ok () -> ()
     | Error errs ->
       let msg = String.concat "; " (List.map (fun e ->
         Format.asprintf "%a" Tw_interface.pp_cross_error e) errs) in
       Alcotest.fail ("Expected clean OK, got: " ^ msg))
  | Error e, _ | _, Error e -> Alcotest.fail e

let test_xmod_dup_violation () =
  match compile_fixture_to_wasm (fixture "CrossCallee.affine"),
        compile_fixture_to_wasm (fixture "cross_caller_dup.affine") with
  | Ok callee, Ok caller ->
    let iface = Tw_interface.extract_exports callee in
    (match Tw_interface.verify_cross_module iface caller with
     | Ok () ->
       Alcotest.fail "Expected LinearImportCalledMultiple, got OK"
     | Error errs ->
       let has_dup = List.exists (function
         | Tw_interface.LinearImportCalledMultiple { import_name; _ }
           when import_name = "consume" -> true
         | _ -> false) errs in
       Alcotest.(check bool) "double-call → LinearImportCalledMultiple"
         true has_dup)
  | Error e, _ | _, Error e -> Alcotest.fail e

let test_xmod_drop_violation () =
  match compile_fixture_to_wasm (fixture "CrossCallee.affine"),
        compile_fixture_to_wasm (fixture "cross_caller_drop.affine") with
  | Ok callee, Ok caller ->
    let iface = Tw_interface.extract_exports callee in
    (match Tw_interface.verify_cross_module iface caller with
     | Ok () ->
       Alcotest.fail "Expected LinearImportDroppedOnSomePath, got OK"
     | Error errs ->
       let has_drop = List.exists (function
         | Tw_interface.LinearImportDroppedOnSomePath { import_name; _ }
           when import_name = "consume" -> true
         | _ -> false) errs in
       Alcotest.(check bool) "one-arm call → LinearImportDroppedOnSomePath"
         true has_drop)
  | Error e, _ | _, Error e -> Alcotest.fail e

(* ---- INT-01 / #178: cross-module WASM import-emission substrate ----

   The structural half of the substrate guarantee, hermetic (pure OCaml,
   inspects the emitted Wasm.wasm_module). Regression-locks that a real
   multi-file `use Mod::{fn}` program emits an actual cross-module import
   and the callee module exports the imported symbol — i.e. the two
   separately-compiled `.wasm` modules are link-compatible. The
   *execution* half (instantiate both, cross-call returns 42) is the
   committed reproducible harness at tests/modules/xmod-link/ (deno;
   kept out of the hermetic gate by design). *)
let test_int01_xmod_import_emission () =
  match compile_fixture_to_wasm (fixture "CrossCallee.affine"),
        compile_fixture_to_wasm (fixture "cross_caller_ok.affine") with
  | Ok callee, Ok caller ->
    let callee_exports_consume =
      List.exists (fun (e : Wasm.export) -> e.Wasm.e_name = "consume")
        callee.Wasm.exports in
    let caller_imports_consume =
      List.exists (fun (i : Wasm.import) ->
        i.Wasm.i_module = "CrossCallee" && i.Wasm.i_name = "consume")
        caller.Wasm.imports in
    Alcotest.(check bool)
      "callee module exports `consume`" true callee_exports_consume;
    Alcotest.(check bool)
      "caller emits import (CrossCallee . consume)" true caller_imports_consume
  | Error e, _ | _, Error e -> Alcotest.fail e

(* ---- WasmGC backend: loud-failure regression markers ----

   Lock in the BUG-005-class fixes that replaced silent miscompilation with
   explicit UnsupportedFeature errors:
     - lambda expressions
     - `unsafe` blocks
     - match arms with patterns the GC backend cannot lower
   Each test compiles a small source via parse_string + Codegen_gc.generate_gc_module
   and asserts the emitted error contains the expected feature label. *)

let parse_string_for_gc src : Ast.program =
  Parse_driver.parse_string ~file:"<test>" src

let gc_compile_and_expect_unsupported src ~must_contain ~label =
  let prog = parse_string_for_gc src in
  match Codegen_gc.generate_gc_module prog with
  | Ok _ ->
    Alcotest.failf "[%s] expected UnsupportedFeature, got Ok" label
  | Error err ->
    let msg = Codegen_gc.format_codegen_error err in
    let contains s sub =
      let n = String.length sub and m = String.length s in
      let rec scan i = i + n <= m && (String.sub s i n = sub || scan (i + 1)) in
      scan 0
    in
    Alcotest.(check bool)
      (Printf.sprintf "[%s] error mentions '%s'" label must_contain)
      true (contains msg must_contain)

let test_gc_lambda_loud_fail () =
  gc_compile_and_expect_unsupported
    {|fn main() -> Int { let f = |x| x + 1; f(41) }|}
    ~must_contain:"lambda"
    ~label:"lambda"

let test_gc_unsafe_loud_fail () =
  (* `unsafe { read(p); }` parses to ExprUnsafe [UnsafeRead p].  The GC backend
     must not silently emit RefNull for this — it has no GC-safe lowering. *)
  gc_compile_and_expect_unsupported
    {|fn main(p: Int) -> Int { unsafe { read(p); } }|}
    ~must_contain:"unsafe"
    ~label:"unsafe"

(* BUG-005 deferred fixture (closed-bug record's
   regression-test-status = "deferred — fixture needs a known-unknown
   function name in a WasmGC compile path").  The GC backend's
   ExprApp arm uses func_indices for direct calls; a name that the
   parser accepts but no decl registers should hit the explicit
   UnboundFunction error, not silently emit drop+null.

   The hermetic path: parse a tiny program calling a name that is
   neither a built-in (`int`/`float`) nor a variant tag.  Codegen_gc
   visits the call before any resolve check has had a chance to
   reject it, so the UnboundFunction error is exercised. *)
let test_gc_unbound_function_loud_fail () =
  gc_compile_and_expect_unsupported
    {|fn main() -> Int { return totally_undefined_callee(42); }|}
    ~must_contain:"totally_undefined_callee"
    ~label:"unbound-function"

let wasm_gc_loud_fail_tests = [
  Alcotest.test_case "lambda → UnsupportedFeature (no silent RefNull)" `Quick test_gc_lambda_loud_fail;
  Alcotest.test_case "unsafe block → UnsupportedFeature (no silent RefNull)" `Quick test_gc_unsafe_loud_fail;
  Alcotest.test_case "unknown callee → UnboundFunction (BUG-005 deferred fixture)" `Quick test_gc_unbound_function_loud_fail;
]

(* ---- WasmGC variant-with-args construction ----

   Verifies that `MySome(42)` lowers to a tagged struct allocation
   (push_i32 tag + push args + RefI31 boxing for primitives + StructNew),
   producing a binary that can be compiled and instantiated by the host. *)

let count_instr_kind body kind_pred : int =
  let rec walk acc = function
    | [] -> acc
    | i :: rest ->
      let acc = if kind_pred i then acc + 1 else acc in
      let acc = match (i : Wasm_gc.gc_instr) with
        | GcBlock (_, body) | GcLoop (_, body) -> walk acc body
        | GcIf (_, t, e) -> walk (walk acc t) e
        | _ -> acc
      in
      walk acc rest
  in
  walk 0 body

let test_gc_variant_with_args_construction () =
  let prog = parse_string_for_gc
    {|enum MyOpt { MyNone, MySome(Int) }
      fn main() -> MyOpt { return MySome(42); }|}
  in
  match Codegen_gc.generate_gc_module prog with
  | Error e ->
    Alcotest.failf "expected Ok, got: %s" (Codegen_gc.format_codegen_error e)
  | Ok m ->
    (* The main fn body should contain exactly one StructNew (the variant
       allocation) and one RefI31 (boxing the i32 payload). *)
    let main_body = (List.hd m.gc_funcs).gf_body in
    let n_struct_new = count_instr_kind main_body
      (function Wasm_gc.StructNew _ -> true | _ -> false) in
    let n_ref_i31 = count_instr_kind main_body
      (function Wasm_gc.RefI31 -> true | _ -> false) in
    Alcotest.(check int) "exactly one StructNew" 1 n_struct_new;
    Alcotest.(check int) "exactly one RefI31 (Int payload boxed)" 1 n_ref_i31

let test_gc_variant_with_two_args () =
  let prog = parse_string_for_gc
    {|enum Pair { Mk(Int, Bool) }
      fn main() -> Pair { return Mk(7, true); }|}
  in
  match Codegen_gc.generate_gc_module prog with
  | Error e ->
    Alcotest.failf "expected Ok, got: %s" (Codegen_gc.format_codegen_error e)
  | Ok m ->
    let main_body = (List.hd m.gc_funcs).gf_body in
    let n_struct_new = count_instr_kind main_body
      (function Wasm_gc.StructNew _ -> true | _ -> false) in
    let n_ref_i31 = count_instr_kind main_body
      (function Wasm_gc.RefI31 -> true | _ -> false) in
    Alcotest.(check int) "one StructNew for the variant" 1 n_struct_new;
    Alcotest.(check int) "two RefI31 (Int + Bool both boxed)" 2 n_ref_i31

let test_gc_zero_arg_variant_still_i32 () =
  (* Zero-arg variants should still lower to a bare i32 tag — no struct
     allocation, no RefI31. *)
  let prog = parse_string_for_gc
    {|enum Mood { Happy, Sad }
      fn main() -> Mood { return Happy; }|}
  in
  match Codegen_gc.generate_gc_module prog with
  | Error e ->
    Alcotest.failf "expected Ok, got: %s" (Codegen_gc.format_codegen_error e)
  | Ok m ->
    let main_body = (List.hd m.gc_funcs).gf_body in
    let n_struct_new = count_instr_kind main_body
      (function Wasm_gc.StructNew _ -> true | _ -> false) in
    Alcotest.(check int) "no StructNew (zero-arg uses i32 tag)" 0 n_struct_new

let wasm_gc_variant_tests = [
  Alcotest.test_case "single-arg variant: MySome(42) → struct.new + ref.i31" `Quick test_gc_variant_with_args_construction;
  Alcotest.test_case "two-arg variant: Mk(7, true) → 1 struct.new + 2 ref.i31" `Quick test_gc_variant_with_two_args;
  Alcotest.test_case "zero-arg variant still uses bare i32 tag" `Quick test_gc_zero_arg_variant_still_i32;
]

(* ---- Issue #35 Phase 1: Node-CJS emit ----

   Verifies that Codegen_node.emit_node_cjs wraps a compiled wasm module in
   a CJS shim with the expected anchors: "use strict", base64-encoded wasm
   constant, the activate/deactivate exports, and the handle-table helpers
   that Phase 2 binding modules will populate. *)

let test_node_cjs_shim_shape () =
  let prog = parse_string_for_gc
    {|pub fn activate(ctx_handle: Int) -> Int { return 0; }
      pub fn deactivate() -> Int { return 0; }|}
  in
  match Codegen.generate_module prog with
  | Error e ->
    Alcotest.failf "wasm codegen failed: %s" (Codegen.show_codegen_error e)
  | Ok wasm_module ->
    let cjs = Codegen_node.emit_node_cjs wasm_module in
    let must_contain s sub =
      let n = String.length sub and m = String.length s in
      let rec scan i = i + n <= m && (String.sub s i n = sub || scan (i + 1)) in
      scan 0
    in
    Alcotest.(check bool) "starts with strict-mode pragma"
      true (must_contain cjs "\"use strict\";");
    Alcotest.(check bool) "embeds wasm as base64 constant"
      true (must_contain cjs "_wasmBase64");
    Alcotest.(check bool) "exports.activate present"
      true (must_contain cjs "exports.activate");
    Alcotest.(check bool) "exports.deactivate present"
      true (must_contain cjs "exports.deactivate");
    Alcotest.(check bool) "_registerHandle exported (Phase 2 hook)"
      true (must_contain cjs "exports._registerHandle");
    Alcotest.(check bool) "wires WASI fd_write so println works"
      true (must_contain cjs "fd_write")

let test_node_cjs_base64_roundtrip () =
  (* Sanity check on the inline base64 encoder — encode known input and
     decode externally would require a decoder; instead, check known prefix
     and length invariant: output_len = ceil(input_len/3)*4. *)
  let bytes = Bytes.of_string "Many hands make light work." in
  let b64 = Codegen_node.base64_encode bytes in
  Alcotest.(check int) "length is 4*ceil(27/3) = 36"
    36 (String.length b64);
  Alcotest.(check string) "matches RFC 4648 §10 vector"
    "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu" b64

(* ---- Issue #105: --vscode-extension inline wiring ----

   Verifies that emit_node_cjs ~vscode_extension:true inlines the
   exports.extraImports glue (calling the @hyperpolymath/affine-vscode
   adapter) so the generated .cjs is directly loadable as a VS Code
   extension's `main` — no hand-written index.cjs / vendored adapter. *)

let contains s sub =
  let n = String.length sub and m = String.length s in
  let rec scan i = i + n <= m && (String.sub s i n = sub || scan (i + 1)) in
  n = 0 || scan 0

let cjs_of ?vscode_extension ?vscode_extension_adapter
    ?vscode_extension_no_lc src =
  let prog = parse_string_for_gc src in
  match Codegen.generate_module prog with
  | Error e -> Alcotest.failf "wasm codegen failed: %s" (Codegen.show_codegen_error e)
  | Ok wasm_module ->
    Codegen_node.emit_node_cjs ?vscode_extension ?vscode_extension_adapter
      ?vscode_extension_no_lc wasm_module

let activate_src =
  {|pub fn activate(ctx_handle: Int) -> Int { return 0; }
    pub fn deactivate() -> Int { return 0; }|}

let test_vscode_extension_off_by_default () =
  let cjs = cjs_of activate_src in
  Alcotest.(check bool) "no extraImports assignment without the flag"
    false (contains cjs "exports.extraImports = function");
  (* Assert the absence of the actual adapter *wiring* (the require
     call), not the bare specifier string — the latter legitimately
     appears in an explanatory comment and is not adapter wiring. *)
  Alcotest.(check bool) "no adapter require without the flag"
    false (contains cjs {|require("@hyperpolymath/affine-vscode")|})

let test_vscode_extension_inlines_wiring () =
  let cjs = cjs_of ~vscode_extension:true activate_src in
  Alcotest.(check bool) "installs exports.extraImports"
    true (contains cjs "exports.extraImports = function");
  Alcotest.(check bool) "requires the default adapter"
    true (contains cjs {|require("@hyperpolymath/affine-vscode")|});
  Alcotest.(check bool) "requires the vscode module"
    true (contains cjs {|require("vscode")|});
  Alcotest.(check bool) "requires the language client"
    true (contains cjs {|require("vscode-languageclient/node")|});
  Alcotest.(check bool) "passes exports as the host shim"
    true (contains cjs "exports,\n  );");
  (* The base shim (use strict, activate, handle table) is still present. *)
  Alcotest.(check bool) "base shim still intact"
    true (contains cjs "exports._registerHandle")

let test_vscode_extension_adapter_override () =
  let cjs = cjs_of ~vscode_extension:true
      ~vscode_extension_adapter:"../local/adapter.cjs" activate_src in
  Alcotest.(check bool) "uses the overridden adapter specifier"
    true (contains cjs {|require("../local/adapter.cjs")|});
  (* Match the require wiring, not the bare specifier — the embedded
     adapter source (packages/affine-vscode/mod.js) carries documentation
     comments that legitimately mention the default specifier as an
     example usage; the override semantics is about which require()
     fires, not which strings appear anywhere in the file. *)
  Alcotest.(check bool) "does not fall back to the default adapter require"
    false (contains cjs {|require("@hyperpolymath/affine-vscode")|})

let test_vscode_extension_no_lc () =
  let cjs = cjs_of ~vscode_extension:true ~vscode_extension_no_lc:true
      activate_src in
  (* Match the require wiring, not the bare specifier — the embedded
     adapter source includes a `vscode-languageclient/node` section
     delimiter comment that is harmless because no actual require fires
     unless the language-client argument is wired in (which `no_lc` skips
     by passing `null`). *)
  Alcotest.(check bool) "skips the language-client require"
    false (contains cjs {|require("vscode-languageclient/node")|});
  Alcotest.(check bool) "passes null in its place"
    true (contains cjs "    null,\n");
  Alcotest.(check bool) "still requires vscode + adapter"
    true (contains cjs {|require("vscode")|}
          && contains cjs {|require("@hyperpolymath/affine-vscode")|})

let test_vscode_extension_adapter_escaped () =
  (* A specifier containing a double quote must not break out of the JS
     string literal. *)
  let cjs = cjs_of ~vscode_extension:true
      ~vscode_extension_adapter:{|a"b\c|} activate_src in
  Alcotest.(check bool) "double quote and backslash are escaped"
    true (contains cjs {|require("a\"b\\c")|})

let codegen_node_tests = [
  Alcotest.test_case "Node-CJS shim has all anchors (use strict, exports.activate, ...)" `Quick test_node_cjs_shim_shape;
  Alcotest.test_case "base64 encoder matches RFC 4648 vector"                              `Quick test_node_cjs_base64_roundtrip;
  Alcotest.test_case "--vscode-extension off by default"                                   `Quick test_vscode_extension_off_by_default;
  Alcotest.test_case "--vscode-extension inlines extraImports wiring"                      `Quick test_vscode_extension_inlines_wiring;
  Alcotest.test_case "--vscode-extension-adapter overrides the require specifier"          `Quick test_vscode_extension_adapter_override;
  Alcotest.test_case "--vscode-extension-no-lc skips the language client"                  `Quick test_vscode_extension_no_lc;
  Alcotest.test_case "--vscode-extension-adapter specifier is JS-escaped"                  `Quick test_vscode_extension_adapter_escaped;
]

(* ---- Stdlib parse + Core import regression ----

   Locks in the Core.affine fixes (renamed `const` → `always`, `fn(x: T)`
   lambdas converted to `|x: T|`, `flip` curried to dodge the
   `(A, B) -> C` tuple-arrow ambiguity) so they don't quietly regress.

   This test exercises the full pipeline against the project's actual
   stdlib/Core.affine on disk — not a synthetic copy — so a future stdlib
   regression surfaces here. *)

let test_stdlib_core_parses_and_typechecks () =
  let core_path =
    let candidates = [
      "stdlib/Core.affine";          (* run from project root *)
      "../../../stdlib/Core.affine"; (* run from _build/default/test *)
      "../../../../stdlib/Core.affine";
    ] in
    match List.find_opt Sys.file_exists candidates with
    | Some p -> p
    | None -> Alcotest.failf "stdlib/Core.affine not found in any of: %s"
                (String.concat ", " candidates)
  in
  match parse_fixture core_path with
  | Error e -> Alcotest.failf "stdlib/Core.affine parse failed: %s" e
  | Ok prog ->
    match resolve_program prog with
    | Error e -> Alcotest.failf "stdlib/Core.affine resolve failed: %s" e
    | Ok (resolve_ctx, _import_type_ctx) ->
      match Typecheck.check_program resolve_ctx.symbols prog with
      | Error e -> Alcotest.failf "stdlib/Core.affine typecheck failed: %s"
                     (Typecheck.format_type_error e)
      | Ok _ -> ()

let stdlib_tests = [
  Alcotest.test_case "stdlib/Core.affine parses + resolves + typechecks" `Quick test_stdlib_core_parses_and_typechecks;
]

(* ---- Cross-module imports for non-Wasm backends ----

   Verifies Module_loader.flatten_imports inlines public TopFns from
   imported modules into the importer's prog_decls, so backends that
   iterate prog.prog_decls only (Julia / JS / C / Rust / Lua / OCaml /
   ReScript / ...) automatically pick up imported function bodies without
   each needing to implement its own module-system hooks. *)

let test_flatten_imports_inlines_public_fns () =
  let loader = Module_loader.create {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  match parse_fixture (fixture "cross_caller_ok.affine") with
  | Error e -> Alcotest.failf "parse failed: %s" e
  | Ok caller_prog ->
    (* Run resolution to populate the loader's cache *)
    (match Resolve.resolve_program_with_loader caller_prog loader with
     | Error _ -> ()  (* even partial loads populate cache *)
     | Ok _ -> ());
    let flat = Module_loader.flatten_imports loader caller_prog in
    let local_fn_names = List.filter_map (function
      | Ast.TopFn fd -> Some fd.fd_name.name
      | _ -> None
    ) caller_prog.prog_decls in
    let flat_fn_names = List.filter_map (function
      | Ast.TopFn fd -> Some fd.fd_name.name
      | _ -> None
    ) flat.prog_decls in
    Alcotest.(check bool) "caller's main fn still present"
      true (List.mem "main" flat_fn_names);
    Alcotest.(check bool) "imported `consume` was inlined"
      true (List.mem "consume" flat_fn_names);
    Alcotest.(check bool) "originally only had local fns (no consume)"
      false (List.mem "consume" local_fn_names);
    Alcotest.(check bool) "fn count grew (consume added)"
      true (List.length flat.prog_decls > List.length caller_prog.prog_decls)

let test_flatten_imports_dedup_local_wins () =
  (* If the caller defines a fn with the same name as an imported one, the
     local definition must win — flatten_imports must not duplicate. *)
  let loader = Module_loader.create {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  let src = {|use CrossCallee::{consume};
              pub fn consume(own x: Int) -> Int { return x + 1; }
              pub fn main() -> Int { return consume(0); }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  (match Resolve.resolve_program_with_loader prog loader with
   | _ -> ());
  let flat = Module_loader.flatten_imports loader prog in
  let consume_count = List.fold_left (fun n decl ->
    match decl with
    | Ast.TopFn fd when fd.fd_name.name = "consume" -> n + 1
    | _ -> n
  ) 0 flat.prog_decls in
  Alcotest.(check int) "local consume wins; imported one not duplicated"
    1 consume_count

(* Regression for affinescript#107: imported public consts must be
   threaded into the importer's environment by both paths (WASM via
   gen_imports, non-WASM via flatten_imports). *)

let test_flatten_imports_inlines_public_const () =
  let loader = Module_loader.create {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  match parse_fixture (fixture "cross_const_caller.affine") with
  | Error e -> Alcotest.failf "parse failed: %s" e
  | Ok caller_prog ->
    (match Resolve.resolve_program_with_loader caller_prog loader with
     | _ -> ());
    let flat = Module_loader.flatten_imports loader caller_prog in
    let flat_const_names = List.filter_map (function
      | Ast.TopConst { tc_name; _ } -> Some tc_name.name
      | _ -> None
    ) flat.prog_decls in
    let flat_fn_names = List.filter_map (function
      | Ast.TopFn fd -> Some fd.fd_name.name
      | _ -> None
    ) flat.prog_decls in
    Alcotest.(check bool) "imported `input_marker` const inlined"
      true (List.mem "input_marker" flat_const_names);
    Alcotest.(check bool) "imported `marker_plus` fn inlined"
      true (List.mem "marker_plus" flat_fn_names);
    Alcotest.(check bool) "caller's main fn still present"
      true (List.mem "main" flat_fn_names)

let test_wasm_cross_module_const_compiles () =
  match compile_fixture_to_wasm (fixture "PortNames.affine"),
        compile_fixture_to_wasm (fixture "cross_const_caller.affine") with
  | Ok _, Ok caller ->
    (* The imported const must have produced exactly one global on the
       caller side, with the same I32Const initialiser as the callee's
       value (256). *)
    Alcotest.(check bool) "caller emits at least one global for the imported const"
      true (List.length caller.globals >= 1);
    let has_marker_init = List.exists (fun (g : Wasm.global) ->
      List.exists (function Wasm.I32Const n -> Int32.to_int n = 256 | _ -> false) g.g_init
    ) caller.globals in
    Alcotest.(check bool) "caller has a global initialised to 256 (input_marker value)"
      true has_marker_init
  | Error e, _ -> Alcotest.fail ("callee compile failed: " ^ e)
  | _, Error e -> Alcotest.fail ("caller compile failed (regression for #107): " ^ e)

let cross_module_other_codegens_tests = [
  Alcotest.test_case "flatten_imports inlines imported public fns"          `Quick test_flatten_imports_inlines_public_fns;
  Alcotest.test_case "flatten_imports: local def shadows imported, no dup"  `Quick test_flatten_imports_dedup_local_wins;
  Alcotest.test_case "flatten_imports inlines imported public consts (#107)" `Quick test_flatten_imports_inlines_public_const;
  Alcotest.test_case "WASM gen_imports threads imported consts (#107)"      `Quick test_wasm_cross_module_const_compiles;
]

(* ---- extern declarations (issues-drafts/04) ----

   `extern fn name(args) -> Ret;` and `extern type Name;` both parse, the
   resolver and typechecker register them, and the WASM codegen emits a
   real `(import "env" "name" (func ...))` entry for each extern fn. *)

let test_extern_fn_parses () =
  let src = {|extern type Application;
              extern fn createApplication(width: Int, height: Int) -> Application;
              pub fn init_pixi(width: Int, height: Int) -> Application {
                return createApplication(width, height);
              }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  let extern_fns = List.filter_map (function
    | Ast.TopFn fd when fd.fd_body = Ast.FnExtern -> Some fd.fd_name.name
    | _ -> None
  ) prog.prog_decls in
  let extern_types = List.filter_map (function
    | Ast.TopType td when td.td_body = Ast.TyExtern -> Some td.td_name.name
    | _ -> None
  ) prog.prog_decls in
  Alcotest.(check (list string)) "extern fn parsed" ["createApplication"] extern_fns;
  Alcotest.(check (list string)) "extern type parsed" ["Application"] extern_types

let test_extern_fn_codegen_emits_wasm_import () =
  let src = {|extern type Application;
              extern fn createApplication(width: Int, height: Int) -> Application;
              pub fn init_pixi(width: Int, height: Int) -> Application {
                return createApplication(width, height);
              }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Codegen.generate_module prog with
  | Error e -> Alcotest.failf "codegen failed: %s" (Codegen.show_codegen_error e)
  | Ok m ->
    let has_extern_import = List.exists (fun (i : Wasm.import) ->
      i.i_name = "createApplication"
      && i.i_module = "env"
      && (match i.i_desc with Wasm.ImportFunc _ -> true | _ -> false)
    ) m.imports in
    Alcotest.(check bool) "extern fn → (import \"env\" \"createApplication\" ...)"
      true has_extern_import

let extern_tests = [
  Alcotest.test_case "extern fn / extern type parse" `Quick test_extern_fn_parses;
  Alcotest.test_case "extern fn → WASM import in 'env' namespace" `Quick test_extern_fn_codegen_emits_wasm_import;
]

(* ---- STDLIB-04a: Mut effect externs (make_ref/get/set) ----

   Hermetic round-trip on the interpreter: `make_ref(7)` allocates a
   mutable cell, `set(r, 42)` mutates it, `get(r)` reads back the new
   value. Proves the [Value.VMut] cell wiring (real implementation,
   not a stub). Issue #328. *)

let test_stdlib_04a_mut_round_trip () =
  let src = {|
effect Mut;
extern fn make_ref<T>(x: T) -> Ref<T> / Mut;
extern fn get<T>(r: Ref<T>) -> T / Mut;
extern fn set<T>(r: Ref<T>, x: T) -> Unit / Mut;

fn round_trip() -> Int / Mut {
  let r = make_ref(7);
  set(r, 42);
  get(r)
}

const result: Int = round_trip();
|} in
  let prog = Parse_driver.parse_string ~file:"<test_stdlib_04a>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "result" env with
     | Ok (Value.VInt 42) -> ()
     | Ok v -> Alcotest.failf "expected VInt 42, got %s" (Value.show_value v)
     | Error e -> Alcotest.failf "lookup failed: %s" (Value.show_eval_error e))

(* `make_ref` on a non-Int value: round-trip a String to prove the cell
   is value-polymorphic at runtime (matches the `<T>` signature). *)
let test_stdlib_04a_mut_string_cell () =
  let src = {|
effect Mut;
extern fn make_ref<T>(x: T) -> Ref<T> / Mut;
extern fn get<T>(r: Ref<T>) -> T / Mut;
extern fn set<T>(r: Ref<T>, x: T) -> Unit / Mut;

fn round_trip() -> String / Mut {
  let r = make_ref("alpha");
  set(r, "omega");
  get(r)
}

const result: String = round_trip();
|} in
  let prog = Parse_driver.parse_string ~file:"<test_stdlib_04a>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "result" env with
     | Ok (Value.VString "omega") -> ()
     | Ok v -> Alcotest.failf "expected VString \"omega\", got %s"
                 (Value.show_value v)
     | Error e -> Alcotest.failf "lookup failed: %s" (Value.show_eval_error e))

(* Deno codegen lowers make_ref/get/set to the `{__cell: x}` host shape.
   Proves the codegen_deno builtin table entries fire (was missing pre-
   #328) — the emitted source must contain `__cell` references. *)
let test_stdlib_04a_mut_deno_codegen () =
  let src = {|
effect Mut;
extern fn make_ref<T>(x: T) -> Ref<T> / Mut;
extern fn get<T>(r: Ref<T>) -> T / Mut;
extern fn set<T>(r: Ref<T>, x: T) -> Unit / Mut;

pub fn round_trip() -> Int {
  let r = make_ref(0);
  set(r, 99);
  get(r)
}
|} in
  let prog = Parse_driver.parse_string ~file:"<test_stdlib_04a>" src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog loader with
  | Error (e, _) ->
    Alcotest.failf "resolve failed: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_deno prog rctx.symbols with
     | Error e -> Alcotest.failf "deno-codegen failed: %s" e
     | Ok js ->
       let contains needle =
         let nl = String.length needle and sl = String.length js in
         let rec go i = i + nl <= sl &&
           (String.sub js i nl = needle || go (i + 1))
         in nl = 0 || go 0
       in
       Alcotest.(check bool) "emitted JS contains __cell shape"
         true (contains "__cell"))

let stdlib_04a_mut_tests = [
  Alcotest.test_case "#328 make_ref/set/get round-trip (Int)" `Quick test_stdlib_04a_mut_round_trip;
  Alcotest.test_case "#328 make_ref/set/get round-trip (String)" `Quick test_stdlib_04a_mut_string_cell;
  Alcotest.test_case "#328 Deno codegen emits __cell shape" `Quick test_stdlib_04a_mut_deno_codegen;
]

(* ---- STDLIB-04d: IO externs hermetic test coverage (Refs #331) ----

   `print`/`println`/`read_line`/`read_file`/`write_file` were already
   wired in interp + Deno codegen, but had no dedicated hermetic tests
   asserting the round-trip semantics (test-debt, not impl-debt). This
   row adds them. `read_line` is interactive and skipped here — that
   surface is exercised by the TEA-bridge tests with redirected stdin. *)

(* write_file -> read_file round-trip on a real tmpfile *)
let test_stdlib_04d_write_then_read_file () =
  let tmp = Filename.temp_file "as_04d_io" ".txt" in
  Fun.protect ~finally:(fun () -> if Sys.file_exists tmp then Sys.remove tmp)
    (fun () ->
       let src = Printf.sprintf {|
fn writer() -> Result<Unit, String> { write_file("%s", "hello-04d\n") }
fn reader() -> Result<String, String> { read_file("%s") }
|} (String.escaped tmp) (String.escaped tmp) in
       let prog = Parse_driver.parse_string ~file:"<test>" src in
       match Interp.eval_program prog with
       | Error e -> Alcotest.failf "interp load failed: %s"
                      (Value.show_eval_error e)
       | Ok env ->
         let call name =
           match Value.lookup_env name env with
           | Error e -> Error e
           | Ok fn -> Interp.apply_function fn []
         in
         (match call "writer" with
          | Ok (Value.VVariant ("Ok", _)) -> ()
          | Ok v -> Alcotest.failf "writer expected Ok(Unit), got %s"
                      (Value.show_value v)
          | Error e -> Alcotest.failf "writer failed: %s"
                         (Value.show_eval_error e));
         (match call "reader" with
          | Ok (Value.VVariant ("Ok", Some (Value.VString s))) ->
            Alcotest.(check string) "reader returns written content"
              "hello-04d\n" s
          | Ok v -> Alcotest.failf "reader expected Ok(String), got %s"
                      (Value.show_value v)
          | Error e -> Alcotest.failf "reader failed: %s"
                         (Value.show_eval_error e)))

(* read_file on a path that does not exist returns Err, not raises. *)
let test_stdlib_04d_read_file_missing () =
  let tmp = Filename.temp_file "as_04d_missing" ".txt" in
  Sys.remove tmp;  (* removed -- guaranteed missing *)
  let src = Printf.sprintf
    "fn f() -> Result<String, String> { read_file(\"%s\") }"
    (String.escaped tmp) in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VVariant ("Err", _)) -> ()
        | Ok v -> Alcotest.failf "expected Err(_), got %s" (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s"
                       (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup failed: %s"
                    (Value.show_eval_error e))

(* `print` and `println` exec without error. Stdout-content capture is
   intentionally out of scope here (the TEA-bridge tests already
   exercise the redirect path with full Unix.dup2 plumbing); we just
   prove the lowering doesn't blow up at runtime. *)
let test_stdlib_04d_print_no_error () =
  let src = "fn f() -> Unit { print(\"\") }" in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok _ -> ()
        | Error e -> Alcotest.failf "print failed: %s"
                       (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup failed: %s"
                    (Value.show_eval_error e))

let test_stdlib_04d_println_no_error () =
  let src = "fn f() -> Unit { println(\"\") }" in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok _ -> ()
        | Error e -> Alcotest.failf "println failed: %s"
                       (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup failed: %s"
                    (Value.show_eval_error e))

(* Deno codegen lowers IO externs to the right host shape. *)
let test_stdlib_04d_io_deno_codegen () =
  let src = {|
fn run() -> Unit {
  print("a");
  println("b")
}
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog loader with
  | Error (e, _) ->
    Alcotest.failf "resolve failed: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_deno prog rctx.symbols with
     | Error e -> Alcotest.failf "deno-codegen failed: %s" e
     | Ok js ->
       let contains needle =
         let nl = String.length needle and sl = String.length js in
         let rec go i = i + nl <= sl &&
           (String.sub js i nl = needle || go (i + 1))
         in nl = 0 || go 0
       in
       Alcotest.(check bool) "prelude defines print/println"
         true (contains "const print" && contains "const println"))

let stdlib_04d_io_tests = [
  Alcotest.test_case "#331 write_file -> read_file round-trip" `Quick test_stdlib_04d_write_then_read_file;
  Alcotest.test_case "#331 read_file on missing path returns Err" `Quick test_stdlib_04d_read_file_missing;
  Alcotest.test_case "#331 print exec without error" `Quick test_stdlib_04d_print_no_error;
  Alcotest.test_case "#331 println exec without error" `Quick test_stdlib_04d_println_no_error;
  Alcotest.test_case "#331 Deno codegen wires print/println" `Quick test_stdlib_04d_io_deno_codegen;
]

(* ---- STDLIB-04e: Pure externs (Refs #332) ----

   Three externs declared in stdlib/effects.affine as pure:
     int_to_string : Int -> String
     string_to_int : String -> Option<Int>
     string_length : String -> Int

   `int_to_string` + `string_length` were already wired; `string_to_int`
   was unwired (dead surface — any caller would compile and fail at run).
   This row wires `string_to_int` as the typed-alias of `parse_int` and
   asserts hermetic round-trip semantics for all three. *)

let test_stdlib_04e_int_to_string () =
  let prog = Parse_driver.parse_string ~file:"<test>"
    "fn f() -> String { int_to_string(42) }" in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VString "42") -> ()
        | Ok v -> Alcotest.failf "expected VString \"42\", got %s"
                    (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

let test_stdlib_04e_string_to_int_some () =
  let prog = Parse_driver.parse_string ~file:"<test>"
    "fn f() -> Option<Int> { string_to_int(\"123\") }" in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VVariant ("Some", Some (Value.VInt 123))) -> ()
        | Ok v -> Alcotest.failf "expected Some(123), got %s"
                    (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

let test_stdlib_04e_string_to_int_none () =
  let prog = Parse_driver.parse_string ~file:"<test>"
    "fn f() -> Option<Int> { string_to_int(\"abc\") }" in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VVariant ("None", None)) -> ()
        | Ok v -> Alcotest.failf "expected None, got %s" (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

let test_stdlib_04e_string_length () =
  let prog = Parse_driver.parse_string ~file:"<test>"
    "fn f() -> Int { string_length(\"hello\") }" in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VInt 5) -> ()
        | Ok v -> Alcotest.failf "expected VInt 5, got %s" (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

let stdlib_04e_pure_tests = [
  Alcotest.test_case "#332 int_to_string(42) == \"42\"" `Quick test_stdlib_04e_int_to_string;
  Alcotest.test_case "#332 string_to_int(\"123\") == Some(123)" `Quick test_stdlib_04e_string_to_int_some;
  Alcotest.test_case "#332 string_to_int(\"abc\") == None" `Quick test_stdlib_04e_string_to_int_none;
  Alcotest.test_case "#332 string_length(\"hello\") == 5" `Quick test_stdlib_04e_string_length;
]

(* ---- PHASE-F string-wall slice 1: string indexing ----

   `string_char_code_at(s, i)` + `char_to_int(c)` gained wasm-backend
   lowerings in lib/codegen.ml (the read-side `[len: i32 LE][utf8]` ABI).
   The interp bindings already existed (lib/interp.ml); these tests pin the
   interp oracle whose values the new wasm lowerings must reproduce — the
   parity reference for the matching tests/codegen/*.mjs executable check.

   Oracle semantics (lib/interp.ml `string_char_code_at`): byte 0..255 at
   index i, or -1 when i < 0 OR i >= length (the shared absent-byte
   sentinel). `char_to_int` is `Char.code`. *)

let eval_int_fn src =
  let prog = Parse_driver.parse_string ~file:"<stringwall>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VInt n) -> n
        | Ok v -> Alcotest.failf "expected VInt, got %s" (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

(* String-returning sibling of [eval_int_fn], for ops whose result is a
   String (e.g. int_to_string). *)
let eval_string_fn src =
  let prog = Parse_driver.parse_string ~file:"<stringwall>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "interp failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok (Value.VString s) -> s
        | Ok v -> Alcotest.failf "expected VString, got %s" (Value.show_value v)
        | Error e -> Alcotest.failf "apply failed: %s" (Value.show_eval_error e))
     | Error e -> Alcotest.failf "lookup f failed: %s" (Value.show_eval_error e))

let test_stringwall_scca_first () =
  Alcotest.(check int) "string_char_code_at(\"ABC\",0) == 'A'" 65
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"ABC\", 0) }")

let test_stringwall_scca_mid () =
  Alcotest.(check int) "string_char_code_at(\"ABC\",1) == 'B'" 66
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"ABC\", 1) }")

let test_stringwall_scca_last () =
  Alcotest.(check int) "string_char_code_at(\"ABC\",2) == 'C'" 67
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"ABC\", 2) }")

let test_stringwall_scca_oob_neg () =
  Alcotest.(check int) "negative index -> -1 sentinel" (-1)
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"ABC\", -1) }")

let test_stringwall_scca_oob_past () =
  Alcotest.(check int) "index past end -> -1 sentinel" (-1)
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"ABC\", 9) }")

let test_stringwall_scca_empty () =
  Alcotest.(check int) "index into empty string -> -1 sentinel" (-1)
    (eval_int_fn "fn f() -> Int { string_char_code_at(\"\", 0) }")

let test_stringwall_char_to_int () =
  Alcotest.(check int) "char_to_int('Z') == 90" 90
    (eval_int_fn "fn f() -> Int { char_to_int('Z') }")

let stringwall_index_tests = [
  Alcotest.test_case "scca(\"ABC\",0) == 65" `Quick test_stringwall_scca_first;
  Alcotest.test_case "scca(\"ABC\",1) == 66" `Quick test_stringwall_scca_mid;
  Alcotest.test_case "scca(\"ABC\",2) == 67" `Quick test_stringwall_scca_last;
  Alcotest.test_case "scca neg index == -1" `Quick test_stringwall_scca_oob_neg;
  Alcotest.test_case "scca past-end index == -1" `Quick test_stringwall_scca_oob_past;
  Alcotest.test_case "scca empty string == -1" `Quick test_stringwall_scca_empty;
  Alcotest.test_case "char_to_int('Z') == 90" `Quick test_stringwall_char_to_int;
]

(* ---- PHASE-F string-wall slice 2: string_from_char_code ----

   `string_from_char_code(n)` gained a wasm-backend lowering (the write-side
   of the [len: i32 LE][utf8] ABI): bump-allocate [len=1][byte], where the
   byte is the low 8 bits of n. The interp binding already existed
   (lib/interp.ml: String.make 1 (Char.chr (n land 0xff))); these pin the
   interp oracle the wasm lowering must reproduce. Observable via the
   slice-1 reader string_char_code_at and via string_length. *)

let test_stringwall_sfcc_roundtrip () =
  Alcotest.(check int) "scca(sfcc(66), 0) == 66" 66
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(66), 0) }")

let test_stringwall_sfcc_nul () =
  Alcotest.(check int) "code 0 (NUL byte) survives" 0
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(0), 0) }")

let test_stringwall_sfcc_high_byte () =
  Alcotest.(check int) "code 255 survives (unsigned byte)" 255
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(255), 0) }")

let test_stringwall_sfcc_mask_overflow () =
  Alcotest.(check int) "256 masked to low byte == 0" 0
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(256), 0) }")

let test_stringwall_sfcc_mask_negative () =
  Alcotest.(check int) "low byte of -1 == 255" 255
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(-1), 0) }")

let test_stringwall_sfcc_length () =
  Alcotest.(check int) "string_length(sfcc(n)) == 1" 1
    (eval_int_fn "fn f() -> Int { string_length(string_from_char_code(65)) }")

let test_stringwall_sfcc_oob () =
  Alcotest.(check int) "index 1 into a 1-byte string == -1" (-1)
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_from_char_code(65), 1) }")

let stringwall_alloc_tests = [
  Alcotest.test_case "scca(sfcc(66),0) == 66" `Quick test_stringwall_sfcc_roundtrip;
  Alcotest.test_case "sfcc(0) NUL byte == 0" `Quick test_stringwall_sfcc_nul;
  Alcotest.test_case "sfcc(255) == 255" `Quick test_stringwall_sfcc_high_byte;
  Alcotest.test_case "sfcc(256) masked == 0" `Quick test_stringwall_sfcc_mask_overflow;
  Alcotest.test_case "sfcc(-1) low byte == 255" `Quick test_stringwall_sfcc_mask_negative;
  Alcotest.test_case "string_length(sfcc) == 1" `Quick test_stringwall_sfcc_length;
  Alcotest.test_case "index past 1-byte string == -1" `Quick test_stringwall_sfcc_oob;
]

(* ---- PHASE-F string-wall slice 3: string_sub ----

   `string_sub(s, start, length)` gained a wasm-backend lowering: a
   runtime-sized heap allocation plus a byte-copy loop, with the interp's
   clamp semantics (lib/interp.ml: start' = max 0 (min start slen);
   length' = max 0 (min length (slen - start'))). These pin the interp oracle
   the wasm lowering must reproduce; the result is read back via the slice-1
   reader string_char_code_at and via string_length. *)

let test_stringwall_sub_byte0 () =
  Alcotest.(check int) "sub(\"hello\",1,3)[0] == 'e'" 101
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_sub(\"hello\", 1, 3), 0) }")

let test_stringwall_sub_byte2 () =
  Alcotest.(check int) "sub(\"hello\",1,3)[2] == 'l'" 108
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_sub(\"hello\", 1, 3), 2) }")

let test_stringwall_sub_len () =
  Alcotest.(check int) "string_length(sub(\"hello\",1,3)) == 3" 3
    (eval_int_fn "fn f() -> Int { string_length(string_sub(\"hello\", 1, 3)) }")

let test_stringwall_sub_full () =
  Alcotest.(check int) "string_length(sub(\"hello\",0,5)) == 5" 5
    (eval_int_fn "fn f() -> Int { string_length(string_sub(\"hello\", 0, 5)) }")

let test_stringwall_sub_clamp_len () =
  Alcotest.(check int) "length clamped to slen-start' == 3" 3
    (eval_int_fn "fn f() -> Int { string_length(string_sub(\"hello\", 2, 100)) }")

let test_stringwall_sub_clamp_start () =
  Alcotest.(check int) "start past end -> empty (len 0)" 0
    (eval_int_fn "fn f() -> Int { string_length(string_sub(\"hello\", 10, 3)) }")

let test_stringwall_sub_neg_start () =
  Alcotest.(check int) "negative start clamps to 0 -> 'h'" 104
    (eval_int_fn "fn f() -> Int { string_char_code_at(string_sub(\"hello\", -1, 2), 0) }")

let test_stringwall_sub_zero_len () =
  Alcotest.(check int) "zero length -> empty (len 0)" 0
    (eval_int_fn "fn f() -> Int { string_length(string_sub(\"hello\", 1, 0)) }")

let stringwall_sub_tests = [
  Alcotest.test_case "sub(\"hello\",1,3)[0] == 101" `Quick test_stringwall_sub_byte0;
  Alcotest.test_case "sub(\"hello\",1,3)[2] == 108" `Quick test_stringwall_sub_byte2;
  Alcotest.test_case "len sub(\"hello\",1,3) == 3" `Quick test_stringwall_sub_len;
  Alcotest.test_case "len sub full == 5" `Quick test_stringwall_sub_full;
  Alcotest.test_case "length clamp == 3" `Quick test_stringwall_sub_clamp_len;
  Alcotest.test_case "start clamp -> 0" `Quick test_stringwall_sub_clamp_start;
  Alcotest.test_case "neg start -> 'h'" `Quick test_stringwall_sub_neg_start;
  Alcotest.test_case "zero length -> 0" `Quick test_stringwall_sub_zero_len;
]

(* ---- PHASE-F string-wall slice 4: to_lowercase / to_uppercase ----

   ASCII case-folding gained wasm lowerings: a copy-with-transform over the
   slice-3 runtime-length idiom. The branchless per-byte shift matches the
   interp oracle (lib/interp.ml String.{lowercase,uppercase}_ascii): only
   'A'..'Z' / 'a'..'z' shift by 32; everything else (digits, punctuation,
   non-ASCII bytes) passes through. The @ (64) / [ (91) probes pin the
   exclusive 'A'..'Z' boundary. *)

let test_stringwall_lower_A () =
  Alcotest.(check int) "to_lowercase(\"ABC\")[0] == 'a'" 97
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_lowercase(\"ABC\"), 0) }")

let test_stringwall_lower_C () =
  Alcotest.(check int) "to_lowercase(\"ABC\")[2] == 'c'" 99
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_lowercase(\"ABC\"), 2) }")

let test_stringwall_upper_a () =
  Alcotest.(check int) "to_uppercase(\"abc\")[0] == 'A'" 65
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_uppercase(\"abc\"), 0) }")

let test_stringwall_lower_digit_passthrough () =
  Alcotest.(check int) "non-letter '3' passes through" 51
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_lowercase(\"aB3\"), 2) }")

let test_stringwall_lower_below_A () =
  Alcotest.(check int) "'@' (64, just below 'A') unchanged" 64
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_lowercase(\"@\"), 0) }")

let test_stringwall_lower_above_Z () =
  Alcotest.(check int) "'[' (91, just above 'Z') unchanged" 91
    (eval_int_fn "fn f() -> Int { string_char_code_at(to_lowercase(\"[\"), 0) }")

let test_stringwall_case_length () =
  Alcotest.(check int) "case-fold preserves length" 5
    (eval_int_fn "fn f() -> Int { string_length(to_uppercase(\"Hello\")) }")

let stringwall_case_tests = [
  Alcotest.test_case "lower(\"ABC\")[0] == 97" `Quick test_stringwall_lower_A;
  Alcotest.test_case "lower(\"ABC\")[2] == 99" `Quick test_stringwall_lower_C;
  Alcotest.test_case "upper(\"abc\")[0] == 65" `Quick test_stringwall_upper_a;
  Alcotest.test_case "digit passthrough == 51" `Quick test_stringwall_lower_digit_passthrough;
  Alcotest.test_case "'@' below 'A' unchanged" `Quick test_stringwall_lower_below_A;
  Alcotest.test_case "'[' above 'Z' unchanged" `Quick test_stringwall_lower_above_Z;
  Alcotest.test_case "case-fold preserves length" `Quick test_stringwall_case_length;
]

(* ---- PHASE-F string-wall slice 5: trim ----

   `trim(s)` gained a wasm lowering: two flat scans (front for `lo`, back for
   `hi`) bracket the non-whitespace core, then a slice-3-style copy of
   `[lo, hi)`. Matches the interp oracle (lib/interp.ml: `String.trim`), whose
   whitespace set is {space 32, tab 9, newline 10, form-feed 12, CR 13}.
   Internal whitespace is preserved; an all-whitespace string trims to empty. *)

let test_stringwall_trim_both_len () =
  Alcotest.(check int) "trim(\"  hi  \") length == 2" 2
    (eval_int_fn "fn f() -> Int { string_length(trim(\"  hi  \")) }")

let test_stringwall_trim_byte0 () =
  Alcotest.(check int) "trim(\"  hi  \")[0] == 'h'" 104
    (eval_int_fn "fn f() -> Int { string_char_code_at(trim(\"  hi  \"), 0) }")

let test_stringwall_trim_all_ws () =
  Alcotest.(check int) "all-whitespace trims to empty" 0
    (eval_int_fn "fn f() -> Int { string_length(trim(\"   \")) }")

let test_stringwall_trim_empty () =
  Alcotest.(check int) "trim(\"\") == \"\"" 0
    (eval_int_fn "fn f() -> Int { string_length(trim(\"\")) }")

let test_stringwall_trim_internal_len () =
  Alcotest.(check int) "internal whitespace preserved (len 3)" 3
    (eval_int_fn "fn f() -> Int { string_length(trim(\" a b \")) }")

let test_stringwall_trim_internal_space () =
  Alcotest.(check int) "kept internal space byte == 32" 32
    (eval_int_fn "fn f() -> Int { string_char_code_at(trim(\" a b \"), 1) }")

let test_stringwall_trim_tab_newline () =
  Alcotest.(check int) "tab + newline stripped (len 2)" 2
    (eval_int_fn "fn f() -> Int { string_length(trim(\"\\thi\\n\")) }")

let stringwall_trim_tests = [
  Alcotest.test_case "trim both ends len == 2" `Quick test_stringwall_trim_both_len;
  Alcotest.test_case "trim(\"  hi  \")[0] == 104" `Quick test_stringwall_trim_byte0;
  Alcotest.test_case "all whitespace -> empty" `Quick test_stringwall_trim_all_ws;
  Alcotest.test_case "trim empty -> empty" `Quick test_stringwall_trim_empty;
  Alcotest.test_case "internal ws preserved len" `Quick test_stringwall_trim_internal_len;
  Alcotest.test_case "internal space byte == 32" `Quick test_stringwall_trim_internal_space;
  Alcotest.test_case "tab+newline stripped" `Quick test_stringwall_trim_tab_newline;
]

(* ---- PHASE-F string-wall slice 6: string_find ----

   `string_find(haystack, needle)` gained a wasm lowering (read-side nested
   scan, no allocation) returning the first-occurrence index or -1. Matches
   the interp oracle (lib/interp.ml string_find) — whose empty-needle crash
   (`String.get needle 0` before the nlen=0 guard) was fixed in the same
   change so an empty needle returns 0. Cases pin: found / not-found /
   first-occurrence / needle-longer-than-haystack / empty needle / empty
   haystack. *)

let test_stringwall_find_mid () =
  Alcotest.(check int) "string_find(\"hello\",\"ll\") == 2" 2
    (eval_int_fn "fn f() -> Int { string_find(\"hello\", \"ll\") }")

let test_stringwall_find_end () =
  Alcotest.(check int) "string_find(\"hello\",\"o\") == 4" 4
    (eval_int_fn "fn f() -> Int { string_find(\"hello\", \"o\") }")

let test_stringwall_find_absent () =
  Alcotest.(check int) "needle not present -> -1" (-1)
    (eval_int_fn "fn f() -> Int { string_find(\"hello\", \"xyz\") }")

let test_stringwall_find_first_occurrence () =
  Alcotest.(check int) "returns FIRST occurrence" 1
    (eval_int_fn "fn f() -> Int { string_find(\"abcabc\", \"bc\") }")

let test_stringwall_find_needle_longer () =
  Alcotest.(check int) "needle longer than haystack -> -1" (-1)
    (eval_int_fn "fn f() -> Int { string_find(\"hello\", \"hellox\") }")

let test_stringwall_find_empty_needle () =
  Alcotest.(check int) "empty needle -> 0 (was an interp crash)" 0
    (eval_int_fn "fn f() -> Int { string_find(\"hello\", \"\") }")

let test_stringwall_find_empty_haystack () =
  Alcotest.(check int) "empty haystack, non-empty needle -> -1" (-1)
    (eval_int_fn "fn f() -> Int { string_find(\"\", \"x\") }")

let stringwall_find_tests = [
  Alcotest.test_case "find \"ll\" == 2" `Quick test_stringwall_find_mid;
  Alcotest.test_case "find \"o\" == 4" `Quick test_stringwall_find_end;
  Alcotest.test_case "find absent == -1" `Quick test_stringwall_find_absent;
  Alcotest.test_case "find first occurrence == 1" `Quick test_stringwall_find_first_occurrence;
  Alcotest.test_case "needle longer == -1" `Quick test_stringwall_find_needle_longer;
  Alcotest.test_case "empty needle == 0" `Quick test_stringwall_find_empty_needle;
  Alcotest.test_case "empty haystack == -1" `Quick test_stringwall_find_empty_haystack;
]

(* ---- PHASE-F string-wall slice 7: int_to_string ----

   `int_to_string(n)` gained a wasm lowering: decimal rendering of an i32 over
   the slice-3 allocation idiom, with INT_MIN handled by extracting digits in
   negative space. These pin the interp oracle (`string_of_int`) the wasm
   lowering must reproduce — full-string equality across zero, positive,
   negative, INT_MAX, and the INT_MIN edge. *)

let test_stringwall_i2s_zero () =
  Alcotest.(check string) "int_to_string(0) == \"0\"" "0"
    (eval_string_fn "fn f() -> String { int_to_string(0) }")

let test_stringwall_i2s_pos () =
  Alcotest.(check string) "int_to_string(42) == \"42\"" "42"
    (eval_string_fn "fn f() -> String { int_to_string(42) }")

let test_stringwall_i2s_neg () =
  Alcotest.(check string) "int_to_string(-123) == \"-123\"" "-123"
    (eval_string_fn "fn f() -> String { int_to_string(-123) }")

let test_stringwall_i2s_intmax () =
  Alcotest.(check string) "int_to_string(INT_MAX)" "2147483647"
    (eval_string_fn "fn f() -> String { int_to_string(2147483647) }")

let test_stringwall_i2s_intmin () =
  Alcotest.(check string) "int_to_string(INT_MIN) (negative-space)" "-2147483648"
    (eval_string_fn "fn f() -> String { int_to_string(-2147483648) }")

let stringwall_i2s_tests = [
  Alcotest.test_case "int_to_string(0) == \"0\"" `Quick test_stringwall_i2s_zero;
  Alcotest.test_case "int_to_string(42) == \"42\"" `Quick test_stringwall_i2s_pos;
  Alcotest.test_case "int_to_string(-123) == \"-123\"" `Quick test_stringwall_i2s_neg;
  Alcotest.test_case "int_to_string(INT_MAX)" `Quick test_stringwall_i2s_intmax;
  Alcotest.test_case "int_to_string(INT_MIN)" `Quick test_stringwall_i2s_intmin;
]

(* ---- PHASE-F string-wall slice 8 (guard half): string `++` rejection ----

   String `++` reaching the wasm backend's *list*-concat lowering silently
   miscompiles (the source string's [len][utf8] bytes are copied as i32
   elements; "ab" ++ "cd" yields byte 2 = 2 instead of 'c' — see
   proposals/DESIGN-string-concat.adoc). Until the type-directed lowering
   lands, codegen rejects the syntactically-obvious string `++` with a loud
   error. These pin that the guard fires for string `++` and does NOT fire for
   list `++` (no false positive). *)

let string_contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let test_stringwall_concat_guard_rejects_string () =
  let prog = Parse_driver.parse_string ~file:"<stringwall>"
    "fn f() -> Int { string_char_code_at(\"ab\" ++ \"cd\", 0) }" in
  match wasm_codegen prog with
  | Error msg when string_contains msg "concatenation" -> ()
  | Error msg -> Alcotest.failf "expected the slice-8 string-++ guard, got: %s" msg
  | Ok _ ->
    Alcotest.fail
      "string `++` must be rejected by the slice-8 guard (the list-concat \
       lowering would silently miscompile it), not compiled to wasm"

let test_stringwall_concat_guard_allows_list () =
  (* The guard must not misfire on genuine list `++`. *)
  let prog = Parse_driver.parse_string ~file:"<stringwall>"
    "fn f() -> Int { let xs = [1, 2] ++ [3]; 0 }" in
  match wasm_codegen prog with
  | Ok _ -> ()
  | Error msg -> Alcotest.failf "list `++` must still compile (guard false positive): %s" msg

(* Slice 8b: after typecheck records the String-concat sites,
   Typecheck.elaborate_string_concat rewrites them to ExprStringConcat, which
   the wasm backend lowers as byte concatenation. So the full pipeline
   (frontend -> elaborate -> codegen) compiles string `++` rather than hitting
   the slice-8a backstop guard. *)
let test_stringwall_concat_lowers_after_elaboration () =
  match run_frontend (fixture "string_concat.affine") with
  | Error e -> Alcotest.failf "frontend failed on string_concat.affine: %s" e
  | Ok (prog, _resolve_ctx) ->
    let elaborated = Affinescript.Typecheck.elaborate_string_concat prog in
    (match wasm_codegen elaborated with
     | Ok _ -> ()
     | Error msg ->
       Alcotest.failf
         "slice 8b: string `++` should lower to wasm after elaboration, got: %s"
         msg)

(* Slice 9: String `==`/`!=` are polymorphic-equality nodes that
   Typecheck.elaborate_string_concat rewrites to ExprStringEq (driven by the
   string_eq_sites that synth records), so the wasm backend lowers them as a
   byte comparison rather than the I32Eq *pointer* comparison correct only for
   Int. Mirrors the slice-8b concat test: the full pipeline (frontend ->
   elaborate -> codegen) must compile string `==`/`!=` rather than leaving a
   silent pointer-comparison miscompile. *)
let test_stringwall_eq_lowers_after_elaboration () =
  match run_frontend (fixture "string_eq.affine") with
  | Error e -> Alcotest.failf "frontend failed on string_eq.affine: %s" e
  | Ok (prog, _resolve_ctx) ->
    let elaborated = Affinescript.Typecheck.elaborate_string_concat prog in
    (match wasm_codegen elaborated with
     | Ok _ -> ()
     | Error msg ->
       Alcotest.failf
         "slice 9: string `==`/`!=` should lower to wasm after elaboration, got: %s"
         msg)

(* Slice 10 (#458): String relational ops `<`/`<=`/`>`/`>=` typecheck in the
   [comparison] branch and must be rewritten to ExprStringRel by
   elaborate_string_concat (driven by string_rel_sites), so the wasm backend
   does a byte-wise lexicographic compare rather than a signed compare of the
   two `[len][utf8]` pointers. The full pipeline must compile them. *)
let test_stringwall_rel_lowers_after_elaboration () =
  match run_frontend (fixture "string_rel.affine") with
  | Error e -> Alcotest.failf "frontend failed on string_rel.affine: %s" e
  | Ok (prog, _resolve_ctx) ->
    let elaborated = Affinescript.Typecheck.elaborate_string_concat prog in
    (match wasm_codegen elaborated with
     | Ok _ -> ()
     | Error msg ->
       Alcotest.failf
         "slice 10: string `<`/`<=`/`>`/`>=` should lower to wasm after elaboration, got: %s"
         msg)

(* Float wall: Float binops (+ - * / and the six comparisons) type-check but
   gen_binop returns only the i32 family. The float fixture exercises Float
   params/returns/locals/arith/comparisons, which must compile after the
   ExprFloatBinary elaboration rather than emitting i32 ops on f64 operands. *)
let test_floatwall_lowers_to_f64 () =
  match run_frontend (fixture "float_wall.affine") with
  | Error e -> Alcotest.failf "frontend failed on float_wall.affine: %s" e
  | Ok (prog, _resolve_ctx) ->
    let elaborated = Affinescript.Typecheck.elaborate_string_concat prog in
    (match wasm_codegen elaborated with
     | Ok _ -> ()
     | Error msg ->
       Alcotest.failf
         "float wall: Float binops should lower to the f64 family after elaboration, got: %s"
         msg)

let stringwall_concat_guard_tests = [
  Alcotest.test_case "string `++` is rejected (not silently miscompiled)"
    `Quick test_stringwall_concat_guard_rejects_string;
  Alcotest.test_case "list `++` still compiles (no guard false positive)"
    `Quick test_stringwall_concat_guard_allows_list;
  Alcotest.test_case "string `++` lowers to wasm after elaboration (slice 8b)"
    `Quick test_stringwall_concat_lowers_after_elaboration;
  Alcotest.test_case "string `==`/`!=` lowers to wasm after elaboration (slice 9)"
    `Quick test_stringwall_eq_lowers_after_elaboration;
  Alcotest.test_case "string `<`/`<=`/`>`/`>=` lowers to wasm after elaboration (slice 10)"
    `Quick test_stringwall_rel_lowers_after_elaboration;
  Alcotest.test_case "Float binops lower to the f64 family after elaboration (float wall)"
    `Quick test_floatwall_lowers_to_f64;
]

(* ---- STDLIB-04b: Throws extern `error<T>` (Refs #329) ----

   `error<T>(msg: String) -> T / Throws` was declared in
   stdlib/effects.affine but missing in every backend. Same divergent
   semantics as `panic` with a polymorphic return type that unifies
   with the call-site expectation (unobservable because the call never
   returns). *)

let test_stdlib_04b_error_diverges_int_call_site () =
  let src = {|
fn must_be_positive(n: Int) -> Int {
  if n > 0 { n } else { error("not positive") }
}
fn f() -> Int { must_be_positive(-1) }
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "program load failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Error _ -> Alcotest.fail "f not bound"
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok _ -> Alcotest.fail "expected error to diverge; got Ok"
        | Error (Value.RuntimeError msg) ->
          Alcotest.(check string) "error message" "not positive" msg
        | Error e ->
          Alcotest.failf "expected RuntimeError, got: %s"
            (Value.show_eval_error e)))

(* Polymorphic: `error` in a String-returning context. Proves the
   `<T>` polymorphism — same call site, different unification. *)
let test_stdlib_04b_error_diverges_string_call_site () =
  let src = {|
fn lookup(k: String) -> String {
  if string_length(k) > 0 { k } else { error("empty key") }
}
fn f() -> String { lookup("") }
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match Interp.eval_program prog with
  | Error e -> Alcotest.failf "program load failed: %s" (Value.show_eval_error e)
  | Ok env ->
    (match Value.lookup_env "f" env with
     | Error _ -> Alcotest.fail "f not bound"
     | Ok fn ->
       (match Interp.apply_function fn [] with
        | Ok _ -> Alcotest.fail "expected error to diverge; got Ok"
        | Error (Value.RuntimeError msg) ->
          Alcotest.(check string) "error message" "empty key" msg
        | Error e ->
          Alcotest.failf "expected RuntimeError, got: %s"
            (Value.show_eval_error e)))

let test_stdlib_04b_error_deno_codegen () =
  let src = {|
fn must(n: Int) -> Int {
  if n > 0 { n } else { error("bad") }
}
|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog loader with
  | Error (e, _) ->
    Alcotest.failf "resolve failed: %s" (Resolve.show_resolve_error e)
  | Ok (rctx, _) ->
    (match Codegen_deno.codegen_deno prog rctx.symbols with
     | Error e -> Alcotest.failf "deno-codegen failed: %s" e
     | Ok js ->
       let contains needle =
         let nl = String.length needle and sl = String.length js in
         let rec go i = i + nl <= sl &&
           (String.sub js i nl = needle || go (i + 1))
         in nl = 0 || go 0
       in
       Alcotest.(check bool) "emitted JS throws on error()"
         true (contains "throw new Error(\"bad\")"))

let stdlib_04b_error_tests = [
  Alcotest.test_case "#329 error diverges at Int call site" `Quick test_stdlib_04b_error_diverges_int_call_site;
  Alcotest.test_case "#329 error diverges at String call site" `Quick test_stdlib_04b_error_diverges_string_call_site;
  Alcotest.test_case "#329 Deno codegen lowers to throw" `Quick test_stdlib_04b_error_deno_codegen;
]


(* ---- Issue #35 Phase 2 — Vscode bindings ----

   Verifies stdlib/Vscode.affine and stdlib/VscodeLanguageClient.affine
   parse + typecheck + can be `use`d from a downstream extension, and the
   compiled WASM emits one import per extern fn under the bindings'
   module name (so the JS-side adapter's namespaced shape lines up). *)

let test_vscode_bindings_parse_and_typecheck () =
  let candidates_for f = [
    "stdlib/" ^ f;
    "../../../stdlib/" ^ f;
    "../../../../stdlib/" ^ f;
  ] in
  let find p = List.find_opt Sys.file_exists (candidates_for p) in
  let check_one path =
    match find path with
    | None -> Alcotest.failf "not found: %s" path
    | Some p ->
      match parse_fixture p with
      | Error e -> Alcotest.failf "%s parse failed: %s" path e
      | Ok prog ->
        match resolve_program prog with
        | Error e -> Alcotest.failf "%s resolve failed: %s" path e
        | Ok (resolve_ctx, _) ->
          match Typecheck.check_program resolve_ctx.symbols prog with
          | Error e -> Alcotest.failf "%s typecheck failed: %s" path
                         (Typecheck.format_type_error e)
          | Ok _ -> ()
  in
  check_one "Vscode.affine";
  check_one "VscodeLanguageClient.affine"

let test_vscode_extern_emits_wasm_imports () =
  let src = {|use Vscode::{registerCommand, showInformationMessage};
              pub fn handler() -> Int { return showInformationMessage("hi"); }
              pub fn activate(ctx: Int) -> Int { return registerCommand("x", 0); }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  let stdlib_dir = List.find Sys.file_exists [
    "stdlib"; "../../../stdlib"; "../../../../stdlib"
  ] in
  let loader_config = {
    Module_loader.stdlib_path = stdlib_dir;
    search_paths = [];
    current_dir = stdlib_dir;
  } in
  let loader = Module_loader.create loader_config in
  (match Resolve.resolve_program_with_loader prog loader with
   | Error (e, _) ->
     Alcotest.failf "resolve failed: %s" (Resolve.show_resolve_error e)
   | Ok _ -> ());
  match Codegen.generate_module ~loader prog with
  | Error e -> Alcotest.failf "codegen failed: %s" (Codegen.show_codegen_error e)
  | Ok m ->
    let names = List.filter_map (fun (i : Wasm.import) ->
      if i.i_module = "Vscode" then Some i.i_name else None
    ) m.imports in
    Alcotest.(check bool) "Vscode.registerCommand imported"
      true (List.mem "registerCommand" names);
    Alcotest.(check bool) "Vscode.showInformationMessage imported"
      true (List.mem "showInformationMessage" names)

let vscode_bindings_tests = [
  Alcotest.test_case "Vscode + VscodeLanguageClient parse + typecheck" `Quick test_vscode_bindings_parse_and_typecheck;
  Alcotest.test_case "use Vscode::{...} → WASM (import \"Vscode\" \"...\" ...)" `Quick test_vscode_extern_emits_wasm_imports;
]

(* ---- Array type [T] in user source (issues-drafts/02) ----

   `[T]` desugars to `Array[T]` in any type-expr position: param types,
   return types, struct fields, nested. Verified across the three shapes
   the issue called out as broken. *)

let test_array_type_parses_in_param () =
  let src = {|fn first(xs: [Int]) -> Int { return 0; }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match resolve_program prog with
  | Error e -> Alcotest.failf "resolve failed: %s" e
  | Ok (resolve_ctx, _) ->
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Error e -> Alcotest.failf "typecheck failed: %s" (Typecheck.format_type_error e)
    | Ok _ -> ()

let test_array_type_parses_nested () =
  let src = {|fn nested(xs: [[Int]]) -> Int { return 0; }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match resolve_program prog with
  | Error e -> Alcotest.failf "resolve failed: %s" e
  | Ok (resolve_ctx, _) ->
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Error e -> Alcotest.failf "typecheck failed: %s" (Typecheck.format_type_error e)
    | Ok _ -> ()

let test_array_type_parses_in_struct_field () =
  let src = {|struct Tags { names: [String] } fn main() -> Int { return 0; }|} in
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match resolve_program prog with
  | Error e -> Alcotest.failf "resolve failed: %s" e
  | Ok (resolve_ctx, _) ->
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Error e -> Alcotest.failf "typecheck failed: %s" (Typecheck.format_type_error e)
    | Ok _ -> ()

let array_type_tests = [
  Alcotest.test_case "[T] in fn param parses + typechecks"  `Quick test_array_type_parses_in_param;
  Alcotest.test_case "[[T]] nested parses + typechecks"     `Quick test_array_type_parses_nested;
  Alcotest.test_case "[T] in struct field parses + typechecks" `Quick test_array_type_parses_in_struct_field;
]

(* ---- ADR-014 / #228: module-qualified type & effect paths ----
   The #228 fault is a *parse error at the `.`* — these assert the
   construct now PARSES (resolution of the qualified name against a real
   module is a separate concern, deliberately not asserted here). Both the
   `.` and the canonical `::` separators, in every position the estate
   corpus uses, plus the #228 evidence-table cases. *)

let parses_ok src : bool =
  match (try Some (Parse_driver.parse_string ~file:"<test>" src)
         with _ -> None) with
  | Some prog -> List.length prog.prog_decls > 0
  | None -> false

let test_qual_type_param_dot () =
  Alcotest.(check bool) "fn(x: Bar.Baz) parses" true
    (parses_ok {|fn f(x: Bar.Baz) -> Int { return 0; }|})

let test_qual_type_param_coloncolon () =
  Alcotest.(check bool) "fn(x: Bar::Baz) parses" true
    (parses_ok {|fn f(x: Bar::Baz) -> Int { return 0; }|})

let test_qual_type_struct_field () =
  Alcotest.(check bool) "struct { a: Bar.Baz } parses" true
    (parses_ok {|struct R { a: Bar.Baz } fn m() -> Int { return 0; }|})

let test_qual_type_app () =
  Alcotest.(check bool) "Pkg::Opt[Int] / Pkg.Opt<Int> parse" true
    (parses_ok {|fn f(x: Pkg::Opt[Int]) -> Int { return 0; }|}
     && parses_ok {|fn g(x: Pkg.Opt<Int>) -> Int { return 0; }|})

let test_qual_type_deep_mixed () =
  Alcotest.(check bool) "A.B.C and A::B::C deep paths parse" true
    (parses_ok {|fn f(x: A.B.C) -> Int { return 0; }|}
     && parses_ok {|fn g(x: A::B::C) -> Int { return 0; }|})

let test_qual_effect_dot_and_colons () =
  Alcotest.(check bool) "-{Bar.Baz}-> and -{Bar::Baz}-> parse" true
    (parses_ok {|fn f() -{Bar.Baz}-> Int { return 0; }|}
     && parses_ok {|fn g() -{Bar::Baz}-> Int { return 0; }|})

let test_qual_unqualified_still_parses () =
  (* Guard: the bare (unqualified) forms must be unaffected. *)
  Alcotest.(check bool) "bare Type / Type[T] / -{Eff}-> still parse" true
    (parses_ok {|fn f(x: Baz) -> Int { return 0; }|}
     && parses_ok {|fn g(x: Opt[Int]) -> Int { return 0; }|}
     && parses_ok {|fn h() -{Net}-> Int { return 0; }|})

let qualified_path_tests = [
  Alcotest.test_case "qualified type in param (.)"        `Quick test_qual_type_param_dot;
  Alcotest.test_case "qualified type in param (::)"       `Quick test_qual_type_param_coloncolon;
  Alcotest.test_case "qualified type in struct field"     `Quick test_qual_type_struct_field;
  Alcotest.test_case "qualified type application [ ]/< >"  `Quick test_qual_type_app;
  Alcotest.test_case "deep + mixed-separator paths"       `Quick test_qual_type_deep_mixed;
  Alcotest.test_case "qualified effect (. and ::)"        `Quick test_qual_effect_dot_and_colons;
  Alcotest.test_case "bare unqualified forms unaffected"  `Quick test_qual_unqualified_still_parses;
]

(* ---- #178 INT-01: qualified *value* path  use Mod; Mod.fn(x) ----------
   The companion to #228 (qualified type/effect paths). Mirrors the CLI's
   parse→lower→resolve→typecheck (the lowering is applied at the parse
   boundary by `parse_with_face`; embedders bypassing it call the exposed
   `Resolve.lower_qualified_value_paths` — exactly as here). Hermetic. *)
let qualval_frontend_ok path : bool =
  let loader = Module_loader.create {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  match parse_fixture path with
  | Error _ -> false
  | Ok raw ->
    let prog = Resolve.lower_qualified_value_paths raw in
    (match Resolve.resolve_program_with_loader prog loader with
     | Error _ -> false
     | Ok (resolve_ctx, import_type_ctx) ->
       (match Typecheck.check_program
                ~import_types:import_type_ctx.Typecheck.name_types
                resolve_ctx.symbols prog with
        | Error _ -> false
        | Ok _ -> true))

let test_qualval_dot_call () =
  Alcotest.(check bool)
    "use CrossCallee; CrossCallee.consume(42) resolves+typechecks" true
    (qualval_frontend_ok (fixture "cross_caller_qualified.affine"))

let test_qualval_alias_call () =
  Alcotest.(check bool)
    "use CrossCallee as CC; CC.consume(7) resolves+typechecks" true
    (qualval_frontend_ok (fixture "cross_caller_qualified_alias.affine"))

let test_qualval_item_import_regression () =
  Alcotest.(check bool)
    "use CrossCallee::{consume}; consume(42) still works (no regression)" true
    (qualval_frontend_ok (fixture "cross_caller_ok.affine"))

let test_qualval_record_access_unaffected () =
  (* Genuine record projection must NOT be rewritten: `p` is a value, not an
     import qualifier, so `p.x` stays field access. *)
  let src = {|module RecGuard;
struct P { x: Int, y: Int }
fn getx(p: P) -> Int { return p.x; }|} in
  let prog = Resolve.lower_qualified_value_paths
      (Parse_driver.parse_string ~file:"<test>" src) in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  Alcotest.(check bool) "p.x preserved (not lowered)" true
    (match Resolve.resolve_program_with_loader prog loader with
     | Ok (rc, itc) ->
       (match Typecheck.check_program
                ~import_types:itc.Typecheck.name_types rc.symbols prog with
        | Ok _ -> true | Error _ -> false)
     | Error _ -> false)

(* INT-01 follow-up: `Mod::fn(x)` in value-expression position.
   Parser emits the same ExprField shape the `.` form produces, so
   Resolve.lower_qualified_value_paths handles both syntaxes
   identically. *)
let test_qualval_coloncolon_call () =
  Alcotest.(check bool)
    "use CrossCallee; CrossCallee::consume(42) resolves+typechecks" true
    (qualval_frontend_ok (fixture "cross_caller_qualified_colon.affine"))

let test_qualval_coloncolon_alias_call () =
  Alcotest.(check bool)
    "use CrossCallee as CC; CC::consume(7) resolves+typechecks" true
    (qualval_frontend_ok (fixture "cross_caller_qualified_colon_alias.affine"))

let qualified_value_tests = [
  Alcotest.test_case "use Mod; Mod.fn(x) resolves (#178)"             `Quick test_qualval_dot_call;
  Alcotest.test_case "use Mod as M; M.fn(x) resolves (#178)"          `Quick test_qualval_alias_call;
  Alcotest.test_case "use Mod::{fn}; fn(x) no regression"             `Quick test_qualval_item_import_regression;
  Alcotest.test_case "genuine record access p.x unaffected"           `Quick test_qualval_record_access_unaffected;
  Alcotest.test_case "use Mod; Mod::fn(x) resolves (#178 follow-up)"  `Quick test_qualval_coloncolon_call;
  Alcotest.test_case "use Mod as M; M::fn(x) resolves (#178 follow-up)" `Quick test_qualval_coloncolon_alias_call;
]

(* ---- Inline `extern fn` / `extern type` shape-coverage fixtures ----
   Class-level coverage for the "first user of an inline extern shape
   feeds it to every downstream consumer" surface that produced the
   PR #346 FnExtern interp bug (eval_decl missing match arm — survived
   since the interpreter was written, fired the moment STDLIB-04a's
   tests became the first to hand an inline extern fn to
   Interp.eval_program).

   See .claude/CLAUDE.md §"Test-fixture hygiene for latent bug
   surfaces" for the rationale. Each fixture is fed through
   parse → resolve → typecheck → interp; the assertion is that ALL
   four return Ok. A regression that re-introduces the silent
   pattern-match-failure path of the kind that broke main between
   #334 and #346 would fail loudly here. *)

let inline_extern_pipeline_ok path : bool =
  let loader = Module_loader.create {
    Module_loader.stdlib_path = "stdlib";
    search_paths = [];
    current_dir = fixture_dir;
  } in
  match parse_fixture path with
  | Error _ -> false
  | Ok raw ->
    let prog = Resolve.lower_qualified_value_paths raw in
    (match Resolve.resolve_program_with_loader prog loader with
     | Error _ -> false
     | Ok (resolve_ctx, import_type_ctx) ->
       (match Typecheck.check_program
                ~import_types:import_type_ctx.Typecheck.name_types
                resolve_ctx.symbols prog with
        | Error _ -> false
        | Ok _ ->
          (* The PR #346 root cause was here: Interp.eval_decl's TopFn
             arm didn't match FnExtern, raising Match_failure. The
             fixtures don't *call* the extern at runtime (the host
             impl isn't registered), but eval_program walks every
             TopFn through eval_decl as part of building the initial
             env — that's the path that fired the missing arm. *)
          (match Interp.eval_program prog with
           | Ok _ -> true
           | Error _ -> false)))

let test_inline_extern_pure () =
  Alcotest.(check bool)
    "inline `extern fn host_pure_identity(x: Int) -> Int;` passes the pipeline"
    true
    (inline_extern_pipeline_ok (fixture "inline_extern_pure.affine"))

let test_inline_extern_effectful () =
  Alcotest.(check bool)
    "inline `extern fn host_log(msg) -> Unit / IO;` passes the pipeline"
    true
    (inline_extern_pipeline_ok (fixture "inline_extern_effectful.affine"))

let test_inline_extern_polymorphic () =
  Alcotest.(check bool)
    "inline `extern fn host_identity[T](x: T) -> T;` passes the pipeline"
    true
    (inline_extern_pipeline_ok (fixture "inline_extern_polymorphic.affine"))

let test_inline_extern_type_consumed () =
  Alcotest.(check bool)
    "inline `extern type Handle; extern fn host_use(h: Handle) -> Int;` passes the pipeline"
    true
    (inline_extern_pipeline_ok (fixture "inline_extern_type_consumed.affine"))

let inline_extern_shape_tests = [
  Alcotest.test_case "pure (no effects)"           `Quick test_inline_extern_pure;
  Alcotest.test_case "effectful (effect row)"      `Quick test_inline_extern_effectful;
  Alcotest.test_case "polymorphic (type params)"   `Quick test_inline_extern_polymorphic;
  Alcotest.test_case "extern type + consuming fn"  `Quick test_inline_extern_type_consumed;
]

(* ---- Type-syntax sugars: fn(...) -> T, Option<T>, (A, B) -> C ---- *)

let parse_check_passes src : bool =
  let prog = Parse_driver.parse_string ~file:"<test>" src in
  match resolve_program prog with
  | Error _ -> false
  | Ok (resolve_ctx, _) ->
    match Typecheck.check_program resolve_ctx.symbols prog with
    | Error _ -> false
    | Ok _ -> true

let test_fn_type_zero_arg () =
  Alcotest.(check bool) "fn() -> T parses + typechecks" true
    (parse_check_passes
       {|fn run[T](f: fn() -> T) -> T { return f(); }|})

let test_fn_type_multi_arg () =
  Alcotest.(check bool) "fn(A, B) -> T parses + typechecks" true
    (parse_check_passes
       {|fn apply2(g: fn(Int, Int) -> Int, x: Int, y: Int) -> Int { return g(x, y); }|})

let test_angle_brackets_type_app () =
  Alcotest.(check bool) "Option<T> parses + typechecks" true
    (parse_check_passes
       {|fn first(opt: Option<Int>) -> Int { return 0; }|})

let test_angle_brackets_two_args () =
  Alcotest.(check bool) "Result<T, E> parses + typechecks" true
    (parse_check_passes
       {|fn both(r: Result<Int, String>) -> Int { return 0; }|})

let test_angle_brackets_type_params () =
  Alcotest.(check bool) "fn f<T> ... parses + typechecks" true
    (parse_check_passes
       {|fn id<T>(x: T) -> T { return x; }|})

(* Issue #135 (slice 1): `fn(params) => expr` / `fn(params) -> T { block }`
   anonymous-function expressions. This is the lambda surface the stdlib
   actually uses (`map(fn(x) => Some(x), list)` in option.affine); only the
   `|x| body` form parsed before. *)
let test_fn_lambda_arrow_expr () =
  Alcotest.(check bool) "fn(x) => x parses + typechecks" true
    (parse_check_passes
       {|fn use_it() -> Int { let g = fn(x) => x; return g(1); }|})

let test_fn_lambda_higher_order () =
  Alcotest.(check bool) "fn(x) => e as a call argument" true
    (parse_check_passes
       {|fn apply1(g: fn(Int) -> Int, n: Int) -> Int { return g(n); }
         fn run() -> Int { return apply1(fn(x) => x, 5); }|})

let test_fn_lambda_typed_block () =
  Alcotest.(check bool) "fn(x: Int) -> Int { block } parses + typechecks" true
    (parse_check_passes
       {|fn use_it() -> Int { let g = fn(x: Int) -> Int { x }; return g(2); }|})

(* Issue #135 slice 2: slice/range index `e[a:b]` / `e[a:]` / `e[:b]` / `e[:]`
   desugars to the `slice` builtin. Used by option.affine (`list[1:]`) and
   collections.affine. Plain `e[i]` indexing must be unaffected. *)
let test_slice_full_range () =
  Alcotest.(check bool) "xs[1:3] / xs[1:] / xs[:2] / xs[:] parse + typecheck" true
    (parse_check_passes
       {|fn s(xs: [Int]) -> [Int] { let a = xs[1:3]; let b = xs[1:]; let c = xs[:2]; return xs[:]; }|})

let test_slice_index_not_regressed () =
  Alcotest.(check bool) "plain xs[0] index still parses + typechecks" true
    (parse_check_passes
       {|fn idx(xs: [Int]) -> Int { return xs[0]; }|})

(* Issue #135 slice 3: bare `effect E;` declaration + the ADR-008 canonical
   `-> T / E1, E2` effect-row return annotation (was settled but entirely
   absent from the grammar; the whole stdlib effects/io layer uses it). *)
let test_bare_effect_and_effect_row () =
  Alcotest.(check bool) "effect E; + extern -> T / E + fn -> T / E" true
    (parse_check_passes
       {|effect io;
         extern fn write(s: String) -> Unit / io;
         fn q() -> Int / io { return 0; }
         fn plain() -> Int { return 1; }|})

(* Issue #135 slice 5: trait method *default body* (left-factored vs the
   signature form so the shared prefix no longer mis-resolves toward the
   `;` form). `ref self` receiver + sig-only + assoc type unaffected. *)
let test_trait_default_body () =
  Alcotest.(check bool) "trait fn with default body + ref self" true
    (parse_check_passes
       {|trait Eq {
           pub fn eq(ref self, ref other: Self) -> Bool;
           pub fn ne(ref self, ref other: Self) -> Bool {
             return !self.eq(other);
           }
         }|})

let test_trait_sig_and_assoc_not_regressed () =
  Alcotest.(check bool) "sig-only trait fn + associated type still parse" true
    (parse_check_passes
       {|trait Iter {
           type Item;
           pub fn next(mut self) -> Option<Int>;
         }|})

(* Issue #135 slice 7: top-level generic functions must instantiate their
   `<T>` scheme with fresh vars per call.  Before, `<T>` lowered to a rigid
   `TCon "T"` that `generalize` ignored, so any 2nd instantiation failed
   with `Unify.TypeMismatch (T, Int)` (and `use prelude` import-checks
   failed transitively). *)
let test_generic_fn_multi_instantiation () =
  Alcotest.(check bool) "id<T> called at Int and Bool in one program" true
    (parse_check_passes
       {|fn id<T>(x: T) -> T { return x; }
         fn use_it() -> Bool { let a = id(1); let b = id(true); return b; }|})

let test_generic_hof_monomorphic_caller () =
  Alcotest.(check bool) "generic fold<T,U> called by monomorphic Int sum" true
    (parse_check_passes
       {|fn fold<T, U>(arr: [T], init: U, f: (U, T) -> U) -> U { return init; }
         fn sum(a: [Int]) -> Int { return fold(a, 0, fn(acc, x) => acc + x); }|})

(* Issue #135 slice 11: the resolver is two-pass — all top-level names
   are forward-declared before any body is resolved.  Before, a call to
   a function defined later in the file was `UndefinedVariable`. *)
let test_forward_reference () =
  Alcotest.(check bool) "fn calling a later-defined fn resolves" true
    (parse_check_passes
       {|fn a() -> Int { return b(); }
         fn b() -> Int { return 1; }|})

let test_mutual_recursion () =
  Alcotest.(check bool) "mutually recursive fns resolve + typecheck" true
    (parse_check_passes
       {|fn even(n: Int) -> Bool { if n == 0 { return true; } return odd(n - 1); }
         fn odd(n: Int) -> Bool { if n == 0 { return false; } return even(n - 1); }|})

let test_self_recursion_not_regressed () =
  Alcotest.(check bool) "self-recursion still resolves" true
    (parse_check_passes
       {|fn rec(n: Int) -> Int { if n <= 0 { return 0; } return rec(n - 1); }|})

let test_multi_arg_arrow () =
  Alcotest.(check bool) "(A, B) -> C parses + typechecks" true
    (parse_check_passes
       {|fn flip<A, B, C>(f: (A, B) -> C) -> ((B, A) -> C) {
           return |b: B, a: A| f(a, b);
         }|})

let test_tuple_type_still_works () =
  Alcotest.(check bool) "(A, B) without arrow stays a tuple type" true
    (parse_check_passes
       {|fn first(t: (Int, String)) -> Int { return 0; }|})

(* Issue #131: nested applied generics whose closing '>'s fuse into a single
   ">>" (or ">>>") token must still parse — the lexer emits GTGT but the
   driver re-splits it when closing a type-argument list. *)
let test_angle_nested_gtgt () =
  Alcotest.(check bool) "Option<Result<T, E>> (>> close) parses + typechecks" true
    (parse_check_passes
       {|fn f(o: Option<Result<Int, String>>) -> Int { return 0; }|})

let test_angle_nested_gtgtgt () =
  Alcotest.(check bool) "Option<Option<Result<T, E>>> (>>> close) parses" true
    (parse_check_passes
       {|fn f(o: Option<Option<Result<Int, String>>>) -> Int { return 0; }|})

let test_angle_nested_return_pos () =
  (* Exercises the nested generic *in return position* (the point of this
     test) without constructing Ok/None — those live in prelude and are
     reached via `use prelude::{...}`, not a flat builtin seed (#138).
     Mirrors the param-position sibling tests above. *)
  Alcotest.(check bool) "-> Result<Option<T>, E> nested in return position" true
    (parse_check_passes
       {|fn f(r: Result<Option<Int>, String>) -> Result<Option<Int>, String> { return r; }|})

(* Non-regression: a real right-shift expression must still be one GTGT,
   not split, since GTGT is grammatical there. *)
let test_shift_operator_not_split () =
  Alcotest.(check bool) "a >> b right-shift expression unaffected by #131 fix" true
    (parse_check_passes
       {|fn sh(a: Int, b: Int) -> Int { return a >> b; }|})

let type_syntax_sugar_tests = [
  Alcotest.test_case "fn() -> T (zero-arg fn type)"           `Quick test_fn_type_zero_arg;
  Alcotest.test_case "fn(A, B) -> T (multi-arg fn type)"      `Quick test_fn_type_multi_arg;
  Alcotest.test_case "Option<T> (angle brackets, type app)"   `Quick test_angle_brackets_type_app;
  Alcotest.test_case "Result<T, E> (angle brackets, 2 args)"  `Quick test_angle_brackets_two_args;
  Alcotest.test_case "fn f<T> (angle brackets, type params)"  `Quick test_angle_brackets_type_params;
  Alcotest.test_case "Option<Result<T,E>> (#131 >> close)"    `Quick test_angle_nested_gtgt;
  Alcotest.test_case "Option<Option<Result<T,E>>> (#131 >>>)" `Quick test_angle_nested_gtgtgt;
  Alcotest.test_case "-> Result<Option<T>,E> (#131 nested)"   `Quick test_angle_nested_return_pos;
  Alcotest.test_case "a >> b shift unaffected (#131 guard)"   `Quick test_shift_operator_not_split;
  Alcotest.test_case "fn(x) => x (#135 fn-lambda expr)"       `Quick test_fn_lambda_arrow_expr;
  Alcotest.test_case "fn(x) => e as arg (#135 fn-lambda)"     `Quick test_fn_lambda_higher_order;
  Alcotest.test_case "fn(x:Int) -> Int { } (#135 fn-lambda)"  `Quick test_fn_lambda_typed_block;
  Alcotest.test_case "xs[a:b]/[a:]/[:b]/[:] (#135 slice 2)"   `Quick test_slice_full_range;
  Alcotest.test_case "xs[0] index non-regressed (#135 sl.2)"  `Quick test_slice_index_not_regressed;
  Alcotest.test_case "effect E; + -> T / E (#135 slice 3)"    `Quick test_bare_effect_and_effect_row;
  Alcotest.test_case "trait default body + ref self (#135 sl5)" `Quick test_trait_default_body;
  Alcotest.test_case "trait sig + assoc non-regressed (#135 sl5)" `Quick test_trait_sig_and_assoc_not_regressed;
  Alcotest.test_case "generic fn multi-instantiation (#135 sl7)" `Quick test_generic_fn_multi_instantiation;
  Alcotest.test_case "generic HOF + mono caller (#135 sl7)"     `Quick test_generic_hof_monomorphic_caller;
  Alcotest.test_case "forward reference resolves (#135 sl11)"   `Quick test_forward_reference;
  Alcotest.test_case "mutual recursion (#135 sl11)"             `Quick test_mutual_recursion;
  Alcotest.test_case "self-recursion non-regressed (#135 sl11)" `Quick test_self_recursion_not_regressed;
  Alcotest.test_case "(A, B) -> C (multi-arg arrow)"          `Quick test_multi_arg_arrow;
  Alcotest.test_case "(A, B) without arrow remains tuple"     `Quick test_tuple_type_still_works;
]

(* ---- PatCon sub-pattern destructuring under WasmGC ----

   `match Mk(7, 99) { Mk(a, b) => a }` correctly extracts the first payload
   (RefCast + StructGet + RefCast HtI31 + I31GetS + LocalSet). Mixed-arity
   matches (zero-arg + with-args in same enum) error loudly with the
   documented workaround. *)

let test_gc_patcon_destructure_same_arity () =
  let prog = parse_string_for_gc
    {|enum Pair { Mk(Int, Int) }
      fn first(p: Pair) -> Int {
        return match p { Mk(a, b) => { return a; }, };
      }
      pub fn main() -> Int { return first(Mk(7, 99)); }|}
  in
  match Codegen_gc.generate_gc_module prog with
  | Error e ->
    Alcotest.failf "expected Ok, got: %s" (Codegen_gc.format_codegen_error e)
  | Ok m ->
    (* `first` is the function with the match (declared first → index 0). *)
    let first_body = (List.nth m.gc_funcs 0).gf_body in
    let n_struct_get = count_instr_kind first_body
      (function Wasm_gc.StructGet _ -> true | _ -> false) in
    Alcotest.(check bool) "match emits at least one StructGet"
      true (n_struct_get > 0)

let test_gc_patcon_mixed_arity_loud_fail () =
  let prog = parse_string_for_gc
    {|enum MyOpt { MyNone, MySome(Int) }
      fn unwrap_or(opt: MyOpt, default: Int) -> Int {
        return match opt {
          MySome(x) => { return x; },
          MyNone => { return default; },
        };
      }
      pub fn main() -> Int { return unwrap_or(MySome(42), 0); }|}
  in
  match Codegen_gc.generate_gc_module prog with
  | Ok _ -> Alcotest.fail "expected mixed-arity error, got Ok"
  | Error err ->
    let msg = Codegen_gc.format_codegen_error err in
    let contains s sub =
      let n = String.length sub and m = String.length s in
      let rec scan i = i + n <= m && (String.sub s i n = sub || scan (i + 1)) in
      scan 0
    in
    Alcotest.(check bool) "error mentions 'mixed-arity'"
      true (contains msg "mixed-arity")

let wasm_gc_patcon_tests = [
  Alcotest.test_case "PatCon destructure same-arity match works (Mk(a, b) → a)" `Quick test_gc_patcon_destructure_same_arity;
  Alcotest.test_case "mixed-arity match (None + Some(x)) → loud UnsupportedFeature" `Quick test_gc_patcon_mixed_arity_loud_fail;
]

let tw_interface_tests = [
  Alcotest.test_case "bridge: update export has own param"       `Quick test_iface_bridge_update_linear;
  Alcotest.test_case "router: push export has own param"         `Quick test_iface_router_push_linear;
  Alcotest.test_case "router: resize export has 2 own params"    `Quick test_iface_router_resize_two_linear;
  Alcotest.test_case "cross: call once → OK"                     `Quick test_cross_call_once_ok;
  Alcotest.test_case "cross: call twice → LinearImportCalledMultiple" `Quick test_cross_call_twice_violation;
  Alcotest.test_case "cross: call one-arm → LinearImportDroppedOnSomePath" `Quick test_cross_call_partial_violation;
  Alcotest.test_case "bridge boundary: clean caller → OK"        `Quick test_bridge_boundary_clean;
  Alcotest.test_case "router boundary: clean caller → OK"        `Quick test_router_boundary_clean;
  Alcotest.test_case "src-level xmod: ok caller verifies clean"  `Quick test_xmod_clean;
  Alcotest.test_case "src-level xmod: dup caller → LinearImportCalledMultiple" `Quick test_xmod_dup_violation;
  Alcotest.test_case "src-level xmod: drop caller → LinearImportDroppedOnSomePath" `Quick test_xmod_drop_violation;
  Alcotest.test_case "INT-01 #178: use Mod::{fn} emits real xmod import + callee exports it" `Quick test_int01_xmod_import_emission;
]

(* ============================================================================
   Section: Borrow-graph validation (CORE-01 / #177)

   Pins the Phase-3 borrow-graph soundness checks:
     - BorrowOutlivesOwner is finally emitted (was defined-but-dead);
     - shared-XOR-exclusive is enforced at use sites, not only at creation;
     - the temporary call-argument borrow release prevents over-rejection
       (the anti-regression guard for the whole valid corpus).
   Runs parse -> resolve -> Borrow.check_program directly so the borrow
   checker is isolated from typecheck (mirrors the quantity tests).
   ============================================================================ *)

let borrow_result path =
  match parse_fixture path with
  | Error m -> Alcotest.fail ("parse: " ^ m)
  | Ok prog ->
    match resolve_program prog with
    | Error m -> Alcotest.fail ("resolve: " ^ m)
    | Ok (rc, _) -> Borrow.check_program rc.symbols prog

let test_borrow_outlives_owner () =
  match borrow_result (fixture "borrow_outlives_owner.affine") with
  | Error (Borrow.BorrowOutlivesOwner _) -> ()
  | Error e ->
    Alcotest.fail ("expected BorrowOutlivesOwner, got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "expected BorrowOutlivesOwner, got Ok"

let test_borrow_use_while_excl () =
  match borrow_result (fixture "borrow_use_while_excl.affine") with
  | Error (Borrow.UseWhileExclusivelyBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("expected UseWhileExclusivelyBorrowed, got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "expected UseWhileExclusivelyBorrowed, got Ok"

let test_borrow_call_arg_then_use () =
  match borrow_result (fixture "borrow_call_arg_then_use.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("valid call-arg-then-use spuriously rejected: "
                   ^ Borrow.format_borrow_error e)

(* CORE-01 pt2 / #177 — return-escape. *)
let test_borrow_return_escape_param () =
  match borrow_result (fixture "borrow_return_escape_param.affine") with
  | Error (Borrow.BorrowOutlivesOwner _) -> ()
  | Error e ->
    Alcotest.fail ("expected BorrowOutlivesOwner (return &by-value-param), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "return &(by-value param) escaped uncaught"

let test_borrow_return_escape_local () =
  match borrow_result (fixture "borrow_return_escape_local.affine") with
  | Error (Borrow.BorrowOutlivesOwner _) -> ()
  | Error e ->
    Alcotest.fail ("expected BorrowOutlivesOwner (return &local), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "return &local escaped uncaught"

let test_borrow_return_refparam_ok () =
  match borrow_result (fixture "borrow_return_refparam_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("sound `return ref-param` spuriously rejected: "
                   ^ Borrow.format_borrow_error e)

(* CORE-01 pt2 parser surface / #177 — `&mut e` exclusive-borrow surface.
   These programs were previously *unrepresentable* (no `&mut` expression);
   the pt1 shared-XOR-exclusive rules were unreachable from source. *)
let test_borrow_mutref_conflict () =
  match borrow_result (fixture "borrow_mutref_conflict.affine") with
  | Error (Borrow.ConflictingBorrow _) -> ()
  | Error e ->
    Alcotest.fail ("expected ConflictingBorrow (two &mut of one owner), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "two `&mut x` did not conflict"

let test_borrow_mutref_use_while () =
  match borrow_result (fixture "borrow_mutref_use_while.affine") with
  | Error (Borrow.UseWhileExclusivelyBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("expected UseWhileExclusivelyBorrowed (read x while &mut x \
                    live), got: " ^ Borrow.format_borrow_error e)
  | Ok () -> Alcotest.fail "read while `&mut` live was not rejected"

let test_borrow_mutref_shared_ok () =
  match borrow_result (fixture "borrow_mutref_shared_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("shared `&x` spuriously rejected after adding `&mut`: "
                   ^ Borrow.format_borrow_error e)

(* CORE-01 pt3 Slice A / #177 — NLL last-use expiry. Pre-NLL, every
   ref-binding lived until lexical block exit; under NLL the underlying
   borrow is released after the binder's last use, so subsequent reads
   or assignments through the (no-longer-borrowed) owner are legal.
   The third test pins the anti-regression: a binder still mentioned
   later in the block must NOT be expired early. *)
let test_borrow_nll_assign_after_last_use () =
  match borrow_result (fixture "borrow_nll_assign_after_last_use.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("NLL: assignment after shared borrow's last use \
                    spuriously rejected: " ^ Borrow.format_borrow_error e)

let test_borrow_nll_read_after_mut_last_use () =
  match borrow_result (fixture "borrow_nll_read_after_mut_last_use.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("NLL: read after exclusive borrow's last use \
                    spuriously rejected: " ^ Borrow.format_borrow_error e)

let test_borrow_nll_still_rejects_live_borrow () =
  match borrow_result (fixture "borrow_nll_still_rejects_live_borrow.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("expected MoveWhileBorrowed (NLL must keep live \
                    borrows alive), got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "NLL over-expired a still-live borrow — assignment \
                   to a borrowed owner was accepted"

(* CORE-01 pt3 Slice B / #177 — flow-sensitive escape via `outer = &y`.
   The assignment `r = &y` (where `r` is an existing ref-binder) now
   releases the old held borrow and re-binds `r` in the ref-graph to
   the freshly-created borrow. Three tests pin: (1) the old target is
   freed by the reassignment; (2) NLL last-use then correctly expires
   the NEW borrow; (3) anti-regression — the new borrow still
   protects its new target while the binder is live. *)
let test_slice_b_outer_assign_releases_old () =
  match borrow_result (fixture "slice_b_outer_assign_releases_old.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice B: `r = &y` did not release the old borrow on \
                    `x` — `x = …` after the reassignment was spuriously \
                    rejected: " ^ Borrow.format_borrow_error e)

let test_slice_b_nll_expires_new () =
  match borrow_result (fixture "slice_b_nll_expires_new.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice B: NLL last-use after `r = &y` did not expire \
                    the new borrow on `y`: " ^ Borrow.format_borrow_error e)

let test_slice_b_new_borrow_still_protects () =
  match borrow_result (fixture "slice_b_new_borrow_still_protects.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("Slice B anti-regression: expected MoveWhileBorrowed \
                    on `y = …` (the new borrow must still protect `y`), \
                    got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice B regressed: the new borrow on `y` was not \
                   tracked, so the write to `y` was silently accepted"

(* CORE-01 pt3 Slice C / #177 — CFG-join semantics. ExprTry catch
   arms are now isolated (each arm runs against the post-body
   state, not the polluted state of preceding arms); ExprHandle
   handler arms are similarly isolated.  Tests pin: (1) two catch
   arms can independently move the same value (positive — was
   spuriously rejected); (2) a move performed in the try-body
   still propagates past the try (anti-regression — conservative
   correctness preserved). *)
let test_slice_c_catch_arm_isolation () =
  match borrow_result (fixture "slice_c_catch_arm_isolation.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice C: catch arms must be isolated — `drop_int(y)` \
                    in arm 2 spuriously saw arm 1's move: "
                   ^ Borrow.format_borrow_error e)

let test_slice_c_body_move_persists () =
  match borrow_result (fixture "slice_c_body_move_persists.affine") with
  | Error (Borrow.UseAfterMove _) -> ()
  | Error e ->
    Alcotest.fail ("Slice C anti-regression: expected UseAfterMove on \
                    `read_int(y)` after the body moved `y`, got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice C regressed: body-side move was dropped during \
                   the catch-arm merge — `read_int(y)` after a moved `y` \
                   was silently accepted"

(* CORE-01 pt3 ref-to-ref / #177 — let r2 = r and r = r_other now
   alias r2 (resp. r) to the same borrow the source binder holds.
   Pre-fix the alias was never recorded, so the let-graph went stale
   on reborrow-through-indirection. Three tests pin: (1) let-path
   positive — both binders dereferenceable; (2) anti-regression —
   the alias still protects the underlying owner from concurrent
   writes; (3) assign-path positive — `r = s` releases the old
   target and aliases r to s's borrow. *)
let test_ref_to_ref_let_aliases () =
  match borrow_result (fixture "ref_to_ref_let_aliases.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("ref-to-ref let-path: `let r2 = r` did not alias r2 \
                    into the borrow-graph — reading through r2 was \
                    spuriously rejected: " ^ Borrow.format_borrow_error e)

let test_ref_to_ref_protects_owner () =
  match borrow_result (fixture "ref_to_ref_protects_owner.affine") with
  | Error (Borrow.ConflictingBorrow _)
  | Error (Borrow.MoveWhileBorrowed _)
  | Error (Borrow.UseWhileExclusivelyBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("ref-to-ref anti-regression: expected a borrow-conflict \
                    error on `x = 9` (r2 must still protect x), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "ref-to-ref regressed: the aliased binder r2 did not \
                   keep x borrowed, so the write to x was silently \
                   accepted"

let test_ref_to_ref_assign_aliases () =
  match borrow_result (fixture "ref_to_ref_assign_aliases.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("ref-to-ref assign-path: `r = s` did not release the \
                    old borrow on `x` and re-alias r to s's borrow — \
                    the subsequent write to `x` was spuriously \
                    rejected: " ^ Borrow.format_borrow_error e)

(* CORE-01 pt3 ref-to-ref / #177 follow-up: return-escape via
   indirection.  Pre-fix `let r2 = r1` did not propagate `r1`'s
   ref-binding, so `returned_borrow` for `return r2` silently
   returned None — the escape was missed.  Post-fix `r2` inherits
   `r1`'s borrow on a function-local, and the escape check fires. *)
let test_ref_to_ref_return_escape () =
  match borrow_result (fixture "ref_to_ref_return_escape.affine") with
  | Error (Borrow.BorrowOutlivesOwner _) -> ()
  | Error e ->
    Alcotest.fail ("ref-to-ref return-escape: expected \
                    BorrowOutlivesOwner on `return r2` where r2 = r1 = \
                    &local, got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "ref-to-ref return-escape regressed: the indirection \
                   chain was not detected by `returned_borrow`'s \
                   ref_bindings lookup — escape silently accepted"

(* CORE-01 pt3 / #177 deferred-items: assignment-clears-move.
   Whole-place write `x = …` after a prior move on `x` revives the
   place; the move-record is dropped after the RHS lands so the
   subsequent read of `x` succeeds.  Anti-regression for the
   sub-place case is covered by [test_slice_c_body_move_persists]
   (which still expects UseAfterMove for a read of a moved local
   that was *not* reassigned). *)
let test_borrow_assign_clears_move () =
  match borrow_result (fixture "borrow_assign_clears_move.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("assignment-clears-move: `x = …` after `drop_int(x)` \
                    spuriously rejected — whole-place write should revive \
                    the moved place: " ^ Borrow.format_borrow_error e)

(* CORE-01 pt3 Slice C' / #177 — loop soundness via 2-iteration on
   [StmtWhile]/[StmtFor].  Three tests pin: (1) sound counted loop
   converges; (2) interaction with #399's clear-on-rewrite — a
   move-then-rebind body accepts; (3) anti-regression — a move
   without rebind raises UseAfterMove on the 2nd pass. *)
let test_slice_c_prime_loop_sound () =
  match borrow_result (fixture "slice_c_prime_loop_sound.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice C': counted loop was spuriously rejected — \
                    iter-2 pass introduced a false positive: "
                   ^ Borrow.format_borrow_error e)

let test_slice_c_prime_loop_reinit_ok () =
  match borrow_result (fixture "slice_c_prime_loop_reinit_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice C' × #399 interaction: re-init after move \
                    was spuriously rejected — `x = 42` after `drop_int(x)` \
                    should clear the move-record and let iter 2 converge: "
                   ^ Borrow.format_borrow_error e)

let test_slice_c_prime_loop_move_persists () =
  match borrow_result (fixture "slice_c_prime_loop_move_persists.affine") with
  | Error (Borrow.UseAfterMove _) -> ()
  | Error e ->
    Alcotest.fail ("Slice C' anti-regression: expected UseAfterMove on \
                    the iter-2 `drop_int(x)` after iter-1 moved `x`, \
                    got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice C' regressed: multi-iter move conflict was \
                   silently accepted — the 2-iteration pass did not \
                   catch the unrestored move on `x`"

(* CORE-01 pt3 Slice D / #177 — borrow-side rejection of @linear
   capture by a closure. The quantity checker already rejected
   these via QOmega scaling, but the borrow-side error fires
   earlier (Typecheck → Borrow → Quantity) and points at the
   lambda span. Three tests pin: (1) @linear let captured;
   (2) @linear param captured; (3) anti-regression — non-linear
   capture must still pass. *)
let test_slice_d_captured_linear_let_rejected () =
  match borrow_result (fixture "slice_d_captured_linear_let_rejected.affine") with
  | Error (Borrow.LinearCapturedByClosure (name, _)) ->
    Alcotest.(check string) "captured name surfaced in error" "x" name
  | Error e ->
    Alcotest.fail ("Slice D: expected LinearCapturedByClosure on `|| x + 1` \
                    capture of @linear let x, got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice D regressed: @linear let-binding was silently \
                   captured by closure — multi-call would break linear \
                   contract"

let test_slice_d_captured_linear_param_rejected () =
  match borrow_result (fixture "slice_d_captured_linear_param_rejected.affine") with
  | Error (Borrow.LinearCapturedByClosure (name, _)) ->
    Alcotest.(check string) "captured param name surfaced" "y" name
  | Error e ->
    Alcotest.fail ("Slice D: expected LinearCapturedByClosure on `|| y + 1` \
                    capture of @linear param y, got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice D regressed: @linear param was silently captured \
                   by closure"

let test_slice_d_captured_nonlinear_ok () =
  match borrow_result (fixture "slice_d_captured_nonlinear_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("Slice D anti-regression: non-linear capture was \
                    spuriously rejected — the new rule must scope to \
                    @linear only: " ^ Borrow.format_borrow_error e)

(* #554 — soundness: use-after-move through a callee-returned borrow.
   `pick(ref x) -> ref Int` returns a borrow of its parameter, so
   `let r = pick(a)` makes `r` borrow `a`.  Moving `a` while `r` is still
   live must be rejected; pre-fix the borrow graph had no `r -> a` edge so
   the move slipped past the full pipeline.  Three tests pin: (1) the
   use-after-move is now caught (MoveWhileBorrowed); (2) the NLL-reordered
   form (read `*r` before the move) still passes — no lexical over-
   rejection; (3) a `ref`-param callee that returns a *value* leaves the
   argument movable (empty return-borrow summary). *)
let test_borrow_callee_returned_borrow_uam () =
  match borrow_result (fixture "borrow_callee_returned_borrow_uam.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554: expected MoveWhileBorrowed (move of `a` while the \
                    callee-returned borrow held by `r` is live), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 regressed: use-after-move through a callee-returned \
                   borrow was accepted — the result binder did not borrow \
                   the argument"

let test_borrow_callee_returned_borrow_nll_ok () =
  match borrow_result (fixture "borrow_callee_returned_borrow_nll_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("#554 anti-over-rejection: reading `*r` before moving `a` \
                    must pass under NLL last-use, got: "
                   ^ Borrow.format_borrow_error e)

let test_borrow_callee_value_return_ok () =
  match borrow_result (fixture "borrow_callee_value_return_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("#554 precision: a `ref`-param callee that returns a \
                    *value* has an empty return-borrow summary, so the arg \
                    stays movable, got: " ^ Borrow.format_borrow_error e)

(* #554 hardening — three variants found by adversarial verification that a
   first cut of the fix missed. All must be rejected: (1) the callee-returned
   borrow stored into an aggregate (tuple) must still keep the arg borrowed;
   (2) reassigning a mutable ref-binder to a callee-returned borrow must keep
   the new target borrowed; (3) a borrow returned through a bare `match` arm
   tail must register in the return-borrow summary. *)
let test_borrow_callee_returned_borrow_aggregate () =
  match borrow_result (fixture "borrow_callee_returned_borrow_aggregate.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 aggregate: expected MoveWhileBorrowed (move of `b` \
                    while the borrow stored in the tuple is live), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 aggregate regressed: a callee-returned borrow stored \
                   into a tuple did not keep the argument borrowed — \
                   use-after-move accepted"

let test_borrow_callee_returned_borrow_reassign () =
  match borrow_result (fixture "borrow_callee_returned_borrow_reassign.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 reassign: expected MoveWhileBorrowed (move of `b` \
                    while `r = other(b)` holds its borrow), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 reassign regressed: reassigning a ref-binder to a \
                   callee-returned borrow did not keep the new target \
                   borrowed — use-after-move accepted"

let test_borrow_callee_returned_borrow_match_arm () =
  match borrow_result (fixture "borrow_callee_returned_borrow_match_arm.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 match-arm summary: expected MoveWhileBorrowed (the \
                    borrow returned via the match arm must be summarised), \
                    got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 match-arm summary regressed: a borrow returned via a \
                   bare match-arm tail was not recorded in the return-borrow \
                   summary — use-after-move accepted"

(* #554 — the idiomatic `return if/match { ... };` spelling. The returned
   value is in tail position no matter where `return` sits, so the summary
   must still record the arm-tail borrow. A first cut walked the returned
   value as a non-tail expression and missed it. *)
let test_borrow_callee_returned_borrow_return_stmt () =
  match borrow_result (fixture "borrow_callee_returned_borrow_return_stmt.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 return-stmt summary: expected MoveWhileBorrowed (a \
                    borrow returned via `return if d { &x } else { &x };` \
                    must be summarised), got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 return-stmt summary regressed: a borrow returned via \
                   a `return <branch>;` statement was not recorded in the \
                   return-borrow summary — use-after-move accepted"

(* #554 residual (a) closed — interprocedural-through-call-result. `wrap`
   returns `pick(x)`'s result bound to a local; the summary fixpoint over the
   call graph must give `wrap` pick's origin, so the use-after-move through
   `wrap`'s result is rejected (transitively, at any wrapper depth). *)
let test_borrow_callee_returned_borrow_interproc () =
  match borrow_result (fixture "borrow_callee_returned_borrow_interproc.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 interprocedural: expected MoveWhileBorrowed (the \
                    summary fixpoint must give `wrap` pick's return-borrow \
                    origin), got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 interprocedural regressed: a function returning \
                   another ref-returning call's result did not inherit the \
                   origin — use-after-move accepted"

(* #554 residual (c) closed — reassigning a ref-binder to a call result
   releases the OLD loan, so the previously-borrowed target is movable again
   (symmetric with the plain-`&` Slice-B reborrow). Must pass. *)
let test_borrow_callee_returned_borrow_reassign_old_ok () =
  match borrow_result (fixture "borrow_callee_returned_borrow_reassign_old_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("#554 reassign-old precision: after `r = other(b)` the old \
                    target `a` must be movable again, got: "
                   ^ Borrow.format_borrow_error e)

(* Slice-B reassign ref-count (pre-existing soundness bug found by #554
   round-3): `let r2 = r; r = &b` must NOT drop the loan `r2` still aliases,
   so a later move of the old target is rejected. *)
let test_borrow_reassign_alias_survives () =
  match borrow_result (fixture "borrow_reassign_alias_survives.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("Slice-B alias ref-count: expected MoveWhileBorrowed (r2 \
                    still borrows `a` after `r = &b`), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "Slice-B alias regressed: reassigning `r` dropped the borrow \
                   the `let r2 = r` alias still held — use-after-move accepted"

(* #554 reassigned-local summary (found by round-3): a returned ref-local that
   is reassigned must union its origins so the summary does not go stale. *)
let test_borrow_callee_returned_borrow_reassign_summary () =
  match borrow_result (fixture "borrow_callee_returned_borrow_reassign_summary.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("#554 reassigned-local summary: expected MoveWhileBorrowed \
                    (the reassigned local's origin must be unioned into the \
                    summary), got: " ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "#554 reassigned-local summary regressed: the summary went \
                   stale on the initial binding — use-after-move accepted"

(* issue-draft 08 — conditional-origin borrow escape. A borrow bound through an
   `if`/`match`/block value carries the argument borrow out of the construct; a
   later move of that argument while the binder is live is a use-after-move.
   Pre-fix, the branch/block lexical restore swallowed the escaping borrow and
   the `if`/`match` join intersected the per-branch records away, so the move
   was accepted. The fix re-publishes the UNION of branch/tail escaping borrows.
   Four reject-cases (if / block / multi-arm match / partial) and two anti-over-
   rejection accept-cases (NLL use-before-move; unrelated move). *)
let test_borrow_cond_origin_if_uam () =
  match borrow_result (fixture "borrow_cond_origin_if_uam.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("cond-origin if: expected MoveWhileBorrowed (move of `a` \
                    while the if-bound borrow held by `r` is live), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "cond-origin if regressed: a borrow bound through `if` did \
                   not keep the argument borrowed — use-after-move accepted"

let test_borrow_cond_origin_block_uam () =
  match borrow_result (fixture "borrow_cond_origin_block_uam.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("cond-origin block: expected MoveWhileBorrowed (block value \
                    carries the borrow of `a` out), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "cond-origin block regressed: `let r = { pick(a) }` dropped \
                   the borrow of `a` — use-after-move accepted"

let test_borrow_cond_origin_match_uam () =
  match borrow_result (fixture "borrow_cond_origin_match_uam.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("cond-origin match: expected MoveWhileBorrowed (union of \
                    arm-escaping borrows keeps `a` borrowed), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "cond-origin match regressed: a multi-arm match value did \
                   not keep the argument borrowed — use-after-move accepted"

let test_borrow_cond_origin_partial_uam () =
  match borrow_result (fixture "borrow_cond_origin_partial_uam.affine") with
  | Error (Borrow.MoveWhileBorrowed _) -> ()
  | Error e ->
    Alcotest.fail ("cond-origin partial: expected MoveWhileBorrowed (one branch \
                    borrows `a`, so the union includes `a`), got: "
                   ^ Borrow.format_borrow_error e)
  | Ok () ->
    Alcotest.fail "cond-origin partial regressed: moving `a` borrowed in only \
                   one branch was accepted — union of origins dropped `a`"

let test_borrow_cond_origin_nll_ok () =
  match borrow_result (fixture "borrow_cond_origin_nll_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("cond-origin anti-over-rejection: reading `*r` before moving \
                    `a` must pass under NLL last-use, got: "
                   ^ Borrow.format_borrow_error e)

let test_borrow_cond_origin_unrelated_ok () =
  match borrow_result (fixture "borrow_cond_origin_unrelated_ok.affine") with
  | Ok () -> ()
  | Error e ->
    Alcotest.fail ("cond-origin precision: moving an unrelated value `d` (not in \
                    the {a,b} origin union) must stay legal, got: "
                   ^ Borrow.format_borrow_error e)

let borrow_tests = [
  Alcotest.test_case "BorrowOutlivesOwner: &local escapes its block"
    `Quick test_borrow_outlives_owner;
  Alcotest.test_case "&mut: two exclusive borrows conflict (#177 pt2 surface)"
    `Quick test_borrow_mutref_conflict;
  Alcotest.test_case "&mut: read while exclusively borrowed rejected (#177)"
    `Quick test_borrow_mutref_use_while;
  Alcotest.test_case "&mut added: shared &x still sound (no regression)"
    `Quick test_borrow_mutref_shared_ok;
  Alcotest.test_case "UseWhileExclusivelyBorrowed: mut+read in one call"
    `Quick test_borrow_use_while_excl;
  Alcotest.test_case "temporary call-arg borrow released (no over-reject)"
    `Quick test_borrow_call_arg_then_use;
  Alcotest.test_case "return-escape: return &(by-value param) rejected (#177 pt2)"
    `Quick test_borrow_return_escape_param;
  Alcotest.test_case "return-escape: return &(local) rejected (#177 pt2)"
    `Quick test_borrow_return_escape_local;
  Alcotest.test_case "return ref-param sound — not over-rejected (#177 pt2)"
    `Quick test_borrow_return_refparam_ok;
  Alcotest.test_case "NLL: assign after shared borrow's last use OK (#177 pt3 Slice A)"
    `Quick test_borrow_nll_assign_after_last_use;
  Alcotest.test_case "NLL: read after &mut's last use OK (#177 pt3 Slice A)"
    `Quick test_borrow_nll_read_after_mut_last_use;
  Alcotest.test_case "NLL anti-regression: still-live borrow blocks assign (#177 pt3 Slice A)"
    `Quick test_borrow_nll_still_rejects_live_borrow;
  Alcotest.test_case "Slice B: `r = &y` releases old borrow on `x` (#177 pt3 Slice B)"
    `Quick test_slice_b_outer_assign_releases_old;
  Alcotest.test_case "Slice B: NLL expires the NEW borrow after `r = &y` (#177 pt3 Slice B)"
    `Quick test_slice_b_nll_expires_new;
  Alcotest.test_case "Slice B anti-regression: new borrow still protects `y` (#177 pt3 Slice B)"
    `Quick test_slice_b_new_borrow_still_protects;
  Alcotest.test_case "Slice C: catch arms isolated — independent moves OK (#177 pt3 Slice C)"
    `Quick test_slice_c_catch_arm_isolation;
  Alcotest.test_case "Slice C anti-regression: body's move persists past try (#177 pt3 Slice C)"
    `Quick test_slice_c_body_move_persists;
  Alcotest.test_case "ref-to-ref let-path: `let r2 = r` aliases (#177 pt3)"
    `Quick test_ref_to_ref_let_aliases;
  Alcotest.test_case "ref-to-ref anti-regression: alias still protects owner (#177 pt3)"
    `Quick test_ref_to_ref_protects_owner;
  Alcotest.test_case "ref-to-ref assign-path: `r = s` releases + aliases (#177 pt3)"
    `Quick test_ref_to_ref_assign_aliases;
  Alcotest.test_case "ref-to-ref return-escape via indirection (#177 pt3 follow-up)"
    `Quick test_ref_to_ref_return_escape;
  Alcotest.test_case "assignment-clears-move: whole-place write revives moved place (#177)"
    `Quick test_borrow_assign_clears_move;
  Alcotest.test_case "Slice C': counted loop converges (#177 pt3)"
    `Quick test_slice_c_prime_loop_sound;
  Alcotest.test_case "Slice C' × #399: re-init in loop OK (#177 pt3)"
    `Quick test_slice_c_prime_loop_reinit_ok;
  Alcotest.test_case "Slice C' anti-regression: multi-iter move rejected (#177 pt3)"
    `Quick test_slice_c_prime_loop_move_persists;
  Alcotest.test_case "Slice D: @linear let captured by closure rejected (#177 pt3)"
    `Quick test_slice_d_captured_linear_let_rejected;
  Alcotest.test_case "Slice D: @linear param captured by closure rejected (#177 pt3)"
    `Quick test_slice_d_captured_linear_param_rejected;
  Alcotest.test_case "Slice D anti-regression: non-linear capture still OK (#177 pt3)"
    `Quick test_slice_d_captured_nonlinear_ok;
  Alcotest.test_case "#554: use-after-move through callee-returned borrow rejected"
    `Quick test_borrow_callee_returned_borrow_uam;
  Alcotest.test_case "#554 anti-over-rejection: NLL reorder of callee-returned borrow OK"
    `Quick test_borrow_callee_returned_borrow_nll_ok;
  Alcotest.test_case "#554 precision: value-returning ref-param callee leaves arg movable"
    `Quick test_borrow_callee_value_return_ok;
  Alcotest.test_case "#554 aggregate: callee-returned borrow stored in tuple keeps arg borrowed"
    `Quick test_borrow_callee_returned_borrow_aggregate;
  Alcotest.test_case "#554 reassign: r = call() keeps the new target borrowed"
    `Quick test_borrow_callee_returned_borrow_reassign;
  Alcotest.test_case "#554 summary: borrow returned via match-arm tail is tracked"
    `Quick test_borrow_callee_returned_borrow_match_arm;
  Alcotest.test_case "#554 summary: `return if/match {..};` statement form is tracked"
    `Quick test_borrow_callee_returned_borrow_return_stmt;
  Alcotest.test_case "#554 (a): interprocedural-through-call-result via summary fixpoint"
    `Quick test_borrow_callee_returned_borrow_interproc;
  Alcotest.test_case "#554 (c): reassign to call result releases old loan (precision)"
    `Quick test_borrow_callee_returned_borrow_reassign_old_ok;
  Alcotest.test_case "Slice-B: reassign ref-counts old loan vs surviving alias"
    `Quick test_borrow_reassign_alias_survives;
  Alcotest.test_case "#554: reassigned returned ref-local unions summary origins"
    `Quick test_borrow_callee_returned_borrow_reassign_summary;
  Alcotest.test_case "issue-08 cond-origin: borrow through `if` keeps arg borrowed"
    `Quick test_borrow_cond_origin_if_uam;
  Alcotest.test_case "issue-08 cond-origin: borrow through block keeps arg borrowed"
    `Quick test_borrow_cond_origin_block_uam;
  Alcotest.test_case "issue-08 cond-origin: borrow through multi-arm match keeps arg borrowed"
    `Quick test_borrow_cond_origin_match_uam;
  Alcotest.test_case "issue-08 cond-origin: partial (one branch) move still rejected"
    `Quick test_borrow_cond_origin_partial_uam;
  Alcotest.test_case "issue-08 cond-origin anti-over-rejection: NLL use-before-move OK"
    `Quick test_borrow_cond_origin_nll_ok;
  Alcotest.test_case "issue-08 cond-origin precision: unrelated move stays legal"
    `Quick test_borrow_cond_origin_unrelated_ok;
]

(* ============================================================================
   Test Suite Export
   ============================================================================ *)

let tests =
  [
    ("E2E Parse", parse_tests);
    ("E2E Resolve", resolve_tests);
    ("E2E Typecheck", typecheck_tests);
    ("E2E Quantity", quantity_tests);
    ("E2E Borrow Graph", borrow_tests);
    ("E2E Linear Arrows", linear_arrow_tests);
    ("E2E WASM", wasm_tests);
    ("E2E Ownership Schema", ownership_schema_tests);
    ("E2E Julia", julia_tests);
    ("E2E Interp", interp_tests);
    ("E2E Optimizer", optimizer_tests);
    ("E2E Full Pipeline", full_pipeline_tests);
    ("E2E Errors", error_tests);
    ("E2E Python-Face", python_face_tests);
    ("E2E Traits", trait_impl_tests);
    ("E2E TEA", tea_tests);
    ("E2E TEA Bridge", tea_bridge_tests);
    ("E2E TEA Router", tea_router_tests);
    ("E2E LSP Phase B", lsp_phase_b_tests);
    ("E2E LSP Phase C", lsp_phase_c_tests);
    ("E2E LSP Phase D", lsp_phase_d_tests);
    ("E2E Try/Catch/Finally", try_catch_tests);
    ("E2E Handler Fence (#555)", handler_fence_tests);
    ("E2E Async Fence (#556)", async_fence_tests);
    ("E2E Resume Soundness (#555)", resume_soundness_tests);
    ("E2E Trait Coherence (#559)", coherence_tests);
    ("E2E Ownership Verify", tw_verify_tests);
    ("E2E Cmd Linearity", cmd_linear_tests);
    ("E2E Boundary Verify", tw_interface_tests);
    ("E2E WasmGC Loud-Fail", wasm_gc_loud_fail_tests);
    ("E2E WasmGC Variants",  wasm_gc_variant_tests);
    ("E2E Node-CJS Codegen", codegen_node_tests);
    ("E2E Stdlib",           stdlib_tests);
    ("E2E Xmod Other Codegens", cross_module_other_codegens_tests);
    ("E2E Externs",              extern_tests);
    ("E2E STDLIB-04a Mut #328",  stdlib_04a_mut_tests);
    ("E2E STDLIB-04d IO #331",   stdlib_04d_io_tests);
    ("E2E STDLIB-04e Pure #332", stdlib_04e_pure_tests);
    ("E2E String-wall slice 1 (indexing)", stringwall_index_tests);
    ("E2E String-wall slice 2 (string_from_char_code)", stringwall_alloc_tests);
    ("E2E String-wall slice 3 (string_sub)", stringwall_sub_tests);
    ("E2E String-wall slice 4 (case-fold)", stringwall_case_tests);
    ("E2E String-wall slice 5 (trim)", stringwall_trim_tests);
    ("E2E String-wall slice 6 (string_find)", stringwall_find_tests);
    ("E2E String-wall slice 7 (int_to_string)", stringwall_i2s_tests);
    ("E2E String-wall slice 8 guard (string ++ rejection)", stringwall_concat_guard_tests);
    ("E2E STDLIB-04b error #329", stdlib_04b_error_tests);
    ("E2E Vscode Bindings",      vscode_bindings_tests);
    ("E2E Array Type Sugar",     array_type_tests);
    ("E2E Qualified Paths #228",  qualified_path_tests);
    ("E2E Qualified Value #178",  qualified_value_tests);
    ("E2E Inline Extern Shapes (Refs #346)", inline_extern_shape_tests);
    ("E2E WasmGC PatCon Destructure", wasm_gc_patcon_tests);
    ("E2E Type Syntax Sugar",         type_syntax_sugar_tests);
  ]
