(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2024-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** Main test runner for AffineScript *)

(* [PROVEN_REPORT=FILE]: write a Proven-Tests run report for the lexer suite
   instead of running Alcotest (tools/ci/proven/proven-report.sh). *)
let () =
  match Sys.getenv_opt "PROVEN_REPORT" with
  | Some path when path <> "" ->
    let bad =
      Proven_report.write ~suite:"affinescript-lexer"
        ~module_path:"test/test_lexer.ml" ~path Test_lexer.tests
    in
    exit (if bad = 0 then 0 else 1)
  | _ -> ()

let () =
  Alcotest.run "AffineScript"
    ([
      ("Lexer", Test_lexer.tests);
      ("Proven-Tests report emitter", Test_proven_report.tests);
      (* ("Parser", Test_parser.tests); *)  (* TODO: Re-enable when test_parser is implemented *)
      ("Golden", Test_golden.tests);
      ("Examples", Test_golden.example_tests);
      ("Effects (#59)", Test_effects.tests);
      ("Effect-sites (#234, ADR-016)", Test_effect_sites.tests);
      ("TW L13 isolation (#10)", Test_tw_isolation.tests);
      ("Qualified paths (#228, ADR-014)", Test_qualified_paths.tests);
      ("Generic enum kinds", Test_generic_enum_kinds.tests);
      ("Module search path ($AFFINESCRIPT_PATH)", Test_module_search_path.tests);
      ("Module root, decl match, flattening, interp binops, Bun-ESM return-in-arm", Test_module_root_flatten.tests);
      ("Record update + imported types", Test_records_and_imports.tests);
      ("Module mut (#548)", Test_module_mut.tests);
      ("Int-div on JS-text backend (#478)", Test_int_div_js.tests);
      ("Deno builtins ↔ stdlib decls consistency", Test_deno_builtins_consistency.tests);
      ("Bun integer block scope", Test_int_scope_bun.tests);
      ("ESM runtime pruning", Test_runtime_prune.tests);
      ("Stdlib algebraic laws (batch 1)", Test_stdlib_laws.tests);
      ("Solo CESK (VM M1, ADR-0025)", Test_solo_cesk.tests);
      ("Polonius solver (ADR-022 M3)", Test_borrow_polonius.tests);
    ] @ Test_e2e.tests @ Test_stdlib_aot.tests)
