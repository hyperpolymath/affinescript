(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2024-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** Main test runner for AffineScript *)

let () =
  Alcotest.run "AffineScript"
    ([
      ("Lexer", Test_lexer.tests);
      (* ("Parser", Test_parser.tests); *)  (* TODO: Re-enable when test_parser is implemented *)
      ("Golden", Test_golden.tests);
      ("Examples", Test_golden.example_tests);
      ("Effects (#59)", Test_effects.tests);
      ("Effect-sites (#234 S2a)", Test_effect_sites.tests);
      ("TW L13 isolation (#10)", Test_tw_isolation.tests);
    ] @ Test_e2e.tests @ Test_stdlib_aot.tests)
