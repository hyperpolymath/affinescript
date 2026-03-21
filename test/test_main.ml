(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2024-2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** Main test runner for AffineScript *)

let () =
  Alcotest.run "AffineScript"
    ([
      ("Lexer", Test_lexer.tests);
      (* ("Parser", Test_parser.tests); *)  (* TODO: Re-enable when test_parser is implemented *)
      ("Golden", Test_golden.tests);
      ("Examples", Test_golden.example_tests);
    ] @ Test_e2e.tests)
