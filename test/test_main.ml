(** Main test runner for AffineScript *)

let () =
  Alcotest.run "AffineScript"
    [
      ("Lexer", Test_lexer.tests);
      ("Parser", Test_parser.tests);
      ("Eval", Test_eval.tests);
    ]
