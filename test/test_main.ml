(** Main test runner for AffineScript *)

let () =
  Alcotest.run "AffineScript"
    [
      ("Lexer", Test_lexer.tests);
      ("Parser", Test_parser.tests);
      ("Golden", Test_golden.tests);
      ("Examples", Test_golden.example_tests);
    ]
