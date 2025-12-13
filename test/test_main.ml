(** Main test runner for AffineScript *)

let () =
  Alcotest.run "AffineScript"
    [
      ("Lexer", Test_lexer.tests);
    ]
