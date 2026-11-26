(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Lexer tests *)

open Affinescript

let token_testable = Alcotest.testable Token.pp Token.equal

let lex_all source =
  let lexer = Lexer.from_string ~file:"<test>" source in
  let rec loop acc =
    let (tok, _) = lexer () in
    if tok = Token.EOF then List.rev (tok :: acc)
    else loop (tok :: acc)
  in
  loop []

let test_keywords () =
  let tokens = lex_all "fn let mut own ref type" in
  Alcotest.(check (list token_testable)) "keywords"
    [Token.FN; Token.LET; Token.MUT; Token.OWN; Token.REF; Token.TYPE; Token.EOF]
    tokens

let test_identifiers () =
  (* Underscore-prefix policy (Refs 2026-05-26 gitbot-fleet#148 sustainabot
     hand-port): `_test` (and `_anything`) is the conventional Rust-idiom
     unused-binding spelling; the lexer treats it as a single LOWER_IDENT.
     Bare `_` is still tokenised as the distinct UNDERSCORE (wildcard) token
     — sedlex applies maximal munch and the underscore branch of
     `lower_ident_start` requires at least one trailing alphanum. *)
  let tokens = lex_all "foo Bar _test test123 _" in
  Alcotest.(check (list token_testable)) "identifiers"
    [Token.LOWER_IDENT "foo"; Token.UPPER_IDENT "Bar";
     Token.LOWER_IDENT "_test"; Token.LOWER_IDENT "test123";
     Token.UNDERSCORE; Token.EOF]
    tokens

let test_literals () =
  let tokens = lex_all "42 3.14 true false" in
  Alcotest.(check (list token_testable)) "literals"
    [Token.INT 42; Token.FLOAT 3.14; Token.TRUE; Token.FALSE; Token.EOF]
    tokens

let test_string_literal () =
  let tokens = lex_all {|"hello world"|} in
  Alcotest.(check (list token_testable)) "string literal"
    [Token.STRING "hello world"; Token.EOF]
    tokens

let test_string_escapes () =
  let tokens = lex_all {|"hello\nworld\t!"|} in
  Alcotest.(check (list token_testable)) "string escapes"
    [Token.STRING "hello\nworld\t!"; Token.EOF]
    tokens

let test_operators () =
  let tokens = lex_all "+ - * / == != < > <= >= -> =>" in
  Alcotest.(check (list token_testable)) "operators"
    [Token.PLUS; Token.MINUS; Token.STAR; Token.SLASH;
     Token.EQEQ; Token.NE; Token.LT; Token.GT; Token.LE; Token.GE;
     Token.ARROW; Token.FAT_ARROW; Token.EOF]
    tokens

let test_punctuation () =
  let tokens = lex_all "( ) { } [ ] , ; : :: . .." in
  Alcotest.(check (list token_testable)) "punctuation"
    [Token.LPAREN; Token.RPAREN; Token.LBRACE; Token.RBRACE;
     Token.LBRACKET; Token.RBRACKET; Token.COMMA; Token.SEMICOLON;
     Token.COLON; Token.COLONCOLON; Token.DOT; Token.DOTDOT; Token.EOF]
    tokens

let test_row_variable () =
  let tokens = lex_all "..rest" in
  Alcotest.(check (list token_testable)) "row variable"
    [Token.ROW_VAR "rest"; Token.EOF]
    tokens

let test_comments () =
  let tokens = lex_all "foo // comment\nbar" in
  Alcotest.(check (list token_testable)) "line comment"
    [Token.LOWER_IDENT "foo"; Token.LOWER_IDENT "bar"; Token.EOF]
    tokens

let test_block_comments () =
  let tokens = lex_all "foo /* block */ bar" in
  Alcotest.(check (list token_testable)) "block comment"
    [Token.LOWER_IDENT "foo"; Token.LOWER_IDENT "bar"; Token.EOF]
    tokens

let test_nested_comments () =
  let tokens = lex_all "foo /* outer /* inner */ still outer */ bar" in
  Alcotest.(check (list token_testable)) "nested comments"
    [Token.LOWER_IDENT "foo"; Token.LOWER_IDENT "bar"; Token.EOF]
    tokens

let test_hex_literal () =
  let tokens = lex_all "0xFF 0x10" in
  Alcotest.(check (list token_testable)) "hex literals"
    [Token.INT 255; Token.INT 16; Token.EOF]
    tokens

let test_binary_literal () =
  let tokens = lex_all "0b1010 0b11" in
  Alcotest.(check (list token_testable)) "binary literals"
    [Token.INT 10; Token.INT 3; Token.EOF]
    tokens

let test_function_decl () =
  let tokens = lex_all "fn add(a: Int, b: Int) -> Int" in
  Alcotest.(check (list token_testable)) "function declaration"
    [Token.FN; Token.LOWER_IDENT "add"; Token.LPAREN;
     Token.LOWER_IDENT "a"; Token.COLON; Token.INT_T; Token.COMMA;
     Token.LOWER_IDENT "b"; Token.COLON; Token.INT_T; Token.RPAREN;
     Token.ARROW; Token.INT_T; Token.EOF]
    tokens

let test_total_function () =
  let tokens = lex_all "total fn safe() -> Nat" in
  Alcotest.(check (list token_testable)) "total function"
    [Token.TOTAL; Token.FN; Token.LOWER_IDENT "safe"; Token.LPAREN;
     Token.RPAREN; Token.ARROW; Token.NAT; Token.EOF]
    tokens

let test_type_decl () =
  let tokens = lex_all "type Option[T] = None | Some(T)" in
  Alcotest.(check (list token_testable)) "type declaration"
    [Token.TYPE; Token.UPPER_IDENT "Option"; Token.LBRACKET;
     Token.UPPER_IDENT "T"; Token.RBRACKET; Token.EQ;
     Token.UPPER_IDENT "None"; Token.PIPE;
     Token.UPPER_IDENT "Some"; Token.LPAREN;
     Token.UPPER_IDENT "T"; Token.RPAREN; Token.EOF]
    tokens

(* #771: every token span used to start where the *previous* token started
   (the start was read before trivia was skipped), so parse errors blamed the
   token before the offender. Spans are (line, col) start .. end-exclusive. *)
let spans_of source =
  let lexer = Lexer.from_string ~file:"<test>" source in
  let rec loop acc =
    let (tok, (sp : Span.t)) = lexer () in
    let r = (sp.start_pos.line, sp.start_pos.col,
             sp.end_pos.line, sp.end_pos.col) in
    if tok = Token.EOF then List.rev acc else loop (r :: acc)
  in
  loop []

let span4 = Alcotest.(list (pair (pair int int) (pair int int)))
let as_pairs = List.map (fun (a, b, c, d) -> ((a, b), (c, d)))

let test_spans_after_trivia () =
  Alcotest.check span4 "spans skip whitespace, newlines and comments"
    (as_pairs [ (1, 1, 1, 7); (1, 8, 1, 13); (1, 13, 1, 14);
                (5, 1, 5, 2); (5, 2, 5, 3) ])
    (as_pairs (spans_of "module Block;\n\n/* c */\n// l\n@x"))

let test_spans_multiline_string () =
  Alcotest.check span4 "a string token spans its opening to closing quote"
    (as_pairs [ (1, 1, 1, 2); (1, 3, 2, 3); (2, 4, 2, 5) ])
    (as_pairs (spans_of "x \"a\nb\" y"))

let test_parse_error_points_at_offender () =
  let src = "module Block;\n\n/* c */\n\n@react.component\nfn make = 1\n" in
  match Parse_driver.parse_string ~file:"<test>" src with
  | _ -> Alcotest.fail "expected a parse error"
  | exception Parse_driver.Parse_error (_, sp) ->
    Alcotest.(check (pair int int)) "error is at the attribute, not the `;`"
      (5, 1) (sp.start_pos.line, sp.start_pos.col)

let tests =
  [
    Alcotest.test_case "spans after trivia (#771)" `Quick test_spans_after_trivia;
    Alcotest.test_case "multi-line string span (#771)" `Quick test_spans_multiline_string;
    Alcotest.test_case "parse error position (#771)" `Quick test_parse_error_points_at_offender;
    Alcotest.test_case "keywords" `Quick test_keywords;
    Alcotest.test_case "identifiers" `Quick test_identifiers;
    Alcotest.test_case "literals" `Quick test_literals;
    Alcotest.test_case "string literal" `Quick test_string_literal;
    Alcotest.test_case "string escapes" `Quick test_string_escapes;
    Alcotest.test_case "operators" `Quick test_operators;
    Alcotest.test_case "punctuation" `Quick test_punctuation;
    Alcotest.test_case "row variable" `Quick test_row_variable;
    Alcotest.test_case "line comment" `Quick test_comments;
    Alcotest.test_case "block comment" `Quick test_block_comments;
    Alcotest.test_case "nested comments" `Quick test_nested_comments;
    Alcotest.test_case "hex literal" `Quick test_hex_literal;
    Alcotest.test_case "binary literal" `Quick test_binary_literal;
    Alcotest.test_case "function decl" `Quick test_function_decl;
    Alcotest.test_case "total function" `Quick test_total_function;
    Alcotest.test_case "type decl" `Quick test_type_decl;
  ]
