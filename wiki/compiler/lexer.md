# Lexer Implementation

The AffineScript lexer converts source text into a stream of tokens.

## Overview

**File**: `lib/lexer.ml`
**Library**: [sedlex](https://github.com/ocaml-community/sedlex) - Unicode-aware lexer generator

## Token Types

Defined in `lib/token.ml`:

```ocaml
type token =
  (* Literals *)
  | INT_LIT of int
  | FLOAT_LIT of float
  | CHAR_LIT of char
  | STRING_LIT of string
  | TRUE | FALSE

  (* Identifiers *)
  | IDENT of string
  | UPPER_IDENT of string

  (* Keywords *)
  | FN | LET | MUT | OWN | REF
  | TYPE | STRUCT | ENUM | TRAIT | IMPL
  | EFFECT | HANDLE | RESUME | PERFORM
  | IF | ELSE | MATCH | WHILE | FOR | IN
  | LOOP | RETURN | BREAK | CONTINUE
  | PUB | TOTAL | UNSAFE | WHERE | AS
  | MOD | USE | FROM | SELF | SUPER

  (* Operators *)
  | PLUS | MINUS | STAR | SLASH | PERCENT
  | EQ | NE | LT | GT | LE | GE
  | AND | OR | NOT
  | AMPERSAND | PIPE | CARET | TILDE
  | SHL | SHR
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | ...
  | ARROW | FAT_ARROW | THIN_ARROW
  | DOT | DOTDOT | DOTDOTEQ
  | QUESTION | COLON | DOUBLE_COLON
  | PIPE_GT | PLUS_PLUS

  (* Punctuation *)
  | LPAREN | RPAREN
  | LBRACKET | RBRACKET
  | LBRACE | RBRACE
  | COMMA | SEMICOLON

  (* Special *)
  | ROW_VAR of string  (* ..name *)
  | OMEGA              (* w or omega *)
  | EOF
```

## Lexer Implementation

### Character Classes

```ocaml
let digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' | '_']
let ident_char = [%sedlex.regexp? letter | digit | '_']
let newline = [%sedlex.regexp? '\n' | '\r' | "\r\n"]
let whitespace = [%sedlex.regexp? ' ' | '\t' | newline]
```

### Keyword Recognition

```ocaml
let keyword_or_ident = function
  | "fn" -> FN
  | "let" -> LET
  | "mut" -> MUT
  | "own" -> OWN
  | "ref" -> REF
  | "type" -> TYPE
  | "struct" -> STRUCT
  | "enum" -> ENUM
  | "trait" -> TRAIT
  | "impl" -> IMPL
  | "effect" -> EFFECT
  | "handle" -> HANDLE
  | "resume" -> RESUME
  | "perform" -> PERFORM
  | "if" -> IF
  | "else" -> ELSE
  | "match" -> MATCH
  | "while" -> WHILE
  | "for" -> FOR
  | "in" -> IN
  | "loop" -> LOOP
  | "return" -> RETURN
  | "break" -> BREAK
  | "continue" -> CONTINUE
  | "pub" -> PUB
  | "total" -> TOTAL
  | "unsafe" -> UNSAFE
  | "where" -> WHERE
  | "as" -> AS
  | "mod" -> MOD
  | "use" -> USE
  | "from" -> FROM
  | "Self" -> SELF_TYPE
  | "self" -> SELF
  | "super" -> SUPER
  | "true" -> TRUE
  | "false" -> FALSE
  | "omega" | "w" -> OMEGA
  | s -> IDENT s
```

### Main Lexer Function

```ocaml
let rec token buf =
  match%sedlex buf with
  (* Whitespace *)
  | Plus whitespace -> token buf

  (* Comments *)
  | "//" -> line_comment buf; token buf
  | "/*" -> block_comment buf 1; token buf

  (* Identifiers and keywords *)
  | letter, Star ident_char ->
      let s = Sedlexing.Utf8.lexeme buf in
      keyword_or_ident s

  (* Numeric literals *)
  | "0x", Plus hex_digit -> INT_LIT (parse_hex (Sedlexing.Utf8.lexeme buf))
  | "0b", Plus ('0' | '1') -> INT_LIT (parse_binary (Sedlexing.Utf8.lexeme buf))
  | "0o", Plus ('0'..'7') -> INT_LIT (parse_octal (Sedlexing.Utf8.lexeme buf))
  | Plus digit, '.', Plus digit -> FLOAT_LIT (float_of_string (Sedlexing.Utf8.lexeme buf))
  | Plus digit -> INT_LIT (int_of_string (Sedlexing.Utf8.lexeme buf))

  (* String and character literals *)
  | '"' -> STRING_LIT (string_literal buf (Buffer.create 64))
  | '\'' -> CHAR_LIT (char_literal buf)

  (* Row variables *)
  | "..", letter, Star ident_char ->
      let s = Sedlexing.Utf8.lexeme buf in
      ROW_VAR (String.sub s 2 (String.length s - 2))

  (* Multi-character operators *)
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "-{" -> EFFECT_ARROW_START
  | "}>" -> EFFECT_ARROW_END
  | "==" -> EQ
  | "!=" -> NE
  | "<=" -> LE
  | ">=" -> GE
  | "&&" -> AND
  | "||" -> OR
  | "<<" -> SHL
  | ">>" -> SHR
  | "::" -> DOUBLE_COLON
  | ".." -> DOTDOT
  | "..=" -> DOTDOTEQ
  | "|>" -> PIPE_GT
  | "++" -> PLUS_PLUS
  | "+=" -> PLUS_ASSIGN
  | "-=" -> MINUS_ASSIGN
  | "*=" -> STAR_ASSIGN
  | "/=" -> SLASH_ASSIGN

  (* Single-character operators *)
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '%' -> PERCENT
  | '<' -> LT
  | '>' -> GT
  | '=' -> ASSIGN
  | '!' -> NOT
  | '&' -> AMPERSAND
  | '|' -> PIPE
  | '^' -> CARET
  | '~' -> TILDE
  | '?' -> QUESTION
  | '.' -> DOT
  | ':' -> COLON
  | '@' -> AT

  (* Punctuation *)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | '{' -> LBRACE
  | '}' -> RBRACE
  | ',' -> COMMA
  | ';' -> SEMICOLON

  (* End of file *)
  | eof -> EOF

  (* Error *)
  | any ->
      let c = Sedlexing.Utf8.lexeme buf in
      raise (Lexer_error (unexpected_char c, current_pos buf))
  | _ -> assert false
```

### String Literal Parsing

```ocaml
and string_literal buf acc =
  match%sedlex buf with
  | '"' -> Buffer.contents acc
  | '\\', 'n' -> Buffer.add_char acc '\n'; string_literal buf acc
  | '\\', 't' -> Buffer.add_char acc '\t'; string_literal buf acc
  | '\\', 'r' -> Buffer.add_char acc '\r'; string_literal buf acc
  | '\\', '\\' -> Buffer.add_char acc '\\'; string_literal buf acc
  | '\\', '"' -> Buffer.add_char acc '"'; string_literal buf acc
  | '\\', '0' -> Buffer.add_char acc '\000'; string_literal buf acc
  | '\\', 'x', hex_digit, hex_digit ->
      let s = Sedlexing.Utf8.lexeme buf in
      let code = int_of_string ("0x" ^ String.sub s 2 2) in
      Buffer.add_char acc (Char.chr code);
      string_literal buf acc
  | '\\', "u{", Plus hex_digit, '}' ->
      let s = Sedlexing.Utf8.lexeme buf in
      let hex = String.sub s 3 (String.length s - 4) in
      let code = int_of_string ("0x" ^ hex) in
      Buffer.add_utf_8_uchar acc (Uchar.of_int code);
      string_literal buf acc
  | newline -> raise (Lexer_error (unterminated_string, current_pos buf))
  | eof -> raise (Lexer_error (unterminated_string, current_pos buf))
  | any ->
      Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
      string_literal buf acc
  | _ -> assert false
```

### Comment Handling

```ocaml
and line_comment buf =
  match%sedlex buf with
  | newline | eof -> ()
  | any -> line_comment buf
  | _ -> assert false

and block_comment buf depth =
  match%sedlex buf with
  | "*/" ->
      if depth = 1 then ()
      else block_comment buf (depth - 1)
  | "/*" -> block_comment buf (depth + 1)
  | eof -> raise (Lexer_error (unterminated_comment, current_pos buf))
  | any -> block_comment buf depth
  | _ -> assert false
```

## Source Location Tracking

```ocaml
type position = {
  line: int;
  column: int;
  offset: int;
}

type span = {
  start: position;
  end_: position;
}

let current_pos buf =
  let pos = Sedlexing.lexing_position buf in
  {
    line = pos.pos_lnum;
    column = pos.pos_cnum - pos.pos_bol;
    offset = pos.pos_cnum;
  }

let make_span start end_ = { start; end_ }

let merge_spans s1 s2 = {
  start = s1.start;
  end_ = s2.end_;
}
```

## Error Handling

```ocaml
exception Lexer_error of string * position

let unexpected_char c =
  Printf.sprintf "unexpected character '%s'" c

let unterminated_string =
  "unterminated string literal"

let unterminated_comment =
  "unterminated block comment"

let invalid_escape seq =
  Printf.sprintf "invalid escape sequence '\\%s'" seq
```

## Testing

Test cases in `test/test_lexer.ml`:

```ocaml
let test_keywords () =
  let tokens = lex "fn let mut own ref type struct enum" in
  Alcotest.(check (list token_testable))
    "keywords"
    [FN; LET; MUT; OWN; REF; TYPE; STRUCT; ENUM; EOF]
    tokens

let test_operators () =
  let tokens = lex "+ - * / == != && || ->" in
  Alcotest.(check (list token_testable))
    "operators"
    [PLUS; MINUS; STAR; SLASH; EQ; NE; AND; OR; ARROW; EOF]
    tokens

let test_string_escapes () =
  let tokens = lex {|"hello\n\tworld"|} in
  Alcotest.(check (list token_testable))
    "string escapes"
    [STRING_LIT "hello\n\tworld"; EOF]
    tokens

let test_nested_comments () =
  let tokens = lex "/* outer /* inner */ outer */ x" in
  Alcotest.(check (list token_testable))
    "nested comments"
    [IDENT "x"; EOF]
    tokens
```

## Future Enhancements

1. **Raw strings**: `r"no \escapes"`, `r#"contains "quotes""#`
2. **String interpolation**: `"Hello, ${name}!"`
3. **Doc comments**: `///` and `//!`
4. **Attributes**: `#[inline]`, `#[derive(...)]`
5. **Better error recovery**: Continue lexing after errors

---

## See Also

- [Architecture](architecture.md) - Compiler overview
- [Parser](parser.md) - Next phase
- [Tokens](../../lib/token.ml) - Token definitions
