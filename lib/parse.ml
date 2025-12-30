(** Parser wrapper for AffineScript *)

exception Parse_error of string * Span.t

(** Menhir incremental parser with error recovery *)
module I = Parser.MenhirInterpreter

(** State for parser with lexer *)
type state = {
  lexer_state : Lexer.state;
  buf : Sedlexing.lexbuf;
  mutable last_token : Token.t * Span.t;
}

(** Convert sedlex position to Lexing.position for Menhir *)
let make_lexing_position (file : string) (pos : Span.pos) : Lexing.position =
  {
    Lexing.pos_fname = file;
    pos_lnum = pos.Span.line;
    pos_bol = pos.offset - pos.col + 1;
    pos_cnum = pos.offset;
  }

(** Create parser state from string *)
let create_state ~file content =
  let buf = Sedlexing.Utf8.from_string content in
  let lexer_state = Lexer.create_state file in
  { lexer_state; buf; last_token = (Token.EOF, Span.dummy) }

(** Create parser state from channel *)
let create_state_channel ~file chan =
  let buf = Sedlexing.Utf8.from_channel chan in
  let lexer_state = Lexer.create_state file in
  { lexer_state; buf; last_token = (Token.EOF, Span.dummy) }

(** Get next token for Menhir *)
let next_token state () =
  let start_pos = Lexer.current_pos state.lexer_state state.buf in
  let tok = Lexer.token state.lexer_state state.buf in
  let end_pos = Lexer.current_pos state.lexer_state state.buf in
  let span = Span.make ~file:state.lexer_state.file ~start_pos ~end_pos in
  state.last_token <- (tok, span);

  (* Convert to Menhir token with positions *)
  let start_lexpos = make_lexing_position state.lexer_state.file start_pos in
  let end_lexpos = make_lexing_position state.lexer_state.file end_pos in

  (* Map our tokens to parser tokens *)
  let menhir_tok = match tok with
    | Token.INT n -> Parser.INT n
    | Token.FLOAT f -> Parser.FLOAT f
    | Token.CHAR c -> Parser.CHAR c
    | Token.STRING s -> Parser.STRING s
    | Token.TRUE -> Parser.TRUE
    | Token.FALSE -> Parser.FALSE
    | Token.LOWER_IDENT s -> Parser.LOWER_IDENT s
    | Token.UPPER_IDENT s -> Parser.UPPER_IDENT s
    | Token.ROW_VAR s -> Parser.ROW_VAR s
    | Token.FN -> Parser.FN
    | Token.LET -> Parser.LET
    | Token.MUT -> Parser.MUT
    | Token.OWN -> Parser.OWN
    | Token.REF -> Parser.REF
    | Token.TYPE -> Parser.TYPE
    | Token.STRUCT -> Parser.STRUCT
    | Token.ENUM -> Parser.ENUM
    | Token.TRAIT -> Parser.TRAIT
    | Token.IMPL -> Parser.IMPL
    | Token.EFFECT -> Parser.EFFECT
    | Token.HANDLE -> Parser.HANDLE
    | Token.RESUME -> Parser.RESUME
    | Token.MATCH -> Parser.MATCH
    | Token.IF -> Parser.IF
    | Token.ELSE -> Parser.ELSE
    | Token.WHILE -> Parser.WHILE
    | Token.FOR -> Parser.FOR
    | Token.RETURN -> Parser.RETURN
    | Token.BREAK -> Parser.BREAK
    | Token.CONTINUE -> Parser.CONTINUE
    | Token.IN -> Parser.IN
    | Token.WHERE -> Parser.WHERE
    | Token.TOTAL -> Parser.TOTAL
    | Token.MODULE -> Parser.MODULE
    | Token.USE -> Parser.USE
    | Token.PUB -> Parser.PUB
    | Token.AS -> Parser.AS
    | Token.UNSAFE -> Parser.UNSAFE
    | Token.ASSUME -> Parser.ASSUME
    | Token.TRANSMUTE -> Parser.TRANSMUTE
    | Token.FORGET -> Parser.FORGET
    | Token.TRY -> Parser.TRY
    | Token.CATCH -> Parser.CATCH
    | Token.FINALLY -> Parser.FINALLY
    | Token.NAT -> Parser.NAT
    | Token.INT_T -> Parser.INT_T
    | Token.BOOL -> Parser.BOOL
    | Token.FLOAT_T -> Parser.FLOAT_T
    | Token.STRING_T -> Parser.STRING_T
    | Token.CHAR_T -> Parser.CHAR_T
    | Token.TYPE_K -> Parser.TYPE_K
    | Token.ROW -> Parser.ROW
    | Token.NEVER -> Parser.NEVER
    | Token.LPAREN -> Parser.LPAREN
    | Token.RPAREN -> Parser.RPAREN
    | Token.LBRACE -> Parser.LBRACE
    | Token.RBRACE -> Parser.RBRACE
    | Token.LBRACKET -> Parser.LBRACKET
    | Token.RBRACKET -> Parser.RBRACKET
    | Token.COMMA -> Parser.COMMA
    | Token.SEMICOLON -> Parser.SEMICOLON
    | Token.COLON -> Parser.COLON
    | Token.COLONCOLON -> Parser.COLONCOLON
    | Token.DOT -> Parser.DOT
    | Token.DOTDOT -> Parser.DOTDOT
    | Token.ARROW -> Parser.ARROW
    | Token.FAT_ARROW -> Parser.FAT_ARROW
    | Token.PIPE -> Parser.PIPE
    | Token.AT -> Parser.AT
    | Token.UNDERSCORE -> Parser.UNDERSCORE
    | Token.BACKSLASH -> Parser.BACKSLASH
    | Token.QUESTION -> Parser.QUESTION
    | Token.ZERO -> Parser.ZERO
    | Token.ONE -> Parser.ONE
    | Token.OMEGA -> Parser.OMEGA
    | Token.PLUS -> Parser.PLUS
    | Token.MINUS -> Parser.MINUS
    | Token.STAR -> Parser.STAR
    | Token.SLASH -> Parser.SLASH
    | Token.PERCENT -> Parser.PERCENT
    | Token.EQ -> Parser.EQ
    | Token.EQEQ -> Parser.EQEQ
    | Token.NE -> Parser.NE
    | Token.LT -> Parser.LT
    | Token.LE -> Parser.LE
    | Token.GT -> Parser.GT
    | Token.GE -> Parser.GE
    | Token.AMPAMP -> Parser.AMPAMP
    | Token.PIPEPIPE -> Parser.PIPEPIPE
    | Token.BANG -> Parser.BANG
    | Token.AMP -> Parser.AMP
    | Token.CARET -> Parser.CARET
    | Token.TILDE -> Parser.TILDE
    | Token.LTLT -> Parser.LTLT
    | Token.GTGT -> Parser.GTGT
    | Token.PLUSEQ -> Parser.PLUSEQ
    | Token.MINUSEQ -> Parser.MINUSEQ
    | Token.STAREQ -> Parser.STAREQ
    | Token.SLASHEQ -> Parser.SLASHEQ
    | Token.EOF -> Parser.EOF
  in
  (menhir_tok, start_lexpos, end_lexpos)

(** Parse a program from string *)
let parse_string ~file content : (Ast.program, string * Span.t) result =
  let state = create_state ~file content in
  let supplier = next_token state in
  try
    let checkpoint = Parser.Incremental.program
      (make_lexing_position file { Span.line = 1; col = 1; offset = 0 }) in
    Ok (I.loop supplier checkpoint)
  with
  | Lexer.Lexer_error (msg, pos) ->
    let span = Span.make ~file ~start_pos:pos ~end_pos:pos in
    Error (msg, span)
  | Parser.Error ->
    let (tok, span) = state.last_token in
    let msg = Printf.sprintf "Syntax error at %s" (Token.to_string tok) in
    Error (msg, span)

(** Parse a program from channel *)
let parse_channel ~file chan : (Ast.program, string * Span.t) result =
  let state = create_state_channel ~file chan in
  let supplier = next_token state in
  try
    let checkpoint = Parser.Incremental.program
      (make_lexing_position file { Span.line = 1; col = 1; offset = 0 }) in
    Ok (I.loop supplier checkpoint)
  with
  | Lexer.Lexer_error (msg, pos) ->
    let span = Span.make ~file ~start_pos:pos ~end_pos:pos in
    Error (msg, span)
  | Parser.Error ->
    let (tok, span) = state.last_token in
    let msg = Printf.sprintf "Syntax error at %s" (Token.to_string tok) in
    Error (msg, span)

(** Parse a program from file *)
let parse_file filename : (Ast.program, string * Span.t) result =
  try
    let chan = open_in_bin filename in
    let result = parse_channel ~file:filename chan in
    close_in chan;
    result
  with
  | Sys_error msg ->
    let span = Span.make ~file:filename
      ~start_pos:{ line = 0; col = 0; offset = 0 }
      ~end_pos:{ line = 0; col = 0; offset = 0 } in
    Error (msg, span)

(** Parse a single expression from string *)
let parse_expr ~file content : (Ast.expr, string * Span.t) result =
  let state = create_state ~file content in
  let supplier = next_token state in
  try
    let checkpoint = Parser.Incremental.expr_only
      (make_lexing_position file { Span.line = 1; col = 1; offset = 0 }) in
    Ok (I.loop supplier checkpoint)
  with
  | Lexer.Lexer_error (msg, pos) ->
    let span = Span.make ~file ~start_pos:pos ~end_pos:pos in
    Error (msg, span)
  | Parser.Error ->
    let (tok, span) = state.last_token in
    let msg = Printf.sprintf "Syntax error at %s" (Token.to_string tok) in
    Error (msg, span)
