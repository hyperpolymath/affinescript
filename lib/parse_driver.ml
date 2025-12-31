(** Parser driver - bridges sedlex lexer with Menhir parser *)

(** Exception for parse errors *)
exception Parse_error of string * Span.t

(** Buffered token stream that provides Menhir-compatible interface *)
type token_buffer = {
  mutable current_token : Token.t;
  mutable current_span : Span.t;
  mutable next_token : unit -> Token.t * Span.t;
}

(** Create a Menhir-compatible lexer function from our token stream *)
let lexer_of_token_stream (next : unit -> Token.t * Span.t) : Lexing.lexbuf -> Parser.token =
  (* We need to track position for Menhir *)
  let buf = ref None in
  let get_next () =
    match !buf with
    | Some (tok, span) ->
        buf := None;
        (tok, span)
    | None -> next ()
  in
  fun lexbuf ->
    let (tok, span) = get_next () in
    (* Update lexbuf positions for Menhir *)
    lexbuf.Lexing.lex_start_p <- {
      Lexing.pos_fname = span.Span.file;
      pos_lnum = span.start_pos.line;
      pos_bol = span.start_pos.offset - span.start_pos.col + 1;
      pos_cnum = span.start_pos.offset;
    };
    lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = span.Span.file;
      pos_lnum = span.end_pos.line;
      pos_bol = span.end_pos.offset - span.end_pos.col + 1;
      pos_cnum = span.end_pos.offset;
    };
    (* Convert our token type to Menhir's *)
    match tok with
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

(** Parse a program from a string *)
let parse_string ~file content =
  let token_stream = Lexer.from_string ~file content in
  let lexbuf = Lexing.from_string content in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = file };
  let lexer = lexer_of_token_stream token_stream in
  try
    Parser.program lexer lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let span = Span.make
        ~file
        ~start_pos:{ Span.line = pos.pos_lnum;
                     col = pos.pos_cnum - pos.pos_bol + 1;
                     offset = pos.pos_cnum }
        ~end_pos:{ Span.line = pos.pos_lnum;
                   col = pos.pos_cnum - pos.pos_bol + 1;
                   offset = pos.pos_cnum }
      in
      raise (Parse_error ("Syntax error", span))

(** Parse a program from a file *)
let parse_file filename =
  let chan = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let content = really_input_string chan (in_channel_length chan) in
      parse_string ~file:filename content)

(** Parse a single expression from a string *)
let parse_expr ~file content =
  let token_stream = Lexer.from_string ~file content in
  let lexbuf = Lexing.from_string content in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_fname = file };
  let lexer = lexer_of_token_stream token_stream in
  try
    Parser.expr_only lexer lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let span = Span.make
        ~file
        ~start_pos:{ Span.line = pos.pos_lnum;
                     col = pos.pos_cnum - pos.pos_bol + 1;
                     offset = pos.pos_cnum }
        ~end_pos:{ Span.line = pos.pos_lnum;
                   col = pos.pos_cnum - pos.pos_bol + 1;
                   offset = pos.pos_cnum }
      in
      raise (Parse_error ("Syntax error", span))
