(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Parser driver - bridges sedlex lexer with Menhir parser *)

(** Exception for parse errors *)
exception Parse_error of string * Span.t

module I = Parser.MenhirInterpreter

(** Buffered token stream that provides Menhir-compatible interface *)
type token_buffer = {
  mutable current_token : Token.t;
  mutable current_span : Span.t;
  mutable next_token : unit -> Token.t * Span.t;
}

(** Convert our token type to Menhir's. *)
let to_menhir_token (tok : Token.t) : Parser.token =
    match tok with
    | Token.INT n -> Parser.INT n
    | Token.FLOAT f -> Parser.FLOAT f
    | Token.CHAR c -> Parser.CHAR c
    | Token.STRING s -> Parser.STRING s
    | Token.TRUE -> Parser.TRUE
    | Token.FALSE -> Parser.FALSE
    | Token.LOWER_IDENT s -> Parser.LOWER_IDENT s
    | Token.UPPER_IDENT s -> Parser.UPPER_IDENT s
    | Token.FN -> Parser.FN
    | Token.LET -> Parser.LET
    | Token.CONST -> Parser.CONST
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
    | Token.EXTERN -> Parser.EXTERN
    | Token.UNSAFE -> Parser.UNSAFE
    | Token.ASSUME -> Parser.ASSUME
    | Token.SELF_KW -> Parser.SELF_KW
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
    | Token.HASH_LBRACE -> Parser.HASH_LBRACE
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
    | Token.PLUSPLUS -> Parser.PLUSPLUS
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
    | Token.ROW_VAR s -> Parser.ROW_VAR s
    | Token.EOF -> Parser.EOF

(** Convert a Span position to a Menhir/Lexing position. *)
let lexing_pos ~file (p : Span.pos) : Lexing.position =
  { Lexing.pos_fname = file;
    pos_lnum = p.Span.line;
    pos_bol = p.offset - p.col + 1;
    pos_cnum = p.offset }

(** Create a Menhir-compatible lexer function from our token stream.
    Retained for any monolithic-API caller; the file/expr entry points
    below use the incremental driver instead. *)
let lexer_of_token_stream (next : unit -> Token.t * Span.t) : Lexing.lexbuf -> Parser.token =
  let buf = ref None in
  let get_next () =
    match !buf with
    | Some (tok, span) -> buf := None; (tok, span)
    | None -> next ()
  in
  fun lexbuf ->
    let (tok, span) = get_next () in
    lexbuf.Lexing.lex_start_p <- lexing_pos ~file:span.Span.file span.start_pos;
    lexbuf.Lexing.lex_curr_p  <- lexing_pos ~file:span.Span.file span.end_pos;
    to_menhir_token tok

(* The lexer emits ">>" as a single GTGT token (the right-shift operator).
   In nested applied generics — `Option<Result<T, E>>` — the inner and outer
   closing '>' fuse into that one GTGT, and the LR grammar, which expects a
   single GT to close each type-argument list, cannot proceed (issue #131).

   Real LR compilers (rustc, Roslyn) solve this by splitting the token at
   parse time, *driven by the parser's own state* rather than a lexical
   guess: a GTGT is re-read as two GT tokens only when, in the current state,
   the parser would accept GT but not GTGT — i.e. we are closing a type-
   argument list, where the shift operator is not grammatical.  Expression
   `a >> b` is untouched: there GTGT is acceptable, so no split happens.
   `>>>` lexes as GTGT then GT and is handled by one split plus the trailing
   GT; deeper nestings compose the same way.  This requires the incremental
   API so we can interrogate the parser via [I.acceptable]. *)
let drive (type a) ~file
    (start : Lexing.position -> a I.checkpoint)
    (next : unit -> Token.t * Span.t) : a =
  let pending : (Parser.token * Lexing.position * Lexing.position) option ref =
    ref None in
  let last_span = ref Span.dummy in
  let next_triple () =
    match !pending with
    | Some t -> pending := None; t
    | None ->
        let (tok, span) = next () in
        last_span := span;
        let sp = lexing_pos ~file:span.Span.file span.start_pos in
        let ep = lexing_pos ~file:span.Span.file span.end_pos in
        (to_menhir_token tok, sp, ep)
  in
  let rec run (cp : a I.checkpoint) =
    match cp with
    | I.InputNeeded _ ->
        let (ptok, sp, ep) = next_triple () in
        let triple =
          match ptok with
          | Parser.GTGT
            when (not (I.acceptable cp Parser.GTGT sp))
                 && I.acceptable cp Parser.GT sp ->
              let mid = { sp with Lexing.pos_cnum = sp.Lexing.pos_cnum + 1 } in
              pending := Some (Parser.GT, mid, ep);
              (Parser.GT, sp, mid)
          | _ -> (ptok, sp, ep)
        in
        run (I.offer cp triple)
    | I.Shifting _ | I.AboutToReduce _ -> run (I.resume cp)
    | I.HandlingError _ | I.Rejected -> raise Parser.Error
    | I.Accepted v -> v
  in
  try run (start (lexing_pos ~file { Span.line = 1; col = 1; offset = 0 }))
  with
  | Parser.Error ->
      raise (Parse_error ("Syntax error", !last_span))
  | Parser_errors.Parse_action_error (msg, startpos, endpos) ->
      let span = Span.make
        ~file
        ~start_pos:{ Span.line = startpos.Lexing.pos_lnum;
                     col = startpos.pos_cnum - startpos.pos_bol + 1;
                     offset = startpos.pos_cnum }
        ~end_pos:{ Span.line = endpos.Lexing.pos_lnum;
                   col = endpos.pos_cnum - endpos.pos_bol + 1;
                   offset = endpos.pos_cnum }
      in
      raise (Parse_error (msg, span))

(** Parse a program from a string *)
let parse_string ~file content =
  drive ~file Parser.Incremental.program (Lexer.from_string ~file content)

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
  drive ~file Parser.Incremental.expr_only (Lexer.from_string ~file content)
