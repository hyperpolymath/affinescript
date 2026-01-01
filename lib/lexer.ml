(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Lexer for AffineScript using sedlex *)

open Token

exception Lexer_error of string * Span.pos

(** Keywords lookup table *)
let keywords = Hashtbl.create 64
let () =
  List.iter (fun (k, v) -> Hashtbl.add keywords k v)
    [
      ("fn", FN);
      ("let", LET);
      ("mut", MUT);
      ("own", OWN);
      ("ref", REF);
      ("type", TYPE);
      ("struct", STRUCT);
      ("enum", ENUM);
      ("trait", TRAIT);
      ("impl", IMPL);
      ("effect", EFFECT);
      ("handle", HANDLE);
      ("resume", RESUME);
      ("match", MATCH);
      ("if", IF);
      ("else", ELSE);
      ("while", WHILE);
      ("for", FOR);
      ("return", RETURN);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("in", IN);
      ("where", WHERE);
      ("total", TOTAL);
      ("module", MODULE);
      ("use", USE);
      ("pub", PUB);
      ("as", AS);
      ("unsafe", UNSAFE);
      ("assume", ASSUME);
      ("transmute", TRANSMUTE);
      ("forget", FORGET);
      ("try", TRY);
      ("catch", CATCH);
      ("finally", FINALLY);
      ("true", TRUE);
      ("false", FALSE);
      ("omega", OMEGA);
      (* Built-in types *)
      ("Nat", NAT);
      ("Int", INT_T);
      ("Bool", BOOL);
      ("Float", FLOAT_T);
      ("String", STRING_T);
      ("Char", CHAR_T);
      ("Type", TYPE_K);
      ("Row", ROW);
      ("Never", NEVER);
    ]

(** Lexer state *)
type state = {
  mutable line : int;
  mutable col : int;
  mutable line_start : int;
  file : string;
}

let create_state file = { line = 1; col = 1; line_start = 0; file }

let current_pos state buf =
  let offset = Sedlexing.lexeme_start buf in
  { Span.line = state.line; col = offset - state.line_start + 1; offset }

let update_newlines state buf =
  let lexeme = Sedlexing.Utf8.lexeme buf in
  String.iter (fun c ->
    if c = '\n' then begin
      state.line <- state.line + 1;
      state.line_start <- Sedlexing.lexeme_end buf
    end
  ) lexeme

(** Character classes *)
let digit = [%sedlex.regexp? '0'..'9']
let hex_digit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let bin_digit = [%sedlex.regexp? '0' | '1']
let oct_digit = [%sedlex.regexp? '0'..'7']
let lower = [%sedlex.regexp? 'a'..'z']
let upper = [%sedlex.regexp? 'A'..'Z']
let alpha = [%sedlex.regexp? lower | upper]
let alphanum = [%sedlex.regexp? alpha | digit | '_']

let lower_ident = [%sedlex.regexp? lower, Star alphanum]
let upper_ident = [%sedlex.regexp? upper, Star alphanum]

let int_lit = [%sedlex.regexp? Opt '-', Plus digit]
let hex_lit = [%sedlex.regexp? "0x", Plus hex_digit]
let bin_lit = [%sedlex.regexp? "0b", Plus bin_digit]
let oct_lit = [%sedlex.regexp? "0o", Plus oct_digit]

let float_lit = [%sedlex.regexp?
  Opt '-', Plus digit, '.', Plus digit, Opt ('e' | 'E', Opt ('+' | '-'), Plus digit)]

let whitespace = [%sedlex.regexp? ' ' | '\t' | '\r']
let newline = [%sedlex.regexp? '\n']

(** Main lexer function *)
let rec token state buf =
  match%sedlex buf with
  (* Whitespace *)
  | Plus whitespace -> token state buf
  | newline ->
    state.line <- state.line + 1;
    state.line_start <- Sedlexing.lexeme_end buf;
    token state buf

  (* Comments *)
  | "//" ->
    line_comment state buf;
    token state buf
  | "/*" ->
    block_comment state buf 1;
    token state buf

  (* Literals *)
  | hex_lit ->
    let s = Sedlexing.Utf8.lexeme buf in
    INT (int_of_string s)
  | bin_lit ->
    let s = Sedlexing.Utf8.lexeme buf in
    INT (int_of_string s)
  | oct_lit ->
    let s = Sedlexing.Utf8.lexeme buf in
    INT (int_of_string s)
  | float_lit ->
    let s = Sedlexing.Utf8.lexeme buf in
    FLOAT (float_of_string s)
  | int_lit ->
    let s = Sedlexing.Utf8.lexeme buf in
    INT (int_of_string s)
  | '"' ->
    STRING (string_lit state buf (Buffer.create 64))
  | '\'' ->
    CHAR (char_lit state buf)

  (* Row variables *)
  | "..", lower_ident ->
    let s = Sedlexing.Utf8.lexeme buf in
    ROW_VAR (String.sub s 2 (String.length s - 2))

  (* Omega symbol *)
  | 0x03C9 -> OMEGA  (* ω *)

  (* Multi-char operators *)
  | "->" -> ARROW
  | "=>" -> FAT_ARROW
  | "::" -> COLONCOLON
  | ".." -> DOTDOT
  | "==" -> EQEQ
  | "!=" -> NE
  | "<=" -> LE
  | ">=" -> GE
  | "<<" -> LTLT
  | ">>" -> GTGT
  | "&&" -> AMPAMP
  | "||" -> PIPEPIPE
  | "+=" -> PLUSEQ
  | "-=" -> MINUSEQ
  | "*=" -> STAREQ
  | "/=" -> SLASHEQ

  (* Single-char tokens *)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | ',' -> COMMA
  | ';' -> SEMICOLON
  | ':' -> COLON
  | '.' -> DOT
  | '|' -> PIPE
  | '@' -> AT
  | '_' -> UNDERSCORE
  | '\\' -> BACKSLASH
  | '?' -> QUESTION
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '%' -> PERCENT
  | '=' -> EQ
  | '<' -> LT
  | '>' -> GT
  | '!' -> BANG
  | '&' -> AMP
  | '^' -> CARET
  | '~' -> TILDE

  (* Quantity: special handling for 0 and 1 after certain contexts *)
  (* For now, just lex as integers; parser will handle context *)

  (* Identifiers *)
  | lower_ident ->
    let s = Sedlexing.Utf8.lexeme buf in
    (try Hashtbl.find keywords s with Not_found -> LOWER_IDENT s)
  | upper_ident ->
    let s = Sedlexing.Utf8.lexeme buf in
    (try Hashtbl.find keywords s with Not_found -> UPPER_IDENT s)

  (* End of file *)
  | eof -> EOF

  (* Error *)
  | any ->
    let pos = current_pos state buf in
    let c = Sedlexing.Utf8.lexeme buf in
    raise (Lexer_error (Printf.sprintf "Unexpected character: %s" c, pos))
  | _ ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Unexpected end of input", pos))

and line_comment state buf =
  match%sedlex buf with
  | newline ->
    state.line <- state.line + 1;
    state.line_start <- Sedlexing.lexeme_end buf
  | eof -> ()
  | any -> line_comment state buf
  | _ -> ()

and block_comment state buf depth =
  match%sedlex buf with
  | "*/" ->
    if depth = 1 then ()
    else block_comment state buf (depth - 1)
  | "/*" ->
    block_comment state buf (depth + 1)
  | newline ->
    state.line <- state.line + 1;
    state.line_start <- Sedlexing.lexeme_end buf;
    block_comment state buf depth
  | eof ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Unterminated block comment", pos))
  | any ->
    block_comment state buf depth
  | _ ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Unexpected end of input in comment", pos))

and string_lit state buf acc =
  match%sedlex buf with
  | '"' -> Buffer.contents acc
  | "\\n" -> Buffer.add_char acc '\n'; string_lit state buf acc
  | "\\r" -> Buffer.add_char acc '\r'; string_lit state buf acc
  | "\\t" -> Buffer.add_char acc '\t'; string_lit state buf acc
  | "\\\\" -> Buffer.add_char acc '\\'; string_lit state buf acc
  | "\\\"" -> Buffer.add_char acc '"'; string_lit state buf acc
  | "\\'" -> Buffer.add_char acc '\''; string_lit state buf acc
  | "\\0" -> Buffer.add_char acc '\000'; string_lit state buf acc
  | "\\x", hex_digit, hex_digit ->
    let s = Sedlexing.Utf8.lexeme buf in
    let code = int_of_string ("0x" ^ String.sub s 2 2) in
    Buffer.add_char acc (Char.chr code);
    string_lit state buf acc
  | newline ->
    state.line <- state.line + 1;
    state.line_start <- Sedlexing.lexeme_end buf;
    Buffer.add_char acc '\n';
    string_lit state buf acc
  | eof ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Unterminated string literal", pos))
  | any ->
    Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
    string_lit state buf acc
  | _ ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Invalid character in string", pos))

and char_lit state buf =
  match%sedlex buf with
  | "\\n", '\'' -> '\n'
  | "\\r", '\'' -> '\r'
  | "\\t", '\'' -> '\t'
  | "\\\\", '\'' -> '\\'
  | "\\'", '\'' -> '\''
  | "\\\"", '\'' -> '"'
  | "\\0", '\'' -> '\000'
  | any, '\'' ->
    let s = Sedlexing.Utf8.lexeme buf in
    if String.length s = 2 then s.[0]
    else begin
      let pos = current_pos state buf in
      raise (Lexer_error ("Invalid character literal", pos))
    end
  | _ ->
    let pos = current_pos state buf in
    raise (Lexer_error ("Invalid character literal", pos))

(** Create a token stream from a string *)
let from_string ~file content =
  let buf = Sedlexing.Utf8.from_string content in
  let state = create_state file in
  fun () ->
    let start_pos = current_pos state buf in
    let tok = token state buf in
    let end_pos = current_pos state buf in
    (tok, Span.make ~file ~start_pos ~end_pos)

(** Create a token stream from a channel *)
let from_channel ~file chan =
  let buf = Sedlexing.Utf8.from_channel chan in
  let state = create_state file in
  fun () ->
    let start_pos = current_pos state buf in
    let tok = token state buf in
    let end_pos = current_pos state buf in
    (tok, Span.make ~file ~start_pos ~end_pos)
