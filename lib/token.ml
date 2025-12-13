(** Token types for the AffineScript lexer *)

type t =
  (* Literals *)
  | INT of int
  | FLOAT of float
  | CHAR of char
  | STRING of string
  | TRUE
  | FALSE

  (* Identifiers *)
  | LOWER_IDENT of string    (** lowercase identifier *)
  | UPPER_IDENT of string    (** Uppercase identifier *)
  | ROW_VAR of string        (** ..rowvar *)

  (* Keywords *)
  | FN
  | LET
  | MUT
  | OWN
  | REF
  | TYPE
  | STRUCT
  | ENUM
  | TRAIT
  | IMPL
  | EFFECT
  | HANDLE
  | RESUME
  | MATCH
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | BREAK
  | CONTINUE
  | IN
  | WHERE
  | TOTAL
  | MODULE
  | USE
  | PUB
  | AS
  | UNSAFE
  | ASSUME
  | TRANSMUTE
  | FORGET
  | TRY
  | CATCH
  | FINALLY

  (* Built-in type names *)
  | NAT
  | INT_T
  | BOOL
  | FLOAT_T
  | STRING_T
  | CHAR_T
  | TYPE_K
  | ROW
  | NEVER

  (* Punctuation *)
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMICOLON
  | COLON
  | COLONCOLON
  | DOT
  | DOTDOT
  | ARROW         (** -> *)
  | FAT_ARROW     (** => *)
  | PIPE
  | AT
  | UNDERSCORE
  | BACKSLASH     (** \ for row restriction *)
  | QUESTION      (** ? for error propagation *)

  (* Quantity annotations *)
  | ZERO          (** 0 *)
  | ONE           (** 1 *)
  | OMEGA         (** ω or omega *)

  (* Operators *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQ
  | EQEQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | AMPAMP
  | PIPEPIPE
  | BANG
  | AMP
  | CARET
  | TILDE
  | LTLT
  | GTGT
  | PLUSEQ
  | MINUSEQ
  | STAREQ
  | SLASHEQ

  (* Special *)
  | EOF
[@@deriving show, eq]

(** Get string representation for error messages *)
let to_string = function
  | INT n -> Printf.sprintf "integer %d" n
  | FLOAT f -> Printf.sprintf "float %f" f
  | CHAR c -> Printf.sprintf "char '%c'" c
  | STRING s -> Printf.sprintf "string \"%s\"" s
  | TRUE -> "true"
  | FALSE -> "false"
  | LOWER_IDENT s -> Printf.sprintf "identifier '%s'" s
  | UPPER_IDENT s -> Printf.sprintf "type '%s'" s
  | ROW_VAR s -> Printf.sprintf "row variable '..%s'" s
  | FN -> "fn"
  | LET -> "let"
  | MUT -> "mut"
  | OWN -> "own"
  | REF -> "ref"
  | TYPE -> "type"
  | STRUCT -> "struct"
  | ENUM -> "enum"
  | TRAIT -> "trait"
  | IMPL -> "impl"
  | EFFECT -> "effect"
  | HANDLE -> "handle"
  | RESUME -> "resume"
  | MATCH -> "match"
  | IF -> "if"
  | ELSE -> "else"
  | WHILE -> "while"
  | FOR -> "for"
  | RETURN -> "return"
  | BREAK -> "break"
  | CONTINUE -> "continue"
  | IN -> "in"
  | WHERE -> "where"
  | TOTAL -> "total"
  | MODULE -> "module"
  | USE -> "use"
  | PUB -> "pub"
  | AS -> "as"
  | UNSAFE -> "unsafe"
  | ASSUME -> "assume"
  | TRANSMUTE -> "transmute"
  | FORGET -> "forget"
  | TRY -> "try"
  | CATCH -> "catch"
  | FINALLY -> "finally"
  | NAT -> "Nat"
  | INT_T -> "Int"
  | BOOL -> "Bool"
  | FLOAT_T -> "Float"
  | STRING_T -> "String"
  | CHAR_T -> "Char"
  | TYPE_K -> "Type"
  | ROW -> "Row"
  | NEVER -> "Never"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | COMMA -> ","
  | SEMICOLON -> ";"
  | COLON -> ":"
  | COLONCOLON -> "::"
  | DOT -> "."
  | DOTDOT -> ".."
  | ARROW -> "->"
  | FAT_ARROW -> "=>"
  | PIPE -> "|"
  | AT -> "@"
  | UNDERSCORE -> "_"
  | BACKSLASH -> "\\"
  | QUESTION -> "?"
  | ZERO -> "0"
  | ONE -> "1"
  | OMEGA -> "ω"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | PERCENT -> "%"
  | EQ -> "="
  | EQEQ -> "=="
  | NE -> "!="
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | AMPAMP -> "&&"
  | PIPEPIPE -> "||"
  | BANG -> "!"
  | AMP -> "&"
  | CARET -> "^"
  | TILDE -> "~"
  | LTLT -> "<<"
  | GTGT -> ">>"
  | PLUSEQ -> "+="
  | MINUSEQ -> "-="
  | STAREQ -> "*="
  | SLASHEQ -> "/="
  | EOF -> "end of file"
