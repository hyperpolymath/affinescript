(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Python-face: source-level transformer for Python-style AffineScript syntax.

    Maps Python surface syntax to canonical AffineScript before lexing and
    parsing with the standard parser. The compiler is face-agnostic (ADR-010):
    only this module and the error-vocabulary layer know about the Python face.

    Surface mappings:
    {v
      def name(...)       →  fn name(...)
      True / False        →  true / false
      None                →  ()
      and / or            →  && / ||
      not EXPR            →  !EXPR
      class Name          →  type Name
      pass                →  ()
      # comment           →  // comment
      import a.b          →  use a::b
      from a import b     →  use a::b
      if cond:            →  if cond {      (block-opening colon → brace)
      else:               →  } else {
      elif cond:          →  } else if cond {
      while cond:         →  while cond {
      for x in e:         →  for x in e {
      match e:            →  match e {
      handle e:           →  handle e {
      INDENT              →  (block opened by preceding {)
      DEDENT              →  }
      statement line      →  line;
    v}

    Implementation note: this is a line-by-line text preprocessor.
    Span information reported by the compiler refers to the canonical
    AffineScript text, not the original Python-style source.  Face-aware
    error vocabulary (ADR-010 §3) is a follow-up task.
*)

(* ─── Character helpers ──────────────────────────────────────────────── *)

let is_id_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_'

let starts_with s prefix =
  let sl = String.length s and pl = String.length prefix in
  sl >= pl && String.sub s 0 pl = prefix

(** Count leading spaces/tabs (each tab = 1 unit for simplicity). *)
let indent_of line =
  let len = String.length line in
  let i = ref 0 in
  while !i < len && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
  !i

(* ─── Comment stripping ──────────────────────────────────────────────── *)

(** Split a line at a Python-style [#] comment, respecting string literals.
    Returns [(code_part, comment_text option)].  The returned [code_part]
    still has its trailing whitespace. *)
let strip_py_comment line =
  let len = String.length line in
  let i = ref 0 in
  let in_str = ref false in
  let str_delim = ref '"' in
  while !i < len do
    let c = line.[!i] in
    if !in_str then begin
      if c = !str_delim && (!i = 0 || line.[!i - 1] <> '\\') then
        in_str := false
    end else begin
      if c = '#' then begin
        (* Found comment start — exit the loop *)
        let code = String.sub line 0 !i in
        let comment = String.sub line (!i + 1) (len - !i - 1) in
        i := len; (* break *)
        (* We need to return here but OCaml loops can't early-return;
           use a reference instead. *)
        ignore (code, comment) (* handled below via exception-free approach *)
      end else if c = '"' || c = '\'' then begin
        in_str := true;
        str_delim := c
      end
    end;
    incr i
  done;
  (* Re-scan cleanly without mutable trickery *)
  let result = ref (line, None) in
  let j = ref 0 in
  let in_s = ref false in
  let sd = ref '"' in
  while !j < len && not (not !in_s && line.[!j] = '#') do
    let c = line.[!j] in
    if !in_s then begin
      if c = !sd && (!j = 0 || line.[!j - 1] <> '\\') then in_s := false
    end else begin
      if c = '"' || c = '\'' then begin in_s := true; sd := c end
    end;
    incr j
  done;
  if !j < len && line.[!j] = '#' then
    result := (String.sub line 0 !j,
               Some (String.sub line (!j + 1) (len - !j - 1)));
  !result

(* ─── Word-level keyword substitution ───────────────────────────────── *)

(** Replace all whole-word occurrences of [from_w] with [to_w] in [s].
    "Whole-word" means not immediately preceded or followed by an id char.
    Does not track string literals — callers should apply to code-only text. *)
let replace_word ~from_w ~to_w s =
  let flen = String.length from_w in
  let slen = String.length s in
  let buf = Buffer.create slen in
  let i = ref 0 in
  while !i < slen do
    if slen - !i >= flen && String.sub s !i flen = from_w then begin
      let before_ok = !i = 0 || not (is_id_char s.[!i - 1]) in
      let after_ok = !i + flen >= slen || not (is_id_char s.[!i + flen]) in
      if before_ok && after_ok then begin
        Buffer.add_string buf to_w;
        i := !i + flen
      end else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Apply all Python→AffineScript keyword substitutions to a code fragment.
    Order matters: longer / more-specific patterns first. *)
let apply_keywords s =
  (* `not` as a whole word; `not x` → `! x` (lexer treats `!` and `x` correctly) *)
  let s = replace_word ~from_w:"not" ~to_w:"!" s in
  let s = replace_word ~from_w:"def"   ~to_w:"fn"    s in
  let s = replace_word ~from_w:"True"  ~to_w:"true"  s in
  let s = replace_word ~from_w:"False" ~to_w:"false" s in
  let s = replace_word ~from_w:"None"  ~to_w:"()"    s in
  let s = replace_word ~from_w:"and"   ~to_w:"&&"    s in
  let s = replace_word ~from_w:"or"    ~to_w:"||"    s in
  let s = replace_word ~from_w:"class" ~to_w:"type"  s in
  let s = replace_word ~from_w:"pass"  ~to_w:"()"    s in
  s

(* ─── Module path helpers ────────────────────────────────────────────── *)

(** Replace `.` with `::` in a dotted module path (e.g. `a.b.c` → `a::b::c`). *)
let dots_to_colons s =
  let len = String.length s in
  let buf = Buffer.create (len + 4) in
  String.iter (fun c ->
    if c = '.' then Buffer.add_string buf "::"
    else Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Try to transform an [import] or [from … import] line.
    Returns [Some canonical_line] or [None] if the line is not an import. *)
let transform_import_line stripped =
  if starts_with stripped "import " then begin
    let path = String.trim (String.sub stripped 7 (String.length stripped - 7)) in
    Some ("use " ^ dots_to_colons path ^ ";")
  end else if starts_with stripped "from " then begin
    let rest = String.trim (String.sub stripped 5 (String.length stripped - 5)) in
    (* Expect: MODULE_PATH import NAME [, NAME2 ...] *)
    (match String.split_on_char ' ' rest with
    | mod_part :: "import" :: names when names <> [] ->
      let name = String.concat "::" (List.filter (fun s -> s <> "") names) in
      Some ("use " ^ dots_to_colons mod_part ^ "::" ^ name ^ ";")
    | _ -> None)
  end else None

(* ─── Block-opening colon detection ─────────────────────────────────── *)

(** True if [stripped] ends with `:` (the block-opening colon pattern).
    We require the line to start with a known block-opening keyword or
    be one of the bare clause keywords so we don't eat type annotation colons
    that happen to be at line-end. *)
let block_opening_prefixes = [
  "if "; "while "; "for "; "with "; "match "; "handle ";
  "fn "; "def "; "class "; "type ";
  "try"; "except"; "finally"; "loop"; "effect ";
]

let is_block_opener stripped =
  let slen = String.length stripped in
  slen > 0 && stripped.[slen - 1] = ':'
  && (List.exists (fun p -> starts_with stripped p) block_opening_prefixes
      || stripped = "else:" || stripped = "try:" || stripped = "finally:")

(** Strip the trailing `:` from a block-opener and return the body. *)
let strip_block_colon stripped =
  let len = String.length stripped in
  assert (len > 0 && stripped.[len - 1] = ':');
  String.trim (String.sub stripped 0 (len - 1))

(* ─── Else / elif detection ──────────────────────────────────────────── *)

let is_else_clause stripped =
  stripped = "else:" || starts_with stripped "else :"

let is_elif_clause stripped =
  starts_with stripped "elif "

(** Extract the condition from an `elif COND:` line. *)
let elif_condition stripped =
  (* stripped = "elif COND:" *)
  let rest = String.sub stripped 5 (String.length stripped - 5) in
  let rest = String.trim rest in
  if String.length rest > 0 && rest.[String.length rest - 1] = ':' then
    String.trim (String.sub rest 0 (String.length rest - 1))
  else rest

(* ─── Main transformer ───────────────────────────────────────────────── *)

(** Transform Python-style AffineScript source text to canonical AffineScript.
    The result is valid input for the standard lexer + Menhir parser. *)
let transform_source source =
  let lines = String.split_on_char '\n' source in
  let out = Buffer.create (String.length source + 256) in
  (* Indentation stack: innermost level at head, outermost (0) at tail. *)
  let stack = ref [0] in

  let top () = match !stack with h :: _ -> h | [] -> 0 in

  let emit_dedents target =
    while top () > target do
      Buffer.add_string out "}\n";
      stack := List.tl !stack
    done
  in

  List.iter (fun raw_line ->
    let ind = indent_of raw_line in
    let (code_part, comment_opt) = strip_py_comment (String.trim raw_line) in
    let stripped = String.trim code_part in

    (* Append optional trailing comment in canonical // style *)
    let with_comment line_text =
      match comment_opt with
      | None -> line_text ^ "\n"
      | Some c -> line_text ^ " // " ^ String.trim c ^ "\n"
    in

    if stripped = "" then begin
      (* Blank or comment-only line *)
      (match comment_opt with
      | Some c -> Buffer.add_string out ("// " ^ String.trim c ^ "\n")
      | None   -> Buffer.add_char out '\n')
    end else if is_else_clause stripped then begin
      (* `else:` appears at the same indent as the matching `if`.
         emit_dedents closes the body of the then-branch (emitting `}`).
         We then continue with `else {` — no leading `}` here because
         emit_dedents already supplied it. *)
      emit_dedents ind;
      Buffer.add_string out (with_comment "else {")
    end else if is_elif_clause stripped then begin
      (* `elif COND:` — same structure as `else:` *)
      emit_dedents ind;
      let cond = apply_keywords (elif_condition stripped) in
      Buffer.add_string out (with_comment ("else if " ^ cond ^ " {"))
    end else begin
      (* Normal line (statement or block-opener) *)
      emit_dedents ind;
      (* Push a new indent level when we step in *)
      if ind > top () then stack := ind :: !stack;

      let indent_str = String.make ind ' ' in

      let line_text = match transform_import_line stripped with
        | Some s -> s
        | None ->
          if is_block_opener stripped then begin
            (* Replace trailing `:` with ` {` *)
            let body = apply_keywords (strip_block_colon stripped) in
            body ^ " {"
          end else begin
            (* Regular statement: terminate with `;` *)
            apply_keywords stripped ^ ";"
          end
      in
      Buffer.add_string out (indent_str ^ with_comment line_text)
    end
  ) lines;

  (* Close any blocks still open at EOF *)
  emit_dedents 0;

  Buffer.contents out

(* ─── Entry points ───────────────────────────────────────────────────── *)

(** Parse a Python-face program from a string, returning a canonical AST.
    Raises {!Parse_driver.Parse_error} or {!Lexer.Lexer_error} on failure. *)
let parse_string_python ~file content =
  let canonical = transform_source content in
  Parse_driver.parse_string ~file canonical

(** Parse a Python-face program from a file, returning a canonical AST. *)
let parse_file_python path =
  let chan = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let content = really_input_string chan (in_channel_length chan) in
      parse_string_python ~file:path content)

(** Expose the text-to-text transform for debugging and testing. *)
let preview_transform source = transform_source source
