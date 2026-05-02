(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** CafeScripto-face: source-level transformer for CoffeeScript-style
    AffineScript.

    Cafe is the catchment face for CoffeeScript developers. It marries
    significant indentation with arrow-function literals and the
    postfix-if / unless / until / @-prefix conveniences that make
    CoffeeScript feel like CoffeeScript.

    Surface mappings:
    {v
      # comment                     →  // comment
      ###  block  ###               →  /*  block  */
      class Foo extends Bar         →  type Foo  /* extends Bar */
      class Foo                     →  type Foo
      @x                            →  self.x
      @method()                     →  self.method()
      (x) -> body                   →  (x) => body
      (x) => body                   →  (x) => body          (CoffeeScript bound; canonical lambda)
      -> body                       →  () => body
      unless cond                   →  if !(cond)
      until cond                    →  while !(cond)
      expr if cond                  →  if cond { expr }
      expr unless cond              →  if !(cond) { expr }
      if x then y else z            →  if x { y } else { z }
      loop                          →  loop {
      Yes / On / true               →  true
      No / Off / false              →  false
      null / undefined              →  ()
      return expr   (last in block) →  expr        (last-expression semantics)
      INDENT                        →  (block opened by preceding {)
      DEDENT                        →  }
    v}

    Limitations (deferred to AST-level rewrites):
    - List comprehensions [(x*2 for x in xs when cond)] are NOT lowered;
      use [.map] / [.filter] explicitly.
    - No-paren calls [f x, y] are NOT lowered; write [f(x, y)].
    - Implicit object literals [a: 1, b: 2] without braces are NOT lowered
      where they cross indentation boundaries.
    - Splats and destructuring [...] are NOT lowered.
    - Span fidelity: errors refer to the canonical text, not Cafe source.
*)

(* ─── Character helpers ──────────────────────────────────────────────── *)

let is_id_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_'

let starts_with s prefix =
  let sl = String.length s and pl = String.length prefix in
  sl >= pl && String.sub s 0 pl = prefix

let ends_with s suffix =
  let sl = String.length s and tl = String.length suffix in
  sl >= tl && String.sub s (sl - tl) tl = suffix

let indent_of line =
  let len = String.length line in
  let i = ref 0 in
  while !i < len && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
  !i

(* ─── Comment handling ───────────────────────────────────────────────── *)

(** Split a line at a Cafe-style [#] comment, respecting string literals
    (including the backtick form CoffeeScript inherits from JS). Returns
    [(code_part, comment_text option)]. *)
let strip_hash_comment line =
  let len = String.length line in
  let in_str = ref false in
  let str_delim = ref '"' in
  let cut = ref (-1) in
  let i = ref 0 in
  while !i < len && !cut < 0 do
    let c = line.[!i] in
    if !in_str then begin
      if c = !str_delim && (!i = 0 || line.[!i - 1] <> '\\') then in_str := false
    end else begin
      if c = '"' || c = '\'' || c = '`' then begin in_str := true; str_delim := c end
      else if c = '#' then cut := !i
    end;
    incr i
  done;
  if !cut < 0 then (line, None)
  else (String.sub line 0 !cut,
        Some (String.sub line (!cut + 1) (len - !cut - 1)))

(** Convert a [###] block-comment delimiter line to canonical [/*] / [*/]. *)
let convert_block_comment_delim line =
  let trimmed = String.trim line in
  if trimmed = "###" then
    let indent_len = String.length line - String.length trimmed in
    let indent = String.sub line 0 indent_len in
    Some (indent, trimmed)
  else None

(* ─── Word substitution ──────────────────────────────────────────────── *)

let replace_word ~from_w ~to_w s =
  let flen = String.length from_w in
  let slen = String.length s in
  let buf = Buffer.create slen in
  let i = ref 0 in
  while !i < slen do
    if slen - !i >= flen && String.sub s !i flen = from_w then begin
      let before_ok = !i = 0 || not (is_id_char s.[!i - 1]) in
      let after_ok  = !i + flen >= slen || not (is_id_char s.[!i + flen]) in
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

(** CoffeeScript literal aliases. *)
let apply_literal_subs s =
  let s = replace_word ~from_w:"Yes"       ~to_w:"true"  s in
  let s = replace_word ~from_w:"yes"       ~to_w:"true"  s in
  let s = replace_word ~from_w:"On"        ~to_w:"true"  s in
  let s = replace_word ~from_w:"on"        ~to_w:"true"  s in
  let s = replace_word ~from_w:"No"        ~to_w:"false" s in
  let s = replace_word ~from_w:"no"        ~to_w:"false" s in
  let s = replace_word ~from_w:"Off"       ~to_w:"false" s in
  let s = replace_word ~from_w:"off"       ~to_w:"false" s in
  let s = replace_word ~from_w:"null"      ~to_w:"()"    s in
  let s = replace_word ~from_w:"undefined" ~to_w:"()"    s in
  s

(** Logical-keyword aliases. CoffeeScript accepts both [and]/[or]/[not]
    and the symbol forms. *)
let apply_logic_subs s =
  let s = replace_word ~from_w:"and" ~to_w:"&&" s in
  let s = replace_word ~from_w:"or"  ~to_w:"||" s in
  let s = replace_word ~from_w:"not" ~to_w:"!"  s in
  let s = replace_word ~from_w:"isnt" ~to_w:"!=" s in
  s

(* ─── @-shorthand ────────────────────────────────────────────────────── *)

(** Replace bare [@] (when followed by an id char or end) with [self.] or
    [self] respectively. Skips occurrences inside string literals. *)
let expand_at_shorthand s =
  let len = String.length s in
  let buf = Buffer.create len in
  let in_str = ref false in
  let str_delim = ref '"' in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    if !in_str then begin
      Buffer.add_char buf c;
      if c = !str_delim && (!i = 0 || s.[!i - 1] <> '\\') then in_str := false;
      incr i
    end else if c = '"' || c = '\'' || c = '`' then begin
      in_str := true; str_delim := c;
      Buffer.add_char buf c; incr i
    end else if c = '@' then begin
      let prev_ok = !i = 0 || not (is_id_char s.[!i - 1]) in
      if prev_ok then begin
        if !i + 1 < len && is_id_char s.[!i + 1] then
          Buffer.add_string buf "self."
        else
          Buffer.add_string buf "self"
      end else
        Buffer.add_char buf c;
      incr i
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* ─── Arrow function bodies ──────────────────────────────────────────── *)

(** [(args) -> body] / [-> body] / [(args) => body] → canonical [(args) => body].
    Only handles arrows that appear after a complete parameter parenthesis
    or as a leading [->] (parameter-less arrow). *)
let transform_arrow s =
  let len = String.length s in
  let buf = Buffer.create len in
  let in_str = ref false in
  let str_delim = ref '"' in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    if !in_str then begin
      Buffer.add_char buf c;
      if c = !str_delim && (!i = 0 || s.[!i - 1] <> '\\') then in_str := false;
      incr i
    end else if c = '"' || c = '\'' || c = '`' then begin
      in_str := true; str_delim := c;
      Buffer.add_char buf c; incr i
    end else if !i + 1 < len && c = '-' && s.[!i + 1] = '>' then begin
      (* Skip back past whitespace to find the previous non-blank char.
         For [(x) -> body] the layout is [)] [SP] [-] [>], so we must
         walk past the space to see the [)]. *)
      let prev_idx = ref (!i - 1) in
      while !prev_idx >= 0
            && (s.[!prev_idx] = ' ' || s.[!prev_idx] = '\t') do
        decr prev_idx
      done;
      let prev_char = if !prev_idx < 0 then None else Some s.[!prev_idx] in
      (* Don't touch a [-> T] that is part of a function return type:
         that pattern is preceded by some identifier or [)] that's part
         of a type ascription [s: T] context — heuristic: if the
         preceding non-blank char is an identifier char (lowercase
         letter, common in type names), it's likely a type arrow, not a
         lambda. We treat only [)] (paren-arrow) and start-of-line /
         post-[=] (bare arrow) as lambda introducers. *)
      (match prev_char with
       | Some ')' ->
         Buffer.add_string buf "=>";
         i := !i + 2
       | None | Some '=' ->
         Buffer.add_string buf "() =>";
         i := !i + 2
       | _ ->
         (* Likely a type arrow [-> T] — leave intact. *)
         Buffer.add_char buf c;
         incr i)
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* ─── Postfix conditionals ───────────────────────────────────────────── *)

(** Move [expr if cond] / [expr unless cond] to canonical prefix form. *)
let transform_postfix_conditional stripped =
  let if_re     = Str.regexp_string " if "     in
  let unless_re = Str.regexp_string " unless " in
  (* Postfix-if first; only if [if] doesn't start the line. *)
  let try_split kw_pos kw_len negate =
    let body = String.trim (String.sub stripped 0 kw_pos) in
    let cond = String.trim (String.sub stripped (kw_pos + kw_len)
                              (String.length stripped - kw_pos - kw_len)) in
    if negate then
      Printf.sprintf "if !(%s) { %s }" cond body
    else
      Printf.sprintf "if %s { %s }" cond body
  in
  if not (starts_with stripped "if ") then begin
    try Some (try_split (Str.search_forward if_re stripped 0) 4 false)
    with Not_found ->
      try Some (try_split (Str.search_forward unless_re stripped 0) 8 true)
      with Not_found -> None
  end else begin
    try Some (try_split (Str.search_forward unless_re stripped 0) 8 true)
    with Not_found -> None
  end

(* ─── Block-opener detection ─────────────────────────────────────────── *)

(** A line is a block-opener if it's a control structure that introduces
    an indented body, where CoffeeScript would emit no brace but Cafe
    needs one. We trigger on the family head and let the indent stack
    insert [}] on dedent. *)
let block_opening_prefixes = [
  "if "; "while "; "for "; "switch "; "loop"; "try"; "catch"; "finally";
  "class "; "->"; "=>";
]

(** Lines that are already canonical AffineScript and should pass through
    untouched — emitted without a trailing semicolon since they introduce
    or close a block on their own. *)
let is_canonical_passthrough stripped =
  let prefixes = ["effect "; "trait "; "impl "; "type "; "fn "; "module ";
                  "use "; "pub "; "struct "; "enum "] in
  List.exists (fun p -> starts_with stripped p) prefixes
  || stripped = "}" || stripped = "};"

(** True if [stripped] is a control-flow block opener whose body is
    indented (no inline [then …]). *)
let is_indent_block_opener stripped =
  if stripped = "loop" || stripped = "try" || stripped = "finally" then true
  else if List.exists (fun p -> starts_with stripped p) block_opening_prefixes then begin
    (* If the line contains "then" inline, it's a single-line if and not
       an indent opener. *)
    let has_inline_then =
      try ignore (Str.search_forward (Str.regexp_string " then ") stripped 0); true
      with Not_found -> false
    in
    not has_inline_then
  end else false

(** Render the head of a block-opener as canonical syntax. *)
let render_block_head stripped =
  if stripped = "loop" then "loop {"
  else if stripped = "try" then "try {"
  else if stripped = "finally" then "} finally {"
  else if starts_with stripped "else if " then begin
    let rest = String.sub stripped 8 (String.length stripped - 8) in
    "} else if " ^ rest ^ " {"
  end
  else if stripped = "else" then "} else {"
  else if starts_with stripped "catch " then
    let rest = String.sub stripped 6 (String.length stripped - 6) in
    "} catch (" ^ rest ^ ") {"
  else if starts_with stripped "class " then begin
    let rest = String.sub stripped 6 (String.length stripped - 6) in
    let rest =
      try
        let i = Str.search_forward (Str.regexp_string " extends ") rest 0 in
        let name = String.sub rest 0 i in
        let parent = String.sub rest (i + 9) (String.length rest - i - 9) in
        Printf.sprintf "%s /* extends %s */" name parent
      with Not_found -> rest
    in
    "type " ^ rest ^ " {"
  end
  else stripped ^ " {"

(* ─── Inline if/then/else, unless, until ─────────────────────────────── *)

(** [if c then a else b] → braced form. [unless], [until] are preprocessed
    into [if !(...)] / [while !(...)] in [normalise_negated_keywords]. *)
let transform_if_then_else stripped =
  if not (starts_with stripped "if ") then None
  else begin
    let then_re = Str.regexp_string " then " in
    let else_re = Str.regexp_string " else " in
    try
      let then_pos = Str.search_forward then_re stripped 0 in
      let cond = String.trim (String.sub stripped 3 (then_pos - 3)) in
      try
        let else_pos = Str.search_forward else_re stripped (then_pos + 5) in
        let t_branch = String.trim
          (String.sub stripped (then_pos + 6) (else_pos - then_pos - 6)) in
        let e_branch = String.trim
          (String.sub stripped (else_pos + 6) (String.length stripped - else_pos - 6)) in
        Some (Printf.sprintf "if %s { %s } else { %s }" cond t_branch e_branch)
      with Not_found ->
        let body = String.trim
          (String.sub stripped (then_pos + 6) (String.length stripped - then_pos - 6)) in
        Some (Printf.sprintf "if %s { %s }" cond body)
    with Not_found -> None
  end

(** Replace leading [unless cond] / [until cond] with negated forms. *)
let normalise_negated_keywords stripped =
  if starts_with stripped "unless " then
    "if !(" ^ String.sub stripped 7 (String.length stripped - 7) ^ ")"
  else if starts_with stripped "until " then
    "while !(" ^ String.sub stripped 6 (String.length stripped - 6) ^ ")"
  else stripped

(* ─── return stripping (last-expression) ─────────────────────────────── *)

let strip_return stripped =
  if starts_with stripped "return " then
    String.sub stripped 7 (String.length stripped - 7)
  else if stripped = "return" then "()"
  else stripped

(* ─── Blank-line check ───────────────────────────────────────────────── *)

let is_blank_line raw =
  let (code, _) = strip_hash_comment (String.trim raw) in
  String.trim code = ""

(* ─── Main transformer ───────────────────────────────────────────────── *)

let transform_source source =
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let n = Array.length lines in
  let out = Buffer.create (String.length source + 256) in
  let stack = ref [0] in
  let in_block_comment = ref false in
  (* Tracks how many canonical-passthrough lines ending in [{] are
     currently open. While [brace_depth > 0] we don't push indent
     levels for body lines — the explicit braces handle scope and any
     [}] in the source closes them, so synthetic indent-driven [}]
     would double-close. *)
  let brace_depth = ref 0 in

  let top () = match !stack with h :: _ -> h | [] -> 0 in

  let emit_dedents target =
    while top () > target do
      Buffer.add_string out "}\n";
      stack := List.tl !stack
    done
  in

  let next_meaningful_indent i =
    let j = ref (i + 1) in
    while !j < n && is_blank_line lines.(!j) do incr j done;
    if !j >= n then -1 else indent_of lines.(!j)
  in

  for i = 0 to n - 1 do
    let raw_line = lines.(i) in

    (* ### block-comment delimiters: convert and pass through. *)
    (match convert_block_comment_delim raw_line with
     | Some (indent, _) when not !in_block_comment ->
       in_block_comment := true;
       Buffer.add_string out (indent ^ "/*\n")
     | Some (indent, _) when !in_block_comment ->
       in_block_comment := false;
       Buffer.add_string out (indent ^ "*/\n")
     | _ ->
       if !in_block_comment then begin
         Buffer.add_string out raw_line;
         Buffer.add_char out '\n'
       end else begin
         let ind = indent_of raw_line in
         let (code_part, comment_opt) = strip_hash_comment (String.trim raw_line) in
         let stripped = String.trim code_part in

         let with_comment line_text =
           match comment_opt with
           | None   -> line_text ^ "\n"
           | Some c -> line_text ^ " // " ^ String.trim c ^ "\n"
         in

         if stripped = "" then begin
           (match comment_opt with
           | Some c -> Buffer.add_string out ("// " ^ String.trim c ^ "\n")
           | None   -> Buffer.add_char out '\n')
         end else if is_canonical_passthrough stripped then begin
           (* Canonical AffineScript declaration line. The user is
              embedding a canonical block ([effect], [fn], [trait], etc.)
              directly. Pass through verbatim — don't run the arrow /
              literal / logic pipeline (it would mangle [-> T] return
              types and other canonical syntax). We DO emit any pending
              dedents at this indent level so a closing canonical [}]
              at zero indent properly drops indented Cafe-block scopes
              that were pushed earlier. *)
           emit_dedents ind;
           let n_chars = String.length stripped in
           if n_chars > 0 && stripped.[n_chars - 1] = '{' then
             incr brace_depth
           else if (stripped = "}" || stripped = "};") && !brace_depth > 0 then
             decr brace_depth;
           let indent_str = String.make ind ' ' in
           Buffer.add_string out (indent_str ^ with_comment stripped)
         end else begin
           (* Inside an explicit-brace canonical block we suppress the
              indent push — the user's [{ … }] handles scope, and any
              indent-driven [}] would double-close. *)
           if !brace_depth = 0 then begin
             emit_dedents ind;
             if ind > top () then stack := ind :: !stack
           end;

           let indent_str = String.make ind ' ' in
           let next_ind = next_meaningful_indent i in
           let is_tail = next_ind < ind in

           (* Pipeline of expression-level rewrites. *)
           let prepared =
             stripped
             |> normalise_negated_keywords
             |> apply_literal_subs
             |> apply_logic_subs
             |> expand_at_shorthand
             |> transform_arrow
             |> strip_return
           in

           (* Add [;] only when the line doesn't already end with one
              (CoffeeScript users may write the [;] explicitly when
              embedding canonical-style statements). *)
           let with_semi s =
             if ends_with (String.trim s) ";" then s else s ^ ";"
           in
           let line_text =
             match transform_postfix_conditional prepared with
             | Some s ->
               if is_tail then s else with_semi s
             | None ->
               match transform_if_then_else prepared with
               | Some s ->
                 if is_tail then s else with_semi s
               | None ->
                 if is_indent_block_opener prepared then
                   render_block_head prepared
                 else if is_tail then
                   prepared
                 else
                   with_semi prepared
           in
           Buffer.add_string out (indent_str ^ with_comment line_text)
         end
       end)
  done;
  emit_dedents 0;
  Buffer.contents out

(* ─── Entry points ───────────────────────────────────────────────────── *)

let parse_string_cafe ~file content =
  let canonical = transform_source content in
  Parse_driver.parse_string ~file canonical

let parse_file_cafe path =
  let chan = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let content = really_input_string chan (in_channel_length chan) in
      parse_string_cafe ~file:path content)

let preview_transform source = transform_source source
