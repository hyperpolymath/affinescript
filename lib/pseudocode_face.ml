(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Pseudocode-face: source-level transformer for natural-language-adjacent
    pseudocode syntax.

    AffineScript's design goal: "should be easier to write than JavaScript,
    Python, or pseudocode."  The pseudocode face meets learners who write
    algorithmic pseudocode (textbook style, interview-whiteboard style) and
    lets them typecheck / run their programs without learning AffineScript
    syntax first.

    Surface mappings:
    {v
      function/procedure name(params) returns T  →  fn name(params) -> T
      function name(params)                      →  fn name(params)
      set x to expr                              →  let x = expr
      set x to mut expr                          →  let mut x = expr
      return expr                                →  expr   (last-expression)
      if condition then                          →  if condition {
      else if condition then                     →  } else if condition {
      else                                       →  } else {
      end if / end while / end for / end / fi   →  }
      while condition do / while condition       →  while condition {
      for x in expr do / for x in expr          →  for x in expr {
      match expr on / match expr                 →  match expr {
      case p =>                                  →  p =>
      and / or / not                             →  && / || / !
      is / equals                                →  ==
      is not / not equals                        →  !=
      is less than                               →  <
      is greater than                            →  >
      is at most / is <= / at most               →  <=
      is at least / is >= / at least             →  >=
      None / nothing / null / nil                →  ()
      yes / YES                                  →  true
      no / NO                                    →  false
      output expr / print expr / display expr   →  IO.println(expr)
      // comment                                 →  (already valid)
      -- comment (Haskell/SQL style)             →  // comment
    v}

    Limitation — [return]: pseudocode uses explicit [return] but AffineScript
    uses last-expression semantics.  The preprocessor strips [return] from the
    last statement in a block.  Interior [return] is not supported in this
    version (it would require restructuring the block, which needs AST-level
    knowledge).

    Limitation — span fidelity: error spans refer to the transformed canonical
    text.
*)

(* ─── Character helpers ────────────────────────────────────────────────── *)

let is_id_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_'

let starts_with s prefix =
  let sl = String.length s and pl = String.length prefix in
  sl >= pl && String.sub s 0 pl = prefix

let ends_with s suffix =
  let sl = String.length s and tl = String.length suffix in
  sl >= tl && String.sub s (sl - tl) tl = suffix

(** Word-boundary-aware substitution. *)
let subst_word line kw replacement =
  let kl = String.length kw and ll = String.length line in
  let buf = Buffer.create ll in
  let i = ref 0 in
  while !i < ll do
    if !i + kl <= ll && String.sub line !i kl = kw then begin
      let before_ok = !i = 0 || not (is_id_char line.[!i - 1]) in
      let after_ok  = !i + kl >= ll || not (is_id_char line.[!i + kl]) in
      if before_ok && after_ok then begin
        Buffer.add_string buf replacement;
        i := !i + kl
      end else begin
        Buffer.add_char buf line.[!i];
        incr i
      end
    end else begin
      Buffer.add_char buf line.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Transform [-- comment] (Haskell/SQL style) to [// comment]. *)
let transform_double_dash_comment line =
  (* Replace leading "--" with "//" at any indent level.
     Only if not inside a string and not inside an existing "//" comment. *)
  let trimmed = String.trim line in
  if starts_with trimmed "--" then
    let indent_len = String.length line - String.length (String.trim line) in
    let indent = String.sub line 0 indent_len in
    indent ^ "//" ^ String.sub trimmed 2 (String.length trimmed - 2)
  else line

(* ─── Operator / keyword substitutions ────────────────────────────────── *)

(** Apply multi-word comparisons first (longest-match order). *)
let apply_comparisons line =
  let line = subst_word line "is not equal to" "!=" in
  let line = subst_word line "is not"          "!=" in
  let line = subst_word line "not equals"      "!=" in
  let line = subst_word line "is less than or equal to" "<=" in
  let line = subst_word line "is greater than or equal to" ">=" in
  let line = subst_word line "is at most"      "<=" in
  let line = subst_word line "at most"         "<=" in
  let line = subst_word line "is at least"     ">=" in
  let line = subst_word line "at least"        ">=" in
  let line = subst_word line "is less than"    "<" in
  let line = subst_word line "is greater than" ">" in
  let line = subst_word line "is equal to"     "==" in
  let line = subst_word line "equals"          "==" in
  let line = subst_word line "is"              "==" in
  line

let apply_boolean_ops line =
  let line = subst_word line "and" "&&" in
  let line = subst_word line "or"  "||" in
  (* "not" needs care — don't clobber "not equal" which was already handled *)
  let line = subst_word line "not " "!" in
  line

let apply_literal_subs line =
  let line = subst_word line "None"    "()" in
  let line = subst_word line "nothing" "()" in
  let line = subst_word line "null"    "()" in
  let line = subst_word line "nil"     "()" in
  let line = subst_word line "yes"     "true" in
  let line = subst_word line "YES"     "true" in
  let line = subst_word line "no"      "false" in
  let line = subst_word line "NO"      "false" in
  line

(* ─── Statement-level transforms ──────────────────────────────────────── *)

(** [function/procedure name(params) returns T {] → [fn name(params) -> T {] *)
let transform_function_decl trimmed =
  let strip_fn_keyword line =
    if starts_with line "function " then
      String.sub line 9 (String.length line - 9)
    else if starts_with line "procedure " then
      String.sub line 10 (String.length line - 10)
    else line
  in
  let body = strip_fn_keyword trimmed in
  (* Replace " returns " with " -> " *)
  let body = subst_word body "returns" "->" in
  (* If line ends with "do" or "then", strip it and add "{" *)
  let body =
    if ends_with body " do" then String.sub body 0 (String.length body - 3) ^ " {"
    else if ends_with body " then" then String.sub body 0 (String.length body - 5) ^ " {"
    else if ends_with body ")" then body ^ " {"
    else body
  in
  "fn " ^ body

(** [set x to expr] → [let x = expr] *)
let transform_set line =
  let line = String.trim line in
  if starts_with line "set " then begin
    let rest = String.sub line 4 (String.length line - 4) in
    (* Look for " to " *)
    match String.split_on_char ' ' rest with
    | name :: "to" :: "mut" :: value_parts ->
      Printf.sprintf "let mut %s = %s" name (String.concat " " value_parts)
    | name :: "to" :: value_parts ->
      Printf.sprintf "let %s = %s" name (String.concat " " value_parts)
    | _ -> line
  end else line

(** Strip [return] from a line (to be used on tail-position lines). *)
let strip_return line =
  let trimmed = String.trim line in
  if starts_with trimmed "return " then begin
    let indent_len = String.length line - String.length (String.trim line) in
    String.sub line 0 indent_len ^
    String.sub trimmed 7 (String.length trimmed - 7)
  end else line

(** [output expr] / [print expr] / [display expr] → [IO.println(expr)] *)
let transform_io_output line =
  let t = String.trim line in
  let indent_len = String.length line - String.length t in
  let indent = String.sub line 0 indent_len in
  let try_io keyword =
    if starts_with t keyword then begin
      let rest = String.trim (String.sub t (String.length keyword)
                                (String.length t - String.length keyword)) in
      Some (indent ^ "IO.println(" ^ rest ^ ")")
    end else None
  in
  match try_io "output " with
  | Some r -> r
  | None -> (match try_io "print " with
    | Some r -> r
    | None -> (match try_io "display " with
      | Some r -> r
      | None -> line))

(** Transform control-flow block openers. *)
let transform_control_flow line =
  let t = String.trim line in
  let indent_len = String.length line - String.length t in
  let indent = String.sub line 0 indent_len in
  let open_block cond keyword_len =
    indent ^ "if " ^ String.trim cond ^ " {"
    |> fun _ ->
    let cond_str = String.trim (String.sub t keyword_len (String.length t - keyword_len)) in
    (* Strip trailing " then" / " do" *)
    let cond_str =
      if ends_with cond_str " then" then String.sub cond_str 0 (String.length cond_str - 5)
      else if ends_with cond_str " do" then String.sub cond_str 0 (String.length cond_str - 3)
      else cond_str
    in
    ignore cond;
    indent ^ "if " ^ cond_str ^ " {"
  in
  let open_while cond =
    indent ^ "while " ^ cond ^ " {"
  in
  let open_for cond =
    indent ^ "for " ^ cond ^ " {"
  in
  if starts_with t "else if " then begin
    let rest = String.sub t 8 (String.length t - 8) in
    let rest = subst_word rest "then" "" in
    let rest = String.trim rest in
    indent ^ "} else if " ^ rest ^ " {"
  end else if t = "else" then
    indent ^ "} else {"
  else if starts_with t "if " then
    open_block "" (String.length "if ")
  else if starts_with t "while " then begin
    let rest = String.sub t 6 (String.length t - 6) in
    let rest = if ends_with rest " do" then String.sub rest 0 (String.length rest - 3) else rest in
    open_while (String.trim rest)
  end else if starts_with t "for " then begin
    let rest = String.sub t 4 (String.length t - 4) in
    let rest = if ends_with rest " do" then String.sub rest 0 (String.length rest - 3) else rest in
    open_for (String.trim rest)
  end else if starts_with t "match " then begin
    let rest = String.sub t 6 (String.length t - 6) in
    let rest = if ends_with rest " on" then String.sub rest 0 (String.length rest - 3) else rest in
    indent ^ "match " ^ String.trim rest ^ " {"
  end else if t = "end if" || t = "end while" || t = "end for"
           || t = "end match" || t = "end" || t = "fi" || t = "od" then
    indent ^ "}"
  else line

(* ─── Line-by-line transform ────────────────────────────────────────────── *)

let transform_line line =
  let t = String.trim line in
  if t = "" then line
  else if starts_with t "// " || starts_with t "//" then line (* already valid comment *)
  else if starts_with t "/*" || starts_with t "*" then line   (* block comment *)
  else begin
    (* 1. Convert double-dash comments *)
    let line = transform_double_dash_comment line in
    let t = String.trim line in
    (* 2. Function/procedure declarations *)
    if starts_with t "function " || starts_with t "procedure " then
      transform_function_decl t
    (* 3. set ... to ... *)
    else if starts_with t "set " then
      transform_set line
    (* 4. Output / print / display *)
    else if starts_with t "output " || starts_with t "print "
         || starts_with t "display " then
      transform_io_output line
    (* 5. Control flow *)
    else if starts_with t "if " || t = "else" || starts_with t "else if "
         || starts_with t "while " || starts_with t "for "
         || starts_with t "match "
         || t = "end if" || t = "end while" || t = "end for"
         || t = "end match" || t = "end" || t = "fi" || t = "od" then
      transform_control_flow line
    (* 6. Strip return (last-expression semantics) *)
    else if starts_with t "return " then
      strip_return line
    (* 7. case patterns → AffineScript match arms *)
    else if starts_with t "case " then begin
      let rest = String.sub t 5 (String.length t - 5) in
      let rest = if ends_with rest " =>" then rest
                 else if ends_with rest ":" then
                   String.sub rest 0 (String.length rest - 1) ^ " =>"
                 else rest ^ " =>" in
      let indent_len = String.length line - String.length (String.trim line) in
      String.sub line 0 indent_len ^ rest
    end
    else begin
      (* 8. General keyword substitutions *)
      let line = apply_comparisons line in
      let line = apply_boolean_ops line in
      let line = apply_literal_subs line in
      line
    end
  end

(* ─── File-level entry points ─────────────────────────────────────────── *)

let transform_source source =
  let lines = String.split_on_char '\n' source in
  let out = List.map transform_line lines in
  String.concat "\n" out

let parse_file_pseudocode path =
  let source = In_channel.with_open_text path In_channel.input_all in
  let canonical = transform_source source in
  Parse_driver.parse_string ~file:path canonical

let preview_transform source =
  transform_source source
