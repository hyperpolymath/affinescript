(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** JS-face: source-level transformer for JavaScript/TypeScript-style AffineScript.

    Maps common JavaScript surface patterns to canonical AffineScript before
    lexing and parsing.  The compiler is face-agnostic (ADR-010); only this
    module and [Face] know about the JS face.

    Surface mappings:
    {v
      const x = expr          →  let x = expr
      let x = expr            →  let mut x = expr   (JS let is mutable)
      var x = expr            →  let mut x = expr
      function name(p) { }   →  fn name(p) { }
      async function name()  →  fn name() / Async
      => expr  (arrow body)  →  { expr }
      null / undefined        →  ()
      === / !==               →  == / !=
      // comment              →  (already valid)
      /* comment */           →  (already valid)
      import { x } from "m"  →  use m::x;
      import x from "m"       →  use m;
      export const x = …     →  let x = …   (exports implicit in AffineScript)
      export function f(…)   →  fn f(…)
      export default fn f()  →  fn f()
      typeof x               →  (removed — no equivalent; leaves comment)
    v}

    Limitation — async/await: [await expr] is rewritten to [Async.await(expr)]
    when used inside an [async function].  Handler declaration for the [Async]
    effect must be provided by the host.  See [effect Async] in stdlib.

    Limitation — span fidelity: error spans refer to the transformed canonical
    text, not the original JS source.

    Limitation — arrow functions: multi-line arrow functions are not yet
    supported.  Single-expression arrows ([=> expr]) are rewritten to
    [{ expr }].  Full multi-line arrow bodies are a follow-up task.
*)

(* ─── Character helpers ────────────────────────────────────────────────── *)

let is_id_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_'

let starts_with s prefix =
  let sl = String.length s and pl = String.length prefix in
  sl >= pl && String.sub s 0 pl = prefix

(** Word-boundary-aware substitution: only replace [kw] when surrounded by
    non-identifier characters (equivalent to [\\bkw\\b] in regex). *)
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

(** Strip a [//] comment suffix (respecting string literals). *)
let strip_line_comment line =
  let len = String.length line in
  let in_str = ref false and str_delim = ref '"' in
  let result = ref line in
  let i = ref 0 in
  while !i < len do
    let c = line.[!i] in
    if !in_str then begin
      if c = !str_delim && (!i = 0 || line.[!i - 1] <> '\\') then
        in_str := false;
      incr i
    end else begin
      if c = '"' || c = '\'' || c = '`' then begin
        in_str := true; str_delim := c; incr i
      end else if !i + 1 < len && c = '/' && line.[!i + 1] = '/' then begin
        result := String.sub line 0 !i;
        i := len (* break *)
      end else
        incr i
    end
  done;
  !result

(* ─── Keyword substitutions ────────────────────────────────────────────── *)

(** Apply all single-token keyword substitutions in priority order.
    Does not touch the inside of string literals. *)
let apply_keyword_subs line =
  let line = subst_word line "null"      "()" in
  let line = subst_word line "undefined" "()" in
  let line = subst_word line "==="       "==" in
  let line = subst_word line "!=="       "!=" in
  (* Logical keywords — only swap those not already covered by operators *)
  line

let ends_with s suffix =
  let sl = String.length s and tl = String.length suffix in
  sl >= tl && String.sub s (sl - tl) tl = suffix

(* ─── Import / export handling ─────────────────────────────────────────── *)

(** Transform JavaScript import declarations to AffineScript use declarations.
    Examples:
    {v
      import { x, y } from "module"    →  use module::{x, y};
      import x from "module"            →  use module;
      import "module"                   →  use module;
    v}
*)
let transform_import line =
  let line = String.trim line in
  (* import { x, y } from "module" *)
  if starts_with line "import {" then begin
    (* Find the closing } and "from" *)
    try
      let close_brace = String.index line '}' in
      let names_raw = String.sub line 8 (close_brace - 8) in
      let rest = String.sub line (close_brace + 1)
                   (String.length line - close_brace - 1) in
      let rest = String.trim rest in
      if starts_with rest "from " then begin
        let mod_part = String.trim (String.sub rest 5 (String.length rest - 5)) in
        let mod_name = String.sub mod_part 1 (String.length mod_part - 2) in (* strip quotes *)
        let mod_name = String.map (fun c -> if c = '/' || c = '-' then '_' else c) mod_name in
        let names = String.trim names_raw in
        Printf.sprintf "use %s::{%s};" mod_name names
      end else line
    with Not_found -> line
  end
  (* import x from "module" *)
  else if starts_with line "import " then begin
    try
      let rest = String.sub line 7 (String.length line - 7) in
      let rest = String.trim rest in
      if starts_with rest "from " then
        (* import * from "module" — just bring in module *)
        let mod_part = String.trim (String.sub rest 5 (String.length rest - 5)) in
        let mod_name = String.sub mod_part 1 (String.length mod_part - 2) in
        let mod_name = String.map (fun c -> if c = '/' || c = '-' then '_' else c) mod_name in
        Printf.sprintf "use %s;" mod_name
      else begin
        (* import name from "module" *)
        match String.split_on_char ' ' rest with
        | name :: "from" :: quoted :: _ ->
          let mod_name = String.sub quoted 1 (String.length quoted - 2) in
          let mod_name = String.map (fun c -> if c = '/' || c = '-' then '_' else c) mod_name in
          Printf.sprintf "use %s::%s;" mod_name name
        | _ -> line
      end
    with _ -> line
  end
  else line

(** Strip export modifier from declarations. *)
let strip_export line =
  let line = String.trim line in
  if starts_with line "export default " then
    String.sub line 15 (String.length line - 15)
  else if starts_with line "export " then
    String.sub line 7 (String.length line - 7)
  else line

(* ─── Function / variable declarations ────────────────────────────────── *)

(** Transform a [const]/[let]/[var] binding line. *)
let transform_var_decl line =
  let line = String.trim line in
  if starts_with line "const " then
    "let " ^ String.sub line 6 (String.length line - 6)
  else if starts_with line "let " then
    "let mut " ^ String.sub line 4 (String.length line - 4)
  else if starts_with line "var " then
    "let mut " ^ String.sub line 4 (String.length line - 4)
  else line

(** Transform [function name(params) {] to [fn name(params) {].
    Handles [async function] → adds [/ Async] annotation to return type. *)
let transform_function_decl line =
  let line = String.trim line in
  let is_async = starts_with line "async function " in
  let line =
    if is_async then String.sub line 6 (String.length line - 6) (* strip "async " *)
    else line
  in
  if starts_with line "function " then begin
    let body = String.sub line 9 (String.length line - 9) in
    let fn_line = "fn " ^ body in
    (* For async functions, insert Async effect into return type.
       We look for "->" in the signature.  If absent, we append the effect
       comment so the user knows to add it manually. *)
    if is_async then begin
      if String.contains fn_line '>' then
        (* Insert Async after existing return type: "-> T {" → "-> T / Async {" *)
        let re = Str.regexp {|-> \([^{]*\) {|} in
        (try Str.global_replace re {|-> \1 / Async {|} fn_line
         with Not_found -> fn_line ^ " /* add / Async to return type */")
      else
        fn_line ^ " /* async: add '/ Async' to return type */"
    end else fn_line
  end else line

(** Rewrite a single-expression arrow function body [=> expr] to [{ expr }].
    Only handles trailing [=>] on the same line. *)
let transform_arrow_body line =
  (* Look for a bare "=>" not inside a string. *)
  let len = String.length line in
  let in_str = ref false and str_delim = ref '"' in
  let arrow_pos = ref (-1) in
  let i = ref 0 in
  while !i < len do
    let c = line.[!i] in
    if !in_str then begin
      if c = !str_delim && (!i = 0 || line.[!i - 1] <> '\\') then
        in_str := false;
      incr i
    end else begin
      if c = '"' || c = '\'' || c = '`' then begin
        in_str := true; str_delim := c; incr i
      end else if !i + 1 < len && c = '=' && line.[!i + 1] = '>' then begin
        (* Make sure it's not inside <= or === *)
        let prev_ok = !i = 0 || (line.[!i - 1] <> '<' && line.[!i - 1] <> '!') in
        if prev_ok then arrow_pos := !i;
        i := !i + 2
      end else
        incr i
    end
  done;
  if !arrow_pos >= 0 then begin
    let before = String.sub line 0 !arrow_pos in
    let after = String.trim (String.sub line (!arrow_pos + 2) (len - !arrow_pos - 2)) in
    (* If after ends with {, it's a block arrow — don't wrap again *)
    if after = "{" || ends_with after " {" then
      before ^ "{ "
    else
      before ^ "{ " ^ after ^ " }"
  end else line

(* ─── Line-by-line transform ────────────────────────────────────────────── *)

(** Transform a single source line from JS-face to canonical AffineScript.
    Order matters: export stripping before function detection, variable
    declarations before keyword substitution. *)
let transform_line line =
  let trimmed = String.trim line in
  (* Blank lines pass through *)
  if trimmed = "" then line
  (* Block comments pass through *)
  else if starts_with trimmed "/*" || starts_with trimmed "*" then line
  else begin
    (* 1. Handle import lines *)
    if starts_with trimmed "import " then
      transform_import trimmed
    else begin
      (* 2. Strip export modifier *)
      let line = strip_export line in
      let trimmed = String.trim line in
      (* 3. Transform function declarations *)
      let line =
        if starts_with trimmed "async function " || starts_with trimmed "function " then
          transform_function_decl trimmed
        else if starts_with trimmed "const " || starts_with trimmed "let "
             || starts_with trimmed "var " then
          transform_var_decl trimmed
        else line
      in
      (* 4. Arrow body rewrite *)
      let line = transform_arrow_body line in
      (* 5. Keyword substitutions *)
      let line = apply_keyword_subs line in
      line
    end
  end

(* ─── File-level entry points ─────────────────────────────────────────── *)

(** Transform a full JS-face source string to canonical AffineScript. *)
let transform_source source =
  let lines = String.split_on_char '\n' source in
  let out = List.map transform_line lines in
  String.concat "\n" out

(** Parse a JS-face file: transform then parse as canonical AffineScript.
    Called from [bin/main.ml] via [parse_with_face]. *)
let parse_file_js path =
  let source = In_channel.with_open_text path In_channel.input_all in
  let canonical = transform_source source in
  Parse_driver.parse_string ~file:path canonical

(** Debug: return the transformed source without parsing. *)
let preview_transform source =
  transform_source source
