(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** LucidScript-face: source-level transformer for PureScript-style AffineScript.

    Lucid is the catchment face for PureScript / Haskell developers. The
    semantic core (row polymorphism, ADTs, effects, type classes) already
    aligns with AffineScript, so the face is mostly aesthetic translation
    plus indentation-to-brace lowering.

    Surface mappings:
    {v
      module Foo where             →  module Foo {
      import Data.X                 →  use Data::X;
      import Data.X (a, b)          →  use Data::X::{a, b};
      import Data.X (a, b) as M     →  use Data::X::{a, b} as M;
      data Maybe a = Just a | Nothing
                                    →  type Maybe[a] = Just(a) | Nothing
      class Eq a where              →  trait Eq[a] {
      instance Eq Foo where         →  impl Eq for Foo {
      f :: Int -> Int               →  // f :: Int -> Int        (signature kept as comment)
      f x y = expr                  →  fn f(x, y) = expr
      \x -> expr                    →  (x) => expr
      if c then a else b            →  if c { a } else { b }
      case e of                     →  match e {
        Just x -> body              →    Just(x) => body
        Nothing -> other            →    Nothing => other
      let x = 1 in expr             →  { let x = 1; expr }
      True / False                  →  true / false
      Unit                          →  ()
      -- comment                    →  // comment
      INDENT (where/of/let body)    →  (block opened by preceding {)
      DEDENT                        →  }
    v}

    Indentation handling mirrors {!Python_face}: a block-opening keyword
    (module/where/of/do/let-in/case-of/class-where/instance-where) emits
    [{], a stricter dedent emits [}], and the last meaningful line of a
    block is emitted without a trailing [;] so its value becomes the
    block's value (matching Haskell/PureScript layout semantics).

    Limitations (deferred to AST-level rewrites):
    - Multi-clause function definitions ([fact 0 = 1; fact n = ...])
      are not auto-folded into a single [match]; write the [case] form.
    - [do]-notation desugaring to handler form is partial: a [do] block
      lowers to [{ ...; }] and individual [<-] arrows lower to [let bind].
      The user is expected to wire [Async] / domain effects explicitly.
    - [where] post-bindings are not hoisted into the function body; use a
      [let ... in ...] in front instead.
    - Span fidelity: errors refer to the canonical text, not Lucid source.
*)

(* ─── Character helpers ──────────────────────────────────────────────── *)

let is_id_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_' || c = '\''

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

(** Convert a leading [-- comment] to [// comment] at the original indent. *)
let convert_dashdash_comment line =
  let trimmed = String.trim line in
  if starts_with trimmed "--" then
    let indent_len = String.length line - String.length trimmed in
    let indent = String.sub line 0 indent_len in
    indent ^ "//" ^ String.sub trimmed 2 (String.length trimmed - 2)
  else line

(** Strip a trailing [-- ...] comment (respecting string literals) and
    return [(code_part, comment_text option)]. *)
let strip_dashdash_comment line =
  let len = String.length line in
  let in_str = ref false in
  let str_delim = ref '"' in
  let cut = ref (-1) in
  let i = ref 0 in
  while !i < len && !cut < 0 do
    let c = line.[!i] in
    if !in_str then begin
      if c = !str_delim && (!i = 0 || line.[!i - 1] <> '\\') then
        in_str := false
    end else begin
      if c = '"' then begin in_str := true; str_delim := c end
      else if !i + 1 < len && c = '-' && line.[!i + 1] = '-' then
        cut := !i
    end;
    incr i
  done;
  if !cut < 0 then (line, None)
  else (String.sub line 0 !cut,
        Some (String.sub line (!cut + 2) (len - !cut - 2)))

(* ─── Word-level keyword substitution ────────────────────────────────── *)

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

(** Boolean / unit literal substitutions. Applied to code-only text. *)
let apply_literal_subs s =
  let s = replace_word ~from_w:"True"  ~to_w:"true"  s in
  let s = replace_word ~from_w:"False" ~to_w:"false" s in
  let s = replace_word ~from_w:"Unit"  ~to_w:"()"    s in
  s

(** Logical operator substitutions. PureScript uses [&&], [||] natively;
    [not] is a function but lower it to the prefix [!] for parser symmetry
    when written as a unary keyword. *)
let apply_logic_subs s =
  let s = replace_word ~from_w:"not" ~to_w:"!" s in
  s

(* ─── Module path helpers ────────────────────────────────────────────── *)

(** [Data.Map.Strict] → [Data::Map::Strict]. *)
let dots_to_colons s =
  let len = String.length s in
  let buf = Buffer.create (len + 4) in
  String.iter (fun c ->
    if c = '.' then Buffer.add_string buf "::"
    else Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(* ─── Import translation ─────────────────────────────────────────────── *)

(** Try to transform a PureScript [import …] line. *)
let transform_import_line stripped =
  if not (starts_with stripped "import ") then None
  else begin
    let rest = String.trim (String.sub stripped 7 (String.length stripped - 7)) in
    (* Split at " as " to detect alias. *)
    let alias, rest =
      let idx =
        let len = String.length rest in
        let found = ref (-1) in
        let i = ref 0 in
        while !i + 3 < len && !found < 0 do
          if rest.[!i] = ' '
             && !i + 3 < len
             && rest.[!i + 1] = 'a' && rest.[!i + 2] = 's' && rest.[!i + 3] = ' '
          then found := !i;
          incr i
        done;
        !found
      in
      if idx >= 0 then
        (Some (String.trim (String.sub rest (idx + 4) (String.length rest - idx - 4))),
         String.trim (String.sub rest 0 idx))
      else (None, rest)
    in
    (* Split at " hiding (...)" — drop the hiding clause; we cannot express
       hiding in the canonical use form. *)
    let rest =
      try
        let i = Str.search_forward (Str.regexp_string " hiding ") rest 0 in
        String.trim (String.sub rest 0 i)
      with Not_found -> rest
    in
    (* Detect explicit name list: [Mod (a, b, c)] *)
    if String.contains rest '(' then begin
      let lparen = String.index rest '(' in
      let mod_part = String.trim (String.sub rest 0 lparen) in
      let after = String.sub rest (lparen + 1) (String.length rest - lparen - 1) in
      let names_raw =
        try String.sub after 0 (String.index after ')')
        with Not_found -> after
      in
      let names =
        String.split_on_char ',' names_raw
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> String.concat ", "
      in
      let alias_clause = match alias with
        | Some a -> " as " ^ a
        | None -> ""
      in
      Some (Printf.sprintf "use %s::{%s}%s;"
              (dots_to_colons mod_part) names alias_clause)
    end else begin
      let alias_clause = match alias with
        | Some a -> " as " ^ a
        | None -> ""
      in
      Some (Printf.sprintf "use %s%s;" (dots_to_colons rest) alias_clause)
    end
  end

(* ─── Module declaration ─────────────────────────────────────────────── *)

(** [module Foo where] / [module Foo (exports) where] → [module Foo;].
    Canonical AffineScript uses [module] as a file-level header (no
    braces); the body is everything that follows in the same file. *)
let transform_module_line stripped =
  if not (starts_with stripped "module ") then None
  else begin
    let rest = String.sub stripped 7 (String.length stripped - 7) in
    (* Drop optional (export, list) and the trailing "where". *)
    let rest =
      if String.contains rest '(' then begin
        let lparen = String.index rest '(' in
        try
          let rparen = String.index_from rest lparen ')' in
          String.sub rest 0 lparen
          ^ String.sub rest (rparen + 1) (String.length rest - rparen - 1)
        with Not_found -> String.sub rest 0 lparen
      end else rest
    in
    let rest = String.trim rest in
    let rest =
      if ends_with rest " where" then
        String.trim (String.sub rest 0 (String.length rest - 6))
      else if rest = "where" then ""
      else rest
    in
    let mod_path = dots_to_colons rest in
    Some (Printf.sprintf "module %s;" mod_path)
  end

(* ─── Type signature handling ────────────────────────────────────────── *)

(** A line of the form [name :: type ...] is a type signature. Lucid keeps
    it as a comment so the canonical type inferer is the source of truth.
    Class-method signatures ([class Eq a where] body) get the same treatment. *)
let is_type_signature stripped =
  (* Must contain " :: " and start with a lowercase identifier. *)
  let len = String.length stripped in
  let has_dcolon =
    try ignore (Str.search_forward (Str.regexp_string " :: ") stripped 0); true
    with Not_found -> false
  in
  has_dcolon && len > 0
  && (stripped.[0] >= 'a' && stripped.[0] <= 'z' || stripped.[0] = '_')

(* ─── Data / class / instance declarations ───────────────────────────── *)

(** [data Foo a b = Ctor1 a | Ctor2] → [type Foo[a, b] = Ctor1(a) | Ctor2].
    Best-effort: parameterised constructor arguments wrapped in parens. *)
let transform_data_decl stripped =
  if not (starts_with stripped "data ") then None
  else begin
    let body = String.sub stripped 5 (String.length stripped - 5) in
    let eq_idx =
      try Some (String.index body '=') with Not_found -> None
    in
    match eq_idx with
    | None -> Some ("type " ^ body)            (* abstract data type *)
    | Some eq ->
      let lhs = String.trim (String.sub body 0 eq) in
      let rhs = String.trim (String.sub body (eq + 1) (String.length body - eq - 1)) in
      (* Convert "Foo a b" → "Foo[a, b]" *)
      let lhs =
        match String.split_on_char ' ' lhs with
        | [single] -> single
        | name :: ps -> Printf.sprintf "%s[%s]" name (String.concat ", " ps)
        | [] -> lhs
      in
      (* Convert each constructor "Ctor a b" → "Ctor(a, b)". *)
      let rhs =
        String.split_on_char '|' rhs
        |> List.map String.trim
        |> List.map (fun ctor ->
            match String.split_on_char ' ' ctor with
            | [single] -> single
            | name :: args -> Printf.sprintf "%s(%s)" name (String.concat ", " args)
            | [] -> ctor)
        |> String.concat " | "
      in
      Some (Printf.sprintf "type %s = %s" lhs rhs)
  end

(** [class Eq a where] → [trait Eq[a] {]. *)
let transform_class_decl stripped =
  if not (starts_with stripped "class ") then None
  else begin
    let body = String.sub stripped 6 (String.length stripped - 6) in
    let body =
      if ends_with body " where" then
        String.trim (String.sub body 0 (String.length body - 6))
      else body
    in
    (* "Eq a" → "Eq[a]"; "Functor f" → "Functor[f]". *)
    let body =
      match String.split_on_char ' ' body with
      | [single] -> single
      | name :: ps -> Printf.sprintf "%s[%s]" name (String.concat ", " ps)
      | [] -> body
    in
    Some (Printf.sprintf "trait %s {" body)
  end

(** [instance Eq Foo where] / [instance eqFoo :: Eq Foo where]
    → [impl Eq for Foo {]. *)
let transform_instance_decl stripped =
  if not (starts_with stripped "instance ") then None
  else begin
    let body = String.sub stripped 9 (String.length stripped - 9) in
    let body =
      if ends_with body " where" then
        String.trim (String.sub body 0 (String.length body - 6))
      else body
    in
    (* PureScript allows an optional "name :: " prefix. *)
    let body =
      try
        let i = Str.search_forward (Str.regexp_string " :: ") body 0 in
        String.trim (String.sub body (i + 4) (String.length body - i - 4))
      with Not_found -> body
    in
    (* Now "Eq Foo" — split into trait and target. *)
    match String.split_on_char ' ' body with
    | trait_name :: target_parts when target_parts <> [] ->
      Some (Printf.sprintf "impl %s for %s {"
              trait_name (String.concat " " target_parts))
    | _ -> Some (Printf.sprintf "impl %s {" body)
  end

(* ─── Function equations ─────────────────────────────────────────────── *)

(** [f x y = expr] — wrap parameters and emit canonical [fn].
    Returns [None] when the line isn't a recognisable equation. *)
let transform_equation stripped =
  match String.index_opt stripped '=' with
  | None -> None
  | Some eq when eq = 0 -> None
  | Some eq ->
    (* Skip operator-equality forms like [==], [<=], [>=], [/=], [:=]. *)
    let len = String.length stripped in
    let next_ok = eq + 1 >= len || stripped.[eq + 1] <> '=' in
    let prev_ok = eq = 0
                  || (let c = stripped.[eq - 1] in
                      c <> '<' && c <> '>' && c <> '/' && c <> '=' && c <> ':' && c <> '!') in
    if not (next_ok && prev_ok) then None
    else begin
      let lhs = String.trim (String.sub stripped 0 eq) in
      let rhs = String.trim (String.sub stripped (eq + 1) (len - eq - 1)) in
      (* lhs must start with a lowercase ident. *)
      if String.length lhs = 0 then None
      else if not (lhs.[0] >= 'a' && lhs.[0] <= 'z' || lhs.[0] = '_') then None
      else begin
        match String.split_on_char ' ' lhs with
        | [name] ->
          (* Bare value: [x = expr] → [let x = expr] *)
          Some (Printf.sprintf "let %s = %s" name rhs)
        | name :: params ->
          (* Drop a leading [()] unit-param idiom (PureScript [main () = …])
             so the canonical signature is just [fn main()] with no params. *)
          let params =
            match params with
            | "()" :: rest -> rest
            | _ -> params
          in
          let params_str = String.concat ", " params in
          if rhs = "" then
            Some (Printf.sprintf "fn %s(%s) {" name params_str)
          else
            (* Canonical AffineScript requires a braced body, not [= expr]. *)
            Some (Printf.sprintf "fn %s(%s) { %s }" name params_str rhs)
        | [] -> None
      end
    end

(* ─── Expression-level substitutions ─────────────────────────────────── *)

(** [\x -> body] / [\x y -> body] → [(x, y) => body]. *)
let transform_lambda_inline s =
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
    end else if c = '"' then begin
      in_str := true; str_delim := c;
      Buffer.add_char buf c; incr i
    end else if c = '\\' && !i + 1 < len && s.[!i + 1] <> '\\' then begin
      (* Found a lambda. Read ident-list up to "->". *)
      let j = ref (!i + 1) in
      let arrow_start = ref (-1) in
      while !j + 1 < len && !arrow_start < 0 do
        if s.[!j] = '-' && s.[!j + 1] = '>' then arrow_start := !j
        else incr j
      done;
      if !arrow_start < 0 then begin
        Buffer.add_char buf c; incr i
      end else begin
        let params =
          String.sub s (!i + 1) (!arrow_start - !i - 1)
          |> String.trim
          |> String.split_on_char ' '
          |> List.filter (fun p -> p <> "")
          |> String.concat ", "
        in
        Buffer.add_string buf (Printf.sprintf "(%s) =>" params);
        i := !arrow_start + 2
      end
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(** [if c then a else b] with [then]/[else] inline → braced form. *)
let transform_if_inline s =
  if not (starts_with (String.trim s) "if ") then s
  else begin
    let then_re = Str.regexp_string " then " in
    let else_re = Str.regexp_string " else " in
    try
      let then_pos = Str.search_forward then_re s 0 in
      try
        let else_pos = Str.search_forward else_re s (then_pos + 5) in
        let cond = String.trim (String.sub s 3 (then_pos - 3)) in
        let then_branch =
          String.trim (String.sub s (then_pos + 6) (else_pos - then_pos - 6))
        in
        let else_branch =
          String.trim (String.sub s (else_pos + 6) (String.length s - else_pos - 6))
        in
        Printf.sprintf "if %s { %s } else { %s }" cond then_branch else_branch
      with Not_found ->
        let cond = String.trim (String.sub s 3 (then_pos - 3)) in
        let body =
          String.trim (String.sub s (then_pos + 6) (String.length s - then_pos - 6))
        in
        Printf.sprintf "if %s { %s }" cond body
    with Not_found -> s
  end

(* ─── Block-opener detection (Haskell-style "where" / "of" / "do") ───── *)

let block_openers = [
  " where"; " of"; " do";
]

let is_block_opener_lucid stripped =
  List.exists (fun suffix -> ends_with stripped suffix) block_openers
  || ends_with stripped " ="    (* multi-line equations *)
  || ends_with stripped " ->"   (* case arms with indented body *)

(** Lines that are already canonical AffineScript and must skip the
    equation / lambda / literal pipeline. These typically introduce or
    close a brace-delimited block of their own. *)
let is_canonical_passthrough stripped =
  let prefixes = ["effect "; "trait "; "impl "; "type "; "fn ";
                  "use "; "pub "; "struct "; "enum "] in
  List.exists (fun p -> starts_with stripped p) prefixes
  || stripped = "}" || stripped = "};"
  (* Treat any line that ends with [{] and isn't an equation as a
     declaration header. *)
  || (let n = String.length stripped in
      n > 0 && stripped.[n - 1] = '{' && not (String.contains stripped '='))

let strip_block_marker stripped =
  let try_suffix suf =
    if ends_with stripped suf then
      Some (String.trim (String.sub stripped 0 (String.length stripped - String.length suf)))
    else None
  in
  match try_suffix " where" with
  | Some r -> (r, "where")
  | None ->
    match try_suffix " of" with
    | Some r -> (r, "of")
    | None ->
      match try_suffix " do" with
      | Some r -> (r, "do")
      | None ->
        match try_suffix " =" with
        | Some r -> (r, "=")
        | None ->
          match try_suffix " ->" with
          | Some r -> (r, "->")
          | None -> (stripped, "")

(** Decide how to render the head of a block-opening line. *)
let render_block_head head marker =
  match marker with
  | "of" ->
    if starts_with head "case " then
      "match " ^ String.sub head 5 (String.length head - 5) ^ " {"
    else head ^ " {"
  | "where" ->
    (* module/class/instance/data already handled upstream;
       [where] anywhere else is post-binding scope — render as block. *)
    head ^ " {"
  | "do" ->
    head ^ " {"
  | "->" ->
    (* Case arm body; emit "head =>" with "{" so the indented body
       becomes the arm body. *)
    head ^ " => {"
  | "=" ->
    head ^ " = {"
  | _ -> head

(* ─── Main transformer ───────────────────────────────────────────────── *)

let is_blank_line raw =
  let (code, _) = strip_dashdash_comment (String.trim raw) in
  String.trim code = ""

let transform_source source =
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let n = Array.length lines in
  let out = Buffer.create (String.length source + 256) in
  let stack = ref [0] in
  (* Tracks how many top-level [module/class/instance] / [data … =] heads
     have opened a brace at the same indent as the keyword (PureScript
     layout allows the body to sit at the same indent as the head). The
     normal indent-based dedent never closes these; we emit one [}] per
     opened head at EOF. *)
  let toplevel_braces = ref 0 in

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
    let ind = indent_of raw_line in
    let line = convert_dashdash_comment raw_line in
    let (code_part, comment_opt) = strip_dashdash_comment (String.trim line) in
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
    end else if is_type_signature stripped then begin
      (* Keep type signatures as a comment so the inferer drives types. *)
      let indent_str = String.make ind ' ' in
      Buffer.add_string out (indent_str ^ "// " ^ stripped ^ "\n")
    end else if is_canonical_passthrough stripped then begin
      (* Canonical declaration line embedded inside a Lucid file
         (e.g. an [effect …] block, a [fn …] header, a closing [}]).
         Pass through without the equation / lambda / literal pipeline,
         which would otherwise mangle [-> T] return types and append
         a stray [;] after a [{]. *)
      let indent_str = String.make ind ' ' in
      Buffer.add_string out (indent_str ^ with_comment stripped)
    end else begin
      emit_dedents ind;
      if ind > top () then stack := ind :: !stack;

      let indent_str = String.make ind ' ' in
      let next_ind = next_meaningful_indent i in
      let is_tail = next_ind < ind in

      (* Specific structural transforms first. [class] / [instance]
         declarations open a brace at top-level indent; record them so
         we can close at EOF (the body sits at the same indent as the
         head in PureScript layout, so the indent-stack dedent never
         fires). [module] in canonical AffineScript is a file header
         with no body braces, so we don't track it. *)
      let track_toplevel s =
        if ind = 0 && String.length s > 0
           && s.[String.length s - 1] = '{'
        then incr toplevel_braces
      in
      let line_text =
        match transform_module_line stripped with
        | Some s -> s
        | None ->
        match transform_import_line stripped with
        | Some s -> s
        | None ->
        match transform_data_decl stripped with
        | Some s -> s
        | None ->
        match transform_class_decl stripped with
        | Some s -> track_toplevel s; s
        | None ->
        match transform_instance_decl stripped with
        | Some s -> track_toplevel s; s
        | None ->
        if is_block_opener_lucid stripped then begin
          let (head, marker) = strip_block_marker stripped in
          render_block_head (apply_logic_subs (apply_literal_subs head)) marker
        end else begin
          (* General expression / equation handling. *)
          let prepared = stripped
                         |> apply_literal_subs
                         |> apply_logic_subs
                         |> transform_lambda_inline
                         |> transform_if_inline
          in
          match transform_equation prepared with
          | Some eq when is_tail -> eq                  (* tail: no `;` *)
          | Some eq -> eq ^ ";"
          | None when is_tail -> prepared
          | None -> prepared ^ ";"
        end
      in
      Buffer.add_string out (indent_str ^ with_comment line_text)
    end
  done;
  emit_dedents 0;
  (* Close any module / class / instance heads that opened a brace at
     top-level indent. *)
  for _ = 1 to !toplevel_braces do
    Buffer.add_string out "}\n"
  done;
  Buffer.contents out

(* ─── Entry points ───────────────────────────────────────────────────── *)

let parse_string_lucid ~file content =
  let canonical = transform_source content in
  Parse_driver.parse_string ~file canonical

let parse_file_lucid path =
  let chan = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let content = really_input_string chan (in_channel_length chan) in
      parse_string_lucid ~file:path content)

let preview_transform source = transform_source source
