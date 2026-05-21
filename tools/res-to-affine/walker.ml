(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(* ---- AST model -------------------------------------------------------------

   Parses the tree-sitter parse default output. The format is paren-
   delimited with embedded byte ranges; each node is

     "(" NTYPE [row, col] "-" [row, col] CHILD-list ")"

   where each CHILD in the list is optionally prefixed by a "FIELD:" tag,
   for example:

     pattern: (value_identifier [0, 4] - [0, 5])

   tree-sitter uses 0-based rows and columns. We keep that internally
   and translate to 1-based line numbers in findings. *)

type pos = { row : int; col : int }

type node = {
  ntype : string;
  field : string option;
  start : pos;
  stop  : pos;
  children : node list;
}

(* ---- s-expression tokenizer + parser --------------------------------------- *)

(* The s-exp grammar we recognise is tiny enough that a hand-rolled
   recursive-descent parser is clearer than a tokenizer + driver. We
   consume the input by index into a [string]. *)

exception Parse_error of string

let buf_take buf =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  s

let is_ident_char c =
  match c with
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

(* Skip whitespace (spaces, tabs, newlines). *)
let skip_ws src i =
  let n = String.length src in
  let i = ref i in
  while !i < n &&
        (let c = src.[!i] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do
    incr i
  done;
  !i

let expect src i ch =
  if i >= String.length src || src.[i] <> ch then
    raise (Parse_error
      (Printf.sprintf "expected %C at offset %d (got %s)" ch i
         (if i >= String.length src then "EOF"
          else Printf.sprintf "%C" src.[i])))
  else i + 1

(* Read [a-zA-Z_][a-zA-Z0-9_]* starting at [i]. *)
let read_ident src i =
  let n = String.length src in
  if i >= n then raise (Parse_error "expected identifier at EOF");
  let buf = Buffer.create 16 in
  let i = ref i in
  while !i < n && is_ident_char src.[!i] do
    Buffer.add_char buf src.[!i];
    incr i
  done;
  if Buffer.length buf = 0 then
    raise (Parse_error
      (Printf.sprintf "expected identifier at offset %d" !i));
  buf_take buf, !i

(* Read a non-negative integer. *)
let read_int src i =
  let n = String.length src in
  let buf = Buffer.create 8 in
  let i = ref i in
  while !i < n &&
        (let c = src.[!i] in c >= '0' && c <= '9') do
    Buffer.add_char buf src.[!i];
    incr i
  done;
  if Buffer.length buf = 0 then
    raise (Parse_error
      (Printf.sprintf "expected integer at offset %d" !i));
  int_of_string (buf_take buf), !i

(* Read [ row , col ]. *)
let read_pos src i =
  let i = skip_ws src i in
  let i = expect src i '[' in
  let i = skip_ws src i in
  let row, i = read_int src i in
  let i = skip_ws src i in
  let i = expect src i ',' in
  let i = skip_ws src i in
  let col, i = read_int src i in
  let i = skip_ws src i in
  let i = expect src i ']' in
  { row; col }, i

(* Try to peek a field tag of the form [FIELD:]. Returns [Some name, i'] if
   one starts at [i] AND is followed by whitespace + '('; otherwise [None,
   i] without advancing. Field names are identifiers per tree-sitter's
   grammar.js conventions. *)
let peek_field src i =
  let n = String.length src in
  if i >= n then None, i
  else if not (let c = src.[i] in
               (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               c = '_') then
    None, i
  else
    let saved = i in
    try
      let name, j = read_ident src i in
      let j = skip_ws src j in
      if j < n && src.[j] = ':' then
        let j = j + 1 in
        let j = skip_ws src j in
        if j < n && src.[j] = '(' then
          Some name, j
        else
          None, saved
      else None, saved
    with Parse_error _ -> None, saved

(* Parse one node. Position points at the opening paren. *)
let rec parse_node ?field src i =
  let i = skip_ws src i in
  let i = expect src i '(' in
  let i = skip_ws src i in
  let ntype, i = read_ident src i in
  let i = skip_ws src i in
  let start, i = read_pos src i in
  let i = skip_ws src i in
  let i = expect src i '-' in
  let stop, i = read_pos src i in
  let i = skip_ws src i in
  let children, i = parse_children src i [] in
  let i = skip_ws src i in
  let i = expect src i ')' in
  { ntype; field; start; stop; children = List.rev children }, i

and parse_children src i acc =
  let i = skip_ws src i in
  let n = String.length src in
  if i >= n || src.[i] = ')' then acc, i
  else
    let field, i = peek_field src i in
    let child, i = parse_node ?field src i in
    parse_children src i (child :: acc)

let parse_sexp (s : string) : node =
  let i = skip_ws s 0 in
  let root, i = parse_node s i in
  let i = skip_ws s i in
  if i <> String.length s then
    raise (Parse_error
      (Printf.sprintf "trailing garbage at offset %d" i));
  root

(* ---- tree-sitter subprocess ----------------------------------------------- *)

let default_grammar_dir = "tools/vendor/tree-sitter-rescript"

(* Run [tree-sitter parse <abs_path>] from inside [grammar_dir]. Tree-sitter
   resolves the parser by looking at the working-directory grammar manifest,
   so [cd grammar_dir] is the simplest way to point it at the vendored
   build without relying on [--paths] flag stability across CLI versions.

   Captures stdout into a single [string]. Returns [Ok s] or [Error msg]. *)
let run_tree_sitter ~grammar_dir ~path : (string, string) result =
  if not (Sys.file_exists grammar_dir) then
    Error (Printf.sprintf
      "grammar directory not found: %s — run `just install-grammar`"
      grammar_dir)
  else begin
    let abs_path =
      if Filename.is_relative path then
        Filename.concat (Sys.getcwd ()) path
      else
        path
    in
    (* stderr is redirected to /dev/null because tree-sitter 0.26+ emits
       a benign "You have not configured any parser directories!" warning
       on every run (we resolve the parser via the cd-into-grammar-dir
       trick, not via its config file). Merging stderr into stdout would
       break our s-exp parser. Errors that matter still surface via the
       process exit code. *)
    let cmd =
      Printf.sprintf "cd %s && tree-sitter parse %s 2>/dev/null"
        (Filename.quote grammar_dir)
        (Filename.quote abs_path)
    in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with End_of_file -> ());
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
    | Unix.WEXITED n ->
        Error (Printf.sprintf
          "tree-sitter parse exited with code %d (stderr suppressed; \
           re-run `tree-sitter parse %s` from %s to see details)"
          n (Filename.quote abs_path) (Filename.quote grammar_dir))
    | Unix.WSIGNALED n ->
        Error (Printf.sprintf "tree-sitter parse killed by signal %d" n)
    | Unix.WSTOPPED _ ->
        Error "tree-sitter parse stopped"
  end

(* ---- detection: side-effect-import ----------------------------------------

   AST shape we look for:

     (let_declaration ...
       (let_binding ...
         pattern: (value_identifier ...)   <- text is exactly "_"
         body: (...)))                     <- subtree contains a module-
                                              qualified access (value_-
                                              identifier_path or
                                              member_expression with
                                              uppercase head)

   The let_declaration must be at *module top level*: parent is
   source_file, or parent is a block that is the body of a
   module_declaration. let _ = X.f() inside a function body is a
   normal "discard return value" idiom, not the module-load side-
   effect anti-pattern; the regex band-aid in #319 was specifically
   to suppress these — the walker eliminates them structurally. *)

(* Slice source text out by [pos] range. *)
let slice ~source ~start ~stop =
  let lines = String.split_on_char '\n' source in
  let lines = Array.of_list lines in
  if start.row = stop.row then begin
    if start.row >= Array.length lines then ""
    else
      let line = lines.(start.row) in
      let n = String.length line in
      let a = min start.col n in
      let b = min stop.col n in
      String.sub line a (max 0 (b - a))
  end else if start.row < Array.length lines then
    (* Multi-line excerpt: return the first line from start.col onwards;
       findings only need a short excerpt and the line number is on the
       first row anyway. *)
    let line = lines.(start.row) in
    let n = String.length line in
    let a = min start.col n in
    String.sub line a (n - a)
  else ""

let truncate s =
  if String.length s <= 80 then s
  else String.sub s 0 77 ^ "..."

(* Is this node's source text exactly the underscore [_]? *)
let is_underscore_pattern ~source n =
  if n.ntype <> "value_identifier" then false
  else
    let text = slice ~source ~start:n.start ~stop:n.stop in
    String.trim text = "_"

(* Recursively search the body subtree for a module-qualified access.
   A [value_identifier_path] node is the canonical shape ([Mod.value]).
   We also accept [member_expression] whose head ([primary_expression])
   surface text starts with an uppercase letter, to catch shapes like
   [Pixi.Sound.register] which the grammar may model as nested member
   accesses. *)
let rec body_has_module_access ~source n =
  if n.ntype = "value_identifier_path" then true
  else if n.ntype = "member_expression" then begin
    match n.children with
    | head :: _ ->
        let text = slice ~source ~start:head.start ~stop:head.stop in
        let text = String.trim text in
        if text = "" then false
        else
          let c0 = text.[0] in
          c0 >= 'A' && c0 <= 'Z'
    | [] -> false
  end
  else
    List.exists (body_has_module_access ~source) n.children

(* Find the [pattern] and [body] children of a [let_binding] node.
   Tree-sitter labels them with fields; we also fall back to positional
   first-child / last-child for resilience against grammar updates that
   might re-order fields. *)
let pattern_and_body ~lb =
  let by_field name =
    List.find_opt (fun c -> c.field = Some name) lb.children
  in
  let pattern =
    match by_field "pattern" with
    | Some n -> Some n
    | None ->
        (* fall back: first child *)
        (match lb.children with [] -> None | n :: _ -> Some n)
  in
  let body =
    match by_field "body" with
    | Some n -> Some n
    | None ->
        (* fall back: last child *)
        (match List.rev lb.children with [] -> None | n :: _ -> Some n)
  in
  pattern, body

(* Walk with ancestor context. [ancestors] lists ancestor node types from
   immediate parent outward; the head is the immediate parent. *)
let rec collect ~source ancestors acc node =
  let acc =
    if node.ntype = "let_declaration" then
      let at_module_toplevel =
        match ancestors with
        | "source_file" :: _ -> true
        | "block" :: parent :: _ when parent = "module_declaration" -> true
        | _ -> false
      in
      if at_module_toplevel then
        List.fold_left
          (fun acc c ->
            if c.ntype = "let_binding" then check_binding ~source acc c
            else acc)
          acc node.children
      else acc
    else acc
  in
  List.fold_left
    (fun acc c -> collect ~source (node.ntype :: ancestors) acc c)
    acc node.children

and check_binding ~source acc lb =
  match pattern_and_body ~lb with
  | Some pat, Some body
    when is_underscore_pattern ~source pat
      && body_has_module_access ~source body ->
      let excerpt =
        truncate
          (String.trim
             (slice ~source ~start:lb.start ~stop:lb.stop))
      in
      let finding : Scanner.finding =
        { kind = Scanner.Side_effect_import
        ; line = lb.start.row + 1
        ; excerpt
        }
      in
      finding :: acc
  | _ -> acc

(* ---- public entry point --------------------------------------------------- *)

let scan ~grammar_dir ~path ~source =
  match run_tree_sitter ~grammar_dir ~path with
  | Error msg -> failwith ("res-to-affine walker: " ^ msg)
  | Ok output ->
      let root =
        try parse_sexp output with Parse_error m ->
          failwith ("res-to-affine walker: s-exp parse failed — " ^ m)
      in
      let findings = collect ~source [] [] root in
      (* Findings accumulated in reverse order; emitter expects
         source order (line ascending). *)
      List.sort
        (fun (a : Scanner.finding) (b : Scanner.finding) -> compare a.line b.line)
        findings
