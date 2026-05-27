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

(* ---- detection ------------------------------------------------------------

   Phase 2b (PR #322) ported Side_effect_import alone. Phase 2c
   extends the walker to the remaining three Phase-1 kinds
   (Raw_js, Untyped_exception, Mutable_global) plus the two kinds
   that were explicitly deferred from Phase 1 because they need
   real AST (Inline_callback_record, Oversized_function), and flips
   --engine=walker to the default in main.ml.

   Node types used below come from rescript-lang/tree-sitter-rescript
   at the pinned commit 990214a (v6.0.0). The grammar names:
   - let_declaration / let_binding (pattern + body fields)
   - extension_expression                                 (the `%raw` shape)
   - try_expression                                       (try/catch)
   - call_expression (function/arguments fields)          (raise(), ref())
   - member_expression / value_identifier_path            (Js.Exn, Promise.catch)
   - mutation_expression                                  (x := y)
   - record / record_field
   - labeled_argument (label/value fields)
   - function                                             (arrow + named fns)

   "Module top level" means: walking ancestors outward from the
   current node, no [function] or [let_binding] body wrapper
   appears before [source_file] or [module_declaration]. Anything
   inside a function body, or inside the body of an outer
   binding, is by definition not a module-load anti-pattern. *)

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

(* True iff the current node sits at module top level — i.e. no
   [function] or [let_binding] body intervenes between the current
   node and the enclosing [source_file] or [module_declaration].
   [ancestors] is parent-first (head is the immediate parent). *)
let rec at_module_toplevel = function
  | [] -> true
  | ("function" | "let_binding") :: _ -> false
  | "source_file" :: _ -> true
  | "module_declaration" :: _ -> true
  | _ :: rest -> at_module_toplevel rest

let node_text ~source n =
  String.trim (slice ~source ~start:n.start ~stop:n.stop)

let starts_with prefix s =
  let pn = String.length prefix in
  String.length s >= pn && String.sub s 0 pn = prefix

let ends_with suffix s =
  let sn = String.length suffix in
  let n = String.length s in
  n >= sn && String.sub s (n - sn) sn = suffix

(* ---- side-effect-import (Phase 2b, preserved) ---- *)

let check_let_binding_side_effect ~source acc lb =
  match pattern_and_body ~lb with
  | Some pat, Some body
    when is_underscore_pattern ~source pat
      && body_has_module_access ~source body ->
      let excerpt =
        truncate (String.trim (slice ~source ~start:lb.start ~stop:lb.stop))
      in
      let finding : Scanner.finding =
        { kind = Scanner.Side_effect_import
        ; line = lb.start.row + 1
        ; excerpt
        }
      in
      finding :: acc
  | _ -> acc

let detect_side_effect_import ~source ancestors acc node =
  if node.ntype = "let_declaration" && at_module_toplevel ancestors then
    List.fold_left
      (fun acc c ->
        if c.ntype = "let_binding"
        then check_let_binding_side_effect ~source acc c
        else acc)
      acc node.children
  else acc

(* ---- raw-js: any extension_expression (covers %raw, %bs.raw, ...) ---- *)

let detect_raw_js ~source acc node =
  if node.ntype = "extension_expression" then
    let finding : Scanner.finding =
      { kind = Scanner.Raw_js
      ; line = node.start.row + 1
      ; excerpt = truncate (node_text ~source node)
      }
    in
    finding :: acc
  else acc

(* ---- untyped-exception: try, raise(), Promise.catch, Js.Exn ---- *)

let detect_untyped_exception ~source acc node =
  let push acc =
    let finding : Scanner.finding =
      { kind = Scanner.Untyped_exception
      ; line = node.start.row + 1
      ; excerpt = truncate (node_text ~source node)
      }
    in
    finding :: acc
  in
  match node.ntype with
  | "try_expression" -> push acc
  | "call_expression" ->
      let fn =
        List.find_opt (fun c -> c.field = Some "function") node.children
      in
      (match fn with
       | Some f when node_text ~source f = "raise" -> push acc
       | _ -> acc)
  | "member_expression" | "value_identifier_path" ->
      let text = node_text ~source node in
      let hits_js_exn = starts_with "Js.Exn" text in
      let hits_promise_catch =
        text = "Promise.catch" || ends_with "Promise.catch" text
      in
      if hits_js_exn || hits_promise_catch then push acc
      else acc
  | _ ->
      (* Tree-sitter labels module paths differently depending on syntactic
         position — `Js.Exn.Error(_)` in a catch-arm pattern is not a
         [member_expression]/[value_identifier_path], so the typed branches
         above miss it. Fall back to a tight leaf-only text match so the
         pattern still flags. The master walker's [dedupe] collapses any
         same-line duplicate against the [try_expression] hit. *)
      if node.children = [] then
        let text = node_text ~source node in
        if String.length text <= 32
           && (starts_with "Js.Exn" text
               || text = "Promise.catch"
               || ends_with ".Promise.catch" text)
        then push acc
        else acc
      else acc

(* ---- mutable-global: top-level [let x = ref(...)] or top-level [:=] ---- *)

let is_ref_call ~source n =
  if n.ntype <> "call_expression" then false
  else
    match List.find_opt (fun c -> c.field = Some "function") n.children with
    | Some f -> node_text ~source f = "ref"
    | None -> false

let detect_mutable_global ~source ancestors acc node =
  if not (at_module_toplevel ancestors) then acc
  else
    match node.ntype with
    | "let_declaration" ->
        List.fold_left
          (fun acc c ->
            if c.ntype <> "let_binding" then acc
            else
              match pattern_and_body ~lb:c with
              | _, Some body when is_ref_call ~source body ->
                  let excerpt =
                    truncate
                      (String.trim (slice ~source ~start:c.start ~stop:c.stop))
                  in
                  let finding : Scanner.finding =
                    { kind = Scanner.Mutable_global
                    ; line = c.start.row + 1
                    ; excerpt
                    }
                  in
                  finding :: acc
              | _ -> acc)
          acc node.children
    | "mutation_expression" ->
        let finding : Scanner.finding =
          { kind = Scanner.Mutable_global
          ; line = node.start.row + 1
          ; excerpt = truncate (node_text ~source node)
          }
        in
        finding :: acc
    | _ -> acc

(* ---- inline-callback-record: >=3 inline function values in a single
   record literal or a single call's argument list ---- *)

let count_inline_function_values children =
  List.fold_left
    (fun n c ->
      match c.ntype with
      | "function" -> n + 1
      | "labeled_argument" | "record_field" | "field" ->
          if List.exists (fun cc -> cc.ntype = "function") c.children
          then n + 1
          else n
      | _ -> n)
    0 children

let call_argument_children node =
  match List.find_opt (fun c -> c.field = Some "arguments") node.children with
  | Some args -> args.children
  | None -> []

let inline_callback_threshold = 3

let detect_inline_callback_record ~source acc node =
  let container_children =
    match node.ntype with
    | "call_expression" -> Some (call_argument_children node)
    | "record" -> Some node.children
    | _ -> None
  in
  match container_children with
  | None -> acc
  | Some children ->
      if count_inline_function_values children >= inline_callback_threshold then
        let finding : Scanner.finding =
          { kind = Scanner.Inline_callback_record
          ; line = node.start.row + 1
          ; excerpt = truncate (node_text ~source node)
          }
        in
        finding :: acc
      else acc

(* ---- oversized-function: row span > 50 ----

   Source-row span as a proxy for "function body >50 LOC". A direct
   line count of the body subtree would be more precise but spans
   are cheap and sufficient for surfacing decomposition candidates;
   precision belongs to Phase 3 where the tool actually rewrites
   bodies. The threshold matches LESSONS.md's stated heuristic. *)

let oversized_row_threshold = 50

let detect_oversized_function ~source acc node =
  if node.ntype <> "function" then acc
  else
    let span = node.stop.row - node.start.row + 1 in
    if span > oversized_row_threshold then
      let finding : Scanner.finding =
        { kind = Scanner.Oversized_function
        ; line = node.start.row + 1
        ; excerpt = truncate (node_text ~source node)
        }
      in
      finding :: acc
    else acc

(* ---- master walker -------------------------------------------------------- *)

(* Visit every node; dispatch to every detector. [ancestors] is the
   list of ancestor node types from immediate parent outward. *)
let rec collect ~source ancestors acc node =
  let acc = detect_side_effect_import ~source ancestors acc node in
  let acc = detect_raw_js ~source acc node in
  let acc = detect_untyped_exception ~source acc node in
  let acc = detect_mutable_global ~source ancestors acc node in
  let acc = detect_inline_callback_record ~source acc node in
  let acc = detect_oversized_function ~source acc node in
  List.fold_left
    (fun acc c -> collect ~source (node.ntype :: ancestors) acc c)
    acc node.children

(* Dedupe by (kind, line). The walker visits structural overlaps that
   the line-based scanner would not — e.g. a Js.Exn member expression
   nested inside a try_expression yields two Untyped_exception
   findings on the same line; we keep one. *)
let dedupe (findings : Scanner.finding list) : Scanner.finding list =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (f : Scanner.finding) ->
      let key = (f.kind, f.line) in
      if Hashtbl.mem seen key then false
      else begin
        Hashtbl.add seen key ();
        true
      end)
    findings

(* ---- public entry point --------------------------------------------------- *)

let scan ~grammar_dir ~path ~source =
  match run_tree_sitter ~grammar_dir ~path with
  | Error msg -> failwith ("res-to-affine walker: " ^ msg)
  | Ok output ->
      let root =
        try parse_sexp output
        with Parse_error m ->
          failwith ("res-to-affine walker: s-exp parse failed — " ^ m)
      in
      let findings = collect ~source [] [] root in
      let findings = dedupe findings in
      List.sort
        (fun (a : Scanner.finding) (b : Scanner.finding) ->
          compare a.line b.line)
        findings
