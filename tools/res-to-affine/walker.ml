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
         position — `Js.Exn.Error(_)` in a catch-arm pattern is a compound
         node (constructor pattern with the `(_)` payload as a child), so
         the typed branches above miss it AND the previous leaf-only
         fallback (`node.children = []`) skipped it too. Fall back to a
         text-based match capped at 32 chars; the cap prevents matching
         the enclosing match/try expression text. The master walker's
         [dedupe] collapses any same-line duplicate against the
         [try_expression] hit. *)
      let text = node_text ~source node in
      if String.length text <= 32
         && (starts_with "Js.Exn" text
             || text = "Promise.catch"
             || ends_with ".Promise.catch" text)
      then push acc
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

(* ---- Phase 3 (slice 1): structural type-declaration translation -----------

   Phase 1/2 only *mark* anti-patterns. Phase 3 begins *translating* the
   pure-structural forms into compilable AffineScript. Slice 1 covers the
   two most mechanical, #228-independent shapes — primitive type aliases
   and simple sum types — and is deliberately conservative: any shape we
   are not certain about (type parameters / generics, qualified paths,
   record bodies, non-primitive references, GADT return annotations,
   variant spreads) is skipped rather than guessed. A skipped form simply
   does not appear in the translation list; the emitter still prints the
   markers and the quoted original for it, so nothing is lost and the tool
   never emits a wrong translation. (This same property makes the walker
   side fail-safe: if a node name below is ever off, matching just falls
   through to "skip", never to a bad rewrite.)

   Node names are from rescript-lang/tree-sitter-rescript @990214a:
     type_declaration   -> type_binding (one per `and`)
     type_binding: field "name" (type_identifier | type_identifier_path),
        optional type_parameters, then either an inline alias type or a
        variant_type body
     variant_type        -> variant_declaration (bar-separated)
     variant_declaration : variant_identifier + optional variant_parameters
                           (+ optional type_annotation, which we refuse)
     variant_parameters  : ( _type , ... )
     type_identifier      /[a-z_'][..]/   (primitive or user lower type)
     variant_identifier   /[A-Z][..]/     (constructor)
     type_identifier_path  qualified (Mod.t)  — skipped in slice 1
     generic_type / tuple_type / record_type  — skipped in slice 1

   A ReScript lower-case type name (`color`) is capitalised (`Color`) so
   it is a referenceable AffineScript type *constructor*: in type position
   lib/parser.mly reads a lower_ident as a type *variable* (TyVar) and only
   an upper_ident as a TyCon. *)

(* ReScript primitive type name -> AffineScript spelling. Only these are
   translated; any other type_identifier text is a user/opaque type we do
   not rewrite in slice 1. *)
let rescript_prim_to_affine = function
  | "int"    -> Some "Int"
  | "float"  -> Some "Float"
  | "string" -> Some "String"
  | "bool"   -> Some "Bool"
  | "char"   -> Some "Char"
  | "unit"   -> Some "()"
  | _        -> None

(* Capitalise the first character, or [None] if the result would not start
   with an ASCII upper-case letter (so we never emit an unreferenceable
   type name). *)
let to_type_con name =
  if String.length name = 0 then None
  else
    let c0 = Char.uppercase_ascii name.[0] in
    if c0 >= 'A' && c0 <= 'Z' then
      Some (String.make 1 c0 ^ String.sub name 1 (String.length name - 1))
    else None

let child_with_field name n =
  List.find_opt (fun c -> c.field = Some name) n.children

let has_child_type ty n = List.exists (fun c -> c.ntype = ty) n.children

(* A type node usable as an alias RHS, a variant payload, or a record-field
   type. Translates primitive type_identifiers and in-scope type parameters
   ([params] maps a ReScript type-var like ['a] to its AffineScript name like
   [A]); anything else — generics, qualified paths, tuples, nested records —
   yields [None], so the whole declaration is skipped. *)
let translate_type_ref ~params ~source n =
  if n.ntype <> "type_identifier" then None
  else
    let t = node_text ~source n in
    match List.assoc_opt t params with
    | Some affine -> Some affine
    | None -> rescript_prim_to_affine t

(* Render one variant_declaration, or [None] if it carries a non-translatable
   payload or a GADT return annotation. *)
let translate_variant ~params ~source vd =
  if has_child_type "type_annotation" vd then None
  else
    match
      List.find_opt (fun c -> c.ntype = "variant_identifier") vd.children
    with
    | None -> None
    | Some nn ->
        let cname = node_text ~source nn in
        (match
           List.find_opt (fun c -> c.ntype = "variant_parameters") vd.children
         with
         | None -> Some cname                       (* nullary constructor *)
         | Some p ->
             let rec go acc = function
               | [] -> Some (List.rev acc)
               | t :: rest ->
                   (match translate_type_ref ~params ~source t with
                    | Some s -> go (s :: acc) rest
                    | None -> None)
             in
             (match go [] p.children with
              | None | Some [] -> None
              | Some ss ->
                  Some (Printf.sprintf "%s(%s)" cname (String.concat ", " ss))))

(* AffineScript type-parameter name for a ReScript one: strip the leading
   apostrophe and capitalise, so ['a] -> [A] (a referenceable TyCon in the
   body). [None] if the result isn't a usable upper_ident. *)
let affine_type_param rescript_tp =
  let s =
    if String.length rescript_tp > 0 && rescript_tp.[0] = '\''
    then String.sub rescript_tp 1 (String.length rescript_tp - 1)
    else rescript_tp
  in
  to_type_con s

(* Extract the decl's type parameters as a [(rescript_name, affine_name)] map
   plus the ["[A, B]"] suffix to print after the type name. [None] if a
   parameter can't be represented (whole decl skipped). With no type
   parameters, returns [Some ([], "")]. *)
let extract_type_params ~source tb =
  match List.find_opt (fun c -> c.ntype = "type_parameters") tb.children with
  | None -> Some ([], "")
  | Some tps ->
      let rescript_names =
        List.filter_map
          (fun c ->
            if c.ntype = "type_identifier" then Some (node_text ~source c)
            else None)
          tps.children
      in
      let rec go assoc affines = function
        | [] -> Some (List.rev assoc, List.rev affines)
        | rs :: rest ->
            (match affine_type_param rs with
             | Some aff -> go ((rs, aff) :: assoc) (aff :: affines) rest
             | None -> None)
      in
      (match go [] [] rescript_names with
       | None | Some (_, []) -> None
       | Some (assoc, affines) ->
           Some (assoc, "[" ^ String.concat ", " affines ^ "]"))

(* Render a record_type as AffineScript struct fields, or [None]. ReScript
   [mutable] and optional-[?] fields are anonymous tokens (absent from the
   parse tree), so we detect them in the field's source text and refuse the
   whole record rather than silently drop the semantics. *)
let translate_record_fields ~params ~source rt =
  let members = rt.children in
  if members = [] then None
  (* anything that isn't a plain record field (spreads, object members) *)
  else if List.exists (fun c -> c.ntype <> "record_type_field") members then None
  else
    let rec go acc = function
      | [] -> Some (List.rev acc)
      | f :: rest ->
          let ftext = String.trim (node_text ~source f) in
          if starts_with "mutable" ftext || String.contains ftext '?' then None
          else
            (match
               ( List.find_opt
                   (fun c -> c.ntype = "property_identifier") f.children,
                 List.find_opt (fun c -> c.ntype = "type_annotation") f.children )
             with
             | Some name_node, Some ann ->
                 (match ann.children with
                  | ty :: _ ->
                      (match translate_type_ref ~params ~source ty with
                       | Some t ->
                           go
                             ((node_text ~source name_node ^ ": " ^ t) :: acc)
                             rest
                       | None -> None)
                  | [] -> None)
             | _ -> None)
    in
    go [] members

(* Translate one type_binding into an AffineScript declaration, or [None]. *)
let translate_type_binding ~source tb =
  match child_with_field "name" tb with
  | None -> None
  | Some nn when nn.ntype <> "type_identifier" -> None  (* qualified name *)
  | Some nn ->
      (match to_type_con (node_text ~source nn) with
       | None -> None
       | Some tycon ->
           (match extract_type_params ~source tb with
            | None -> None
            | Some (params, suffix) ->
                let header = tycon ^ suffix in
                (match
                   List.find_opt (fun c -> c.ntype = "variant_type") tb.children
                 with
                 | Some vt ->
                     if has_child_type "variant_type_spread" vt then None
                     else
                       let vds =
                         List.filter
                           (fun c -> c.ntype = "variant_declaration")
                           vt.children
                       in
                       let rec go acc = function
                         | [] -> Some (List.rev acc)
                         | vd :: rest ->
                             (match translate_variant ~params ~source vd with
                              | Some s -> go (s :: acc) rest
                              | None -> None)
                       in
                       (match go [] vds with
                        | None | Some [] -> None
                        | Some arms ->
                            let body =
                              String.concat "\n"
                                (List.map (fun a -> "  | " ^ a) arms)
                            in
                            Some (Printf.sprintf "type %s =\n%s" header body))
                 | None ->
                     (match
                        List.find_opt
                          (fun c -> c.ntype = "record_type") tb.children
                      with
                      | Some rt ->
                          (match translate_record_fields ~params ~source rt with
                           | None -> None
                           | Some fields ->
                               let body =
                                 String.concat ",\n"
                                   (List.map (fun f -> "  " ^ f) fields)
                               in
                               Some
                                 (Printf.sprintf "struct %s {\n%s\n}" header body))
                      | None ->
                          (* alias: the non-name RHS type node *)
                          (match
                             List.find_opt
                               (fun c ->
                                 c.field <> Some "name"
                                 && c.ntype = "type_identifier")
                               tb.children
                           with
                           | None -> None
                           | Some r ->
                               (match translate_type_ref ~params ~source r with
                                | None -> None
                                | Some rhs ->
                                    Some
                                      (Printf.sprintf "type %s = %s" header rhs)))))))

(* ---- value bindings: `let x = <literal>` -> `const x: T = <literal>;` -----

   AffineScript has no module-level `let`; a top-level value binding is
   `const name: Type = value;` (type annotation + terminating semicolon both
   required). We translate only when the RHS is a literal whose type is
   unambiguous — int / float / string / bool — so the inferred annotation is
   sound and the emitted const type-checks standalone. A `ref(...)` body (the
   mutable-global anti-pattern) is not a literal, so it is left untranslated
   and still surfaces as a marker. Number forms are restricted to plain
   decimal int and D+.D+ float; hex / octal / binary / signed / scientific /
   underscored literals are skipped rather than risk a form the AffineScript
   lexer might not accept. *)

let classify_number t =
  let digits s =
    String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s
  in
  match String.index_opt t '.' with
  | None -> if digits t then Some "Int" else None
  | Some i ->
      if String.contains_from t (i + 1) '.' then None
      else
        let intp = String.sub t 0 i in
        let frac = String.sub t (i + 1) (String.length t - i - 1) in
        if digits intp && digits frac then Some "Float" else None

(* Infer the AffineScript type + value text of a literal expression node, or
   [None] if the body isn't a translatable literal. *)
let translate_literal ~source n =
  match n.ntype with
  | "number" -> (
      match classify_number (node_text ~source n) with
      | Some ty -> Some (ty, node_text ~source n)
      | None -> None)
  | "string" -> Some ("String", node_text ~source n)
  | "true" | "false" -> Some ("Bool", node_text ~source n)
  | _ -> None

(* Translate one let_binding whose pattern is a plain identifier and whose
   body is a literal, to a module-level `const`. [None] otherwise (a
   destructuring pattern, an underscore discard, or a non-literal body). *)
let translate_let_const ~source lb =
  match child_with_field "pattern" lb with
  | Some pat when pat.ntype = "value_identifier" -> (
      match child_with_field "body" lb with
      | None -> None
      | Some body -> (
          match translate_literal ~source body with
          | None -> None
          | Some (ty, value) ->
              Some
                (Printf.sprintf "const %s: %s = %s;"
                   (node_text ~source pat) ty value)))
  | _ -> None

(* Walk the tree; translate every module-top-level type_declaration's bindings
   and `let <id> = <literal>` value bindings. Returns [(source_line,
   affinescript)] in tree order. *)
let rec collect_translations ~source ancestors acc node =
  let acc =
    if not (at_module_toplevel ancestors) then acc
    else if node.ntype = "type_declaration" then
      List.fold_left
        (fun acc c ->
          if c.ntype = "type_binding" then
            (match translate_type_binding ~source c with
             | Some s -> (c.start.row + 1, s) :: acc
             | None -> acc)
          else acc)
        acc node.children
    else if node.ntype = "let_declaration" then
      List.fold_left
        (fun acc c ->
          if c.ntype = "let_binding" then
            (match translate_let_const ~source c with
             | Some s -> (c.start.row + 1, s) :: acc
             | None -> acc)
          else acc)
        acc node.children
    else acc
  in
  List.fold_left
    (fun acc c -> collect_translations ~source (node.ntype :: ancestors) acc c)
    acc node.children

(* ---- #488 partial-port mode ----------------------------------------------

   A SEPARATE model from the declaration translator above. It renders module-
   top-level function bindings (`let f = (x) => body`) into AffineScript `fn`
   skeletons with `switch`->`match` and best-effort expression translation.
   The output is DELIBERATELY not type-checked: un-annotated ReScript params
   become `_` type holes and any expression/pattern we can't translate is
   emitted as a `() /* TODO */` (expr) or `_ /* TODO */` (pattern) hole. The
   point is a partial port a human finishes; everything still PARSES. *)

let partial_excerpt ~source n =
  let s = node_text ~source n in
  let s = String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s in
  let s = Str.global_replace (Str.regexp_string "*/") "* /" s in
  let s = String.trim s in
  if String.length s > 48 then String.sub s 0 45 ^ "..." else s

let todo_expr ~source n =
  Printf.sprintf "() /* TODO: %s */" (partial_excerpt ~source n)

let todo_pattern ~source n =
  Printf.sprintf "_ /* TODO: %s */" (partial_excerpt ~source n)

(* The binary operator is an anonymous token (absent from the named tree);
   we slice it from source. Normalise ReScript float ops + identity equality
   to their AffineScript spellings. *)
let affine_binop raw =
  match String.trim raw with
  | "+." -> "+" | "-." -> "-" | "*." -> "*" | "/." -> "/"
  | "===" -> "==" | "!==" -> "!="
  | op -> op

let rec translate_pattern ~source n =
  match n.ntype with
  | "value_identifier" | "number" | "string" | "true" | "false" | "character"
    -> node_text ~source n
  | "tuple_item_pattern" -> (
      match List.find_opt (fun c -> c.ntype <> "type_annotation") n.children with
      | Some p -> translate_pattern ~source p
      | None -> "_")
  | "tuple_pattern" ->
      Printf.sprintf "(%s)"
        (String.concat ", " (List.map (translate_pattern ~source) n.children))
  | "variant_pattern" -> (
      match
        List.find_opt (fun c -> c.ntype = "variant_identifier") n.children
      with
      | None -> todo_pattern ~source n
      | Some v -> (
          let name = node_text ~source v in
          match
            List.find_opt (fun c -> c.ntype = "formal_parameters") n.children
          with
          | None -> name
          | Some fp ->
              let args =
                List.filter_map
                  (fun c ->
                    if c.ntype = "type_annotation" then None
                    else Some (translate_pattern ~source c))
                  fp.children
              in
              if args = [] then name
              else Printf.sprintf "%s(%s)" name (String.concat ", " args)))
  | _ -> todo_pattern ~source n

let rec translate_expr ~source n =
  match n.ntype with
  | "number" | "string" | "true" | "false" | "character" | "value_identifier"
    -> node_text ~source n
  (* a module-qualified value (`Js.log`); the dotted form parses in
     AffineScript as field access, which is enough for a skeleton. *)
  | "value_identifier_path" -> node_text ~source n
  | "unit" -> "()"
  | "expression_statement" -> (
      match List.filter (fun c -> c.ntype <> "comment") n.children with
      | [ e ] -> translate_expr ~source e
      | _ -> todo_expr ~source n)
  | "switch_expression" -> translate_switch ~source n
  | "parenthesized_expression" -> (
      match List.filter (fun c -> c.ntype <> "comment") n.children with
      | [ inner ] -> Printf.sprintf "(%s)" (translate_expr ~source inner)
      | _ -> todo_expr ~source n)
  | "pipe_expression" -> translate_pipe ~source n
  | "if_expression" -> translate_if ~source n
  | "block" -> translate_block ~source n
  | "sequence_expression" -> (
      match List.filter (fun c -> c.ntype <> "comment") n.children with
      | [] -> "()"
      | [ single ] -> translate_expr ~source single
      | _ -> translate_block ~source n)
  | "ternary_expression" -> (
      match
        ( child_with_field "condition" n,
          child_with_field "consequence" n,
          child_with_field "alternative" n )
      with
      | Some c, Some a, Some b ->
          Printf.sprintf "if %s { %s } else { %s }" (translate_expr ~source c)
            (translate_expr ~source a) (translate_expr ~source b)
      | _ -> todo_expr ~source n)
  | "call_expression" ->
      let fn =
        match child_with_field "function" n with
        | Some f -> translate_expr ~source f
        | None -> "todo_fn"
      in
      let args =
        match child_with_field "arguments" n with
        | None -> []
        | Some a ->
            List.filter_map
              (fun c ->
                match c.ntype with
                | "type_annotation" -> None
                | "labeled_argument" -> Some (translate_labeled_arg ~source c)
                | _ -> Some (translate_expr ~source c))
              a.children
      in
      Printf.sprintf "%s(%s)" fn (String.concat ", " args)
  | "binary_expression" -> (
      match List.filter (fun c -> c.ntype <> "comment") n.children with
      | [ l; r ] ->
          let op = affine_binop (slice ~source ~start:l.stop ~stop:r.start) in
          Printf.sprintf "%s %s %s" (translate_expr ~source l) op
            (translate_expr ~source r)
      | _ -> todo_expr ~source n)
  | "member_expression" -> (
      match (child_with_field "record" n, child_with_field "property" n) with
      | Some r, Some p ->
          Printf.sprintf "%s.%s" (translate_expr ~source r)
            (node_text ~source p)
      | _ -> todo_expr ~source n)
  | _ -> todo_expr ~source n

and translate_labeled_arg ~source n =
  let value =
    match child_with_field "value" n with
    | Some v -> translate_expr ~source v
    | None -> "()"
  in
  match child_with_field "label" n with
  | Some l -> Printf.sprintf "/* ~%s */ %s" (node_text ~source l) value
  | None -> value

and translate_switch ~source sw =
  let scrutinee =
    match List.find_opt (fun c -> c.ntype <> "switch_match") sw.children with
    | Some s -> translate_expr ~source s
    | None -> "()"
  in
  let arms =
    List.filter_map
      (fun c ->
        if c.ntype <> "switch_match" then None
        else
          let pat =
            match child_with_field "pattern" c with
            | Some p -> translate_pattern ~source p
            | None -> "_"
          in
          let guard =
            match List.find_opt (fun g -> g.ntype = "guard") c.children with
            | Some g -> (
                match g.children with
                | e :: _ -> " if " ^ translate_expr ~source e
                | [] -> "")
            | None -> ""
          in
          let body =
            match child_with_field "body" c with
            | Some b -> translate_expr ~source b
            | None -> "()"
          in
          Some (Printf.sprintf "  %s%s => %s," pat guard body))
      sw.children
  in
  Printf.sprintf "match %s {\n%s\n}" scrutinee (String.concat "\n" arms)

(* ReScript pipe-first: `a -> f(b)` desugars to `f(a, b)`, `a -> f` to `f(a)`.
   Pipes are left-nested so chains fall out of the recursion on [left]. *)
and translate_pipe ~source n =
  match List.filter (fun c -> c.ntype <> "comment") n.children with
  | [ left; right ] -> (
      let lhs = translate_expr ~source left in
      match right.ntype with
      | "call_expression" ->
          let fn =
            match child_with_field "function" right with
            | Some f -> translate_expr ~source f
            | None -> "todo_fn"
          in
          let rest =
            match child_with_field "arguments" right with
            | None -> []
            | Some a ->
                List.filter_map
                  (fun c ->
                    match c.ntype with
                    | "type_annotation" -> None
                    | "labeled_argument" ->
                        Some (translate_labeled_arg ~source c)
                    | _ -> Some (translate_expr ~source c))
                  a.children
          in
          Printf.sprintf "%s(%s)" fn (String.concat ", " (lhs :: rest))
      | _ -> Printf.sprintf "%s(%s)" (translate_expr ~source right) lhs)
  | _ -> todo_expr ~source n

and translate_if ~source n =
  (* positional children: condition, then-block, optional else_clause. *)
  match List.filter (fun c -> c.ntype <> "comment") n.children with
  | cond :: then_blk :: rest ->
      let else_part =
        match rest with
        | ec :: _ when ec.ntype = "else_clause" -> (
            match List.filter (fun c -> c.ntype <> "comment") ec.children with
            | [ b ] -> " else " ^ translate_as_block ~source b
            | _ -> "")
        | _ -> ""
      in
      Printf.sprintf "if %s %s%s" (translate_expr ~source cond)
        (translate_as_block ~source then_blk) else_part
  | _ -> todo_expr ~source n

(* Render a node as an AffineScript braced block (if/else branches require it). *)
and translate_as_block ~source n =
  match n.ntype with
  | "block" -> translate_block ~source n
  | "if_expression" -> translate_if ~source n (* else-if chain *)
  | _ -> Printf.sprintf "{ %s }" (translate_expr ~source n)

and translate_block ~source n =
  Printf.sprintf "{ %s }" (translate_block_inner ~source n)

(* The statements of a block, `;`-joined, WITHOUT the surrounding braces (so a
   function body can place them directly inside the `fn { … }`). *)
and translate_block_inner ~source n =
  String.concat "; "
    (List.filter_map
       (fun c ->
         match c.ntype with
         | "comment" -> None
         | "let_declaration" -> Some (translate_block_let ~source c)
         | _ -> Some (translate_expr ~source c))
       n.children)

and translate_block_let ~source ld =
  let one lb =
    let name =
      match child_with_field "pattern" lb with
      | Some p -> translate_pattern ~source p
      | None -> "_"
    in
    let v =
      match child_with_field "body" lb with
      | Some b -> translate_expr ~source b
      | None -> "()"
    in
    Printf.sprintf "let %s = %s" name v
  in
  String.concat "; "
    (List.map one (List.filter (fun c -> c.ntype = "let_binding") ld.children))

let translate_param ~source p =
  match p.ntype with
  | "value_identifier" -> node_text ~source p ^ ": _"
  | "parameter" | "labeled_parameter" -> (
      match
        List.find_opt (fun c -> c.ntype = "value_identifier") p.children
      with
      | Some nm -> node_text ~source nm ^ ": _"
      | None -> "_arg: _")
  | "unit" -> "_unit: ()"
  | _ -> "_arg: _"

let partial_function ~source ~name fn =
  let params =
    match child_with_field "parameter" fn with
    | Some single -> [ translate_param ~source single ]
    | None -> (
        match
          List.find_opt (fun c -> c.ntype = "formal_parameters") fn.children
        with
        | Some fp ->
            List.filter_map
              (fun c ->
                if c.ntype = "type_annotation" || c.ntype = "comment" then None
                else Some (translate_param ~source c))
              fp.children
        | None -> [])
  in
  let body =
    match child_with_field "body" fn with
    (* a block body's statements go directly inside the fn braces (no nesting) *)
    | Some b when b.ntype = "block" -> translate_block_inner ~source b
    | Some b -> translate_expr ~source b
    | None -> "()"
  in
  Printf.sprintf
    "// TODO(partial-port): fill the `_` types and `() /* TODO */` holes.\n\
     fn %s(%s) -> _ {\n  %s\n}"
    name (String.concat ", " params) body

(* Translate a `let f = (..) => body` whose pattern is a plain identifier and
   whose body is a function, into an `fn` skeleton. [None] otherwise. *)
let translate_partial_let ~source lb =
  match child_with_field "pattern" lb with
  | Some pat when pat.ntype = "value_identifier" -> (
      match child_with_field "body" lb with
      | Some body when body.ntype = "function" ->
          Some (partial_function ~source ~name:(node_text ~source pat) body)
      | _ -> None)
  | _ -> None

let rec collect_partial ~source ancestors acc node =
  let acc =
    if at_module_toplevel ancestors && node.ntype = "let_declaration" then
      List.fold_left
        (fun acc c ->
          if c.ntype = "let_binding" then
            (match translate_partial_let ~source c with
             | Some s -> (c.start.row + 1, s) :: acc
             | None -> acc)
          else acc)
        acc node.children
    else acc
  in
  List.fold_left
    (fun acc c -> collect_partial ~source (node.ntype :: ancestors) acc c)
    acc node.children

(* ---- public entry point --------------------------------------------------- *)

let parse_file ~grammar_dir ~path =
  match run_tree_sitter ~grammar_dir ~path with
  | Error msg -> failwith ("res-to-affine walker: " ^ msg)
  | Ok output ->
      (try parse_sexp output
       with Parse_error m ->
         failwith ("res-to-affine walker: s-exp parse failed — " ^ m))

let scan ~grammar_dir ~path ~source =
  let root = parse_file ~grammar_dir ~path in
  let findings = collect ~source [] [] root in
  let findings = dedupe findings in
  List.sort
    (fun (a : Scanner.finding) (b : Scanner.finding) -> compare a.line b.line)
    findings

let translate ~grammar_dir ~path ~source =
  let root = parse_file ~grammar_dir ~path in
  collect_translations ~source [] [] root
  |> List.sort (fun (l1, _) (l2, _) -> compare l1 l2)

let translate_partial ~grammar_dir ~path ~source =
  let root = parse_file ~grammar_dir ~path in
  collect_partial ~source [] [] root
  |> List.sort (fun (l1, _) (l2, _) -> compare l1 l2)
