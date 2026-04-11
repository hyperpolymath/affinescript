(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** JSON-RPC LSP server for AffineScript (LSP 3.17 subset).

    Runs over stdin/stdout using Content-Length framing.

    Supported capabilities:
    - Full document synchronisation (didOpen / didChange / didClose)
    - Diagnostic push (textDocument/publishDiagnostics)
    - Hover (textDocument/hover)
    - Go-to-definition (textDocument/definition)
    - Completion (textDocument/completion)

    Start with:  [affinescript server --stdio]

    The server runs entirely in-process — no subprocess spawning.  The
    compiler pipeline is invoked directly for each document open/change
    event and results are cached per URI.  Hover, definition, and
    completion queries read from the cache.
*)

(** {1 State} *)

(** In-memory document store: URI → source text. *)
let documents : (string, string) Hashtbl.t = Hashtbl.create 16

(** Per-URI pipeline cache entry. *)
type cache_entry = {
  symbols : Symbol.t;
  refs    : Json_output.reference list;
  diags   : Json_output.diagnostic list;
}

(** Pipeline cache: URI → last successful pipeline result. *)
let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 16

(** {1 URI utilities} *)

(** Strip the [file://] prefix to recover a filesystem path.

    [file:///home/user/foo.affine] → [/home/user/foo.affine]. *)
let uri_to_path (uri : string) : string =
  if String.length uri >= 7 && String.sub uri 0 7 = "file://"
  then String.sub uri 7 (String.length uri - 7)
  else uri

(** Prepend [file://] to a filesystem path to form a URI. *)
let path_to_uri (path : string) : string = "file://" ^ path

(** {1 LSP transport — Content-Length framing} *)

(** Read one JSON-RPC message from stdin using Content-Length framing.
    Returns [None] on EOF or unrecoverable I/O error. *)
let read_message () : Yojson.Basic.t option =
  try
    let content_length = ref 0 in
    (* Read HTTP-style headers until the blank separator line. *)
    let rec read_headers () =
      let line = input_line stdin in
      (* Trim the trailing \r that appears in \r\n line endings. *)
      let line =
        let n = String.length line in
        if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1)
        else line
      in
      if line <> "" then begin
        let prefix = "Content-Length: " in
        let plen   = String.length prefix in
        if String.length line >= plen && String.sub line 0 plen = prefix then
          (try
            content_length :=
              int_of_string (String.trim
                (String.sub line plen (String.length line - plen)))
          with Failure _ -> ());
        read_headers ()
      end
    in
    read_headers ();
    if !content_length = 0 then None
    else begin
      let buf = Bytes.create !content_length in
      really_input stdin buf 0 !content_length;
      (try Some (Yojson.Basic.from_string (Bytes.to_string buf))
       with _ -> None)
    end
  with End_of_file | Sys_error _ -> None

(** Serialise and write one JSON-RPC message to stdout. *)
let write_message (json : Yojson.Basic.t) : unit =
  let body = Yojson.Basic.to_string json in
  let n    = String.length body in
  print_string (Printf.sprintf "Content-Length: %d\r\n\r\n%s" n body);
  flush stdout

(** {1 JSON-RPC helpers} *)

(** Build a success response. *)
let response ~(id : Yojson.Basic.t) ~(result : Yojson.Basic.t) : Yojson.Basic.t =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id",      id);
    ("result",  result);
  ]

(** Build an error response (JSON-RPC error object). *)
let error_response ~(id : Yojson.Basic.t) ~(code : int) ~(message : string)
    : Yojson.Basic.t =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id",      id);
    ("error",   `Assoc [("code", `Int code); ("message", `String message)]);
  ]

(** Build a server→client notification (no [id]). *)
let notification ~(method_ : string) ~(params : Yojson.Basic.t)
    : Yojson.Basic.t =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method",  `String method_);
    ("params",  params);
  ]

(** Look up a string field inside a JSON object; [None] if absent or wrong type. *)
let field_string (key : string) (obj : Yojson.Basic.t) : string option =
  match obj with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String s) -> Some s
     | _ -> None)
  | _ -> None

(** Look up an int field inside a JSON object; [None] if absent or wrong type. *)
let field_int (key : string) (obj : Yojson.Basic.t) : int option =
  match obj with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`Int n) -> Some n
     | _ -> None)
  | _ -> None

(** Look up a nested JSON object field; [`Null] if absent or wrong type. *)
let field_obj (key : string) (obj : Yojson.Basic.t) : Yojson.Basic.t =
  match obj with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some v -> v
     | None   -> `Null)
  | _ -> `Null

(** {1 Compiler pipeline runner} *)

(** Rewrite every span's [file] field to [path] so that diagnostics
    refer to the editor's path rather than the temp file. *)
let fix_span (path : string) (s : Span.t) : Span.t = { s with file = path }

let fix_diag (path : string) (d : Json_output.diagnostic)
    : Json_output.diagnostic =
  { d with
    span   = fix_span path d.span;
    labels = List.map (fun (s, msg) -> (fix_span path s, msg)) d.labels;
  }

(** Run the full compiler pipeline on [source] (the editor's in-memory text).
    [path] is the logical filesystem path used for error spans.
    Returns [(diagnostics, symbols_option, references)].

    The source is written to a temp file so the existing [Parse_driver]
    can be re-used unchanged.  The temp file is cleaned up on exit. *)
let run_pipeline (path : string) (source : string)
    : Json_output.diagnostic list * Symbol.t option * Json_output.reference list =
  let tmp = Filename.temp_file "affinescript_lsp" ".affine" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with _ -> ())
    (fun () ->
      let oc = open_out tmp in
      output_string oc source;
      close_out oc;
      let diags      = ref [] in
      let add d      = diags := d :: !diags in
      let symbols_ref = ref None in
      let refs_ref    = ref [] in
      (try
        let prog          = Parse_driver.parse_file tmp in
        let loader_config = Module_loader.default_config () in
        let loader        = Module_loader.create loader_config in
        (match Resolve.resolve_program_with_loader prog loader with
        | Error (e, span) ->
          add (fix_diag path (Json_output.of_resolve_error e span))
        | Ok (resolve_ctx, _) ->
          symbols_ref := Some resolve_ctx.symbols;
          refs_ref :=
            List.rev resolve_ctx.references
            |> List.map (fun (r : Resolve.reference) ->
                 Json_output.{
                   ref_symbol_id = r.ref_symbol_id;
                   ref_span      = fix_span path r.ref_span;
                 });
          (match Typecheck.check_program resolve_ctx.symbols prog with
          | Error e -> add (fix_diag path (Json_output.of_type_error e))
          | Ok _ ->
            (match Borrow.check_program resolve_ctx.symbols prog with
            | Error e -> add (fix_diag path (Json_output.of_borrow_error e))
            | Ok () ->
              (match Quantity.check_program_quantities prog with
              | Error (err, span) ->
                add (fix_diag path (Json_output.of_quantity_error (err, span)))
              | Ok () -> ()))))
      with
      | Lexer.Lexer_error (msg, pos) ->
        add (fix_diag path (Json_output.of_lexer_error msg pos path))
      | Parse_driver.Parse_error (msg, span) ->
        add (fix_diag path (Json_output.of_parse_error msg span)));
      (List.rev !diags, !symbols_ref, !refs_ref))

(** {1 LSP position and range helpers} *)

(** Convert a 1-based compiler column/line to a 0-based LSP position.

    LSP uses 0-based line and character offsets (LSP 3.17 §3.17.1).
    The compiler uses 1-based positions throughout. *)
let lsp_pos (line : int) (col : int) : Yojson.Basic.t =
  `Assoc [
    ("line",      `Int (max 0 (line - 1)));
    ("character", `Int (max 0 (col  - 1)));
  ]

(** Convert a compiler [Span.t] to an LSP [Range] (0-based). *)
let lsp_range (s : Span.t) : Yojson.Basic.t =
  `Assoc [
    ("start", lsp_pos s.start_pos.Span.line s.start_pos.Span.col);
    ("end",   lsp_pos s.end_pos.Span.line   s.end_pos.Span.col);
  ]

(** {1 LSP diagnostic serialization} *)

(** Map a diagnostic severity to an LSP DiagnosticSeverity integer.
    LSP: 1=Error 2=Warning 3=Information 4=Hint. *)
let lsp_severity = function
  | Json_output.Error   -> 1
  | Json_output.Warning -> 2
  | Json_output.Info    -> 3
  | Json_output.Hint    -> 4

(** Serialise one compiler diagnostic to the LSP Diagnostic object format. *)
let diag_to_lsp (d : Json_output.diagnostic) : Yojson.Basic.t =
  let base = [
    ("range",    lsp_range d.span);
    ("severity", `Int (lsp_severity d.severity));
    ("code",     `String d.code);
    ("source",   `String "affinescript");
    ("message",  `String d.message);
  ] in
  let with_help = match d.help with
    | None   -> base
    | Some h ->
      base @ [("relatedInformation", `List [
        `Assoc [
          ("location", `Assoc [
            ("uri",   `String (path_to_uri d.span.file));
            ("range", lsp_range d.span);
          ]);
          ("message", `String h);
        ]
      ])]
  in
  `Assoc with_help

(** Send a [textDocument/publishDiagnostics] notification for [uri]. *)
let publish_diagnostics (uri : string) (diags : Json_output.diagnostic list)
    : unit =
  write_message (notification
    ~method_:"textDocument/publishDiagnostics"
    ~params:(`Assoc [
      ("uri",         `String uri);
      ("diagnostics", `List (List.map diag_to_lsp diags));
    ]))

(** {1 CompletionItemKind mapping} *)

(** Map an AffineScript symbol kind string to an LSP CompletionItemKind.
    LSP 3.17 §3.18.1 values used: 2=Method 3=Function 4=Constructor
    6=Variable 7=Class 8=Interface 9=Module 14=Keyword 25=TypeParameter. *)
let lsp_completion_kind (kind : string) : int =
  match kind with
  | "function"         -> 3
  | "variable"         -> 6
  | "type"             -> 7
  | "type_variable"    -> 25
  | "constructor"      -> 4
  | "trait"            -> 8
  | "effect"           -> 8
  | "effect_operation" -> 2
  | "module"           -> 9
  | "keyword"          -> 14
  | _                  -> 1   (* Text fallback *)

(** {1 Request handlers} *)

(** Handle [initialize]: declare server capabilities and reply. *)
let handle_initialize (id : Yojson.Basic.t) (_params : Yojson.Basic.t) : unit =
  write_message (response ~id ~result:(
    `Assoc [
      ("capabilities", `Assoc [
        (* Full document sync — send complete text on every change. *)
        ("textDocumentSync",   `Int 1);
        ("hoverProvider",      `Bool true);
        ("definitionProvider", `Bool true);
        ("completionProvider", `Assoc [
          ("triggerCharacters", `List [`String "."; `String "@"]);
          ("resolveProvider",   `Bool false);
        ]);
      ]);
      ("serverInfo", `Assoc [
        ("name",    `String "affinescript");
        ("version", `String "0.1.0");
      ]);
    ]
  ))

(** Run the pipeline on [source], cache the result, and push diagnostics. *)
let refresh_document (uri : string) (source : string) : unit =
  Hashtbl.replace documents uri source;
  let path = uri_to_path uri in
  let (diags, symbols_opt, refs) = run_pipeline path source in
  (match symbols_opt with
  | Some symbols -> Hashtbl.replace cache uri { symbols; refs; diags }
  | None         -> Hashtbl.remove cache uri);
  publish_diagnostics uri diags

let handle_did_open (uri : string) (source : string) : unit =
  refresh_document uri source

let handle_did_change (uri : string) (source : string) : unit =
  refresh_document uri source

let handle_did_close (uri : string) : unit =
  Hashtbl.remove documents uri;
  Hashtbl.remove cache uri;
  (* Clear the editor's diagnostic panel for this file. *)
  publish_diagnostics uri []

(** Handle [textDocument/hover]: return symbol info at cursor.

    [line] and [col] are 0-based (LSP convention).  Converted to 1-based
    before querying the symbol table. *)
let handle_hover
    (id     : Yojson.Basic.t)
    (uri    : string)
    (line   : int)
    (col    : int)
    : unit =
  let line1 = line + 1 in
  let col1  = col  + 1 in
  let result =
    match Hashtbl.find_opt cache uri with
    | None -> `Null
    | Some entry ->
      (match Json_output.find_symbol_at entry.symbols entry.refs line1 col1 with
      | None -> `Null
      | Some sym ->
        let type_md = match sym.sym_type with
          | Some te -> Printf.sprintf "\n```affinescript\n%s\n```"
                         (Ast.show_type_expr te)
          | None    -> ""
        in
        let qty_md = match sym.sym_quantity with
          | Some q -> Printf.sprintf "\n\n*quantity: %s*" (Ast.show_quantity q)
          | None   -> ""
        in
        `Assoc [
          ("contents", `Assoc [
            ("kind",  `String "markdown");
            ("value", `String (Printf.sprintf
              "**%s** — %s%s%s"
              sym.sym_name
              (Json_output.symbol_kind_to_string sym.sym_kind)
              type_md
              qty_md));
          ]);
          (* Range is optional in LSP hover; include it so editors can
             highlight the exact symbol token. *)
          ("range", lsp_range sym.sym_span);
        ])
  in
  write_message (response ~id ~result)

(** Handle [textDocument/definition]: return the definition location.

    [line] and [col] are 0-based. *)
let handle_definition
    (id   : Yojson.Basic.t)
    (uri  : string)
    (line : int)
    (col  : int)
    : unit =
  let line1 = line + 1 in
  let col1  = col  + 1 in
  let result =
    match Hashtbl.find_opt cache uri with
    | None -> `Null
    | Some entry ->
      (match Json_output.find_symbol_at entry.symbols entry.refs line1 col1 with
      | None -> `Null
      | Some sym ->
        `Assoc [
          ("uri",   `String (path_to_uri sym.sym_span.Span.file));
          ("range", lsp_range sym.sym_span);
        ])
  in
  write_message (response ~id ~result)

(** Handle [textDocument/completion]: return candidates at cursor.

    [line] and [col] are 0-based.  The prefix is extracted from the
    in-memory source using [Json_output.extract_prefix_at] (1-based API). *)
let handle_completion
    (id   : Yojson.Basic.t)
    (uri  : string)
    (line : int)
    (col  : int)
    : unit =
  let line1 = line + 1 in
  let col1  = col  + 1 in
  let items =
    match Hashtbl.find_opt cache uri, Hashtbl.find_opt documents uri with
    | Some entry, Some source ->
      let (prefix, dot_ctx) =
        Json_output.extract_prefix_at source line1 col1
      in
      Json_output.collect_completions entry.symbols prefix dot_ctx
    | _ -> []
  in
  let lsp_items = List.map (fun (item : Json_output.completion_item) ->
    let kind   = lsp_completion_kind item.Json_output.comp_kind in
    let detail = match item.Json_output.comp_type with
      | Some t -> t
      | None   -> item.Json_output.comp_kind
    in
    `Assoc [
      ("label",      `String item.Json_output.comp_name);
      ("kind",       `Int kind);
      ("detail",     `String detail);
      ("insertText", `String item.Json_output.comp_name);
    ]
  ) items in
  write_message (response ~id ~result:(
    `Assoc [
      ("isIncomplete", `Bool false);
      ("items",        `List lsp_items);
    ]
  ))

(** {1 Main message dispatch} *)

(** Handle one incoming JSON-RPC message.
    Returns [false] when the server should stop ([exit] notification). *)
let handle_message (msg : Yojson.Basic.t) : bool =
  let field k = field_obj k msg in
  let method_opt =
    match field "method" with `String s -> Some s | _ -> None
  in
  let id     = field "id" in
  let params = field "params" in
  (match method_opt with

  (* ── Lifecycle ── *)
  | Some "initialize"  -> handle_initialize id params
  | Some "initialized" -> ()   (* Client confirmation — no response needed. *)
  | Some "shutdown"    ->
    write_message (response ~id ~result:`Null)
  | Some "exit"        -> ()   (* Handled via return value below. *)

  (* ── Document synchronisation ── *)
  | Some "textDocument/didOpen" ->
    let td = field_obj "textDocument" params in
    (match field_string "uri" td, field_string "text" td with
    | Some uri, Some text -> handle_did_open uri text
    | _ -> ())

  | Some "textDocument/didChange" ->
    let td      = field_obj "textDocument" params in
    let changes = field_obj "contentChanges" params in
    (match field_string "uri" td with
    | None -> ()
    | Some uri ->
      (* With full sync (mode 1) the array has exactly one entry. *)
      let text_opt =
        match changes with
        | `List (first :: _) -> field_string "text" first
        | _ -> None
      in
      (match text_opt with
      | Some text -> handle_did_change uri text
      | None -> ()))

  | Some "textDocument/didClose" ->
    let td = field_obj "textDocument" params in
    (match field_string "uri" td with
    | Some uri -> handle_did_close uri
    | None -> ())

  (* ── Queries ── *)
  | Some "textDocument/hover" ->
    let td  = field_obj "textDocument" params in
    let pos = field_obj "position"     params in
    (match field_string "uri" td, field_int "line" pos, field_int "character" pos with
    | Some uri, Some line, Some col -> handle_hover id uri line col
    | _ -> write_message (response ~id ~result:`Null))

  | Some "textDocument/definition" ->
    let td  = field_obj "textDocument" params in
    let pos = field_obj "position"     params in
    (match field_string "uri" td, field_int "line" pos, field_int "character" pos with
    | Some uri, Some line, Some col -> handle_definition id uri line col
    | _ -> write_message (response ~id ~result:`Null))

  | Some "textDocument/completion" ->
    let td  = field_obj "textDocument" params in
    let pos = field_obj "position"     params in
    (match field_string "uri" td, field_int "line" pos, field_int "character" pos with
    | Some uri, Some line, Some col -> handle_completion id uri line col
    | _ ->
      write_message (response ~id ~result:(
        `Assoc [("isIncomplete", `Bool false); ("items", `List [])])))

  (* ── Unknown ── *)
  | Some m ->
    (* For requests (have an [id]), reply with MethodNotFound.
       For notifications (no [id]), silently ignore. *)
    (match id with
    | `Null -> ()
    | _ ->
      write_message (error_response ~id ~code:(-32601)
        ~message:(Printf.sprintf "Method not found: %s" m)))

  | None -> ());  (* Ignore stray messages with no method field. *)

  (* Stop the loop on the exit notification. *)
  match method_opt with Some "exit" -> false | _ -> true

(** {1 Server entry point} *)

(** Start the LSP server loop.

    Switches stdin/stdout to binary mode (required for correct Content-Length
    framing on all platforms), then reads and dispatches messages until the
    client sends an [exit] notification or EOF is reached. *)
let run () : unit =
  set_binary_mode_out stdout true;
  set_binary_mode_in  stdin  true;
  let continue = ref true in
  while !continue do
    match read_message () with
    | None     -> continue := false
    | Some msg ->
      (try continue := handle_message msg
       with e ->
         (* Never let a handler exception kill the server loop. *)
         Format.eprintf "affinescript-lsp: unhandled exception: %s@."
           (Printexc.to_string e))
  done
