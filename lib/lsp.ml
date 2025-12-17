(** Language Server Protocol implementation for AffineScript *)

(** LSP Position *)
type position = {
  line: int;
  character: int;
}
[@@deriving show]

(** LSP Range *)
type range = {
  start: position;
  end_: position;
}
[@@deriving show]

(** LSP Location *)
type location = {
  uri: string;
  range: range;
}
[@@deriving show]

(** Diagnostic severity *)
type severity =
  | Error
  | Warning
  | Information
  | Hint
[@@deriving show]

(** LSP Diagnostic *)
type diagnostic = {
  range: range;
  severity: severity;
  code: string option;
  source: string;
  message: string;
}
[@@deriving show]

(** Completion item kind *)
type completion_kind =
  | CKText
  | CKMethod
  | CKFunction
  | CKConstructor
  | CKField
  | CKVariable
  | CKClass
  | CKInterface
  | CKModule
  | CKProperty
  | CKUnit
  | CKValue
  | CKEnum
  | CKKeyword
  | CKSnippet
  | CKColor
  | CKFile
  | CKReference
  | CKFolder
  | CKEnumMember
  | CKConstant
  | CKStruct
  | CKEvent
  | CKOperator
  | CKTypeParameter
[@@deriving show]

(** Completion item *)
type completion_item = {
  label: string;
  kind: completion_kind;
  detail: string option;
  documentation: string option;
  insert_text: string option;
  insert_text_format: int;  (** 1 = plain, 2 = snippet *)
}
[@@deriving show]

(** Hover content *)
type hover = {
  contents: string;
  range: range option;
}
[@@deriving show]

(** Symbol kind *)
type symbol_kind =
  | SKFile
  | SKModule
  | SKNamespace
  | SKPackage
  | SKClass
  | SKMethod
  | SKProperty
  | SKField
  | SKConstructor
  | SKEnum
  | SKInterface
  | SKFunction
  | SKVariable
  | SKConstant
  | SKString
  | SKNumber
  | SKBoolean
  | SKArray
  | SKObject
  | SKKey
  | SKNull
  | SKEnumMember
  | SKStruct
  | SKEvent
  | SKOperator
  | SKTypeParameter
[@@deriving show]

(** Document symbol *)
type document_symbol = {
  name: string;
  kind: symbol_kind;
  range: range;
  selection_range: range;
  children: document_symbol list;
}
[@@deriving show]

(** Server capabilities *)
type capabilities = {
  text_document_sync: int;  (** 0=None, 1=Full, 2=Incremental *)
  hover_provider: bool;
  completion_provider: bool;
  definition_provider: bool;
  references_provider: bool;
  document_symbol_provider: bool;
  workspace_symbol_provider: bool;
  code_action_provider: bool;
  rename_provider: bool;
  formatting_provider: bool;
  signature_help_provider: bool;
}
[@@deriving show]

let default_capabilities = {
  text_document_sync = 1;
  hover_provider = true;
  completion_provider = true;
  definition_provider = true;
  references_provider = true;
  document_symbol_provider = true;
  workspace_symbol_provider = true;
  code_action_provider = true;
  rename_provider = true;
  formatting_provider = true;
  signature_help_provider = true;
}

(** Document state *)
type document = {
  doc_uri: string;
  doc_version: int;
  doc_content: string;
  mutable doc_ast: Ast.program option;
  mutable doc_symbols: Symbol.symbol_table option;
  mutable doc_diagnostics: diagnostic list;
}

(** LSP Server state *)
type server_state = {
  mutable initialized: bool;
  mutable shutdown: bool;
  documents: (string, document) Hashtbl.t;
  mutable root_uri: string option;
  mutable capabilities: capabilities;
}

let create_server () = {
  initialized = false;
  shutdown = false;
  documents = Hashtbl.create 32;
  root_uri = None;
  capabilities = default_capabilities;
}

(** Convert span to LSP range *)
let span_to_range (span: Span.t) : range = {
  start = { line = span.start_pos.line - 1; character = span.start_pos.col - 1 };
  end_ = { line = span.end_pos.line - 1; character = span.end_pos.col - 1 };
}

(** Parse and analyze document *)
let analyze_document doc =
  try
    let prog = Parse_driver.parse_string ~file:doc.doc_uri doc.doc_content in
    doc.doc_ast <- Some prog;
    let (symtab, resolve_errors) = Resolve.resolve_program prog in
    doc.doc_symbols <- Some symtab;
    let type_errors = Typecheck.check_program prog in
    let borrow_errors = Borrow.check_program prog in
    (* Convert errors to diagnostics *)
    let resolve_diags = List.map (fun err ->
      let msg = Resolve.error_to_string err in
      let range = match err with
        | Resolve.UnboundVariable (_, span) -> span_to_range span
        | Resolve.UnboundType (_, span) -> span_to_range span
        | Resolve.DuplicateDefinition (_, span, _) -> span_to_range span
        | _ -> { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } }
      in
      { range; severity = Error; code = Some "E0001"; source = "affinescript"; message = msg }
    ) resolve_errors in
    let type_diags = List.map (fun err ->
      let msg = Typecheck.error_to_string err in
      { range = { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } };
        severity = Error; code = Some "E0002"; source = "affinescript"; message = msg }
    ) type_errors in
    let borrow_diags = List.map (fun err ->
      let msg = Borrow.error_to_string err in
      { range = { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } };
        severity = Error; code = Some "E0003"; source = "affinescript"; message = msg }
    ) borrow_errors in
    doc.doc_diagnostics <- resolve_diags @ type_diags @ borrow_diags
  with
  | Lexer.Lexer_error (msg, pos) ->
      doc.doc_diagnostics <- [{
        range = { start = { line = pos.Span.line - 1; character = pos.Span.col - 1 };
                  end_ = { line = pos.Span.line - 1; character = pos.Span.col } };
        severity = Error;
        code = Some "E0000";
        source = "affinescript";
        message = "Lexer error: " ^ msg;
      }]
  | Parse_driver.Parse_error (msg, span) ->
      doc.doc_diagnostics <- [{
        range = span_to_range span;
        severity = Error;
        code = Some "E0000";
        source = "affinescript";
        message = "Parse error: " ^ msg;
      }]

(** Get completions at position *)
let get_completions server uri pos =
  match Hashtbl.find_opt server.documents uri with
  | None -> []
  | Some doc ->
      (* Get symbols in scope at position *)
      let items = ref [] in
      (* Add keywords *)
      let keywords = [
        "fn"; "let"; "mut"; "if"; "else"; "match"; "while"; "for"; "in";
        "return"; "break"; "continue"; "struct"; "enum"; "trait"; "impl";
        "type"; "effect"; "handle"; "resume"; "pub"; "use"; "module";
        "true"; "false"; "own"; "ref"; "total"; "unsafe";
      ] in
      List.iter (fun kw ->
        items := { label = kw; kind = CKKeyword; detail = None;
                   documentation = None; insert_text = None; insert_text_format = 1 } :: !items
      ) keywords;
      (* Add built-in types *)
      let types = ["Int"; "Bool"; "Float"; "Char"; "String"; "Unit"; "Never"; "Array"; "Option"; "Result"] in
      List.iter (fun ty ->
        items := { label = ty; kind = CKClass; detail = Some "built-in type";
                   documentation = None; insert_text = None; insert_text_format = 1 } :: !items
      ) types;
      (* Add built-in functions *)
      let builtins = [
        ("print", "fn(any) -> ()");
        ("println", "fn(any) -> ()");
        ("len", "fn(Array[T]) -> Int");
        ("range", "fn(Int) -> Array[Int]");
        ("map", "fn((T -> U), Array[T]) -> Array[U]");
        ("filter", "fn((T -> Bool), Array[T]) -> Array[T]");
        ("fold", "fn((A, T -> A), A, Array[T]) -> A");
      ] in
      List.iter (fun (name, sig_) ->
        items := { label = name; kind = CKFunction; detail = Some sig_;
                   documentation = None; insert_text = None; insert_text_format = 1 } :: !items
      ) builtins;
      (* Add symbols from document *)
      (match doc.doc_symbols with
       | Some symtab ->
           let symbols = Symbol.symbols_in_scope (Symbol.current_scope symtab) in
           List.iter (fun sym ->
             let kind = match sym.Symbol.sym_kind with
               | Symbol.SKVariable -> CKVariable
               | Symbol.SKFunction -> CKFunction
               | Symbol.SKType -> CKClass
               | Symbol.SKTrait -> CKInterface
               | Symbol.SKEffect -> CKEvent
               | Symbol.SKVariant -> CKEnumMember
               | Symbol.SKField -> CKField
               | Symbol.SKModule -> CKModule
               | _ -> CKValue
             in
             items := { label = sym.Symbol.sym_name; kind; detail = None;
                        documentation = None; insert_text = None; insert_text_format = 1 } :: !items
           ) symbols
       | None -> ());
      !items

(** Get hover information *)
let get_hover server uri pos =
  match Hashtbl.find_opt server.documents uri with
  | None -> None
  | Some doc ->
      (* Find symbol at position *)
      (* For now, return generic info *)
      Some { contents = "AffineScript symbol"; range = None }

(** Get definition location *)
let get_definition server uri pos =
  match Hashtbl.find_opt server.documents uri with
  | None -> None
  | Some doc ->
      (* Find definition of symbol at position *)
      None

(** Get references to symbol *)
let get_references server uri pos =
  match Hashtbl.find_opt server.documents uri with
  | None -> []
  | Some doc ->
      (* Find all references to symbol at position *)
      []

(** Get document symbols *)
let get_document_symbols server uri =
  match Hashtbl.find_opt server.documents uri with
  | None -> []
  | Some doc ->
      match doc.doc_ast with
      | None -> []
      | Some prog ->
          (* Convert AST to document symbols *)
          let symbols = ref [] in
          List.iter (fun decl ->
            match decl with
            | Ast.TopFn fd ->
                let range = span_to_range fd.fd_name.span in
                symbols := {
                  name = fd.fd_name.name;
                  kind = SKFunction;
                  range;
                  selection_range = range;
                  children = [];
                } :: !symbols
            | Ast.TopType td ->
                let range = span_to_range td.td_name.span in
                let kind = match td.td_body with
                  | Ast.TyEnum _ -> SKEnum
                  | Ast.TyStruct _ -> SKStruct
                  | Ast.TyAlias _ -> SKClass
                in
                symbols := {
                  name = td.td_name.name;
                  kind;
                  range;
                  selection_range = range;
                  children = [];
                } :: !symbols
            | Ast.TopTrait td ->
                let range = span_to_range td.trd_name.span in
                symbols := {
                  name = td.trd_name.name;
                  kind = SKInterface;
                  range;
                  selection_range = range;
                  children = [];
                } :: !symbols
            | Ast.TopEffect ed ->
                let range = span_to_range ed.ed_name.span in
                symbols := {
                  name = ed.ed_name.name;
                  kind = SKEvent;
                  range;
                  selection_range = range;
                  children = [];
                } :: !symbols
            | Ast.TopConst tc ->
                let range = span_to_range tc.tc_name.span in
                symbols := {
                  name = tc.tc_name.name;
                  kind = SKConstant;
                  range;
                  selection_range = range;
                  children = [];
                } :: !symbols
            | _ -> ()
          ) prog.prog_decls;
          !symbols

(** Format document *)
let format_document _server _uri =
  (* Would implement pretty printer *)
  []

(** JSON-RPC message handling *)

type json =
  | JNull
  | JBool of bool
  | JInt of int
  | JFloat of float
  | JString of string
  | JArray of json list
  | JObject of (string * json) list

let rec json_to_string = function
  | JNull -> "null"
  | JBool b -> if b then "true" else "false"
  | JInt i -> string_of_int i
  | JFloat f -> Printf.sprintf "%g" f
  | JString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | JArray arr ->
      "[" ^ String.concat ", " (List.map json_to_string arr) ^ "]"
  | JObject obj ->
      "{" ^ String.concat ", " (List.map (fun (k, v) ->
        Printf.sprintf "\"%s\": %s" k (json_to_string v)
      ) obj) ^ "}"

(** LSP message types *)
type lsp_message =
  | Request of int * string * json
  | Notification of string * json
  | Response of int * json option * json option

(** Handle initialize request *)
let handle_initialize server params =
  server.initialized <- true;
  JObject [
    ("capabilities", JObject [
      ("textDocumentSync", JInt 1);
      ("hoverProvider", JBool true);
      ("completionProvider", JObject [
        ("triggerCharacters", JArray [JString "."; JString ":"]);
      ]);
      ("definitionProvider", JBool true);
      ("referencesProvider", JBool true);
      ("documentSymbolProvider", JBool true);
      ("workspaceSymbolProvider", JBool true);
      ("codeActionProvider", JBool true);
      ("renameProvider", JBool true);
      ("documentFormattingProvider", JBool true);
    ]);
    ("serverInfo", JObject [
      ("name", JString "affinescript-lsp");
      ("version", JString "0.1.0");
    ]);
  ]

(** Handle shutdown request *)
let handle_shutdown server =
  server.shutdown <- true;
  JNull

(** Handle textDocument/didOpen *)
let handle_did_open server params =
  (* Extract URI and content from params *)
  let uri = "unknown" in  (* Would parse from params *)
  let content = "" in
  let doc = {
    doc_uri = uri;
    doc_version = 1;
    doc_content = content;
    doc_ast = None;
    doc_symbols = None;
    doc_diagnostics = [];
  } in
  Hashtbl.replace server.documents uri doc;
  analyze_document doc

(** Handle textDocument/didChange *)
let handle_did_change server params =
  let uri = "unknown" in  (* Would parse from params *)
  match Hashtbl.find_opt server.documents uri with
  | None -> ()
  | Some doc ->
      (* Update content and reanalyze *)
      analyze_document doc

(** Handle textDocument/completion *)
let handle_completion server params =
  let uri = "unknown" in
  let pos = { line = 0; character = 0 } in
  let items = get_completions server uri pos in
  JObject [
    ("isIncomplete", JBool false);
    ("items", JArray (List.map (fun item ->
      JObject [
        ("label", JString item.label);
        ("kind", JInt (match item.kind with CKFunction -> 3 | CKVariable -> 6 | CKKeyword -> 14 | _ -> 1));
      ]
    ) items));
  ]

(** Handle textDocument/hover *)
let handle_hover server params =
  let uri = "unknown" in
  let pos = { line = 0; character = 0 } in
  match get_hover server uri pos with
  | None -> JNull
  | Some hover ->
      JObject [
        ("contents", JObject [
          ("kind", JString "markdown");
          ("value", JString hover.contents);
        ]);
      ]

(** Handle textDocument/documentSymbol *)
let handle_document_symbol server params =
  let uri = "unknown" in
  let symbols = get_document_symbols server uri in
  JArray (List.map (fun sym ->
    JObject [
      ("name", JString sym.name);
      ("kind", JInt (match sym.kind with SKFunction -> 12 | SKClass -> 5 | SKEnum -> 10 | _ -> 1));
      ("range", JObject [
        ("start", JObject [("line", JInt sym.range.start.line); ("character", JInt sym.range.start.character)]);
        ("end", JObject [("line", JInt sym.range.end_.line); ("character", JInt sym.range.end_.character)]);
      ]);
      ("selectionRange", JObject [
        ("start", JObject [("line", JInt sym.selection_range.start.line); ("character", JInt sym.selection_range.start.character)]);
        ("end", JObject [("line", JInt sym.selection_range.end_.line); ("character", JInt sym.selection_range.end_.character)]);
      ]);
    ]
  ) symbols)

(** Main message dispatcher *)
let handle_message server msg =
  match msg with
  | Request (id, "initialize", params) ->
      Some (Response (id, Some (handle_initialize server params), None))
  | Request (id, "shutdown", _) ->
      Some (Response (id, Some (handle_shutdown server), None))
  | Request (id, "textDocument/completion", params) ->
      Some (Response (id, Some (handle_completion server params), None))
  | Request (id, "textDocument/hover", params) ->
      Some (Response (id, Some (handle_hover server params), None))
  | Request (id, "textDocument/documentSymbol", params) ->
      Some (Response (id, Some (handle_document_symbol server params), None))
  | Notification ("initialized", _) -> None
  | Notification ("textDocument/didOpen", params) ->
      handle_did_open server params;
      None
  | Notification ("textDocument/didChange", params) ->
      handle_did_change server params;
      None
  | Notification ("exit", _) ->
      if server.shutdown then exit 0 else exit 1
  | _ -> None

(** Run LSP server *)
let run () =
  let server = create_server () in
  (* Read messages from stdin, write to stdout *)
  (* Simplified - would need proper JSON-RPC framing *)
  Format.eprintf "AffineScript LSP server starting...@.";
  let rec loop () =
    (* Would read Content-Length header and JSON body *)
    loop ()
  in
  loop ()
