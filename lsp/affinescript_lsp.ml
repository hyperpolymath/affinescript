(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath *)

(** Language Server Protocol implementation for AffineScript.

    Provides LSP server for editor integration with diagnostics,
    completion, hover, and go-to-definition support.
*)

open Yojson.Basic
open Yojson.Basic.Util

(** LSP server state *)
type state = {
  root_uri : string option;
  documents : (string * string) list;  (* uri -> content *)
  mutable next_request_id : int;
}

let create_state () : state = {
  root_uri = None;
  documents = [];
  next_request_id = 1;
}

(** Send JSON-RPC response *)
let send_response (id : json) (result : json) : unit =
  let response = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result);
  ] in
  let str = to_string response in
  Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length str) str;
  flush stdout

(** Send JSON-RPC notification *)
let send_notification (method_name : string) (params : json) : unit =
  let notification = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_name);
    ("params", params);
  ] in
  let str = to_string notification in
  Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length str) str;
  flush stdout

(** Parse AffineScript file and return diagnostics *)
let diagnose_file (content : string) (uri : string) : json list =
  try
    let lexbuf = Sedlexing.Utf8.from_string content in
    let _tokens = Lexer.tokenize lexbuf in

    (* Parse *)
    let prog_result = Parse_driver.parse_string content in
    match prog_result with
    | Ok _prog ->
      (* TODO: Run type checker and return type errors *)
      []
    | Error err_msg ->
      let diagnostic = `Assoc [
        ("range", `Assoc [
          ("start", `Assoc [("line", `Int 0); ("character", `Int 0)]);
          ("end", `Assoc [("line", `Int 0); ("character", `Int 0)]);
        ]);
        ("severity", `Int 1);  (* Error *)
        ("source", `String "affinescript");
        ("message", `String err_msg);
      ] in
      [diagnostic]
  with
  | Failure msg ->
    let diagnostic = `Assoc [
      ("range", `Assoc [
        ("start", `Assoc [("line", `Int 0); ("character", `Int 0)]);
        ("end", `Assoc [("line", `Int 0); ("character", `Int 0)]);
      ]);
      ("severity", `Int 1);
      ("source", `String "affinescript");
      ("message", `String msg);
    ] in
    [diagnostic]

(** Handle initialize request *)
let handle_initialize (id : json) (params : json) (state : state) : state =
  let root_uri = params |> member "rootUri" |> to_string_option in
  let result = `Assoc [
    ("capabilities", `Assoc [
      ("textDocumentSync", `Int 1);  (* Full *)
      ("completionProvider", `Assoc [
        ("triggerCharacters", `List [`String "."]);
      ]);
      ("hoverProvider", `Bool true);
      ("definitionProvider", `Bool true);
      ("documentFormattingProvider", `Bool true);
    ]);
    ("serverInfo", `Assoc [
      ("name", `String "affinescript-lsp");
      ("version", `String "0.1.0");
    ]);
  ] in
  send_response id result;
  { state with root_uri }

(** Handle textDocument/didOpen notification *)
let handle_did_open (params : json) (state : state) : state =
  let text_document = params |> member "textDocument" in
  let uri = text_document |> member "uri" |> to_string in
  let text = text_document |> member "text" |> to_string in

  let diagnostics = diagnose_file text uri in
  send_notification "textDocument/publishDiagnostics" (`Assoc [
    ("uri", `String uri);
    ("diagnostics", `List diagnostics);
  ]);

  { state with documents = (uri, text) :: state.documents }

(** Handle textDocument/didChange notification *)
let handle_did_change (params : json) (state : state) : state =
  let text_document = params |> member "textDocument" in
  let uri = text_document |> member "uri" |> to_string in
  let changes = params |> member "contentChanges" |> to_list in

  match changes with
  | change :: _ ->
    let text = change |> member "text" |> to_string in
    let diagnostics = diagnose_file text uri in
    send_notification "textDocument/publishDiagnostics" (`Assoc [
      ("uri", `String uri);
      ("diagnostics", `List diagnostics);
    ]);
    let documents' = List.remove_assoc uri state.documents in
    { state with documents = (uri, text) :: documents' }
  | [] -> state

(** Handle textDocument/completion request *)
let handle_completion (id : json) (_params : json) (_state : state) : unit =
  let completions = [
    `Assoc [("label", `String "fn"); ("kind", `Int 3)];  (* Keyword *)
    `Assoc [("label", `String "let"); ("kind", `Int 3)];
    `Assoc [("label", `String "mut"); ("kind", `Int 3)];
    `Assoc [("label", `String "return"); ("kind", `Int 3)];
    `Assoc [("label", `String "if"); ("kind", `Int 3)];
    `Assoc [("label", `String "match"); ("kind", `Int 3)];
  ] in
  send_response id (`List completions)

(** Handle textDocument/hover request *)
let handle_hover (id : json) (_params : json) (_state : state) : unit =
  let result = `Assoc [
    ("contents", `String "AffineScript: Rust-inspired language with affine types");
  ] in
  send_response id result

(** Main message handler *)
let handle_message (msg : json) (state : state) : state =
  try
    let method_name = msg |> member "method" |> to_string in
    let params = msg |> member "params" in
    let id = msg |> member "id" in

    match method_name with
    | "initialize" -> handle_initialize id params state
    | "initialized" -> state
    | "textDocument/didOpen" -> handle_did_open params state
    | "textDocument/didChange" -> handle_did_change params state
    | "textDocument/completion" -> handle_completion id params state; state
    | "textDocument/hover" -> handle_hover id params state; state
    | "shutdown" -> send_response id `Null; state
    | "exit" -> exit 0
    | _ -> state
  with
  | _ -> state

(** Read Content-Length header *)
let rec read_headers (ic : in_channel) : int option =
  try
    let line = input_line ic in
    if line = "\r" || line = "" then None
    else if String.sub line 0 15 = "Content-Length:" then
      Some (int_of_string (String.trim (String.sub line 15 (String.length line - 15))))
    else read_headers ic
  with End_of_file -> None

(** Main server loop *)
let rec server_loop (state : state) : unit =
  match read_headers stdin with
  | Some length ->
    let buffer = Bytes.create length in
    really_input stdin buffer 0 length;
    let content = Bytes.to_string buffer in
    let msg = from_string content in
    let state' = handle_message msg state in
    server_loop state'
  | None -> exit 0

(** Entry point *)
let () =
  let state = create_state () in
  server_loop state
