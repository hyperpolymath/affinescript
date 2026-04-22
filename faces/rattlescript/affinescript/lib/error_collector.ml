(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Error collection for reporting multiple errors *)

(** Error severity levels *)
type severity =
  | Error
  | Warning
  | Note

(** A collected error or warning *)
type collected_error = {
  severity : severity;
  code : string option;
  location : Span.t;
  message : string;
  help : string option;
}

(** Error collector *)
type t = {
  mutable errors : collected_error list;
  mutable max_errors : int;
}

(** Create a new error collector *)
let create ?(max_errors = 10) () : t =
  { errors = []; max_errors }

(** Add an error to the collector *)
let add_error
    ?(code : string option)
    ?(help : string option)
    (collector : t)
    (location : Span.t)
    (message : string)
    : unit =
  if List.length collector.errors < collector.max_errors then
    let err = {
      severity = Error;
      code;
      location;
      message;
      help;
    } in
    collector.errors <- err :: collector.errors

(** Add a warning to the collector *)
let add_warning
    ?(code : string option)
    ?(help : string option)
    (collector : t)
    (location : Span.t)
    (message : string)
    : unit =
  let warn = {
    severity = Warning;
    code;
    location;
    message;
    help;
  } in
  collector.errors <- warn :: collector.errors

(** Check if there are any errors *)
let has_errors (collector : t) : bool =
  List.exists (fun e -> e.severity = Error) collector.errors

(** Get all collected errors *)
let get_errors (collector : t) : collected_error list =
  List.rev collector.errors

(** Get error count *)
let error_count (collector : t) : int =
  List.length (List.filter (fun e -> e.severity = Error) collector.errors)

(** Get warning count *)
let warning_count (collector : t) : int =
  List.length (List.filter (fun e -> e.severity = Warning) collector.errors)

(** Format severity as string *)
let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

(** Format all collected errors *)
let format_all (collector : t) : string =
  let errors = get_errors collector in
  let formatted = List.map (fun err ->
    let span_str = Format.asprintf "%a" Span.pp_short err.location in
    Error_formatter.format_error
      ?code:err.code
      ~severity:(severity_to_string err.severity)
      span_str
      (match err.help with
       | Some h -> err.message ^ "\n  " ^ Error_formatter.colorize Error_formatter.Green ("help: " ^ h)
       | None -> err.message)
  ) errors in
  String.concat "\n\n" formatted

(** Format summary line *)
let format_summary (collector : t) : string =
  let err_count = error_count collector in
  let warn_count = warning_count collector in

  let parts = [] in
  let parts = if err_count > 0 then
    (Error_formatter.colorize Error_formatter.Red
      (Printf.sprintf "%d error%s" err_count (if err_count = 1 then "" else "s"))) :: parts
  else parts in
  let parts = if warn_count > 0 then
    (Error_formatter.colorize Error_formatter.Yellow
      (Printf.sprintf "%d warning%s" warn_count (if warn_count = 1 then "" else "s"))) :: parts
  else parts in

  if parts = [] then
    Error_formatter.colorize Error_formatter.Green "No errors"
  else
    String.concat ", " (List.rev parts)
