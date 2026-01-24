(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Error formatting with ANSI colors for terminal output *)

(** ANSI color codes *)
type color =
  | Red
  | Yellow
  | Blue
  | Green
  | Cyan
  | Magenta
  | Bold
  | Reset

(** Convert color to ANSI code *)
let color_code = function
  | Red -> "\027[31m"
  | Yellow -> "\027[33m"
  | Blue -> "\027[34m"
  | Green -> "\027[32m"
  | Cyan -> "\027[36m"
  | Magenta -> "\027[35m"
  | Bold -> "\027[1m"
  | Reset -> "\027[0m"

(** Check if terminal supports colors *)
let supports_color () =
  try
    let term = Sys.getenv "TERM" in
    not (term = "dumb" || term = "")
  with Not_found -> false

(** Colorize text if terminal supports it *)
let colorize (c : color) (text : string) : string =
  if supports_color () then
    color_code c ^ text ^ color_code Reset
  else
    text

(** Format error severity level with color *)
let format_severity (severity : string) : string =
  match severity with
  | "error" -> colorize Red (colorize Bold "error")
  | "warning" -> colorize Yellow (colorize Bold "warning")
  | "note" -> colorize Cyan (colorize Bold "note")
  | "help" -> colorize Green (colorize Bold "help")
  | _ -> severity

(** Format file location with color *)
let format_location (loc : string) : string =
  colorize Bold (colorize Cyan loc)

(** Format error code *)
let format_error_code (code : string) : string =
  colorize Magenta ("[" ^ code ^ "]")

(** Format a complete error message with colors *)
let format_error
    ?code
    ?(severity : string = "error")
    (location : string)
    (message : string)
    : string =
  let code_str = match code with
    | Some c -> format_error_code c ^ " "
    | None -> ""
  in
  let sev = format_severity severity in
  let loc = format_location location in
  Printf.sprintf "%s %s%s: %s" loc code_str sev message

(** Format a multi-line error with source context *)
let format_error_with_context
    ?code
    ?(severity : string = "error")
    ?help
    (location : string)
    (message : string)
    (source_line : string option)
    (col : int option)
    : string =
  let header = format_error ?code ~severity location message in

  match source_line, col with
  | Some line, Some c ->
    (* Show source context with caret *)
    let padding = String.make (c - 1) ' ' in
    let caret = colorize Red "^" in
    let context = Printf.sprintf "\n  %s\n  %s%s" line padding caret in
    let help_text = match help with
      | Some h -> "\n  " ^ colorize Green ("help: " ^ h)
      | None -> ""
    in
    header ^ context ^ help_text
  | _ ->
    let help_text = match help with
      | Some h -> "\n  " ^ colorize Green ("help: " ^ h)
      | None -> ""
    in
    header ^ help_text
