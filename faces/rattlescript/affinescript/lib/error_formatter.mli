(* SPDX-License-Identifier: PMPL-1.0-or-later *)
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

(** Colorize text if terminal supports it *)
val colorize : color -> string -> string

(** Format error severity level with color *)
val format_severity : string -> string

(** Format file location with color *)
val format_location : string -> string

(** Format error code *)
val format_error_code : string -> string

(** Format a complete error message with colors *)
val format_error :
  ?code:string ->
  ?severity:string ->
  string ->
  string ->
  string

(** Format a multi-line error with source context *)
val format_error_with_context :
  ?code:string ->
  ?severity:string ->
  ?help:string ->
  string ->
  string ->
  string option ->
  int option ->
  string
