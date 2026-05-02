(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Face pragma detector.

    Reads a face declaration from the first non-blank, non-shebang lines of a
    source file. The pragma is the canonical mechanism for selecting a parser
    face when every source file shares the [.affine] extension; it is consulted
    after an explicit [--face] CLI flag and before any extension-based hint.

    Recognised forms (case-insensitive on the face name; leading whitespace
    permitted; separator is [:], [=], or whitespace):
    {v
      # face: rattle
      // face: js
      (* face: canonical *)
      -- face: pseudocode
      ; face = python
    v}

    Recognised face names and aliases:
    - canonical
    - python | py | rattle | rattlescript
    - js | javascript
    - pseudocode | pseudo

    The detector scans up to {!max_lines} leading lines and stops at the first
    non-comment, non-blank line so a stray ["face:"] later in the file is not
    mistaken for a pragma. A leading shebang ([#!...]) is skipped.
*)

(** Maximum number of leading lines scanned for a pragma. *)
let max_lines = 16

(** Default cap on bytes read from a source file when probing for a pragma. *)
let default_max_bytes = 4096

(** Split [s] into at most [n] lines on ['\n'], without including the
    terminating newline. Stops scanning once [n] lines have been collected. *)
let split_lines (s : string) (n : int) : string list =
  let len = String.length s in
  let acc = ref [] in
  let count = ref 0 in
  let start = ref 0 in
  let i = ref 0 in
  while !i < len && !count < n do
    if s.[!i] = '\n' then begin
      acc := String.sub s !start (!i - !start) :: !acc;
      start := !i + 1;
      incr count
    end;
    incr i
  done;
  if !start < len && !count < n then
    acc := String.sub s !start (len - !start) :: !acc;
  List.rev !acc

(** If [line] starts with a recognised comment marker, return the body of the
    comment with the marker stripped. Returns [None] for blank or non-comment
    lines (the caller must handle blank lines separately). *)
let strip_comment_marker (line : string) : string option =
  let s = String.trim line in
  let n = String.length s in
  if n = 0 then None
  else if n >= 2 && String.sub s 0 2 = "//" then
    Some (String.sub s 2 (n - 2))
  else if n >= 2 && String.sub s 0 2 = "(*" then begin
    let body = String.sub s 2 (n - 2) in
    let bn = String.length body in
    let body =
      if bn >= 2 && String.sub body (bn - 2) 2 = "*)"
      then String.sub body 0 (bn - 2)
      else body
    in
    Some body
  end
  else if n >= 2 && String.sub s 0 2 = "--" then
    Some (String.sub s 2 (n - 2))
  else if s.[0] = '#' then
    Some (String.sub s 1 (n - 1))
  else if s.[0] = ';' then
    Some (String.sub s 1 (n - 1))
  else None

(** Resolve a face name (case-insensitive) to a {!Face.face}. Established
    faces are accepted under both their generic names ([python], [js],
    [pseudocode], [lucid], [cafe]) and their brand names ([rattlescript],
    [jaffascript], [pseudoscript], [lucidscript], [cafescripto]). *)
let parse_face_name (name : string) : Face.face option =
  match String.lowercase_ascii (String.trim name) with
  | "affinescript" | "canonical"                       -> Some Face.Canonical
  | "python" | "py" | "rattle" | "rattlescript"        -> Some Face.Python
  | "js" | "javascript" | "jaffa" | "jaffascript"      -> Some Face.Js
  | "pseudocode" | "pseudo" | "pseudoscript"           -> Some Face.Pseudocode
  | "lucid" | "lucidscript" | "purescript" | "ps"      -> Some Face.Lucid
  | "cafe" | "cafescripto" | "coffee" | "coffeescript" -> Some Face.Cafe
  | _                                                  -> None

(** Try to interpret [body] (a comment body) as a [face: NAME] directive.
    Tolerates [face:NAME], [face=NAME], or [face NAME] with arbitrary
    surrounding whitespace. *)
let parse_directive (body : string) : Face.face option =
  let body = String.trim body in
  let lc = String.lowercase_ascii body in
  if String.length lc < 4 || String.sub lc 0 4 <> "face" then None
  else begin
    let after = String.sub body 4 (String.length body - 4) in
    let after = String.trim after in
    let after =
      if after = "" then after
      else match after.[0] with
        | ':' | '=' -> String.trim (String.sub after 1 (String.length after - 1))
        | _ -> after
    in
    let token =
      try
        let i = String.index after ' ' in
        String.sub after 0 i
      with Not_found -> after
    in
    parse_face_name token
  end

(** Detect a face pragma in the in-memory source [s]. *)
let detect_in_source (s : string) : Face.face option =
  let lines = split_lines s max_lines in
  let rec scan = function
    | [] -> None
    | line :: rest ->
      let trimmed = String.trim line in
      if trimmed = "" then scan rest
      else if String.length trimmed >= 2 && String.sub trimmed 0 2 = "#!" then
        scan rest
      else begin
        match strip_comment_marker line with
        | None -> None                                       (* hit code; stop *)
        | Some body ->
          begin match parse_directive body with
          | Some _ as f -> f
          | None -> scan rest                                (* non-pragma comment *)
          end
      end
  in
  scan lines

(** Detect a face pragma in the file at [path]. Reads at most [max_bytes]
    bytes from the head of the file and never raises: any I/O error yields
    [None] so callers can fall through to the next dispatch step. *)
let detect_in_file ?(max_bytes = default_max_bytes) (path : string) : Face.face option =
  try
    let ic = open_in_bin path in
    let len = min max_bytes (in_channel_length ic) in
    let s = really_input_string ic len in
    close_in ic;
    detect_in_source s
  with _ -> None
