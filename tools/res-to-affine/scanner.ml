(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

type kind =
  | Side_effect_import
  | Raw_js
  | Untyped_exception
  | Mutable_global
  | Inline_callback_record
  | Oversized_function

let kind_to_label = function
  | Side_effect_import     -> "side-effect-import"
  | Raw_js                 -> "raw-js"
  | Untyped_exception      -> "untyped-exception"
  | Mutable_global         -> "mutable-global"
  | Inline_callback_record -> "inline-callback-record"
  | Oversized_function     -> "oversized-function"

let kind_to_guidance = function
  | Side_effect_import ->
      "ReScript module-load side effect. AffineScript modules do not run \
       code at load time — rewrite as an explicit registration call."
  | Raw_js ->
      "%raw JS block. AffineScript has no untyped FFI — replace with \
       a typed extern (see docs/reference/ABI-FFI.md) or wait for the matching \
       binding."
  | Untyped_exception ->
      "Untyped exception / Promise.catch. AffineScript prefers \
       Result[E, A] for fail-fast paths and Validation[E, A] for \
       accumulating errors."
  | Mutable_global ->
      "Top-level mutable global. AffineScript does not encourage \
       module-scoped mutation; pass state as an affine record or \
       through an effect handler."
  | Inline_callback_record ->
      "3+ inline callbacks in one record/call. Lift to a row-polymorphic \
       handler record (see LESSONS.md §callback-record) so each handler \
       is named and individually overridable."
  | Oversized_function ->
      "Function spans >50 source lines. Re-decompose before porting; a \
       direct translation will preserve the size and the implicit \
       contract it bakes in."

type finding = {
  kind : kind;
  line : int;
  excerpt : string;
}

(* ---- regexes, compiled once ----

   The LESSONS.md anti-patterns these regexes target are *top-level*
   shapes (module-load side effects, module-scoped mutable globals).
   The two top-level regexes therefore anchor at column 0 to keep
   in-function `let _ = X.foo(...)` (a normal "discard the return
   value of a chained call" idiom) and intra-function `x := ...`
   (normal local ref mutation) from generating noise. The trade-off
   is that we miss module-load side effects nested inside a
   `module X = { ... }` block, which Phase 2's AST walker recovers. *)

let re_side_effect_import =
  Str.regexp "^let[ \t]+_[ \t]*=[ \t]*[A-Z][a-zA-Z0-9_]*\\."

let re_raw_js = Str.regexp_case_fold "%raw\\|\\[%bs\\.raw"

let re_untyped_exn =
  Str.regexp
    "Promise\\.catch\\|Js\\.Exn\\|\\(^\\|[^a-zA-Z_]\\)raise[ (]\\|\\(^\\|[^a-zA-Z_]\\)try[ {]"

let re_mutable_global = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*:="

let trim s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let j = ref (n - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '\r') do decr j done;
  String.sub s !i (!j - !i + 1)

let truncate s =
  if String.length s <= 80 then s
  else String.sub s 0 77 ^ "..."

(* A line is "code" when it isn't a comment-only line and isn't blank.
   We don't strip in-line comments; the patterns we match are unlikely
   to live inside a // comment. *)
let is_codeish line =
  let t = trim line in
  if t = "" then false
  else not (String.length t >= 2 && t.[0] = '/' && t.[1] = '/')

let try_match re line =
  try
    let _ = Str.search_forward re line 0 in true
  with Not_found -> false

let scan (source : string) : finding list =
  let lines = String.split_on_char '\n' source in
  let acc = ref [] in
  List.iteri
    (fun i raw ->
      if is_codeish raw then begin
        let excerpt = truncate (trim raw) in
        let lineno = i + 1 in
        let push k = acc := { kind = k; line = lineno; excerpt } :: !acc in
        if try_match re_side_effect_import raw then push Side_effect_import;
        if try_match re_raw_js raw           then push Raw_js;
        if try_match re_untyped_exn raw      then push Untyped_exception;
        if try_match re_mutable_global raw   then push Mutable_global
      end)
    lines;
  List.rev !acc
