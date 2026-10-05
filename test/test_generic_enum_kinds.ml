(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)
(** Kinds of user-declared parametric enums.

    [infer_kind] used to hard-code the kinds of the builtin constructors
    (Option, Result, ...) and give every other named type kind [Type], so
    naming a user enum applied to arguments anywhere in a signature —
    `fn mk<M>(x: M) -> Box<M>` — failed with "Too many arguments for kind".
    That made typed UI libraries (`Html<Msg>`) impossible. These tests pin
    the fix in a single module, across a module import, and keep the kind
    check honest by planting an over-application that must still fail. *)

open Affinescript

(** parse -> resolve (with a loader rooted at [dir]) -> typecheck. *)
let frontend ?(dir = Sys.getcwd ()) (src : string) : (unit, string) result =
  let ( let* ) = Result.bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_generic_enum_kinds>" src)
    with
    | Parse_driver.Parse_error (m, sp) ->
      Error (Printf.sprintf "Parse error at %s: %s" (Span.show sp) m)
    | e -> Error (Printf.sprintf "Unexpected: %s" (Printexc.to_string e))
  in
  let config = { (Module_loader.default_config ()) with current_dir = dir; search_paths = [ dir ] } in
  let loader = Module_loader.create config in
  let* resolve_ctx, type_ctx =
    match Resolve.resolve_program_with_loader prog loader with
    | Ok (rc, tc) -> Ok (rc, tc)
    | Error (e, _) -> Error ("Resolution error: " ^ Resolve.show_resolve_error e)
  in
  match
    Typecheck.check_program ~import_types:type_ctx.Typecheck.name_types
      resolve_ctx.symbols prog
  with
  | Ok _ -> Ok ()
  | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)

(** Assert [src] type-checks. *)
let passes ?dir src =
  match frontend ?dir src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok, got: %s" m

(** Assert [src] is rejected with a message containing [needle]. *)
let fails_with ~needle src =
  match frontend src with
  | Ok () -> Alcotest.failf "expected a type error mentioning %S, got Ok" needle
  | Error m ->
    let nl = String.length needle and ml = String.length m in
    let rec go i = i + nl <= ml && (String.sub m i nl = needle || go (i + 1)) in
    if not (go 0) then Alcotest.failf "expected %S in: %s" needle m

let generic_return_type () =
  passes "pub enum Box<M> { B(M), E }\npub fn mk<M>(x: M) -> Box<M> = B(x);\n"

let concrete_application_in_signature () =
  passes "pub enum Box<M> { B(M), E }\npub fn f(x: Box<Int>) -> Int = 1;\n"

let two_parameter_enum () =
  passes
    "pub enum Pair<A, B> { P(A, B) }\n\
     pub fn swap<A, B>(p: Pair<A, B>) -> Pair<B, A> = match p { Pair::P(a, b) => P(b, a) };\n"

let function_payload () =
  passes
    "pub enum Attr<M> { On(String, Int -> M) }\n\
     pub fn on_click<M>(f: Int -> M) -> Attr<M> = On(\"click\", f);\n"

let parametric_extern_type () =
  passes
    "pub extern type Cell<T>;\n\
     pub extern fn cell_new<T>(v: T) -> Cell<T>;\n\
     pub fn mk() -> Cell<Int> = cell_new(1);\n"

(* Planted negative: the kind check must still reject over-application. *)
let over_application_still_rejected () =
  fails_with ~needle:"Too many arguments for kind"
    "pub enum Box<M> { B(M), E }\npub fn f(x: Box<Int, Int>) -> Int = 1;\n"

(* Cross-module: the importer only receives value schemes, so the arity of
   an imported `enum Html<M>` must be recovered from them. *)
let imported_enum_kind () =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "as_kinds_%d" (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out (Filename.concat dir "Html.affine") in
  output_string oc
    "module Html;\n\
     pub enum Html<M> { Text(String), Node(String, [Html<M>]) }\n\
     pub fn text<M>(s: String) -> Html<M> = Text(s);\n";
  close_out oc;
  passes ~dir
    "use Html::{Html, text};\n\
     pub enum Msg { Go }\n\
     pub fn view(n: Int) -> Html<Msg> = text(\"hi\");\n"

let tests =
  [
    Alcotest.test_case "generic return type" `Quick generic_return_type;
    Alcotest.test_case "concrete application in signature" `Quick
      concrete_application_in_signature;
    Alcotest.test_case "two-parameter enum" `Quick two_parameter_enum;
    Alcotest.test_case "function payload" `Quick function_payload;
    Alcotest.test_case "parametric extern type" `Quick parametric_extern_type;
    Alcotest.test_case "over-application still rejected" `Quick
      over_application_still_rejected;
    Alcotest.test_case "imported enum kind" `Quick imported_enum_kind;
  ]
