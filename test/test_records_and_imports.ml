(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)
(** Record update typing and imported type definitions.

    - `S #{ ..base, f: v }` used to ignore the spread when typing, so the
      result had only the explicit fields and an update could never produce
      the struct it started from.
    - An imported struct was an opaque name in the importer (imports carry
      value schemes only), so its fields could not be read. *)

open Affinescript

(** parse -> resolve (loader rooted at [dir]) -> typecheck. *)
let frontend ?(dir = Sys.getcwd ()) (src : string) : (unit, string) result =
  let ( let* ) = Result.bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_records_and_imports>" src)
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

(** Assert [src] is rejected by the *type checker* (a parse or resolution
    error would hide whether field typing ran), with a message containing
    every string in [needles]. *)
let fails ?dir ?(needles = []) src =
  match frontend ?dir src with
  | Ok () -> Alcotest.fail "expected a type error, got Ok"
  | Error m ->
    let has n =
      let nl = String.length n and ml = String.length m in
      let rec go i = i + nl <= ml && (String.sub m i nl = n || go (i + 1)) in
      go 0
    in
    if not (has "Type error") then Alcotest.failf "expected a type error, got: %s" m;
    List.iter (fun n -> if not (has n) then Alcotest.failf "expected %S in: %s" n m) needles

let p = "struct P { a: Int, b: String }\n"

let update_keeps_struct_type () = passes (p ^ "pub fn f(q: P) -> P = P #{ ..q, a: 2 };\n")
let spread_only () = passes (p ^ "pub fn f(q: P) -> P = P #{ ..q };\n")
let untouched_field_readable () =
  passes (p ^ "pub fn f(q: P) -> String { let r = P #{ ..q, a: 2 }; r.b }\n")
(* The result type is `String` (read from `r.a`), which the old
   spread-ignoring typing would also accept: rejection here comes only from
   validating the incompatible update itself. *)
let update_cannot_change_field_type () =
  fails (p ^ "pub fn f(q: P) -> String { let r = P #{ ..q, a: \"x\" }; r.a }\n")
let update_through_destructured_tuple () =
  passes (p ^ "fn mk(q: P) -> (P, Int) = (q, 1);\n\
               pub fn f(q: P) -> P { let (r, n) = mk(q); P #{ ..r, a: n } }\n")

(** A temp directory holding [name].affine with [body]. *)
let module_dir name body =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "as_rec_%s_%d" name (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out (Filename.concat dir (name ^ ".affine")) in
  output_string oc body;
  close_out oc;
  dir

let imported_struct_fields () =
  let dir = module_dir "Shapes"
      "module Shapes;\npub struct Point { x: Float, y: Float }\n\
       pub fn origin() -> Point = Point #{ x: 0.0, y: 0.0 };\n" in
  passes ~dir
    "use Shapes::{Point, origin};\n\
     pub fn sum(p: Point) -> Float = p.x + p.y;\n\
     pub fn moved() -> Point { let o = origin(); Point #{ ..o, x: 1.0 } }\n"

let imported_struct_unknown_field_rejected () =
  let dir = module_dir "Shapes2"
      "module Shapes2;\npub struct Point { x: Float, y: Float }\n" in
  fails ~dir ~needles:[ "z" ] "use Shapes2::{Point};\npub fn f(p: Point) -> Float = p.z;\n"

(* A function-local binding must not replace the module-level binding of
   the same name in what importers see. *)
let local_binding_does_not_shadow_export () =
  let dir = module_dir "Shadow"
      "module Shadow;\npub fn node(x: Int) -> Int = x + 1;\n\
       pub fn uses() -> Bool { let node = true; node }\n" in
  passes ~dir "use Shadow::{node};\npub fn f() -> Int = node(41);\n"

let tests =
  [
    Alcotest.test_case "update keeps struct type" `Quick update_keeps_struct_type;
    Alcotest.test_case "spread only" `Quick spread_only;
    Alcotest.test_case "untouched field readable" `Quick untouched_field_readable;
    Alcotest.test_case "update cannot change field type" `Quick update_cannot_change_field_type;
    Alcotest.test_case "update through destructured tuple" `Quick update_through_destructured_tuple;
    Alcotest.test_case "imported struct fields" `Quick imported_struct_fields;
    Alcotest.test_case "imported struct unknown field rejected" `Quick
      imported_struct_unknown_field_rejected;
    Alcotest.test_case "local binding does not shadow export" `Quick
      local_binding_does_not_shadow_export;
  ]
