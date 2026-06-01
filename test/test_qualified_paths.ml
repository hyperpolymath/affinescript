(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)

(** ADR-014 / #228 — module-qualified type/effect path resolution.

    The parser (after #241) accepts `Pkg.T` / `Pkg::T` (mixed seps) at
    type and effect positions, folding the segments into a canonical
    `::`-joined name. This test module exercises the *resolution*
    counterpart shipped here: the typechecker strips the leading `Mod::`
    qualifier when `Mod` was introduced by `use Mod;`, and raises a
    clear [UnknownModule] error when it was not. Symmetric to
    [Resolve.lower_qualified_value_paths] (#178, value position).
*)

open Affinescript

(** parse -> resolve -> typecheck an inline source string. *)
let frontend (src : string) : (unit, string) result =
  let open Result in
  let ( let* ) = bind in
  let* prog =
    try Ok (Parse_driver.parse_string ~file:"<test_qualified_paths>" src)
    with
    | Parse_driver.Parse_error (m, sp) ->
      Error (Printf.sprintf "Parse error at %s: %s" (Span.show sp) m)
    | e -> Error (Printf.sprintf "Unexpected: %s" (Printexc.to_string e))
  in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  let* resolve_ctx =
    match Resolve.resolve_program_with_loader prog loader with
    | Ok (rc, _) -> Ok rc
    | Error (e, _) -> Error ("Resolution error: " ^ Resolve.show_resolve_error e)
  in
  match Typecheck.check_program resolve_ctx.symbols prog with
  | Ok _ -> Ok ()
  | Error e -> Error ("Type error: " ^ Typecheck.format_type_error e)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* `use Mod;` + qualified type ref → strips back to bare name, passes
   typecheck. `Ajv.Schema` lowers to `Schema` (current TyCon leniency
   keeps the unknown name as an abstract `TCon` — that is the
   pre-existing behaviour we deliberately do not change here). *)
let qualified_type_with_use_passes () =
  let src = "use Ajv;\npub fn f(x: Ajv.Schema) -> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok, got: %s" m

(* `::` separator works the same as `.` (parser folds both). *)
let qualified_type_with_double_colon_passes () =
  let src = "use Ajv;\npub fn f(x: Ajv::Schema) -> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok, got: %s" m

(* No `use`, qualified type ref → UnknownModule error mentioning the
   exact qualifier and ADR-014 / #228 attribution. *)
let qualified_type_unknown_module_rejected () =
  let src = "pub fn f(x: NoSuchMod.Thing) -> () { () }\n" in
  match frontend src with
  | Ok () -> Alcotest.fail "expected UnknownModule error, got Ok"
  | Error m ->
    Alcotest.(check bool) "names the missing module" true
      (contains ~needle:"NoSuchMod" m);
    Alcotest.(check bool) "cites ADR-014 / #228" true
      (contains ~needle:"#228" m);
    Alcotest.(check bool) "suggests use" true
      (contains ~needle:"use NoSuchMod" m)

(* No `use`, qualified effect ref → same UnknownModule path (not the
   permissive `Unknown effect` message). Pre-this-change the misleading
   `declare \`effect NoSuchMod::IO;\`` hint was emitted. *)
let qualified_effect_unknown_module_rejected () =
  let src = "pub fn f() -{NoSuchMod.IO}-> () { () }\n" in
  match frontend src with
  | Ok () -> Alcotest.fail "expected UnknownModule error, got Ok"
  | Error m ->
    Alcotest.(check bool) "names the missing module" true
      (contains ~needle:"NoSuchMod" m);
    Alcotest.(check bool) "is the UnknownModule error not UnknownEffect" true
      (contains ~needle:"#228" m)

(* Bare (unqualified) type refs are unaffected — the strip helper is a
   no-op when no `::` is present. Regression guard: this change must
   not perturb existing single-name lookup behaviour. *)
let bare_typecon_unaffected () =
  let src = "pub fn f(x: Int) -> Int { x }\n\
             pub fn g(y: SomeAbstract) -> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected permissive Ok on bare names, got: %s" m

(* Qualified ref with `use` but to a name that *is* a v1 effect after
   stripping (`Net` is reserved) resolves into the reserved-effect path,
   confirming the strip happens *before* the canonical-name lookup. *)
let qualified_reserved_effect_with_use_passes () =
  let src = "use Network;\npub fn f() -{Network.Net}-> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m -> Alcotest.failf "expected Ok, got: %s" m

(* #448 item 1: `qualified_type_name` head now accepts `lower_ident`
   so stdlib's lowercase module names (json, option, prelude, dict,
   alib, collections, io, string, result, testing, effects, math)
   are representable at type position. Parse-side regression guard. *)
let lowercase_qualified_type_parses () =
  let src = "use json;\npub fn f(x: json.Value) -> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m ->
    (* `json` may not be a known module in this isolated frontend run,
       so accept any non-parse error — what we explicitly forbid is the
       pre-fix `parse error at .` from the lowercase head. *)
    Alcotest.(check bool) "no parse error on lowercase qualifier head"
      false (contains ~needle:"Parse error" m);
    Alcotest.(check bool) "no `parse error at .`"
      false (contains ~needle:"parse error at" m);
    ignore m

(* Same shape with the `::` separator. *)
let lowercase_qualified_type_double_colon_parses () =
  let src = "use json;\npub fn f(x: json::Value) -> () { () }\n" in
  match frontend src with
  | Ok () -> ()
  | Error m ->
    Alcotest.(check bool) "no parse error on lowercase qualifier head (`::`)"
      false (contains ~needle:"Parse error" m);
    ignore m

(* Tail segment still requires UpperCase (TyCon name), so a fully-
   lowercase qualified path (`json.value`) must STILL parse-error. *)
let fully_lowercase_qualified_type_still_rejected () =
  let src = "use json;\npub fn f(x: json.value) -> () { () }\n" in
  match frontend src with
  | Ok () -> Alcotest.fail "expected parse error on lowercase tail segment"
  | Error _ -> ()

let tests = [
  Alcotest.test_case "qualified type + use → passes" `Quick
    qualified_type_with_use_passes;
  Alcotest.test_case "qualified type + use (`::`) → passes" `Quick
    qualified_type_with_double_colon_passes;
  Alcotest.test_case "qualified type, no use → UnknownModule" `Quick
    qualified_type_unknown_module_rejected;
  Alcotest.test_case "qualified effect, no use → UnknownModule" `Quick
    qualified_effect_unknown_module_rejected;
  Alcotest.test_case "bare TyCon unaffected (regression)" `Quick
    bare_typecon_unaffected;
  Alcotest.test_case "qualified reserved effect + use → passes" `Quick
    qualified_reserved_effect_with_use_passes;
  Alcotest.test_case "#448(1) lowercase qualifier head parses" `Quick
    lowercase_qualified_type_parses;
  Alcotest.test_case "#448(1) lowercase qualifier head + `::` parses" `Quick
    lowercase_qualified_type_double_colon_parses;
  Alcotest.test_case "#448(1) fully-lowercase qualified path still rejected" `Quick
    fully_lowercase_qualified_type_still_rejected;
]
