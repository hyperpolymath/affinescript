(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath (Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>) *)

(** Soundness pin harness for docs/SOUNDNESS.adoc (property 5).

    Two kinds of executable soundness-row check live here:

    - [pins] — *xfail* pins for `residual (pinned)` rows. Each asserts the
      DESIRED behaviour; because the hole is still present the assertion fails,
      which is expected and reported [XFAIL-OK]. The day the hole is fixed it
      passes — reported [XPASS], non-zero exit — telling the engineer to update
      the ledger row rather than silence the pin. (Fail-closed: any OTHER
      exception is [XERROR], also non-zero — broken pin infrastructure must not
      masquerade as an expected failure.)

    - [fences] — positive checks for `loud-fail` rows: the hole is closed by a
      loud failure, and the check asserts that loud failure still holds (it
      passes now and would fail if the silent behaviour regressed).

    The shell gate (tools/check-soundness-ledger.sh, property 5) runs this
    executable and asserts every pin named by a `residual (pinned)` / `open
    (tracked)` row reports XFAIL-OK; XPASS is surfaced as the distinct "is the
    hole fixed?" alarm. The exe exits non-zero on any XPASS / XERROR / fence
    failure, so `dune runtest` catches them too. *)

open Affinescript

exception Xfail of string

let fixtures_dir =
  let cands =
    (match Sys.getenv_opt "AFFINE_FIXTURES" with Some d -> [ d ] | None -> [])
    @ [ "../e2e/fixtures"; "e2e/fixtures"; "test/e2e/fixtures" ]
  in
  match List.find_opt Sys.file_exists cands with
  | Some d -> d
  | None ->
    failwith
      "xfail: cannot locate test/e2e/fixtures (set AFFINE_FIXTURES to its path)"

let fixture name = Filename.concat fixtures_dir name

let parse path =
  try Parse_driver.parse_file path
  with e -> failwith ("parse " ^ path ^ ": " ^ Printexc.to_string e)

let resolve_symbols prog =
  let loader = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog loader with
  | Ok (rctx, _) -> rctx.symbols
  | Error (e, _) -> failwith ("resolve: " ^ Resolve.show_resolve_error e)

(* ---- xfail pin: #555 / #623 — interpreter non-tail single-shot resume -----
   `let x = op(); x + 100` with `op() => resume(5)` should yield 105 once the
   continuation is reified; the shallow tree-walking interpreter yields 5. *)
let test_resume_nontail_xfail () =
  let path = fixture "handle_resume_nontail.affine" in
  let prog = parse path in
  let _ = resolve_symbols prog in
  let v =
    match Interp.eval_program prog with
    | Error e -> failwith ("eval: " ^ Value.show_eval_error e)
    | Ok env -> (
      match Value.lookup_env "main" env with
      | Error _ -> failwith "no `main` binding"
      | Ok f -> (
        match Interp.apply_function f [] with
        | Ok v -> v
        | Error e -> failwith ("apply main: " ^ Value.show_eval_error e)))
  in
  match v with
  | Value.VInt 105 -> () (* desired met -> XPASS: the hole is fixed *)
  | Value.VInt 5 ->
    raise (Xfail "non-tail resume returns 5, not 105 (shallow continuation)")
  | other ->
    raise
      (Xfail ("non-tail resume returned " ^ Value.show_value other ^ ", not 105"))

(* ---- fence: #555-stub / #624 — Lean backend fails loud on `return` ---------
   Fixed: `codegen_lean` now returns Error on an early `return` (the
   fn_body_contains_return fence) instead of silently lowering to `:= ()`. This
   positive check asserts the loud failure holds; it would fail if the silent
   drop regressed. *)
let test_stub_backend_return_fenced () =
  let path = fixture "stub_backend_return_dropped.affine" in
  let prog = parse path in
  let symbols = resolve_symbols prog in
  match Lean_codegen.codegen_lean prog symbols with
  | Error _ -> () (* desired loud failure holds *)
  | Ok _ ->
    failwith
      "Lean_codegen.codegen_lean returned Ok on an early `return` — the #624 \
       loud fence regressed to a silent drop"

let pins =
  [ ("test_resume_nontail_xfail",
     "#555/#623 interp non-tail resume -> 5 not 105", test_resume_nontail_xfail) ]

let fences =
  [ ("test_stub_backend_return_fenced",
     "#555-stub/#624 Lean fails loud on `return`", test_stub_backend_return_fenced) ]

let () =
  let bad = ref false in
  List.iter
    (fun (name, reason, body) ->
      match try `Ok (body ()) with Xfail m -> `Xfail m | e -> `Err (Printexc.to_string e) with
      | `Xfail _ -> Printf.printf "XFAIL-OK %s -- %s\n" name reason
      | `Ok () ->
        bad := true;
        Printf.printf
          "XPASS %s -- pin passed; is the hole fixed? update docs/SOUNDNESS.adoc\n"
          name
      | `Err m ->
        bad := true;
        Printf.printf "XERROR %s -- pin infrastructure failed: %s\n" name m)
    pins;
  List.iter
    (fun (name, reason, body) ->
      match try `Ok (body ()) with e -> `Err (Printexc.to_string e) with
      | `Ok () -> Printf.printf "FENCE-OK %s -- %s\n" name reason
      | `Err m ->
        bad := true;
        Printf.printf "FENCE-FAIL %s -- loud failure regressed: %s\n" name m)
    fences;
  flush stdout;
  if !bad then exit 1 else exit 0
