(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath (Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>) *)

(** xfail pin harness for the soundness ledger (docs/SOUNDNESS.adoc, property 5).

    Each pin asserts the DESIRED behaviour of a known soundness hole. Because the
    hole is still present, the assertion currently FAILS — which is the expected
    state, reported as [XFAIL-OK]. The day the hole is fixed, the assertion
    PASSES, which is unexpected and reported as [XPASS]: the process exits
    non-zero (so [dune runtest] and the gate both go red) with a message telling
    the engineer to update the ledger row rather than silence the pin.

    Polarity / fail-closed contract:
      - pin body raises [Xfail] when the desired behaviour is NOT met (hole
        present)         -> XFAIL-OK (exit 0 contribution)
      - pin body returns normally (desired behaviour met, hole gone)
                          -> XPASS    (exit 1)
      - pin body raises ANY OTHER exception (parse error, missing fixture, …)
                          -> XERROR   (exit 1) — pin infrastructure is broken,
                             so we fail closed rather than mistake it for an
                             expected failure.

    The shell gate (tools/check-soundness-ledger.sh, property 5) runs this
    executable, asserts every pin named by a `residual (pinned)` / `open
    (tracked)` ledger row reports XFAIL-OK, and surfaces XPASS as the distinct
    "is the hole fixed?" alarm. *)

open Affinescript

exception Xfail of string

(* Locate test/e2e/fixtures across the run contexts we care about:
   AFFINE_FIXTURES override (the gate sets this), the dune-test sandbox
   (cwd = _build/default/test/xfail, fixtures at ../e2e/fixtures via the
   source_tree dep), cwd = test/, and cwd = repo root. Fail closed if none. *)
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

(* ---- Pin: #555 / #623 — interpreter non-tail single-shot resume ----------
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

(* ---- Pin: #555-stub / #624 — Lean backend drops `return` ------------------
   The agreed remedy (#624) is to fail loud on early `return`; today
   `codegen_lean` returns Ok and silently lowers the body to `:= ()`. *)
let test_stub_backend_return_xfail () =
  let path = fixture "stub_backend_return_dropped.affine" in
  let prog = parse path in
  let symbols = resolve_symbols prog in
  match Lean_codegen.codegen_lean prog symbols with
  | Error _ -> () (* desired met -> XPASS: the backend now fails loud *)
  | Ok _ ->
    raise
      (Xfail
         "Lean_codegen.codegen_lean returned Ok; early `return` silently dropped")

let pins =
  [ ("test_resume_nontail_xfail",
     "#555/#623 interp non-tail resume -> 5 not 105", test_resume_nontail_xfail);
    ("test_stub_backend_return_xfail",
     "#555-stub/#624 Lean drops `return`", test_stub_backend_return_xfail) ]

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
  flush stdout;
  if !bad then exit 1 else exit 0
