(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Proven-Tests run report (schema_version 2) for an Alcotest suite.

    The producer half of the hyperpolymath/proven-tests-and-benches
    integration contract, written in OCaml because the framework's own
    emitter is Python.  The report is checked by the framework's Rust
    validator, [proven-evidence check] (see tools/ci/proven/proven-report.sh).

    Every entry is stated at the tier its evidence supports and no higher:
    [Unproven], with an empty proof ladder.  None of these tests yet carries
    a demonstrated firing fixture (an input with the defect, shown to make
    the test fail), so each one is [FixtureDebt], referencing
    [debt_ref].  The contract leaves debt entries out of the pass/fail
    ledger, so the emitter itself fails when any test does not pass:
    a failure never disappears into the debt column. *)

let debt_ref =
  "hyperpolymath/affinescript tools/ci/proven/VENDOR.adoc#proven-tests \
   (no demonstrated firing fixture yet)"

(* The size of the taxonomy's (category, aspect) lattice, as pinned in the
   contract (contract-version.json, LATTICE_CELLS in proven-evidence). *)
let cat_aspect_cells = 238

type outcome = Passed | Failed of string

let outcome_json = function
  | Passed -> `Assoc [ ("outcome", `String "passed") ]
  | Failed m -> `Assoc [ ("outcome", `String "failed"); ("message", `String m) ]

let run_case (f : unit -> unit) : outcome * int =
  let t0 = Unix.gettimeofday () in
  let r =
    try f (); Passed with
    (* An Alcotest check failure and any other exception both mean the test
       did not pass; the contract's "error" outcome is not distinguished. *)
    | e -> Failed (Printexc.to_string e)
  in
  (r, int_of_float ((Unix.gettimeofday () -. t0) *. 1000.))

let entry ~module_path (name, _speed, f) =
  let outcome, ms = run_case f in
  let json =
    `Assoc
      [ ( "test_id",
          `Assoc
            [ ("module_path", `String module_path);
              ("test_name", `String name);
              ("line_number", `Int 0) ] );
        ("category", `String "Unit");
        ("aspect", `String "Functionality");
        ("typesafe_category", `Null);
        ("provenance", `String "Unproven");
        ("evidence", `Assoc []);
        ("proof_ladder", `List []);
        ("result", outcome_json outcome);
        ("fixture_obligation", `String "FixtureDebt");
        ("fixture_debt_ref", `String debt_ref);
        ("duration_ms", `Int ms) ]
  in
  (json, outcome)

(** Run [tests] and write the report to [path].  Returns the number of
    tests that did not pass. *)
let write ~suite ~module_path ~path tests =
  let results = List.map (entry ~module_path) tests in
  let n = List.length results in
  let not_passed =
    List.length (List.filter (fun (_, o) -> o <> Passed) results)
  in
  (* Debt entries count in the tier tally only (contract counting rule). *)
  let summary =
    `Assoc
      [ ("passed", `Int 0); ("failed", `Int 0); ("errors", `Int 0);
        ("skipped", `Int 0); ("total", `Int 0);
        ("actually_proven", `Int 0); ("provisionally_proven", `Int 0);
        ("unproven", `Int n); ("covered_cells", `Int 0);
        ("cat_aspect_cells", `Int cat_aspect_cells);
        ("lattice_covered", `Int 0) ]
  in
  let doc =
    `Assoc
      [ ("schema_version", `Int 2);
        ("suite", `String suite);
        ("generated_at",
          `String
            (let t = Unix.gmtime (Unix.time ()) in
             Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
               (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
               t.tm_hour t.tm_min t.tm_sec));
        ("toolchain",
          `Assoc [ ("ocaml", `String Sys.ocaml_version);
                   ("runner", `String "alcotest") ]);
        ("summary", summary);
        ("entries", `List (List.map fst results)) ]
  in
  Yojson.Safe.to_file path doc;
  List.iter
    (fun (j, o) ->
      match o, j with
      | Passed, _ -> ()
      | Failed m, `Assoc kv ->
        let name =
          match List.assoc "test_id" kv with
          | `Assoc t -> (match List.assoc "test_name" t with `String s -> s | _ -> "?")
          | _ -> "?"
        in
        Printf.eprintf "proven-report: %s did not pass: %s\n" name m
      | _ -> ())
    results;
  not_passed
