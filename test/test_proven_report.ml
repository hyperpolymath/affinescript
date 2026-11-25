(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** The Proven-Tests report emitter (test/proven_report.ml) must never hide a
    failing test, and must never claim more than [Unproven]. *)

let read path = Yojson.Safe.from_file path

let member k = function
  | `Assoc kv -> (try List.assoc k kv with Not_found -> `Null)
  | _ -> `Null

let test_failure_is_reported () =
  let path = Filename.temp_file "proven" ".json" in
  let cases =
    [ ("ok", `Quick, (fun () -> ()));
      ("bad", `Quick, (fun () -> Alcotest.(check int) "x" 1 2)) ]
  in
  let saved = Unix.dup Unix.stderr in
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Unix.dup2 null Unix.stderr;
  let bad =
    Fun.protect
      ~finally:(fun () -> Unix.dup2 saved Unix.stderr; Unix.close null)
      (fun () -> Proven_report.write ~suite:"t" ~module_path:"m" ~path cases)
  in
  Alcotest.(check int) "one test did not pass" 1 bad;
  let doc = read path in
  Sys.remove path;
  let entries = match member "entries" doc with `List l -> l | _ -> [] in
  let outcomes =
    List.map (fun e -> member "outcome" (member "result" e)) entries
  in
  Alcotest.(check (list string)) "outcomes kept per entry" [ "passed"; "failed" ]
    (List.map (function `String s -> s | _ -> "?") outcomes);
  List.iter
    (fun e ->
      Alcotest.(check string) "tier" "Unproven"
        (match member "provenance" e with `String s -> s | _ -> "?");
      Alcotest.(check bool) "empty proof ladder" true
        (member "proof_ladder" e = `List []))
    entries;
  Alcotest.(check bool) "summary tallies both as unproven" true
    (member "unproven" (member "summary" doc) = `Int 2)

let tests =
  [ Alcotest.test_case "a failing test is reported, not hidden" `Quick
      test_failure_is_reported ]
