(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Polonius loan-solver tests (ADR-022 M3, solver milestone), CRG-mapped:
    - UT  : one assertion per datalog rule (liveness, kill, invalidation, error).
    - CTR : the soundness/NLL invariants — a killed loan is dead downstream (no
            spurious error after last use); a live conflicting access errors.
    - the subset transitive closure used by the M3 extractor for reborrow chaining.

    These exercise the solver on hand-built fact bases, independent of the (not
    yet wired) constraint extractor — the algorithmic heart, verified before the
    CFG-extraction + parallel-run-diff increments land. *)

open Borrow_polonius

let check_mem name p l = Alcotest.(check bool) name true (List.mem p l)
let check_nmem name p l = Alcotest.(check bool) name false (List.mem p l)

(* ── rule 1 : loan liveness over the CFG ──────────────────────────────────────── *)

(* loan 1 born at point 0; CFG 0→1→2; never killed ⇒ live at 0,1,2 *)
let t_live_straight () =
  let d = Solve.solve
    { Types.empty_facts with borrow_at = [(1, 0)]; cfg_edge = [(0, 1); (1, 2)] } in
  check_mem  "live@0" (1, 0) d.Types.loan_live_at;
  check_mem  "live@1" (1, 1) d.Types.loan_live_at;
  check_mem  "live@2" (1, 2) d.Types.loan_live_at

(* killed at point 1 ⇒ dead at 1 and everything past it (NLL last-use) *)
let t_kill_stops_liveness () =
  let d = Solve.solve
    { Types.empty_facts with
      borrow_at = [(1, 0)]; cfg_edge = [(0, 1); (1, 2)]; killed = [(1, 1)] } in
  check_mem  "still live@0" (1, 0) d.Types.loan_live_at;
  check_nmem "dead@1 (killed)" (1, 1) d.Types.loan_live_at;
  check_nmem "dead@2 (past kill)" (1, 2) d.Types.loan_live_at

(* branch: 0→1 and 0→2 ⇒ live along both arms *)
let t_live_branches () =
  let d = Solve.solve
    { Types.empty_facts with borrow_at = [(1, 0)]; cfg_edge = [(0, 1); (0, 2)] } in
  check_mem "live@1 (arm a)" (1, 1) d.Types.loan_live_at;
  check_mem "live@2 (arm b)" (1, 2) d.Types.loan_live_at

(* ── rules 2 + 3 : invalidation and error ─────────────────────────────────────── *)

(* live loan, conflicting access at point 1 ⇒ invalidated there ⇒ error(1) *)
let t_invalidation_errors () =
  let d = Solve.solve
    { Types.empty_facts with
      borrow_at = [(1, 0)]; cfg_edge = [(0, 1)]; conflict_at = [(1, 1)] } in
  check_mem "invalidated@1" (1, 1) d.Types.loan_invalidated_at;
  check_mem "error@1" 1 d.Types.errors

(* a conflicting access where the loan is NOT live ⇒ no invalidation, no error *)
let t_no_conflict_no_error () =
  let d = Solve.solve
    { Types.empty_facts with borrow_at = [(1, 0)]; cfg_edge = [(0, 1)] } in
  Alcotest.(check (list int)) "no errors" [] d.Types.errors

(* THE NLL SOUNDNESS CASE: access conflicts at a point AFTER the loan's last use
   (killed) ⇒ loan dead there ⇒ no error. Mirrors borrow_*_nll_ok at the lexical
   tier — Polonius must agree (zero-divergence is M3's CI gate). *)
let t_nll_conflict_after_kill_ok () =
  let d = Solve.solve
    { Types.empty_facts with
      borrow_at = [(1, 0)]; cfg_edge = [(0, 1); (1, 2)];
      killed = [(1, 1)]; conflict_at = [(1, 2)] } in
  Alcotest.(check (list int)) "no error after last use" [] d.Types.errors

(* two loans, only one conflicts ⇒ exactly one error point *)
let t_two_loans_one_conflicts () =
  let d = Solve.solve
    { Types.empty_facts with
      borrow_at = [(1, 0); (2, 0)]; cfg_edge = [(0, 1)];
      conflict_at = [(2, 1)] } in
  check_nmem "loan1 not invalidated" (1, 1) d.Types.loan_invalidated_at;
  check_mem  "loan2 invalidated" (2, 1) d.Types.loan_invalidated_at;
  Alcotest.(check (list int)) "single error point" [1] d.Types.errors

(* ── subset transitive closure (reborrow chaining input) ──────────────────────── *)

let t_subset_closure () =
  let c = Solve.subset_closure
    { Types.empty_facts with subset = [(1, 2, 0); (2, 3, 0)] } in
  check_mem "reflexive (1,1)" (1, 1) c;
  check_mem "direct (1,2)" (1, 2) c;
  check_mem "transitive (1,3)" (1, 3) c;
  check_nmem "no spurious (3,1)" (3, 1) c

(* ── degenerate ───────────────────────────────────────────────────────────────── *)

let t_empty () =
  let d = Solve.solve Types.empty_facts in
  Alcotest.(check (list int)) "empty errors" [] d.Types.errors;
  Alcotest.(check int) "empty liveness" 0 (List.length d.Types.loan_live_at)

let tests =
  [
    Alcotest.test_case "rule1: liveness straight-line" `Quick t_live_straight;
    Alcotest.test_case "rule1: kill stops liveness (NLL)" `Quick t_kill_stops_liveness;
    Alcotest.test_case "rule1: liveness across branches" `Quick t_live_branches;
    Alcotest.test_case "rule2+3: invalidation → error" `Quick t_invalidation_errors;
    Alcotest.test_case "rule2: no conflict → no error" `Quick t_no_conflict_no_error;
    Alcotest.test_case "CTR/NLL: conflict after last-use kill is sound (no error)"
      `Quick t_nll_conflict_after_kill_ok;
    Alcotest.test_case "two loans, one conflicts → one error" `Quick t_two_loans_one_conflicts;
    Alcotest.test_case "subset transitive closure (reborrow chaining)" `Quick t_subset_closure;
    Alcotest.test_case "empty facts → empty derived" `Quick t_empty;
  ]
