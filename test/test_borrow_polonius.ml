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

open Affinescript
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

(* ── M3 (2/3) : end-to-end extraction from REAL programs, diffed vs lexical ───── *)
(* The payoff: extract facts from an actual .affine fixture, run the solver, and
   check its verdict AGREES with the lexical checker (Borrow.check_program) — the
   zero-divergence property M3's parallel-run gate will enforce corpus-wide. Scope
   is straight-line bodies (see Borrow_extract); these fixtures qualify. *)
let polonius_vs_lexical path =
  match Test_e2e.parse_fixture (Test_e2e.fixture path) with
  | Error m -> Alcotest.fail ("parse: " ^ m)
  | Ok prog ->
    match Test_e2e.resolve_program prog with
    | Error m -> Alcotest.fail ("resolve: " ^ m)
    | Ok (rc, _) ->
      let symbols = rc.symbols in
      let ctx = Borrow.build_context prog in
      let polonius_err = Borrow_extract.program_has_borrow_error ctx symbols prog in
      let lexical_err =
        match Borrow.check_program symbols prog with Error _ -> true | Ok () -> false in
      (polonius_err, lexical_err)

(* #554 straight-line use-after-move: both tiers must flag it *)
let t_extract_uam () =
  let (pol, lex) = polonius_vs_lexical "borrow_callee_returned_borrow_uam.affine" in
  Alcotest.(check bool) "Polonius flags the use-after-move" true pol;
  Alcotest.(check bool) "agrees with lexical verdict" lex pol

(* NLL-reordered (read *r before the move): both tiers must accept *)
let t_extract_nll_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_callee_returned_borrow_nll_ok.affine" in
  Alcotest.(check bool) "Polonius accepts the NLL-safe form" false pol;
  Alcotest.(check bool) "agrees with lexical verdict" lex pol

(* callee returns a VALUE (empty return-borrow summary) ⇒ arg movable, no error *)
let t_extract_value_return_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_callee_value_return_ok.affine" in
  Alcotest.(check bool) "Polonius: no loan, no error" false pol;
  Alcotest.(check bool) "agrees with lexical verdict" lex pol

(* expression-level branches (issue-draft 08 conditional-origin family): the RHS
   borrows the UNION of its arm sources. Polonius must now match the lexical fix. *)
let t_extract_cond_if () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_if_uam.affine" in
  Alcotest.(check bool) "Polonius flags if-bound UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

let t_extract_cond_block () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_block_uam.affine" in
  Alcotest.(check bool) "Polonius flags block-bound UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

let t_extract_cond_match () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_match_uam.affine" in
  Alcotest.(check bool) "Polonius flags match-bound UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

let t_extract_cond_partial () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_partial_uam.affine" in
  Alcotest.(check bool) "Polonius flags partial-branch UAM (union has a)" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

let t_extract_cond_nll_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_nll_ok.affine" in
  Alcotest.(check bool) "Polonius accepts NLL-safe cond-bound" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

let t_extract_cond_unrelated_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_cond_origin_unrelated_ok.affine" in
  Alcotest.(check bool) "Polonius accepts unrelated move (not in {a,b})" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

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
    (* M3 (2/3): extraction from real programs, diffed against the lexical checker *)
    Alcotest.test_case "extract+solve: #554 UAM flagged, agrees with lexical" `Quick t_extract_uam;
    Alcotest.test_case "extract+solve: NLL-safe accepted, agrees with lexical" `Quick t_extract_nll_ok;
    Alcotest.test_case "extract+solve: value-return no error, agrees with lexical" `Quick t_extract_value_return_ok;
    (* M3 branch extraction: conditional-origin family (issue-draft 08) *)
    Alcotest.test_case "extract+solve: if-bound UAM, agrees with lexical" `Quick t_extract_cond_if;
    Alcotest.test_case "extract+solve: block-bound UAM, agrees with lexical" `Quick t_extract_cond_block;
    Alcotest.test_case "extract+solve: match-bound UAM, agrees with lexical" `Quick t_extract_cond_match;
    Alcotest.test_case "extract+solve: partial-branch UAM, agrees with lexical" `Quick t_extract_cond_partial;
    Alcotest.test_case "extract+solve: cond NLL-safe accepted, agrees with lexical" `Quick t_extract_cond_nll_ok;
    Alcotest.test_case "extract+solve: cond unrelated move ok, agrees with lexical" `Quick t_extract_cond_unrelated_ok;
  ]
