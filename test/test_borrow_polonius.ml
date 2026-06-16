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

(* ── use-after-move (plain, loan-free) forward moved-state dataflow ───────────── *)

(* var 1 moved at point 0, used again at point 1 ⇒ moved_in@1 ⇒ error(1).
   The move at 0 is also a use, but moved_in@0 is false (nothing moved it yet). *)
let t_uam_double_move () =
  let d = Solve.solve
    { Types.empty_facts with
      cfg_edge = [(0, 1)]; move_at = [(1, 0)]; use_at = [(1, 0); (1, 1)] } in
  check_mem  "moved_in@1" (1, 1) d.Types.moved_in;
  check_nmem "not moved_in@0" (1, 0) d.Types.moved_in;
  Alcotest.(check (list int)) "use-after-move error at 1" [1] d.Types.errors

(* a whole-place reinit between move and second use revives the var ⇒ no error *)
let t_uam_reinit_revives () =
  let d = Solve.solve
    { Types.empty_facts with
      cfg_edge = [(0, 1); (1, 2)];
      move_at = [(1, 0)]; reinit_at = [(1, 1)]; use_at = [(1, 0); (1, 2)] } in
  check_nmem "not moved_in@2 (revived)" (1, 2) d.Types.moved_in;
  Alcotest.(check (list int)) "no use-after-move error" [] d.Types.errors

(* a use of a NEVER-moved var is never an error (reads are always emitted) *)
let t_uam_use_without_move_ok () =
  let d = Solve.solve
    { Types.empty_facts with cfg_edge = [(0, 1)]; use_at = [(1, 0); (1, 1)] } in
  Alcotest.(check (list int)) "no error without a move" [] d.Types.errors

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

(* M3 loan-vs-loan exclusivity (#553 ADR-022): two simultaneously-live `&mut`
   borrows of the same place — the second `&mut x` reads `x` at its creation
   point while the first is still live, so it conflicts. *)
let t_extract_mutref_conflict () =
  let (pol, lex) = polonius_vs_lexical "borrow_mutref_conflict.affine" in
  Alcotest.(check bool) "Polonius flags &mut/&mut overlap" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* a plain read of `x` while `&mut x` is live (use-while-exclusively-borrowed) *)
let t_extract_mutref_use_while () =
  let (pol, lex) = polonius_vs_lexical "borrow_mutref_use_while.affine" in
  Alcotest.(check bool) "Polonius flags use-while-&mut-borrowed" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* a SHARED `&x` does NOT forbid reads — `let a = &x; let y = x; *a` is valid
   under both tiers (only `&mut` is exclusive). Guards against over-rejection. *)
let t_extract_shared_read_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_shared_read_ok.affine" in
  Alcotest.(check bool) "Polonius accepts read-while-shared-borrowed" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

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

(* ── M3 statement-level control flow: moves/borrows written as STATEMENTS inside
      branch bodies (not arm tails). The extractor now descends nested block
      statements; each must still agree with the lexical checker. ─────────────── *)

(* use-after-move via a `consume(a)` STATEMENT inside an if-then body, loan live *)
let t_extract_stmt_if_uam () =
  let (pol, lex) = polonius_vs_lexical "borrow_stmt_if_uam.affine" in
  Alcotest.(check bool) "Polonius flags branch-statement UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* r's last use precedes the branch ⇒ loan dead at the nested move ⇒ accept *)
let t_extract_stmt_if_nll_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_stmt_if_nll_ok.affine" in
  Alcotest.(check bool) "Polonius accepts NLL-safe branch move" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* move lives in the ELSE arm's statement body *)
let t_extract_stmt_else_uam () =
  let (pol, lex) = polonius_vs_lexical "borrow_stmt_else_uam.affine" in
  Alcotest.(check bool) "Polonius flags else-arm-statement UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* move is a statement inside a match-arm BODY block (not the arm tail) *)
let t_extract_stmt_match_uam () =
  let (pol, lex) = polonius_vs_lexical "borrow_stmt_match_uam.affine" in
  Alcotest.(check bool) "Polonius flags match-arm-statement UAM" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* the nested move is of an UNborrowed place ⇒ no conflict ⇒ accept *)
let t_extract_stmt_branch_unrelated_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_stmt_branch_unrelated_ok.affine" in
  Alcotest.(check bool) "Polonius accepts unrelated nested move" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* ── plain use-after-move from REAL programs (no loan), diffed vs lexical ──────── *)

(* `consume(a); consume(a)` — second move reads moved `a`; both tiers flag it *)
let t_extract_uam_double_move () =
  let (pol, lex) = polonius_vs_lexical "borrow_uam_double_move.affine" in
  Alcotest.(check bool) "Polonius flags the plain use-after-move" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* a whole-place rewrite between moves revives the var; both tiers accept *)
let t_extract_uam_reinit_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_uam_reinit_ok.affine" in
  Alcotest.(check bool) "Polonius accepts reinit-revived move" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* ── loop unrolling: iter-1 move reaching an iter-2 use (2-iteration model) ────── *)

(* the canonical Slice-C' soundness fixture: the loop body moves `x` and never
   rebinds it, so iteration 2 reads a moved value. The lexical checker flags it
   via its 2-iteration pass; the extractor's loop unrolling (a fresh iter-2 CFG
   point) makes the iter-1 move_at reach the iter-2 use_at, so both agree. *)
let t_extract_loop_move () =
  let (pol, lex) = polonius_vs_lexical "slice_c_prime_loop_move_persists.affine" in
  Alcotest.(check bool) "Polonius flags cross-iteration use-after-move" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* a loop body that moves nothing ⇒ unrolling must not invent an error *)
let t_extract_loop_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_uam_loop_ok.affine" in
  Alcotest.(check bool) "Polonius accepts the move-free loop" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* ── reassignment loan release (reborrow): old loan dies at the rebind ─────────── *)

(* `let mut r = pick(a); r = other(b); consume(a)` — reassigning r RELEASES its
   loan on a, so moving a is legal. Was a FALSE POSITIVE (Polonius kept a's loan
   live to r's last use); the reassignment-kill fix makes both tiers accept. *)
let t_extract_reassign_old_ok () =
  let (pol, lex) = polonius_vs_lexical "borrow_callee_returned_borrow_reassign_old_ok.affine" in
  Alcotest.(check bool) "Polonius accepts move after reassignment releases loan" false pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* the move-while-still-bound counterpart must still be flagged by both tiers *)
let t_extract_reassign_uam () =
  let (pol, lex) = polonius_vs_lexical "borrow_callee_returned_borrow_reassign.affine" in
  Alcotest.(check bool) "Polonius flags move while reassigned loan live" true pol;
  Alcotest.(check bool) "agrees with lexical" lex pol

(* ── M3 (3/3): the corpus-wide parallel-run diff gate ─────────────────────────
   Run BOTH tiers over every .affine fixture and assert the Polonius
   extractor+solver verdict never diverges from the lexical [Borrow.check_program]
   EXCEPT on a documented allowlist of known bounded-scope cases. A divergence on
   any non-allowlisted fixture fails this test — that is the zero-divergence
   regression gate M3 exists to provide. The extractor is NOT wired into
   bin/main.ml; this gate guards the equivalence claim, not the build verdict.

   Each allowlist entry is (fixture, reason). 16 are sound UNDER-reporting —
   features the extractor does not model yet, where lexical flags an error and
   Polonius (conservatively) does not. 1 is a known false positive
   (reassign_old_ok) awaiting the reborrow/loan-release increment. As later
   increments land, entries graduate OFF this list; an allowlisted fixture that
   has started AGREEING is logged (prune it) but does not fail the gate. *)
let known_divergences : (string * string) list =
  [ (* loan-vs-loan exclusivity (use-while-exclusively-borrowed) is now modeled
       by the extractor (a direct read of a place while a live [&mut] loan
       covers it is a conflict), so borrow_mutref_conflict / borrow_mutref_use_while
       AGREE and were pruned. borrow_use_while_excl is the call-aliasing shape
       (passing [x] to a [mut] param and reading [x] in the same call) — a
       [mut]-param argument is not yet modeled as a call-scoped exclusive loan,
       so it stays here. *)
    "borrow_use_while_excl.affine", "mut-param-arg aliasing (call-scoped excl borrow not modeled)";
    (* unmodeled: return-escape / borrow-outlives-owner (no escape analysis) *)
    "borrow_outlives_owner.affine", "borrow-outlives-owner";
    "borrow_return_escape_local.affine", "return-escape (local)";
    "borrow_return_escape_param.affine", "return-escape (param)";
    "ref_to_ref_return_escape.affine", "ref-to-ref + return-escape";
    (* unmodeled: captured-linear (lambda capture) *)
    "slice_d_captured_linear_let_rejected.affine", "captured-linear (let)";
    "slice_d_captured_linear_param_rejected.affine", "captured-linear (param)";
  ]

let t_parallel_run_diff () =
  let dir = Test_e2e.fixture_dir in
  let files = Sys.readdir dir |> Array.to_list |> List.sort compare
              |> List.filter (fun f -> Filename.check_suffix f ".affine") in
  let agree = ref 0 and skip = ref 0 in
  let unexpected = ref [] and now_agreeing = ref [] in
  List.iter (fun f ->
    match (try Some (polonius_vs_lexical f) with _ -> None) with
    | None -> incr skip   (* doesn't parse/resolve standalone — not borrow-checkable here *)
    | Some (pol, lex) ->
      if pol = lex then begin
        incr agree;
        if List.mem_assoc f known_divergences then now_agreeing := f :: !now_agreeing
      end else if not (List.mem_assoc f known_divergences) then
        unexpected := (f, pol, lex) :: !unexpected)
    files;
  List.iter (fun f ->
    Printf.eprintf "[diff-gate] allowlisted fixture now AGREES (prune it): %s\n%!" f)
    !now_agreeing;
  List.iter (fun (f, pol, lex) ->
    Printf.eprintf "[diff-gate] UNEXPECTED divergence: %s polonius=%b lexical=%b\n%!" f pol lex)
    !unexpected;
  Printf.eprintf "[diff-gate] total=%d agree=%d skip=%d allowlisted-divergences=%d\n%!"
    (List.length files) !agree !skip (List.length known_divergences);
  Alcotest.(check int) "no NEW (non-allowlisted) extractor↔lexical divergence"
    0 (List.length !unexpected)

let tests =
  [
    Alcotest.test_case "M3 3/3: corpus parallel-run diff gate" `Quick t_parallel_run_diff;
    Alcotest.test_case "rule1: liveness straight-line" `Quick t_live_straight;
    Alcotest.test_case "rule1: kill stops liveness (NLL)" `Quick t_kill_stops_liveness;
    Alcotest.test_case "rule1: liveness across branches" `Quick t_live_branches;
    Alcotest.test_case "rule2+3: invalidation → error" `Quick t_invalidation_errors;
    Alcotest.test_case "rule2: no conflict → no error" `Quick t_no_conflict_no_error;
    Alcotest.test_case "CTR/NLL: conflict after last-use kill is sound (no error)"
      `Quick t_nll_conflict_after_kill_ok;
    Alcotest.test_case "two loans, one conflicts → one error" `Quick t_two_loans_one_conflicts;
    Alcotest.test_case "UAM: double move → moved-state use error" `Quick t_uam_double_move;
    Alcotest.test_case "UAM: reinit revives (no error)" `Quick t_uam_reinit_revives;
    Alcotest.test_case "UAM: use without move is fine" `Quick t_uam_use_without_move_ok;
    Alcotest.test_case "subset transitive closure (reborrow chaining)" `Quick t_subset_closure;
    Alcotest.test_case "empty facts → empty derived" `Quick t_empty;
    (* M3 (2/3): extraction from real programs, diffed against the lexical checker *)
    Alcotest.test_case "extract+solve: #554 UAM flagged, agrees with lexical" `Quick t_extract_uam;
    Alcotest.test_case "extract+solve: NLL-safe accepted, agrees with lexical" `Quick t_extract_nll_ok;
    Alcotest.test_case "extract+solve: value-return no error, agrees with lexical" `Quick t_extract_value_return_ok;
    Alcotest.test_case "extract+solve: &mut/&mut overlap flagged (loan-vs-loan)" `Quick t_extract_mutref_conflict;
    Alcotest.test_case "extract+solve: use-while-&mut-borrowed flagged" `Quick t_extract_mutref_use_while;
    Alcotest.test_case "extract+solve: read-while-&-shared accepted (no over-reject)" `Quick t_extract_shared_read_ok;
    (* M3 branch extraction: conditional-origin family (issue-draft 08) *)
    Alcotest.test_case "extract+solve: if-bound UAM, agrees with lexical" `Quick t_extract_cond_if;
    Alcotest.test_case "extract+solve: block-bound UAM, agrees with lexical" `Quick t_extract_cond_block;
    Alcotest.test_case "extract+solve: match-bound UAM, agrees with lexical" `Quick t_extract_cond_match;
    Alcotest.test_case "extract+solve: partial-branch UAM, agrees with lexical" `Quick t_extract_cond_partial;
    Alcotest.test_case "extract+solve: cond NLL-safe accepted, agrees with lexical" `Quick t_extract_cond_nll_ok;
    Alcotest.test_case "extract+solve: cond unrelated move ok, agrees with lexical" `Quick t_extract_cond_unrelated_ok;
    (* M3 statement-level control flow: moves/borrows as statements in branch bodies *)
    Alcotest.test_case "extract+solve: if-stmt UAM, agrees with lexical" `Quick t_extract_stmt_if_uam;
    Alcotest.test_case "extract+solve: if-stmt NLL-safe accepted, agrees with lexical" `Quick t_extract_stmt_if_nll_ok;
    Alcotest.test_case "extract+solve: else-arm-stmt UAM, agrees with lexical" `Quick t_extract_stmt_else_uam;
    Alcotest.test_case "extract+solve: match-arm-stmt UAM, agrees with lexical" `Quick t_extract_stmt_match_uam;
    Alcotest.test_case "extract+solve: unrelated nested move ok, agrees with lexical" `Quick t_extract_stmt_branch_unrelated_ok;
    (* M3 plain use-after-move (loan-free) from real programs *)
    Alcotest.test_case "extract+solve: plain double-move flagged, agrees with lexical" `Quick t_extract_uam_double_move;
    Alcotest.test_case "extract+solve: reinit-revived move ok, agrees with lexical" `Quick t_extract_uam_reinit_ok;
    (* M3 loop unrolling: cross-iteration use-after-move (2-iteration model) *)
    Alcotest.test_case "extract+solve: loop cross-iter UAM flagged, agrees with lexical" `Quick t_extract_loop_move;
    Alcotest.test_case "extract+solve: move-free loop ok, agrees with lexical" `Quick t_extract_loop_ok;
    (* M3 reborrow: reassignment releases the old loan *)
    Alcotest.test_case "extract+solve: reassign releases old loan (was false +), agrees" `Quick t_extract_reassign_old_ok;
    Alcotest.test_case "extract+solve: move while reassigned loan live flagged, agrees" `Quick t_extract_reassign_uam;
  ]
