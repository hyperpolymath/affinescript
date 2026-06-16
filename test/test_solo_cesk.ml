(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Solo-core CESK machine tests (VM M1, ADR-0025), CRG-mapped:
    - UT  : one assertion per Solo small-step rule (β, ⊗-proj, ⊕-case, let, env).
    - EXE : runs closed terms to a value (the machine actually executes).
    - CTR : the Q4 runtime invariant — affine enforcement ON rejects over-use,
            permits under-use (affine ≤1), and ω allows reuse; OFF never rejects.
    - PER : tropical cost-meter accumulates; a budget too small → Infeasible.
    - determinism (metamorphic): same term ⇒ same value + cost.

    Full property-based testing (qcheck, 1000+ generated cases) is the matrix GAP
    tracked in docs/TESTING-AND-BENCH-MATRIX.adoc; this is the deterministic seed. *)

open Affinescript
open Solo_cesk

(* ── helpers ─────────────────────────────────────────────────────────────────── *)
let runs_to ?(cfg = default_config) (t : term) (expected : string) () =
  let (v, _) = run ~cfg t in
  Alcotest.(check string) "value" expected (show_value v)

let raises_affine (t : term) () =
  match run t with
  | exception Affine_violation _ -> ()
  | (v, _) -> Alcotest.failf "expected Affine_violation, got %s" (show_value v)

let unchecked = { enforce = false; budget = None }

(* sample terms *)
let id1   = Lam (One, Var 0)                         (* λ^1 x. x *)
let kcomb = Lam (Omega, Lam (One, Var 1))            (* λ^ω x. λ^1 y. x *)

(* ── UT + EXE : one rule per case ────────────────────────────────────────────── *)
let t_beta       = runs_to (App (id1, TUnit)) "()"
let t_unit       = runs_to TUnit "()"
let t_pair       = runs_to (Pair (TUnit, Inl TUnit)) "((), inl ())"
let t_fst        = runs_to (Fst (Pair (TUnit, Inl TUnit))) "()"          (* drops snd — affine *)
let t_snd        = runs_to (Snd (Pair (TUnit, Inl TUnit))) "inl ()"      (* drops fst — affine *)
let t_inl        = runs_to (Inl TUnit) "inl ()"
let t_inr        = runs_to (Inr TUnit) "inr ()"
let t_case_inl   = runs_to (Case (Inl TUnit, One, Var 0, TUnit)) "()"
let t_case_inr   = runs_to (Case (Inr TUnit, One, TUnit, Var 0)) "()"
let t_let        = runs_to (Let (One, TUnit, Var 0)) "()"
let t_kcomb_env  = runs_to (App (App (kcomb, TUnit), Inl TUnit)) "()"    (* de Bruijn / closures *)

(* ── CTR : the affine runtime invariant (Q4) ─────────────────────────────────── *)
let dup = App (Lam (One, Pair (Var 0, Var 0)), TUnit)   (* linear param used twice *)
let t_affine_violation   = raises_affine dup                              (* ON  ⇒ reject *)
let t_affine_unchecked   = runs_to ~cfg:unchecked dup "((), ())"          (* OFF ⇒ allowed *)
let t_omega_reuse_ok     = runs_to (App (Lam (Omega, Pair (Var 0, Var 0)), TUnit)) "((), ())"
let t_affine_underuse_ok = runs_to (App (Lam (One, TUnit), TUnit)) "()"   (* used 0 ≤ 1 — OK *)
let t_zero_use_rejected  = raises_affine (App (Lam (Zero, Var 0), TUnit)) (* erased var used *)

(* ── PER : tropical cost-metering ────────────────────────────────────────────── *)
let t_cost_positive () =
  let (_, c) = run (App (id1, TUnit)) in
  Alcotest.(check bool) "cost > 0" true (c > 0)

let t_budget_infeasible () =
  match run ~cfg:{ enforce = true; budget = Some 2 } (App (id1, TUnit)) with
  | exception Infeasible _ -> ()
  | (v, _) -> Alcotest.failf "expected Infeasible, got %s" (show_value v)

(* ── determinism (metamorphic) ──────────────────────────────────────────────── *)
let t_deterministic () =
  let prog = App (App (kcomb, TUnit), Inl TUnit) in
  let (v1, c1) = run prog and (v2, c2) = run prog in
  Alcotest.(check string) "value stable" (show_value v1) (show_value v2);
  Alcotest.(check int) "cost stable" c1 c2

(* ── M2 : deep effect handlers + multi-shot resume (#555) ─────────────────────── *)
(* These are the FIRST runtime handler tests in the repo (ADR-0025 M2 noted "zero
   exist today"). Op labels are ints; an op clause body sees de Bruijn 0 = the
   operation argument, 1 = the resumption [k]. *)
let raises_unhandled (t : term) () =
  match run t with
  | exception Unhandled_effect _ -> ()
  | (v, _) -> Alcotest.failf "expected Unhandled_effect, got %s" (show_value v)

(* handle (return ()) with { return x ⇒ x }  — the return clause fires *)
let t_handle_return =
  runs_to (Handle (TUnit, { h_ret = (One, Var 0); h_ops = [] })) "()"

(* handle (perform op0 ()) with { return x⇒x | op0(_) k ⇒ resume k (inl ()) }
   single-shot: the op clause resumes once; the injected value flows back to the
   handled body's value, then through the return clause. *)
let t_handle_single_shot =
  runs_to
    (Handle (Perform (0, TUnit),
       { h_ret = (One, Var 0);
         h_ops = [ (0, One, One, Resume (Var 1, Inl TUnit)) ] }))
    "inl ()"

(* MULTI-SHOT: op0(_) k ⇒ (resume k (inl ()), resume k (inr ())).  The SAME
   reified continuation is resumed twice — only possible because [k] is data.
   resume quantity is ω (the resumption is used twice). Result pairs both runs. *)
let t_handle_multi_shot =
  runs_to
    (Handle (Perform (0, TUnit),
       { h_ret = (One, Var 0);
         h_ops = [ (0, One, Omega,
                    Pair (Resume (Var 1, Inl TUnit), Resume (Var 1, Inr TUnit))) ] }))
    "(inl (), inr ())"

(* DEEP: let _ = perform op0 (inl ()) in perform op0 (inr ()), handled by
   { op0(arg) k ⇒ resume k arg }. The SECOND perform sits inside the resumed
   computation and must be caught by the SAME handler (deep) — if it weren't, it
   would raise Unhandled_effect. Final value is the second perform's argument. *)
let t_handle_deep =
  runs_to
    (Handle (
       Let (Omega, Perform (0, Inl TUnit), Perform (0, Inr TUnit)),
       { h_ret = (One, Var 0);
         h_ops = [ (0, One, One, Resume (Var 1, Var 0)) ] }))
    "inr ()"

(* perform with no enclosing handler ⇒ Unhandled_effect (loud, not silent). *)
let t_perform_unhandled = raises_unhandled (Perform (0, TUnit))

let tests =
  [
    Alcotest.test_case "β-reduction" `Quick t_beta;
    Alcotest.test_case "unit" `Quick t_unit;
    Alcotest.test_case "pair intro" `Quick t_pair;
    Alcotest.test_case "fst (drops snd)" `Quick t_fst;
    Alcotest.test_case "snd (drops fst)" `Quick t_snd;
    Alcotest.test_case "inl" `Quick t_inl;
    Alcotest.test_case "inr" `Quick t_inr;
    Alcotest.test_case "case inl" `Quick t_case_inl;
    Alcotest.test_case "case inr" `Quick t_case_inr;
    Alcotest.test_case "let" `Quick t_let;
    Alcotest.test_case "de Bruijn / closures (K)" `Quick t_kcomb_env;
    Alcotest.test_case "CTR: affine over-use rejected (enforce on)" `Quick t_affine_violation;
    Alcotest.test_case "CTR: same allowed under --unchecked" `Quick t_affine_unchecked;
    Alcotest.test_case "CTR: ω permits reuse" `Quick t_omega_reuse_ok;
    Alcotest.test_case "CTR: affine under-use ok (≤1)" `Quick t_affine_underuse_ok;
    Alcotest.test_case "CTR: erased (0) var use rejected" `Quick t_zero_use_rejected;
    Alcotest.test_case "PER: cost accumulates" `Quick t_cost_positive;
    Alcotest.test_case "PER: budget exceeded → Infeasible" `Quick t_budget_infeasible;
    Alcotest.test_case "determinism (value + cost)" `Quick t_deterministic;
    (* M2 — deep effect handlers + multi-shot resume (#555) *)
    Alcotest.test_case "M2: handler return clause fires" `Quick t_handle_return;
    Alcotest.test_case "M2: single-shot resume" `Quick t_handle_single_shot;
    Alcotest.test_case "M2: MULTI-SHOT resume (resume k twice)" `Quick t_handle_multi_shot;
    Alcotest.test_case "M2: deep handler (perform inside resumption)" `Quick t_handle_deep;
    Alcotest.test_case "M2: unhandled effect → Unhandled_effect" `Quick t_perform_unhandled;
  ]
