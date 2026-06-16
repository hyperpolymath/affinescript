(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Polonius-style naive-datalog loan solver — ADR-022.

    M1 STUB: the bottom-up worklist fixpoint (rules 1–3 of ADR-022) is
    implemented in M3. This placeholder derives nothing, so when M3 first wires
    it into [bin/main.ml] for the parallel-run diff it is a behaviour-preserving
    no-op (it reports zero errors) until the rules are filled in. Keeping the
    signature stable now means M3 is purely additive. *)

(** Run the solver over the input [facts], returning the derived relations.
    M1: identity-empty. M3: naive worklist to least fixed point. *)
let solve (_facts : Types.facts) : Types.derived =
  { Types.loan_live_at = []; loan_invalidated_at = []; errors = [] }
