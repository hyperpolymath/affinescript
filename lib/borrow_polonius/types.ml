(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath (Jonathan D.A. Jewell)
   <j.d.a.jewell@open.ac.uk> *)

(** Polonius-style loan solver — type definitions only (M1 stub).

    See ADR-022 / docs/decisions/0022-polonius-origin-variables.adoc for
    the design. This module deliberately holds only the relation-fact
    types; the worklist fixpoint solver lands in M3. Nothing in
    [bin/main.ml] references this library yet. *)

(** Program point — an opaque CFG point. M2 wires concrete points from
    the typechecker's CFG construction. *)
type point = int

(** Loan identifier. Each [record_borrow] call in the operational
    checker corresponds to one loan in the Polonius view. *)
type loan = int

(** Origin (region) variable. Alias of [Ast.origin_var] for clarity in
    Polonius-internal code. *)
type origin = int

(** Loan kind mirrors [Borrow.borrow_kind]; duplicated here so the
    polonius library is not coupled to the rest of [affinescript] at M1.
    M4 (cut-over) collapses the duplication. *)
type loan_kind =
  | Shared
  | Exclusive

(** A subset constraint [O1 ⊆ O2 at P]: any loan reaching [O1] also
    reaches [O2] when control flow passes through point [P]. Emitted at
    let-bindings, assignments to ref binders, and call sites in M3. *)
type subset_constraint = {
  sc_sub  : origin;   (** [O1] — the subset *)
  sc_sup  : origin;   (** [O2] — the superset *)
  sc_at   : point;    (** [P] — the program point where the constraint holds *)
}

(** Datalog input fact base. M2 populates this from the elaborator's
    side-table; M3 hands it to the solver. *)
type fact_base = {
  borrows      : (loan * point) list;
    (** [borrow_at(L, P)] — loan [L] created at point [P]. *)
  loan_origins : (loan * origin) list;
    (** [loan_origin(L, O)] — loan [L] flows into origin [O]. *)
  subsets      : subset_constraint list;
    (** [subset(O1, O2, P)] — see [subset_constraint]. *)
  killed       : (loan * point) list;
    (** [killed(L, P)] — loan [L] killed at point [P] (move of base,
        scope end, or re-assignment of the binder). *)
  cfg_edges    : (point * point) list;
    (** [cfg_edge(P1, P2)] — control-flow edge from [P1] to [P2]. *)
  loan_kinds   : (loan * loan_kind) list;
    (** Per-loan kind, lifted from the operational checker so the
        invalidation rule (shared-XOR-exclusive) can be evaluated. *)
}

(** Derived facts produced by the solver in M3. *)
type derived = {
  live_at        : (loan * point) list;
    (** [loan_live_at(L, P)] — least fixed point. *)
  invalidated_at : (loan * point) list;
    (** [loan_invalidated_at(L, P)] — live + access-conflict. *)
  errors         : point list;
    (** [error(P)] — any invalidated-at error at [P]. *)
}

(** Empty fact base for testing. *)
let empty_facts : fact_base = {
  borrows      = [];
  loan_origins = [];
  subsets      = [];
  killed       = [];
  cfg_edges    = [];
  loan_kinds   = [];
}
