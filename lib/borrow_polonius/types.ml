(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Polonius-style loan-solver relation types — ADR-022, milestone M1.

    Defined but NOT wired (the [affinescript] library does not depend on this
    one yet). These are the published Polonius "naive datalog" relations
    (Matsakis 2018): input fact tables derived from typing + CFG construction,
    and the derived facts the {!Solve} worklist will compute from M3 onward.

    Identities are plain ints (program points, loans, origins) — the same
    fresh-int identity as {!Ast.origin_var}; a real solver would intern them. *)

type point  = int [@@deriving show, eq]  (** a program point (CFG node) *)
type loan   = int [@@deriving show, eq]  (** a loan / borrow identity (mirrors [Borrow.borrow.b_id]) *)
type origin = int [@@deriving show, eq]  (** an origin / region variable (mirrors [Ast.origin_var]) *)

(** Input facts (ADR-022 §"Algorithm sketch"). *)
type facts = {
  borrow_at   : (loan * point) list;          (** loan [L] is created at point [P] *)
  loan_origin : (loan * origin) list;         (** loan [L] flows into origin [O] *)
  subset      : (origin * origin * point) list; (** [O1 ⊆ O2] holds at point [P] *)
  killed      : (loan * point) list;          (** loan [L] is killed at [P] (base moved / scope end) *)
  cfg_edge    : (point * point) list;         (** control-flow edge [P1 → P2] *)
}
[@@deriving show, eq]

let empty_facts : facts =
  { borrow_at = []; loan_origin = []; subset = []; killed = []; cfg_edge = [] }

(** Derived facts + the verdict (ADR-022 rules 1–3). *)
type derived = {
  loan_live_at        : (loan * point) list;   (** least fixed point over [cfg_edge] minus [killed] *)
  loan_invalidated_at : (loan * point) list;   (** an access at [P] conflicts with a live loan *)
  errors              : point list;            (** points carrying an invalidated-at error *)
}
[@@deriving show, eq]
