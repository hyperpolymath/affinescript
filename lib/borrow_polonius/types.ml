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
type var    = int [@@deriving show, eq]  (** a movable variable (mirrors [Borrow.root_var]'s symbol id) *)

(** Input facts (ADR-022 §"Algorithm sketch"). *)
type facts = {
  borrow_at   : (loan * point) list;          (** loan [L] is created at point [P] *)
  loan_origin : (loan * origin) list;         (** loan [L] flows into origin [O] *)
  subset      : (origin * origin * point) list; (** [O1 ⊆ O2] holds at point [P] *)
  killed      : (loan * point) list;          (** loan [L] is killed at [P] (base moved / scope end) *)
  cfg_edge    : (point * point) list;         (** control-flow edge [P1 → P2] *)
  conflict_at : (loan * point) list;          (** an access at [P] conflicts with loan [L]'s kind
                                                  under shared-XOR-exclusive — the [check_use] rule,
                                                  hoisted out of the imperative checker (ADR-022 rule 2).
                                                  Populated by M3 extraction. *)
  (* ── plain use-after-move of an owned value (the lexical [state.moved] /
        [check_use] rule, distinct from loan conflicts). A variable [V] moved at
        [move_at(V,P)] is in moved-state on every CFG path forward until a whole-
        place [reinit_at(V,P)] revives it; a [use_at(V,P)] reaching a moved-state
        point is a use-after-move error. This is what catches [consume(a);
        consume(a)] and the loop equivalent — cases with no loan involved that the
        loan-conflict rules above are blind to. *)
  move_at     : (var * point) list;           (** owned value [V] is moved out at [P] *)
  use_at      : (var * point) list;           (** [V] is read at [P] (a move arg is also a use) *)
  reinit_at   : (var * point) list;           (** whole-place write to [V] at [P] revives it *)
}
[@@deriving show, eq]

let empty_facts : facts =
  { borrow_at = []; loan_origin = []; subset = []; killed = []; cfg_edge = [];
    conflict_at = []; move_at = []; use_at = []; reinit_at = [] }

(** Derived facts + the verdict (ADR-022 rules 1–3 + the use-after-move rule). *)
type derived = {
  loan_live_at        : (loan * point) list;   (** least fixed point over [cfg_edge] minus [killed] *)
  loan_invalidated_at : (loan * point) list;   (** an access at [P] conflicts with a live loan *)
  moved_in            : (var * point) list;    (** [V] is in moved-state on entry to [P] (fwd dataflow) *)
  errors              : point list;            (** points carrying an invalidation OR a use-after-move *)
}
[@@deriving show, eq]
