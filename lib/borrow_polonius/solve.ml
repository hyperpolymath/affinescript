(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Polonius-style naive-datalog loan solver — ADR-022, milestone M3 (solver).

    Implements the three rules of ADR-022 §"Algorithm sketch" as a bottom-up
    least-fixed-point worklist over the input {!Types.facts}:

    {ol
    {- [loan_live_at(L, Q)] — [L] is live at [Q] iff there is a CFG path from its
       creation point [borrow_at(L, P)] to [Q] along which [L] is never [killed].
       (Loan-centric liveness, the [dlv_naive] shape — origin-liveness propagation
       is the extraction-time refinement, see {!subset_closure}.)}
    {- [loan_invalidated_at(L, Q)] — a live loan whose protected place is accessed
       conflictingly at [Q] ([conflict_at]; the hoisted [check_use] rule).}
    {- [error(Q)] — any point carrying an invalidation.}}

    Termination: liveness is monotone over the finite [point × loan] space and the
    worklist only ever adds facts, so the fixpoint is reached in ≤ |points|·|loans|
    steps. Complexity is the ADR's stated [O((P×L)+(E×L))] — fine for the corpus
    (median ≪ 100 loans × ≪ 1k points); [opt_naive] (difference relations) is a
    later follow-up only if a pathological program appears. *)

open Types

(* small-set helpers over assoc-style fact lists (corpus is tiny; lists suffice) *)
let mem_pair (x : 'a) (y : 'b) (l : ('a * 'b) list) : bool =
  List.exists (fun (a, b) -> a = x && b = y) l

let dedup (l : 'a list) : 'a list =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l
  |> List.rev

(** Transitive closure of [subset]: [(O, O')] iff a loan in origin [O] also flows
    into [O'] (i.e. [O ⊆ O'] holds, chained over all points). Exposed for the M3
    extractor, which uses it to chain reborrows — [let r2 = r1] makes [r2]'s origin
    a superset of [r1]'s, so a loan in [r1] is carried by [r2]. The base solver
    rules below are loan-centric and do not consume this yet (origin-liveness
    propagation is the M4 refinement); it is provided here so extraction and
    diagnostics share one notion of origin reachability. *)
let subset_closure (f : facts) : (origin * origin) list =
  let edges = List.map (fun (a, b, _p) -> (a, b)) f.subset in
  (* reflexive-transitive closure by naive iteration to a fixpoint *)
  let origins =
    dedup (List.concat_map (fun (a, b) -> [a; b]) edges) in
  let init = List.map (fun o -> (o, o)) origins @ edges in
  let rec close acc =
    let grown =
      acc @ List.concat_map (fun (a, b) ->
        List.filter_map (fun (c, d) -> if b = c then Some (a, d) else None) acc) acc
      |> dedup
    in
    if List.length grown = List.length acc then acc else close grown
  in
  close (dedup init)

(** Least-fixpoint loan liveness (rule 1). Worklist seeded with creation points;
    a loan propagates to a CFG successor unless it is [killed] there. *)
let compute_live (f : facts) : (loan * point) list =
  let killed_at l q = mem_pair l q f.killed in
  let live = ref [] in
  let work = ref [] in
  let add l p =
    if not (mem_pair l p !live) then begin
      live := (l, p) :: !live;
      work := (l, p) :: !work
    end
  in
  (* seed: live at creation point (a loan killed at its own creation point is
     dead immediately — degenerate, but handled uniformly) *)
  List.iter (fun (l, p) -> if not (killed_at l p) then add l p) f.borrow_at;
  let rec loop () =
    match !work with
    | [] -> ()
    | (l, p) :: rest ->
      work := rest;
      List.iter (fun (p1, q) ->
        if p1 = p && not (killed_at l q) then add l q) f.cfg_edge;
      loop ()
  in
  loop ();
  dedup !live

(** Forward least-fixpoint moved-state dataflow (the use-after-move rule).

    [moved_out(V,P)] = [move_at(V,P)] ∨ ([moved_in(V,P)] ∧ ¬[reinit_at(V,P)]);
    [moved_in(V,P)]  = ∃Q. [cfg_edge(Q,P)] ∧ [moved_out(V,Q)].

    A move makes [V] moved on exit from its point; the state then flows forward
    along the CFG, surviving every point that does not re-initialise [V]. Returns
    [moved_in] (moved-state on ENTRY), which is what a [use_at] is checked against
    — so the move site itself (where [V] is not yet moved on entry) is not flagged,
    but a later use, or a second move (which is also a use), is. Monotone over the
    finite [var × point] space ⇒ terminates; same shape/cost as {!compute_live}. *)
let compute_moved_in (f : facts) : (var * point) list =
  let reinit_at v p = mem_pair v p f.reinit_at in
  let moved_in = ref [] and moved_out = ref [] in
  let work = ref [] in
  let add_out v p =
    if not (mem_pair v p !moved_out) then begin
      moved_out := (v, p) :: !moved_out;
      work := (v, p) :: !work
    end
  in
  let add_in v p =
    if not (mem_pair v p !moved_in) then begin
      moved_in := (v, p) :: !moved_in;
      (* moved-state survives into [P]'s exit unless [P] re-initialises [V] *)
      if not (reinit_at v p) then add_out v p
    end
  in
  (* seed: a move makes [V] moved-out at the move point — UNLESS the same point
     also re-initialises [V]. At top-level-statement point granularity a loop body
     like [drop_int(x); x = 42] collapses the move and the reviving rewrite onto
     one point; letting reinit dominate the exit-state is conservative toward
     ACCEPTANCE (it can only suppress an error, never invent one), which keeps the
     false-positive direction — rejecting valid code — off the table. *)
  List.iter (fun (v, p) -> if not (reinit_at v p) then add_out v p) f.move_at;
  let rec loop () =
    match !work with
    | [] -> ()
    | (v, p) :: rest ->
      work := rest;
      List.iter (fun (p1, q) -> if p1 = p then add_in v q) f.cfg_edge;
      loop ()
  in
  loop ();
  dedup !moved_in

(** Run the solver over the input [facts]. *)
let solve (f : facts) : derived =
  let loan_live_at = compute_live f in
  (* rule 2: a live loan, conflictingly accessed at a point, is invalidated there *)
  let loan_invalidated_at =
    List.filter (fun (l, q) -> mem_pair l q f.conflict_at) loan_live_at |> dedup
  in
  (* use-after-move: a use of [V] at a point where [V] is already in moved-state *)
  let moved_in = compute_moved_in f in
  let uam_errors =
    List.filter_map (fun (v, p) ->
      (* a use of [V] where [V] is moved on entry AND not revived at this very
         point is the error; a same-point reinit is assumed (conservatively) to
         precede the use, so it is not flagged. *)
      if mem_pair v p moved_in && not (mem_pair v p f.reinit_at)
      then Some p else None)
      f.use_at
  in
  (* rule 3 + UAM: the points carrying any invalidation or use-after-move *)
  let errors = dedup (List.map snd loan_invalidated_at @ uam_errors) in
  { loan_live_at; loan_invalidated_at; moved_in; errors }
