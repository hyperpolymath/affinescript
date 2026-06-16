(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** ADR-022 M3 (2/3): constraint/fact extraction.

    Derives {!Borrow_polonius.Types.facts} from a real (resolved) program, so the
    solver ({!Borrow_polonius.Solve}) can be run on actual code. Reuses the lexical
    checker's machinery — {!Borrow.build_context} (param ownerships + the #554
    return-borrow summaries), {!Borrow.expr_to_place}/{!Borrow.places_overlap}, and
    {!Borrow.compute_last_use_index} — so the derived facts agree with the lexical
    verdict *by construction*, which is what M3's zero-divergence diff needs.

    SCOPE (bounded, honest). The CFG is still the linear chain over the
    function body's TOP-LEVEL statements (point [i] = statement [i]; the tail
    expression is point [n]) — but extraction now DESCENDS into nested block
    statements (the bodies of [if]/[match] arms and explicit [{ … }] blocks
    written in statement position), recording the loans they create and the
    moves they perform AT THE ENCLOSING TOP-LEVEL POINT. This is exactly the
    granularity {!Borrow.compute_last_use_index} uses — it attributes every
    mention, however deeply nested, to the index of the enclosing top-level
    statement — so a loan's kill point (binder last-use + 1) and a nested move's
    conflict point line up with the lexical checker by construction. The win:
    a use-after-move written as a STATEMENT inside a branch
    ([let r = pick(a); if c { consume(a); } … *r]) is now seen, where before the
    branch body's statements were invisible (apps/rhs_borrows stop at block
    tails). Loop BACK-EDGES are still NOT modelled — the lexical checker's
    2-iteration unrolling (Slice C', #177) of [while]/[for] is not mirrored, so a
    cross-iteration conflict is out of scope (a loop body is descended for the
    loans/moves visible WITHIN one iteration, which is sound but not the
    multi-iteration semantics); that is the next increment. Field/index
    sub-places and reborrow [subset] chains remain unmodelled too.

    PLAIN USE-AFTER-MOVE (no loan) is now modelled too, via the
    [move_at]/[use_at]/[reinit_at] facts and the solver's forward moved-state
    dataflow: a value moved into an [own] parameter is [move_at] (and a use), a
    whole-place write [x = e] is [reinit_at] (revives it), and every variable
    read is [use_at] — so [consume(a); consume(a)] is flagged where the loan
    rules see nothing. A use reached by a move with no intervening reinit is the
    error. CROSS-ITERATION use-after-move in loops is handled by unrolling: a
    [while]/[for] body's move/use/reinit facts are emitted a SECOND time at a
    fresh CFG point with edges [pt → p2 → pt+1], mirroring the lexical checker's
    2-iteration approximation (Slice C' / #177), so an iter-1 move reaches an
    iter-2 use. (Loan facts are not unrolled — a borrow conflict in a loop
    already manifests at iteration 1, so the loan rules agree without it.) The
    point granularity is still coarse — [compute_last_use_index] collapses a loop
    body to its enclosing top-level index, so loan kill points inside a loop are
    conservative — but verdicts match the lexical checker on the corpus.

    Borrow sources handled: [&p] / [&mut p], and a call whose return-borrow
    summary borrows an argument ([let r = pick(a)] — #554). Moves: arguments
    passed to [own] parameters. This module is NOT wired into [bin/main.ml]; the
    parallel-run diff against the lexical checker is increment 3/3. *)

open Ast
module PF = Borrow_polonius.Types

let rec peel = function ExprSpan (e, _) -> peel e | e -> e

(** Every application [(callee_name, args)] reachable through the straight-line
    expression forms. Stops at binders/lambdas/branches (out of scope). *)
let rec apps (e : expr) : (string * expr list) list =
  match peel e with
  | ExprApp (f, args) ->
    let here = match peel f with ExprVar id -> [ (id.name, args) ] | _ -> [] in
    here @ List.concat_map apps args
  | ExprUnary (_, x) -> apps x
  | ExprBinary (a, _, b) -> apps a @ apps b
  | ExprIndex (a, b) -> apps a @ apps b
  | ExprField (b, _) | ExprTupleIndex (b, _) -> apps b
  | ExprTuple xs | ExprArray xs -> List.concat_map apps xs
  (* expression-level branches: a move in any arm is a (conservative) conflict *)
  | ExprIf ei ->
    apps ei.ei_cond @ apps ei.ei_then
    @ (match ei.ei_else with Some e -> apps e | None -> [])
  | ExprMatch em ->
    apps em.em_scrutinee @ List.concat_map (fun a -> apps a.ma_body) em.em_arms
  | ExprBlock blk -> (match blk.blk_expr with Some e -> apps e | None -> [])
  | _ -> []

(** All borrows a let-RHS *value* may carry out, as [(place, exclusive?)] — the
    UNION over expression-level branches, mirroring the lexical checker's
    [value_escaping] (issue-draft 08). Base sources: [&p] / [&mut p], and a call
    whose return-borrow summary borrows an argument. An [if]/[match]/block RHS
    contributes the union of its arm/tail sources, so
    [let r = if c { pick(a) } else { pick(b) }] borrows both [a] and [b]. *)
let rec rhs_borrows (ctx : Borrow.context) (symbols : Symbol.t) (e : expr)
  : (Borrow.place * bool) list =
  let place_of inner excl =
    match Borrow.expr_to_place symbols inner with Some p -> [ (p, excl) ] | None -> [] in
  match peel e with
  | ExprUnary (OpRef, inner)    -> place_of inner false
  | ExprUnary (OpMutRef, inner) -> place_of inner true
  | ExprApp (f, args) ->
    (match peel f with
     | ExprVar id ->
       (match Hashtbl.find_opt ctx.Borrow.fn_sigs id.name with
        | Some sg ->
          List.filter_map (fun i ->
            match List.nth_opt args i with
            | Some arg ->
              (match Borrow.expr_to_place symbols arg with Some p -> Some (p, false) | None -> None)
            | None -> None)
            sg.Borrow.fn_ret_borrow_params
        | None -> [])
     | _ -> [])
  (* expression-level branches: union the arm/tail sources (sound: over-approx) *)
  | ExprIf ei ->
    rhs_borrows ctx symbols ei.ei_then
    @ (match ei.ei_else with Some e -> rhs_borrows ctx symbols e | None -> [])
  | ExprMatch em -> List.concat_map (fun a -> rhs_borrows ctx symbols a.ma_body) em.em_arms
  | ExprBlock blk -> (match blk.blk_expr with Some t -> rhs_borrows ctx symbols t | None -> [])
  | _ -> []

(** Places moved by [e]: arguments passed to [own] parameters of called funcs. *)
let moved_places (ctx : Borrow.context) (symbols : Symbol.t) (e : expr)
  : Borrow.place list =
  List.concat_map (fun (fname, args) ->
    match Hashtbl.find_opt ctx.Borrow.fn_sigs fname with
    | Some sg ->
      let owns = sg.Borrow.fn_param_ownerships in
      List.concat (List.mapi (fun i arg ->
        match List.nth_opt owns i with
        | Some (Some Own) ->
          (match Borrow.expr_to_place symbols arg with Some p -> [ p ] | None -> [])
        | _ -> []) args)
    | None -> []) (apps e)

type rloan = { rl_id : int; rl_place : Borrow.place }

(** Extract the Polonius facts for one (straight-line) function body. *)
let extract_fn (ctx : Borrow.context) (symbols : Symbol.t) (fd : fn_decl) : PF.facts =
  match fd.fd_body with
  | FnBlock blk ->
    let last_use = Borrow.compute_last_use_index symbols blk in
    let nstmts = List.length blk.blk_stmts in
    (* linear CFG spine: point i → i+1 for i in 0 .. nstmts-1 (tail is point
       nstmts). Loop unrolling (below) appends extra points/edges past [nstmts]. *)
    let base_cfg = List.init (max 0 nstmts) (fun i -> (i, i + 1)) in
    let next_loan = ref 0 and next_origin = ref 0 in
    let next_point = ref (nstmts + 1) and extra_edges = ref [] in
    let fresh r = let n = !r in incr r; n in
    let loans = ref [] in
    let borrow_at = ref [] and loan_origin = ref []
    and killed = ref [] and conflict_at = ref [] in
    (* a binder's loan dies at its last-use + 1 (NLL); [compute_last_use_index]
       keys by symbol regardless of nesting, so a nested [let] binder's kill
       point is found the same way as a top-level one. *)
    let kill_point_of (id : ident) (fallback : int) : int =
      match Borrow.lookup_symbol_by_name symbols id.name with
      | Some sym ->
        (match Hashtbl.find_opt last_use sym.Symbol.sym_id with
         | Some k -> k + 1 | None -> fallback)
      | None -> fallback
    in
    let record_let_loans (pt : int) (id : ident) (rhs : expr) : unit =
      (* a branchy RHS may borrow several places (union over arms); each becomes
         its own loan, all born at [pt] and killed at the binder's last use *)
      let kp = kill_point_of id (pt + 1) in
      List.iter (fun (place, _excl) ->
        let l = fresh next_loan in
        borrow_at   := (l, pt) :: !borrow_at;
        loan_origin := (l, fresh next_origin) :: !loan_origin;
        loans       := { rl_id = l; rl_place = place } :: !loans;
        killed      := (l, kp) :: !killed)
        (rhs_borrows ctx symbols rhs)
    in
    (* PASS 1 — loans. Record each let's loans at the enclosing top-level point,
       descending through nested branch/block STATEMENTS (their tail expressions
       are accounted by the enclosing let's [rhs_borrows], so we only recurse into
       statement position here — no double counting). *)
    let rec loans_stmt pt s =
      match s with
      | StmtLet sl ->
        (match sl.sl_pat with PatVar id -> record_let_loans pt id sl.sl_value | _ -> ());
        loans_expr pt sl.sl_value
      | StmtExpr e -> loans_expr pt e
      | StmtAssign (l, _, r) -> loans_expr pt l; loans_expr pt r
      | StmtWhile (c, b) -> loans_expr pt c; loans_block pt b
      | StmtFor (_, it, b) -> loans_expr pt it; loans_block pt b
    and loans_expr pt e =
      match peel e with
      | ExprIf ei ->
        loans_expr pt ei.ei_cond; loans_expr pt ei.ei_then;
        (match ei.ei_else with Some e -> loans_expr pt e | None -> ())
      | ExprMatch em ->
        loans_expr pt em.em_scrutinee;
        List.iter (fun a -> loans_expr pt a.ma_body) em.em_arms
      | ExprBlock blk -> loans_block pt blk
      | ExprApp (f, args) -> loans_expr pt f; List.iter (loans_expr pt) args
      | ExprUnary (_, x) -> loans_expr pt x
      | ExprBinary (a, _, b) | ExprIndex (a, b) -> loans_expr pt a; loans_expr pt b
      | ExprField (b, _) | ExprTupleIndex (b, _) -> loans_expr pt b
      | ExprTuple xs | ExprArray xs -> List.iter (loans_expr pt) xs
      | _ -> ()
    (* a nested block's STATEMENTS are the new surface; its tail value is carried
       OUT and already counted by the enclosing let's [rhs_borrows]. *)
    and loans_block pt blk = List.iter (loans_stmt pt) blk.blk_stmts in
    List.iteri loans_stmt blk.blk_stmts;
    (* PASS 2 — moves/uses/reinits for both the loan-conflict rule and the plain
       use-after-move rule, descending the same way as pass 1. A move of a place
       overlapping a loan's place conflicts there; a move is ALSO recorded as a
       [move_at] + [use_at] (a moved arg is a use of the value), and every read of
       a variable is a [use_at], so [consume(a); consume(a)] surfaces as a
       use-after-move even with no loan involved. *)
    let move_at = ref [] and use_at = ref [] and reinit_at = ref [] in
    (* root vars read directly in [e] — mirrors [apps]'s recursion (into
       call/operator/branch sub-expressions and block TAILS), stopping at block
       statements (the statement recursion visits those at their own point). *)
    let rec record_uses pt e =
      (match Borrow.expr_to_place symbols (peel e) with
       | Some p -> (match Borrow.root_var p with
                    | Some v -> use_at := (v, pt) :: !use_at | None -> ())
       | None -> ());
      match peel e with
      | ExprApp (f, args) -> record_uses pt f; List.iter (record_uses pt) args
      | ExprUnary (_, x) -> record_uses pt x
      | ExprBinary (a, _, b) | ExprIndex (a, b) -> record_uses pt a; record_uses pt b
      | ExprField (b, _) | ExprTupleIndex (b, _) -> record_uses pt b
      | ExprTuple xs | ExprArray xs -> List.iter (record_uses pt) xs
      | ExprIf ei ->
        record_uses pt ei.ei_cond; record_uses pt ei.ei_then;
        (match ei.ei_else with Some e -> record_uses pt e | None -> ())
      | ExprMatch em ->
        record_uses pt em.em_scrutinee;
        List.iter (fun a -> record_uses pt a.ma_body) em.em_arms
      | ExprBlock blk -> (match blk.blk_expr with Some t -> record_uses pt t | None -> ())
      | _ -> ()
    in
    let do_point pt e =
      record_uses pt e;
      List.iter (fun mp ->
        (match Borrow.root_var mp with
         | Some v -> move_at := (v, pt) :: !move_at; use_at := (v, pt) :: !use_at
         | None -> ());
        List.iter (fun rl ->
          if Borrow.places_overlap mp rl.rl_place then
            conflict_at := (rl.rl_id, pt) :: !conflict_at) !loans)
        (moved_places ctx symbols e)
    in
    (* a whole-place write [x = e] (LHS is a bare variable) revives [x]; a
       sub-place write [x.f = e] / [x[i] = e] instead READS the parent place. *)
    let record_assign_lhs pt lhs =
      match Borrow.expr_to_place symbols (peel lhs) with
      | Some (Borrow.PlaceVar _ as p) ->
        (match Borrow.root_var p with Some v -> reinit_at := (v, pt) :: !reinit_at | None -> ())
      | _ -> do_point pt lhs  (* sub-place write reads the parent (+ any nested moves) *)
    in
    let rec moves_stmt pt s =
      match s with
      | StmtLet sl -> do_point pt sl.sl_value; moves_expr pt sl.sl_value
      | StmtExpr e -> do_point pt e; moves_expr pt e
      | StmtAssign (l, _, r) ->
        record_assign_lhs pt l; do_point pt r; moves_expr pt l; moves_expr pt r
      (* loop unrolling for the use-after-move rule: a loop body runs ≥0 times,
         so an iter-1 move can reach an iter-2 use. We mirror the lexical
         checker's 2-iteration approximation (Slice C' / #177) by emitting the
         body's move/use/reinit facts a SECOND time at a fresh CFG point, with
         edges [pt → p2] (continue to iter 2) and [p2 → pt+1] (exit after iter 2);
         the spine edge [pt → pt+1] already serves as exit-after-iter-1. Loan
         facts are NOT unrolled — those already agree with the lexical checker at
         iteration 1 (a borrow conflict in a loop manifests on the first pass). *)
      | StmtWhile (c, b) ->
        do_point pt c; moves_expr pt c; moves_block pt b;
        let p2 = fresh next_point in
        extra_edges := (pt, p2) :: (p2, pt + 1) :: !extra_edges;
        do_point p2 c; moves_expr p2 c; moves_block p2 b
      | StmtFor (_, it, b) ->
        do_point pt it; moves_expr pt it; moves_block pt b;
        let p2 = fresh next_point in
        extra_edges := (pt, p2) :: (p2, pt + 1) :: !extra_edges;
        do_point p2 it; moves_expr p2 it; moves_block p2 b
    and moves_expr pt e =
      match peel e with
      | ExprIf ei ->
        moves_expr pt ei.ei_cond; moves_expr pt ei.ei_then;
        (match ei.ei_else with Some e -> moves_expr pt e | None -> ())
      | ExprMatch em ->
        moves_expr pt em.em_scrutinee;
        List.iter (fun a -> moves_expr pt a.ma_body) em.em_arms
      | ExprBlock blk -> moves_block pt blk
      | ExprApp (f, args) -> moves_expr pt f; List.iter (moves_expr pt) args
      | ExprUnary (_, x) -> moves_expr pt x
      | ExprBinary (a, _, b) | ExprIndex (a, b) -> moves_expr pt a; moves_expr pt b
      | ExprField (b, _) | ExprTupleIndex (b, _) -> moves_expr pt b
      | ExprTuple xs | ExprArray xs -> List.iter (moves_expr pt) xs
      | _ -> ()
    and moves_block pt blk =
      List.iter (moves_stmt pt) blk.blk_stmts;
      (match blk.blk_expr with Some e -> do_point pt e; moves_expr pt e | None -> ())
    in
    List.iteri moves_stmt blk.blk_stmts;
    (match blk.blk_expr with Some e -> do_point nstmts e; moves_expr nstmts e | None -> ());
    { PF.empty_facts with
      borrow_at = !borrow_at; loan_origin = !loan_origin;
      killed = !killed; cfg_edge = base_cfg @ !extra_edges; conflict_at = !conflict_at;
      move_at = !move_at; use_at = !use_at; reinit_at = !reinit_at }
  | _ -> PF.empty_facts

let function_decls (program : program) : fn_decl list =
  List.filter_map (function TopFn fd -> Some fd | _ -> None) program.prog_decls

(** Solver-side verdict for a whole program: does the Polonius solver report a
    borrow error in any (straight-line) function? Each function is extracted +
    solved independently (its points are local). Used by the M3 parallel-run diff
    against [Borrow.check_program]. *)
let program_has_borrow_error (ctx : Borrow.context) (symbols : Symbol.t)
    (program : program) : bool =
  List.exists (fun fd ->
    let d = Borrow_polonius.Solve.solve (extract_fn ctx symbols fd) in
    d.PF.errors <> []) (function_decls program)
