(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** ADR-022 M3 (2/3): constraint/fact extraction.

    Derives {!Borrow_polonius.Types.facts} from a real (resolved) program, so the
    solver ({!Borrow_polonius.Solve}) can be run on actual code. Reuses the lexical
    checker's machinery — {!Borrow.build_context} (param ownerships + the #554
    return-borrow summaries), {!Borrow.expr_to_place}/{!Borrow.places_overlap}, and
    {!Borrow.compute_last_use_index} — so the derived facts agree with the lexical
    verdict *by construction*, which is what M3's zero-divergence diff needs.

    SCOPE (bounded, honest): straight-line function bodies — a sequence of
    [StmtLet]/[StmtExpr]/[StmtAssign] with a linear CFG (point [i] = statement
    [i]; the tail expression is point [n]). Branch/loop CFG ([if]/[match]/[while]),
    field/index sub-places, and reborrow [subset] chains are NOT modelled yet
    (later M3 increments). Borrow sources handled: [&p] / [&mut p], and a call
    whose return-borrow summary borrows an argument ([let r = pick(a)] — #554).
    Moves: arguments passed to [own] parameters. This module is NOT wired into
    [bin/main.ml]; the parallel-run diff against the lexical checker is increment
    3/3. *)

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
  | _ -> []

(** The borrow a let-RHS produces, if any: [(borrowed place, exclusive?)]. *)
let rhs_borrow (ctx : Borrow.context) (symbols : Symbol.t) (e : expr)
  : (Borrow.place * bool) option =
  let place_of inner excl =
    match Borrow.expr_to_place symbols inner with
    | Some p -> Some (p, excl) | None -> None
  in
  match peel e with
  | ExprUnary (OpRef, inner)    -> place_of inner false
  | ExprUnary (OpMutRef, inner) -> place_of inner true
  | ExprApp (f, args) ->
    (match peel f with
     | ExprVar id ->
       (match Hashtbl.find_opt ctx.Borrow.fn_sigs id.name with
        | Some sg ->
          (* take the first returned-borrow parameter (bounded: one origin) *)
          (match sg.Borrow.fn_ret_borrow_params with
           | i :: _ ->
             (match List.nth_opt args i with
              | Some arg -> place_of arg false
              | None -> None)
           | [] -> None)
        | None -> None)
     | _ -> None)
  | _ -> None

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
    (* linear CFG: point i → i+1 for i in 0 .. nstmts-1 (tail is point nstmts) *)
    let cfg_edge = List.init (max 0 nstmts) (fun i -> (i, i + 1)) in
    let next_loan = ref 0 and next_origin = ref 0 in
    let fresh r = let n = !r in incr r; n in
    let loans = ref [] in
    let borrow_at = ref [] and loan_origin = ref []
    and killed = ref [] and conflict_at = ref [] in
    (* pass 1 — record each loan at its creation point + its kill at last-use+1 *)
    List.iteri (fun i s ->
      match s with
      | StmtLet sl ->
        (match sl.sl_pat, rhs_borrow ctx symbols sl.sl_value with
         | PatVar id, Some (place, _excl) ->
           let l = fresh next_loan in
           borrow_at   := (l, i) :: !borrow_at;
           loan_origin := (l, fresh next_origin) :: !loan_origin;
           loans       := { rl_id = l; rl_place = place } :: !loans;
           (match Borrow.lookup_symbol_by_name symbols id.name with
            | Some sym ->
              let lu = match Hashtbl.find_opt last_use sym.Symbol.sym_id with
                | Some k -> k | None -> i in
              killed := (l, lu + 1) :: !killed
            | None ->
              (* binder unresolved → kill immediately after creation *)
              killed := (l, i + 1) :: !killed)
         | _ -> ())
      | _ -> ()) blk.blk_stmts;
    (* pass 2 — a move of a place overlapping a loan's place conflicts there *)
    let do_point i e =
      List.iter (fun mp ->
        List.iter (fun rl ->
          if Borrow.places_overlap mp rl.rl_place then
            conflict_at := (rl.rl_id, i) :: !conflict_at) !loans)
        (moved_places ctx symbols e)
    in
    List.iteri (fun i s ->
      match s with
      | StmtLet sl -> do_point i sl.sl_value
      | StmtExpr e -> do_point i e
      | StmtAssign (l, _, r) -> do_point i l; do_point i r
      | _ -> ()) blk.blk_stmts;
    (match blk.blk_expr with Some e -> do_point nstmts e | None -> ());
    { PF.empty_facts with
      borrow_at = !borrow_at; loan_origin = !loan_origin;
      killed = !killed; cfg_edge; conflict_at = !conflict_at }
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
