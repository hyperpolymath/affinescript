(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Shared call-site numbering for the effect-threaded async-boundary
    side-table (ADR-016, issue #234).

    The AST carries no location or node id on [ExprApp], and ADR-016
    rejects annotating it. Instead this module defines a single,
    deterministic pre-order traversal that assigns every [ExprApp] a
    0-based ordinal. Both the producer ([Typecheck], which records
    [ordinal -> effect_row]) and the consumer (the WasmGC CPS
    boundary detector) obtain ordinals by calling *this same function*,
    so the keys cannot drift — without changing the AST shape.

    Ordering contract (stable; do not change without amending ADR-016):
    a strict left-to-right *pre-order* walk of the program. An
    [ExprApp] node is numbered *before* its callee and argument
    sub-expressions are descended into. Sub-structures are visited in
    source order. [TopFn]/[TopConst]/[TopImpl]/[TopTrait] default
    bodies are walked in [prog_decls] order.

    This module is pure and depends only on [Ast]; it has no notion of
    effects itself (S2a). S2b/S3 build the table on top of it. *)

open Ast

(* The visitor threads an accumulator and a mutable next-ordinal
   counter (held in a ref captured by the closures, so the ordinal is a
   pure function of traversal position). *)

let fold_calls (type a) (f : a -> int -> expr -> a) (init : a)
    (prog : program) : a =
  let acc = ref init in
  let next = ref 0 in
  let rec go_expr (e : expr) : unit =
    (match e with
     | ExprApp (fn, args) ->
       (* Number THIS call site before descending (pre-order). *)
       let ord = !next in
       incr next;
       acc := f !acc ord e;
       go_expr fn;
       List.iter go_expr args
     | ExprLit _ | ExprVar _ | ExprVariant _ -> ()
     | ExprLet l ->
       go_expr l.el_value;
       (match l.el_body with Some b -> go_expr b | None -> ())
     | ExprIf i ->
       go_expr i.ei_cond;
       go_expr i.ei_then;
       (match i.ei_else with Some e -> go_expr e | None -> ())
     | ExprMatch m ->
       go_expr m.em_scrutinee;
       List.iter go_arm m.em_arms
     | ExprLambda l -> go_expr l.elam_body
     | ExprField (e, _) | ExprTupleIndex (e, _) | ExprRowRestrict (e, _)
     | ExprSpan (e, _) | ExprUnary (_, e) ->
       go_expr e
     | ExprIndex (a, b) | ExprBinary (a, _, b) ->
       go_expr a;
       go_expr b
     | ExprTuple es | ExprArray es -> List.iter go_expr es
     | ExprRecord r ->
       List.iter
         (fun (_, eo) -> match eo with Some e -> go_expr e | None -> ())
         r.er_fields;
       (match r.er_spread with Some e -> go_expr e | None -> ())
     | ExprBlock b -> go_block b
     | ExprReturn eo | ExprResume eo ->
       (match eo with Some e -> go_expr e | None -> ())
     | ExprTry t ->
       go_block t.et_body;
       (match t.et_catch with Some arms -> List.iter go_arm arms | None -> ());
       (match t.et_finally with Some b -> go_block b | None -> ())
     | ExprHandle h ->
       go_expr h.eh_body;
       List.iter go_handler h.eh_handlers
     | ExprUnsafe ops -> List.iter go_unsafe ops)
  and go_arm (a : match_arm) : unit =
    (match a.ma_guard with Some g -> go_expr g | None -> ());
    go_expr a.ma_body
  and go_handler = function
    | HandlerReturn (_, e) -> go_expr e
    | HandlerOp (_, _, e) -> go_expr e
  and go_unsafe = function
    | UnsafeRead e | UnsafeForget e -> go_expr e
    | UnsafeWrite (a, b) | UnsafeOffset (a, b) ->
      go_expr a;
      go_expr b
    | UnsafeTransmute (_, _, e) -> go_expr e
  and go_block (b : block) : unit =
    List.iter go_stmt b.blk_stmts;
    (match b.blk_expr with Some e -> go_expr e | None -> ())
  and go_stmt = function
    | StmtLet l -> go_expr l.sl_value
    | StmtExpr e -> go_expr e
    | StmtAssign (a, _, b) ->
      go_expr a;
      go_expr b
    | StmtWhile (c, b) ->
      go_expr c;
      go_block b
    | StmtFor (_, e, b) ->
      go_expr e;
      go_block b
  in
  let go_fn_body = function
    | FnBlock b -> go_block b
    | FnExpr e -> go_expr e
    | FnExtern -> ()
  in
  let go_top = function
    | TopFn fd -> go_fn_body fd.fd_body
    | TopConst c -> go_expr c.tc_value
    | TopImpl ib ->
      List.iter
        (function ImplFn fd -> go_fn_body fd.fd_body | ImplType _ -> ())
        ib.ib_items
    | TopTrait trd ->
      List.iter
        (function
          | TraitFnDefault fd -> go_fn_body fd.fd_body
          | TraitFn _ | TraitType _ -> ())
        trd.trd_items
    | TopType _ | TopEffect _ | TopExternType _ | TopExternFn _ -> ()
  in
  List.iter go_top prog.prog_decls;
  !acc

(** Total number of call sites ([ExprApp] nodes) in [prog]. *)
let count (prog : program) : int =
  fold_calls (fun n _ _ -> n + 1) 0 prog

(** All call sites as [(ordinal, node)] in traversal (= ordinal) order. *)
let to_list (prog : program) : (int * expr) list =
  List.rev (fold_calls (fun acc ord e -> (ord, e) :: acc) [] prog)

(** [iter f prog] runs [f ordinal node] for every call site, in order. *)
let iter (f : int -> expr -> unit) (prog : program) : unit =
  fold_calls (fun () ord e -> f ord e) () prog
