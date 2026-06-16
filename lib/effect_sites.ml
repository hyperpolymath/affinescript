(* SPDX-License-Identifier: MPL-2.0 *)
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

    The traversal core ([visit_*]/[fold_calls]/[exists_call]) is pure
    and depends only on [Ast]. The S3 ordinal-keyed async oracle
    (bottom of this file) is the producer/consumer bridge built on that
    numbering (ADR-016 S2b/S3/S4). *)

open Ast

(* The visitor threads an accumulator and a mutable next-ordinal
   counter (held in a ref captured by the closures, so the ordinal is a
   pure function of traversal position). *)

(* Single source of the traversal: [visit] is called on every
   [ExprApp] node in strict left-to-right pre-order (the call node
   *before* its callee/args are descended). [fold_calls] (program) and
   [exists_call] (expr) both use it, so the numbering and any
   sub-expression call scan can never diverge. *)
let rec visit_expr (visit : expr -> unit) (e : expr) : unit =
  let go_expr = visit_expr visit in
  let go_arm (a : match_arm) : unit =
    (match a.ma_guard with Some g -> go_expr g | None -> ());
    go_expr a.ma_body
  in
  let go_handler = function
    | HandlerReturn (_, e) -> go_expr e
    | HandlerOp (_, _, e) -> go_expr e
  in
  let go_unsafe = function
    | UnsafeRead e | UnsafeForget e -> go_expr e
    | UnsafeWrite (a, b) | UnsafeOffset (a, b) ->
      go_expr a;
      go_expr b
    | UnsafeTransmute (_, _, e) -> go_expr e
  in
  match e with
  | ExprApp (fn, args) ->
    (* Visit THIS call site before descending (pre-order). *)
    visit e;
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
  | ExprSpan (e, _) | ExprUnary (_, e) | ExprFloatTupleIndex (e, _) ->
    go_expr e
  | ExprIndex (a, b) | ExprBinary (a, _, b) | ExprStringConcat (a, b)
  | ExprStringEq (a, b, _) | ExprStringRel (a, b, _) | ExprFloatBinary (a, _, b)
  | ExprFloatIndex (a, b) ->
    (* ExprStringConcat (slice 8b) recurses like ExprBinary and is NOT a call
       site: string `++` is not an effect operation, so keeping it out of the
       ExprApp census preserves effect-ordinal parity between the interpreter
       (which sees the original ExprBinary) and the wasm backend. *)
    go_expr a;
    go_expr b
  | ExprTuple es | ExprArray es | ExprFloatArray es | ExprFloatTuple es ->
    List.iter go_expr es
  | ExprRecord r ->
    List.iter
      (fun (_, eo) -> match eo with Some e -> go_expr e | None -> ())
      r.er_fields;
    (match r.er_spread with Some e -> go_expr e | None -> ())
  | ExprBlock b -> visit_block visit b
  | ExprReturn eo | ExprResume eo ->
    (match eo with Some e -> go_expr e | None -> ())
  | ExprBreak _ | ExprContinue _ -> ()
  | ExprTry t ->
    visit_block visit t.et_body;
    (match t.et_catch with Some arms -> List.iter go_arm arms | None -> ());
    (match t.et_finally with Some b -> visit_block visit b | None -> ())
  | ExprHandle h ->
    go_expr h.eh_body;
    List.iter go_handler h.eh_handlers
  | ExprUnsafe ops -> List.iter go_unsafe ops

and visit_block (visit : expr -> unit) (b : block) : unit =
  let go_stmt = function
    | StmtLet l -> visit_expr visit l.sl_value
    | StmtExpr e -> visit_expr visit e
    | StmtAssign (a, _, b) ->
      visit_expr visit a;
      visit_expr visit b
    | StmtWhile (c, b) ->
      visit_expr visit c;
      visit_block visit b
    | StmtFor (_, e, b) ->
      visit_expr visit e;
      visit_block visit b
  in
  List.iter go_stmt b.blk_stmts;
  (match b.blk_expr with Some e -> visit_expr visit e | None -> ())

let visit_program (visit : expr -> unit) (prog : program) : unit =
  let go_fn_body = function
    | FnBlock b -> visit_block visit b
    | FnExpr e -> visit_expr visit e
    | FnExtern -> ()
  in
  let go_top = function
    | TopFn fd -> go_fn_body fd.fd_body
    | TopConst c -> visit_expr visit c.tc_value
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
  List.iter go_top prog.prog_decls

let fold_calls (type a) (f : a -> int -> expr -> a) (init : a)
    (prog : program) : a =
  let acc = ref init in
  let next = ref 0 in
  visit_program
    (fun call ->
      let ord = !next in
      incr next;
      acc := f !acc ord call)
    prog;
  !acc

(** [exists_call pred e] — is there a call site within [e] (any depth)
    for which [pred] holds? Uses the SAME pre-order traversal as
    [fold_calls], so a sub-expression scan can never miss a call shape
    the numbering counts. *)
let exists_call (pred : expr -> bool) (e : expr) : bool =
  let found = ref false in
  visit_expr (fun call -> if pred call then found := true) e;
  !found

(** Total number of call sites ([ExprApp] nodes) in [prog]. *)
let count (prog : program) : int =
  fold_calls (fun n _ _ -> n + 1) 0 prog

(** All call sites as [(ordinal, node)] in traversal (= ordinal) order. *)
let to_list (prog : program) : (int * expr) list =
  List.rev (fold_calls (fun acc ord e -> (ord, e) :: acc) [] prog)

(** [iter f prog] runs [f ordinal node] for every call site, in order. *)
let iter (f : int -> expr -> unit) (prog : program) : unit =
  fold_calls (fun () ord e -> f ord e) () prog

(* ── ADR-016 / #234 S3: ordinal-keyed async oracle ─────────────────────

   The producer ([Typecheck.populate_call_effects]) records, per call-site
   ORDINAL, whether the callee's effect row ⊇ [Async]. The consumer
   ([Codegen]) runs on the *post-optimizer* AST. The only optimizer is
   constant folding ([Opt.fold_constants_program]) which folds literal
   bin/unops to literals but never adds, removes, or reorders a function
   call ([ExprApp]) — so the pre-order ordinal of every call is STABLE
   across optimization. Hence the ORDINAL (not physical identity) bridges
   the producer's pre-opt AST and the consumer's post-opt AST, exactly as
   ADR-016 specifies.

   Lifecycle (per compilation): the producer fills [async_by_ord];
   [bind_consumer prog] renumbers the (possibly post-opt) [prog] with the
   SAME traversal and materialises a physical-identity predicate over
   *that* prog's own nodes. Default empty ⇒ [is_async_call] is always
   false ⇒ codegen emits no CPS transform (the sound table-miss path
   under S4 #278; zero regression if the producer never ran or the
   counts disagree). *)

let async_by_ord : (int, bool) Hashtbl.t = Hashtbl.create 64
let consumer_async : (expr * bool) list ref = ref []

(** Producer entry: replace the ordinal→has-async map. *)
let set_async_by_ord (tbl : (int, bool) Hashtbl.t) : unit =
  Hashtbl.reset async_by_ord;
  Hashtbl.iter (fun k v -> Hashtbl.replace async_by_ord k v) tbl

(** Reset both sides (defensive; long-lived processes / LSP). *)
let clear_async () =
  Hashtbl.reset async_by_ord;
  consumer_async := []

(** Consumer entry: bind the predicate to [prog]'s own nodes. If the
    call count disagrees with the producer's map size, the ordinal
    bridge is not trustworthy (an optimizer changed call structure) ⇒
    bind nothing, so codegen safely falls back to structural. *)
let bind_consumer (prog : program) : unit =
  if Hashtbl.length async_by_ord = 0 || count prog <> Hashtbl.length async_by_ord
  then consumer_async := []
  else
    consumer_async :=
      fold_calls
        (fun acc ord node ->
          let b =
            match Hashtbl.find_opt async_by_ord ord with
            | Some b -> b
            | None -> false
          in
          (node, b) :: acc)
        [] prog

(** [is_async_call node] — does this call's (declared/inferred) effect
    row include [Async]? Physical-identity lookup over the bound prog. *)
let is_async_call (node : expr) : bool =
  match List.assq node !consumer_async with
  | b -> b
  | exception Not_found -> false
