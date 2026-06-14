(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Borrow checker for ownership verification.

    This module implements borrow checking to ensure memory safety:
    - No use after move
    - No conflicting borrows
    - Borrows don't outlive owners

    [IMPL-DEP: Phase 3]
*)

open Ast

(** A place is an l-value that can be borrowed *)
type place =
  | PlaceVar of string * Symbol.symbol_id  (** human name, symbol id *)
  | PlaceField of place * string
  | PlaceIndex of place * int option  (** None for dynamic index *)
  | PlaceDeref of place
[@@deriving show]

(** Borrow kind *)
type borrow_kind =
  | Shared     (** Immutable borrow (&) *)
  | Exclusive  (** Mutable borrow (&mut) *)
[@@deriving show, eq]

(** Human-readable borrow kind for error messages. *)
let borrow_kind_name (k : borrow_kind) : string =
  match k with
  | Shared    -> "shared"
  | Exclusive -> "exclusive"

(** A borrow record *)
type borrow = {
  b_place : place;
  b_kind : borrow_kind;
  b_span : Span.t;
  b_id : int;
}
[@@deriving show]

(** Move record for tracking move sites *)
type move_record = {
  m_place : place;
  m_span : Span.t;
}
[@@deriving show]

(** Function signature for ownership tracking *)
type fn_signature = {
  fn_name : string;
  fn_param_ownerships : ownership option list;

  (** Parameter indices whose borrow may flow out through the function's
      return value — the function's *return-borrow summary*. Non-empty
      exactly when some return position yields a borrow rooted at a
      [ref]/[mut] (caller-owned) parameter (directly [&p] / [return p], or
      through a let-bound ref-local chain).  A call [let r = f(a, b)] where
      [i] is in this set means the result [r] borrows argument [i] — the
      borrow-graph edge that was previously missing, letting a use of the
      moved argument through [r] slip past (#554).  Computed syntactically
      from the body (name-based) at signature-build time; the
      interprocedural-through-call-result case (a return whose origin is
      itself another ref-returning call result bound to a local) is the
      documented residual that true Polonius origins (ADR-022 / #553)
      close. *)
  fn_ret_borrow_params : int list;
}

(** Borrow checker context with function signatures *)
type context = {
  fn_sigs : (string, fn_signature) Hashtbl.t;
}

(** Borrow checker state *)
type state = {
  (** Active borrows *)
  mutable borrows : borrow list;

  (** Moved places with their move sites *)
  mutable moved : move_record list;

  (** Next borrow ID *)
  mutable next_id : int;

  (** Mutable bindings - places that were declared with 'let mut' *)
  mutable mutable_bindings : place list;

  (** Borrow-graph edges: a reference binder symbol -> the borrow it holds.
      Populated when a [let r = &x] / [let r = &mut x] binds a reference to
      a place.  These borrows are *escaping* (they live as long as the
      binder's lexical scope), unlike call-argument borrows which are
      temporary and released when the call expression completes.  This is
      the graph that borrow-graph validation (CORE-01 / #177) walks. *)
  mutable ref_bindings : (Symbol.symbol_id * borrow) list;

  (** Symbols bound by a [let] in the current block, innermost first.
      Used to decide whether a borrowed owner is block-local (so a
      reference that escapes the block outlives its owner). *)
  mutable block_local_syms : Symbol.symbol_id list;

  (** Sym-ids of the current function's *callee-owned* parameters: those
      with effective ownership [None] (by-value) or [Some Own]. A reference
      rooted at one of these escapes the callee frame if it is *returned*,
      exactly like a borrow of a block-local. [ref]/[mut] params are
      *caller-owned* referents and are deliberately absent here — returning
      a borrow of them is sound. CORE-01 pt2 / #177 (return-escape). *)
  mutable callee_owned_params : Symbol.symbol_id list;

  (** Sym-ids of bindings declared @linear (`QOne`) in the current
      function — surfaced as explicit `@linear` annotations on lets/params
      and as inferred `QOne` from a [Cmd _] type annotation.  Maintained
      alongside the quantity checker's per-block linear-tracking so the
      borrow checker can reject *capture* of these bindings by a closure:
      a closure can be called 0..N times, so capturing a linear binding
      makes its consumption count unprovable at borrow time.  The quantity
      checker also catches this via [QOmega] scaling of captures, but the
      borrow-side error fires earlier in the pipeline and points at the
      *lambda* span (the capture site) rather than at a downstream "used
      multiple times" diagnostic.  CORE-01 pt3 Slice D / #177. *)
  mutable linear_bindings : Symbol.symbol_id list;

  (** Escaping borrows produced by the most-recently-checked call
      expression whose callee has a non-empty return-borrow summary.  A call
      [f(a)] whose result borrows [a] keeps that argument borrow *live* on
      [state.borrows] (not released at call end) and stashes it here so the
      immediately-following [record_ref_binding] can claim it as the result
      binder's ref-graph edge ([let r = f(a)] → [r] borrows [a]), subjecting
      it to the same NLL last-use expiry and return-escape checks as
      [let r = &a].  An *unclaimed* escaping borrow (the result flowed into
      an aggregate, was dereferenced, or discarded) behaves exactly like an
      unclaimed plain `&` borrow: it simply lingers on [state.borrows] until
      lexical block exit.  This list is only a hand-off pointer for the claim
      step; the borrows it names live or die on [state.borrows]. #554. *)
  mutable result_borrows : borrow list;
}

(** Borrow checker errors *)
type borrow_error =
  | UseAfterMove of place * Span.t * Span.t  (** place, use site, move site *)
  | ConflictingBorrow of borrow * borrow
  | BorrowOutlivesOwner of borrow * Symbol.symbol_id
  | MoveWhileBorrowed of place * borrow
  | CannotMoveOutOfBorrow of place * borrow
  | CannotBorrowAsMutable of place * Span.t
  | UseWhileExclusivelyBorrowed of place * borrow * Span.t
      (** use of [place] (at the trailing span) while a still-live exclusive
          borrow holds it — the shared-XOR-exclusive aliasing rule, enforced
          at use sites, not only at borrow creation. CORE-01 / #177. *)
  | LinearCapturedByClosure of string * Span.t
      (** the named @linear binding has been captured as a closure free
          variable at the lambda's span — capturing extends consumption
          beyond what the @linear contract can be checked at borrow time
          (a closure may be called 0..N times). CORE-01 pt3 Slice D / #177. *)
[@@deriving show]

type 'a result = ('a, borrow_error) Result.t

(* Result bind - define before use *)
let ( let* ) = Result.bind

(** Effective ownership of a parameter.

    AffineScript source encodes `own`/`ref`/`mut` on a parameter as the
    *type* constructors [TyOwn]/[TyRef]/[TyMut] (the same encoding
    [codegen.ml] reads to derive WASM param kinds), not as the legacy
    [p_ownership] field — which the parser leaves [None] for surface
    `a: mut Int`.  Reading only [p_ownership] (the prior behaviour) meant
    the owned/ref/mut borrow discipline was effectively *unenforced from
    source*.  Prefer an explicit [p_ownership]; otherwise derive it from
    the parameter type.  CORE-01 / #177. *)
let ty_ownership (t : type_expr) : ownership option =
  match t with
  | TyOwn _ -> Some Own
  | TyRef _ -> Some Ref
  | TyMut _ -> Some Mut
  | _ -> None
let param_ownership (p : param) : ownership option =
  match p.p_ownership with
  | Some _ as o -> o
  | None -> ty_ownership p.p_ty

(** Compute a function's *return-borrow summary*: the set of parameter
    indices whose borrow may be returned (see [fn_ret_borrow_params]).

    Purely syntactic and name-based (no symbol table needed, so it can run
    at signature-build time).  A return position contributes parameter [i]
    when its value is a borrow rooted at the [ref]/[mut] parameter at index
    [i] — either [&p] / [&mut p] for such a [p], the bare [return p] of a
    [ref]/[mut] parameter (returning the reference propagates the caller's
    borrow), or a let-bound ref-local that was itself seeded from one of
    those.  Borrows of by-value/[own] parameters or locals are NOT included:
    those are return-escapes the in-function [check_return_escape] already
    rejects, so the function never type-checks and its summary is moot.

    Sound direction: the summary may *over*-approximate origins (e.g. across
    branches, or a local shadowing a parameter name) — over-approximation
    only ever keeps more argument borrows alive at call sites, never fewer,
    so it cannot reintroduce a use-after-move false negative.  Bodies of
    nested lambdas are skipped (a [return] there is the lambda's, not this
    function's).  #554. *)
let compute_ret_borrow_params (lookup : string -> int list option)
    (fd : fn_decl) : int list =
  let param_idx : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let ref_param : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  List.iteri (fun i (p : param) ->
    Hashtbl.replace param_idx p.p_name.name i;
    (match param_ownership p with
     | Some Ref | Some Mut -> Hashtbl.replace ref_param p.p_name.name ()
     | _ -> ())
  ) fd.fd_params;
  (* name -> param-index set a let-bound ref-local may borrow *)
  let local_origins : (string, int list) Hashtbl.t = Hashtbl.create 8 in
  let result = ref [] in
  let add_origins l =
    List.iter (fun i -> if not (List.mem i !result) then result := i :: !result) l
  in
  let rec peel = function ExprSpan (e, _) -> peel e | e -> e in
  let rec root_name (e : expr) : string option =
    match peel e with
    | ExprVar id -> Some id.name
    | ExprField (b, _) | ExprIndex (b, _) | ExprTupleIndex (b, _) -> root_name b
    | _ -> None
  in
  (* Param indices a ref-source expression borrows, [] if none.  [local_origins]
     is consulted BEFORE [ref_param] so a local that shadows a parameter name
     (its entry, possibly [], was set by [record_let]) wins — otherwise a
     value-local shadowing a ref-param would be spuriously treated as a
     returned borrow of that param (false positive). *)
  let rec origins_of_ref_source (e : expr) : int list =
    match peel e with
    | ExprUnary ((OpRef | OpMutRef), inner) ->
      (match root_name inner with
       | Some n ->
         (match Hashtbl.find_opt local_origins n with
          | Some l -> l
          | None ->
            (match Hashtbl.find_opt param_idx n with Some i -> [i] | None -> []))
       | None -> [])
    | ExprVar id ->
      (match Hashtbl.find_opt local_origins id.name with
       | Some l -> l
       | None ->
         if Hashtbl.mem ref_param id.name then
           (match Hashtbl.find_opt param_idx id.name with Some i -> [i] | None -> [])
         else [])
    | ExprApp (f, args) ->
      (* Interprocedural: a call whose result borrows the callee's parameter
         [i] makes OUR function borrow whatever argument [i] resolves to in our
         frame.  [lookup] returns the callee's *current* return-borrow summary,
         driven to a fixpoint by [build_context], so a function that returns
         another ref-returning call's result (e.g.
         [wrap(ref x){ let t = pick(x); return t }]) inherits the origin.
         #554 residual (a). *)
      (match root_name f with
       | Some fname ->
         (match lookup fname with
          | Some callee_sum ->
            List.concat_map (fun i ->
              match List.nth_opt args i with
              | Some arg -> origins_of_ref_source arg
              | None -> []) callee_sum
          | None -> [])
       | None -> [])
    | _ -> []
  in
  (* Always record the binding (even to []) so a shadowing value-local masks
     the parameter it shadows for the rest of the scan. *)
  let record_let (pat : pattern) (value : expr) : unit =
    match pat with
    | PatVar id -> Hashtbl.replace local_origins id.name (origins_of_ref_source value)
    | _ -> ()
  in
  (* [walk_tail] visits an expression in *return/tail* position: its value, if
     a ref-source, is a return origin, and so are the tails of any [if]/[match]/
     block it resolves to.  [walk_expr] visits a non-tail expression: it only
     harvests explicit [return]s and threads [let]-bindings into [local_origins].
     Splitting the two is what lets a borrow returned via a bare [match]/[if]
     arm tail (e.g. `match k { _ => &x }`) register as an origin. *)
  let rec walk_tail (e : expr) : unit =
    add_origins (origins_of_ref_source e);
    match e with
    | ExprSpan (e, _) -> walk_tail e
    | ExprReturn (Some r) -> walk_tail r
    | ExprReturn None -> ()
    | ExprLet lb ->
      walk_expr lb.el_value;
      record_let lb.el_pat lb.el_value;
      (match lb.el_body with Some b -> walk_tail b | None -> ())
    | ExprBlock blk -> walk_block_tail blk
    | ExprIf ei ->
      walk_expr ei.ei_cond; walk_tail ei.ei_then;
      (match ei.ei_else with Some e -> walk_tail e | None -> ())
    | ExprMatch em ->
      walk_expr em.em_scrutinee;
      List.iter (fun arm ->
        (match arm.ma_guard with Some g -> walk_expr g | None -> ());
        walk_tail arm.ma_body) em.em_arms
    | ExprHandle eh ->
      walk_expr eh.eh_body;
      List.iter (fun arm ->
        match arm with HandlerReturn (_, b) | HandlerOp (_, _, b) -> walk_tail b)
        eh.eh_handlers
    | ExprTry et ->
      walk_block_tail et.et_body;
      (match et.et_catch with
       | Some arms -> List.iter (fun arm -> walk_tail arm.ma_body) arms
       | None -> ());
      (match et.et_finally with Some b -> walk_block_tail b | None -> ())
    | _ -> walk_expr e
  and walk_expr (e : expr) : unit =
    match e with
    | ExprSpan (e, _) -> walk_expr e
    (* A returned value is in tail/return position no matter where the
       [return] textually sits, so it must be walked as a TAIL — otherwise a
       borrow returned via [return if c { &x } else { &x };] (or match/block)
       is missed and the #554 stamp is bypassed by the idiomatic spelling.
       Found by second-pass adversarial verification. *)
    | ExprReturn (Some r) -> walk_tail r
    | ExprReturn None -> ()
    | ExprLet lb ->
      walk_expr lb.el_value;
      record_let lb.el_pat lb.el_value;
      (match lb.el_body with Some b -> walk_expr b | None -> ())
    | ExprBlock blk -> walk_block blk
    | ExprIf ei ->
      walk_expr ei.ei_cond; walk_expr ei.ei_then;
      (match ei.ei_else with Some e -> walk_expr e | None -> ())
    | ExprMatch em ->
      walk_expr em.em_scrutinee;
      List.iter (fun arm ->
        (match arm.ma_guard with Some g -> walk_expr g | None -> ());
        walk_expr arm.ma_body) em.em_arms
    | ExprApp (f, args) -> walk_expr f; List.iter walk_expr args
    | ExprBinary (a, _, b) -> walk_expr a; walk_expr b
    | ExprUnary (_, e) -> walk_expr e
    | ExprField (b, _) | ExprTupleIndex (b, _) | ExprRowRestrict (b, _) -> walk_expr b
    | ExprIndex (a, b) -> walk_expr a; walk_expr b
    | ExprTuple es | ExprArray es -> List.iter walk_expr es
    | ExprRecord r ->
      List.iter (fun (_, eo) -> match eo with Some e -> walk_expr e | None -> ()) r.er_fields;
      (match r.er_spread with Some e -> walk_expr e | None -> ())
    | ExprHandle eh ->
      walk_expr eh.eh_body;
      List.iter (fun arm ->
        match arm with HandlerReturn (_, b) | HandlerOp (_, _, b) -> walk_expr b)
        eh.eh_handlers
    | ExprTry et ->
      walk_block et.et_body;
      (match et.et_catch with
       | Some arms -> List.iter (fun arm -> walk_expr arm.ma_body) arms
       | None -> ());
      (match et.et_finally with Some b -> walk_block b | None -> ())
    | ExprResume (Some e) -> walk_expr e
    (* Lambda bodies are skipped: a return there belongs to the lambda. *)
    | _ -> ()
  and walk_stmt (s : stmt) : unit =
    match s with
    | StmtLet sl -> walk_expr sl.sl_value; record_let sl.sl_pat sl.sl_value
    | StmtExpr e -> walk_expr e
    | StmtAssign (l, _, r) ->
      walk_expr l; walk_expr r;
      (* A whole-var reassignment may change which parameters a ref-local
         borrows.  UNION the new origins into the local's set (rather than
         replace) so the flow-insensitive summary never *drops* a possible
         origin — conservative, so [let mut t = pick(y); t = pick(x); return t]
         summarises {x,y} not the stale {y}, closing the reassigned-local
         false negative.  #554 round-3. *)
      (match peel l with
       | ExprVar id ->
         let prior =
           match Hashtbl.find_opt local_origins id.name with Some x -> x | None -> []
         in
         Hashtbl.replace local_origins id.name
           (List.sort_uniq compare (prior @ origins_of_ref_source r))
       | _ -> ())
    | StmtWhile (c, b) -> walk_expr c; walk_block b
    | StmtFor (_, it, b) -> walk_expr it; walk_block b
  and walk_block (blk : block) : unit =
    (* non-tail block: only explicit returns inside it reach the fn return *)
    List.iter walk_stmt blk.blk_stmts;
    match blk.blk_expr with Some e -> walk_expr e | None -> ()
  and walk_block_tail (blk : block) : unit =
    (* block in tail position: its value expression is in tail position *)
    List.iter walk_stmt blk.blk_stmts;
    match blk.blk_expr with Some e -> walk_tail e | None -> ()
  in
  (match fd.fd_body with
   | FnBlock blk -> walk_block_tail blk
   | FnExpr e -> walk_tail e
   | FnExtern -> ());
  List.sort_uniq compare !result

(** Create a new borrow checker context *)
let create_context () : context =
  {
    fn_sigs = Hashtbl.create 64;
  }

(** Create a new borrow checker state *)
let create () : state =
  {
    borrows = [];
    moved = [];
    next_id = 0;
    mutable_bindings = [];
    ref_bindings = [];
    block_local_syms = [];
    callee_owned_params = [];
    linear_bindings = [];
    result_borrows = [];
  }

(** Mirror of [Quantity.quantity_of_ty_annotation]: returns [QOne] when the
    given type annotation is a [Cmd _] application (linear by construction
    per ADR-002 / Stage 11), [QOmega] otherwise. Duplicated here so
    [Borrow] does not depend on [Quantity]; the canonical helper still
    lives in [quantity.ml]. CORE-01 pt3 Slice D / #177. *)
let borrow_quantity_of_ty (te_opt : type_expr option) : quantity =
  match te_opt with
  | Some (TyApp ({ name = "Cmd"; _ }, _)) -> QOne
  | _ -> QOmega

(** Returns true if a let-binding declared with the given explicit-quantity
    annotation and type annotation should be tracked as @linear (QOne) by
    the borrow checker. CORE-01 pt3 Slice D / #177. *)
let let_is_linear (q_opt : quantity option) (ty_opt : type_expr option) : bool =
  match q_opt with
  | Some q -> q = QOne
  | None -> borrow_quantity_of_ty ty_opt = QOne

(** Add a function signature to context *)
let add_fn_signature (ctx : context) (fd : fn_decl) : unit =
  let sig_ = {
    fn_name = fd.fd_name.name;
    fn_param_ownerships = List.map param_ownership fd.fd_params;
    (* Standalone use: intraprocedural only (no callee summaries available).
       [build_context] recomputes interprocedurally via a fixpoint. *)
    fn_ret_borrow_params = compute_ret_borrow_params (fun _ -> None) fd;
  } in
  Hashtbl.replace ctx.fn_sigs fd.fd_name.name sig_

(** Build context from program.

    Return-borrow summaries ([fn_ret_borrow_params]) are computed
    *interprocedurally* by a monotone fixpoint: each signature is seeded with
    an empty summary, then every function's summary is recomputed against the
    callees' current summaries until none change.  [compute_ret_borrow_params]
    resolves a returned call result through the callee's summary, so a
    function that returns another ref-returning call's result eventually
    inherits the origin.  Origins only grow and are bounded by arity, so the
    loop terminates.  #554 (interprocedural residual (a)). *)
let build_context (program : program) : context =
  let ctx = create_context () in
  let fds =
    List.filter_map (function TopFn fd -> Some fd | _ -> None) program.prog_decls
  in
  List.iter (fun (fd : fn_decl) ->
    Hashtbl.replace ctx.fn_sigs fd.fd_name.name {
      fn_name = fd.fd_name.name;
      fn_param_ownerships = List.map param_ownership fd.fd_params;
      fn_ret_borrow_params = [];
    }) fds;
  let lookup name =
    match Hashtbl.find_opt ctx.fn_sigs name with
    | Some s -> Some s.fn_ret_borrow_params
    | None -> None
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (fd : fn_decl) ->
      let new_sum = compute_ret_borrow_params lookup fd in
      match Hashtbl.find_opt ctx.fn_sigs fd.fd_name.name with
      | Some s when new_sum <> s.fn_ret_borrow_params ->
        Hashtbl.replace ctx.fn_sigs fd.fd_name.name
          { s with fn_ret_borrow_params = new_sum };
        changed := true
      | _ -> ()
    ) fds
  done;
  ctx

(** Generate a fresh borrow ID *)
let fresh_id (state : state) : int =
  let id = state.next_id in
  state.next_id <- id + 1;
  id

(** Check if a type is Copy (doesn't need to be moved) *)
let rec is_copy_type (ty_opt : type_expr option) : bool =
  match ty_opt with
  | None -> false  (* Unknown type, assume not Copy *)
  | Some ty ->
    begin match ty with
      | TyCon id when id.name = "Int" || id.name = "Bool" || id.name = "Char" -> true
      | TyCon id when id.name = "Unit" -> true
      | TyTuple tys -> List.for_all (fun t -> is_copy_type (Some t)) tys
      | TyRef _ -> true  (* Shared references are Copy *)
      | _ -> false  (* Records, arrays, owned types, etc. are not Copy *)
    end

(** Check if an expression has a Copy type (heuristic for literals) *)
let is_copy_expr (expr : expr) : bool =
  match expr with
  | ExprLit (LitInt _) -> true
  | ExprLit (LitBool _) -> true
  | ExprLit (LitChar _) -> true
  | ExprLit (LitUnit _) -> true
  | ExprUnary ((OpRef | OpMutRef), _) -> true  (* Reference creation produces a Copy pointer *)
  | _ -> false

(** Walk to the root variable of a place, if any. *)
let rec root_var (p : place) : Symbol.symbol_id option =
  match p with
  | PlaceVar (_, id)     -> Some id
  | PlaceField (base, _)
  | PlaceIndex (base, _)
  | PlaceDeref base      -> root_var base

(** Check if two places overlap.

    Two places overlap when they share the same root variable. This is
    deliberately conservative — [a.x] and [a.y] overlap, [a[0]] and [a[1]]
    overlap — which is the safe direction for a borrow checker (it may
    report conflicts that a more precise analysis would allow, but never
    misses a real conflict). The rule terminates trivially because
    [root_var] always descends.

    The previous implementation case-split on shape and recursed into
    sub-place pairs; that worked for [PlaceVar]/[PlaceField] alone but
    produced an infinite recursion once [PlaceIndex] was added to the
    cross-shape arms, and silently returned [false] on
    [PlaceIndex] vs [PlaceVar] before that — which is why writes through
    [mut buf: Array[T]] parameters were spuriously rejected. *)
let places_overlap (p1 : place) (p2 : place) : bool =
  match root_var p1, root_var p2 with
  | Some r1, Some r2 -> r1 = r2
  | _ -> false

(** Check if a place is moved and return the move site if so *)
let find_move (state : state) (place : place) : Span.t option =
  List.find_map (fun mr ->
    if places_overlap place mr.m_place then Some mr.m_span
    else None
  ) state.moved

(** Check if a place is moved *)
let is_moved (state : state) (place : place) : bool =
  Option.is_some (find_move state place)

(** Check if a borrow conflicts with existing borrows *)
let find_conflicting_borrow (state : state) (new_borrow : borrow) : borrow option =
  List.find_opt (fun existing ->
    places_overlap new_borrow.b_place existing.b_place &&
    (new_borrow.b_kind = Exclusive || existing.b_kind = Exclusive)
  ) state.borrows

(** Record a move *)
let record_move (state : state) (place : place) (span : Span.t) : unit result =
  (* Check for active borrows *)
  match List.find_opt (fun b -> places_overlap place b.b_place) state.borrows with
  | Some borrow -> Error (MoveWhileBorrowed (place, borrow))
  | None ->
    state.moved <- { m_place = place; m_span = span } :: state.moved;
    Ok ()

(** Check if a place is mutable *)
let is_mutable (state : state) (place : place) : bool =
  List.exists (fun mut_place -> places_overlap place mut_place) state.mutable_bindings

(** Record a borrow *)
let record_borrow (state : state) (place : place) (kind : borrow_kind)
    (span : Span.t) : borrow result =
  (* Check if moved and report the original move site *)
  match find_move state place with
  | Some move_site ->
    Error (UseAfterMove (place, span, move_site))
  | None ->
    (* Check if trying to mutably borrow an immutable place *)
    let mut_check = match kind with
    | Exclusive ->
      if not (is_mutable state place) then
        Error (CannotBorrowAsMutable (place, span))
      else
        Ok ()
    | Shared -> Ok ()
    in
    match mut_check with
    | Error _ as err -> err
    | Ok () ->
    let new_borrow = {
      b_place = place;
      b_kind = kind;
      b_span = span;
      b_id = fresh_id state;
    } in
    match find_conflicting_borrow state new_borrow with
    | Some conflict -> Error (ConflictingBorrow (new_borrow, conflict))
    | None ->
      state.borrows <- new_borrow :: state.borrows;
      Ok new_borrow

(** End a borrow *)
let end_borrow (state : state) (borrow : borrow) : unit =
  state.borrows <- List.filter (fun b -> b.b_id <> borrow.b_id) state.borrows

(** Find a still-live exclusive borrow that aliases [place], if any.

    With call-argument borrows released after their call (see [check_expr]
    [ExprApp]), a persistent exclusive borrow overlapping [place] is a real
    escaping `&mut` binding — reading or otherwise using the place while it
    is exclusively borrowed violates shared-XOR-exclusive. This is the
    use-site half of the rule that [find_conflicting_borrow] only enforced
    at borrow *creation*. CORE-01 / #177. *)
let find_aliasing_exclusive (state : state) (place : place) : borrow option =
  List.find_opt (fun b ->
    b.b_kind = Exclusive && places_overlap place b.b_place
  ) state.borrows

(** Check a use of a place *)
let check_use (state : state) (place : place) (span : Span.t) : unit result =
  match find_move state place with
  | Some move_site -> Error (UseAfterMove (place, span, move_site))
  | None ->
    match find_aliasing_exclusive state place with
    | Some b -> Error (UseWhileExclusivelyBorrowed (place, b, span))
    | None -> Ok ()

(** Format a place for display in error messages. *)
let rec format_place (p : place) : string =
  match p with
  | PlaceVar (name, _)        -> name
  | PlaceField (base, f)      -> format_place base ^ "." ^ f
  | PlaceIndex (base, Some i) -> format_place base ^ "[" ^ string_of_int i ^ "]"
  | PlaceIndex (base, None)   -> format_place base ^ "[_]"
  | PlaceDeref p'             -> "*" ^ format_place p'

(** Format a span for display. *)
let format_span (span : Span.t) : string =
  Format.asprintf "%a" Span.pp_short span

(** Format a borrow error as a human-readable string. *)
let format_borrow_error (e : borrow_error) : string =
  match e with
  | UseAfterMove (place, use_span, move_span) ->
    Printf.sprintf
      "use of moved value: `%s`\n  \
       value used at %s\n  \
       value moved at %s"
      (format_place place) (format_span use_span) (format_span move_span)
  | ConflictingBorrow (b1, b2) ->
    Printf.sprintf
      "conflicting borrows on `%s`:\n  \
       %s borrow (id %d) at %s conflicts with earlier %s borrow (id %d) at %s"
      (format_place b1.b_place)
      (borrow_kind_name b1.b_kind) b1.b_id (format_span b1.b_span)
      (borrow_kind_name b2.b_kind) b2.b_id (format_span b2.b_span)
  | BorrowOutlivesOwner (b, sym_id) ->
    Printf.sprintf
      "borrow of `%s` (id %d) outlives its owner (symbol %d)"
      (format_place b.b_place) b.b_id sym_id
  | MoveWhileBorrowed (place, b) ->
    Printf.sprintf
      "cannot move `%s` while it is %s-borrowed at %s"
      (format_place place) (borrow_kind_name b.b_kind) (format_span b.b_span)
  | CannotMoveOutOfBorrow (place, b) ->
    Printf.sprintf
      "cannot move out of `%s`, which is behind a %s borrow at %s"
      (format_place place) (borrow_kind_name b.b_kind) (format_span b.b_span)
  | CannotBorrowAsMutable (place, span) ->
    Printf.sprintf
      "cannot borrow `%s` as mutable — it is not declared with `let mut` (at %s)"
      (format_place place) (format_span span)
  | UseWhileExclusivelyBorrowed (place, b, use_span) ->
    Printf.sprintf
      "cannot use `%s` at %s while it is exclusively borrowed:\n  \
       exclusive borrow (id %d) taken at %s is still live"
      (format_place place) (format_span use_span)
      b.b_id (format_span b.b_span)
  | LinearCapturedByClosure (name, lam_span) ->
    Printf.sprintf
      "cannot capture @linear binding `%s` in a closure (at %s)\n  \
       a closure may be called zero or many times, so capturing a \
       @linear binding makes its consumption count unprovable. \
       Inline the use, or move the binding into the closure body."
      name (format_span lam_span)

(** Get span from an expression *)
let rec expr_span (expr : expr) : Span.t =
  match expr with
  | ExprSpan (_, span) -> span
  | ExprLit lit -> lit_span lit
  | ExprVar id -> id.span
  | ExprLet { el_pat; _ } -> pattern_span el_pat
  | ExprIf { ei_cond; _ } -> expr_span ei_cond
  | ExprMatch { em_scrutinee; _ } -> expr_span em_scrutinee
  | ExprLambda { elam_params; _ } ->
    begin match elam_params with
      | p :: _ -> p.p_name.span
      | [] -> Span.dummy
    end
  | ExprApp (f, _) -> expr_span f
  | ExprField (e, _) -> expr_span e
  | ExprTupleIndex (e, _) -> expr_span e
  | ExprIndex (e, _) -> expr_span e
  | ExprTuple exprs ->
    begin match exprs with
      | e :: _ -> expr_span e
      | [] -> Span.dummy
    end
  | ExprArray exprs ->
    begin match exprs with
      | e :: _ -> expr_span e
      | [] -> Span.dummy
    end
  | ExprRecord { er_fields; _ } ->
    begin match er_fields with
      | (id, _) :: _ -> id.span
      | [] -> Span.dummy
    end
  | ExprRowRestrict (e, _) -> expr_span e
  | ExprBinary (e, _, _) -> expr_span e
  | ExprUnary (_, e) -> expr_span e
  | ExprBlock { blk_stmts; blk_expr } ->
    begin match blk_stmts with
      | StmtLet { sl_pat; _ } :: _ -> pattern_span sl_pat
      | StmtExpr e :: _ -> expr_span e
      | StmtAssign (e, _, _) :: _ -> expr_span e
      | StmtWhile (e, _) :: _ -> expr_span e
      | StmtFor (p, _, _) :: _ -> pattern_span p
      | [] -> match blk_expr with Some e -> expr_span e | None -> Span.dummy
    end
  | ExprReturn _ -> Span.dummy
  | ExprBreak sp -> sp
  | ExprContinue sp -> sp
  | ExprTry _ -> Span.dummy
  | ExprHandle { eh_body; _ } -> expr_span eh_body
  | ExprResume _ -> Span.dummy
  | ExprUnsafe _ -> Span.dummy
  | ExprVariant (id, _) -> id.span

and lit_span (lit : literal) : Span.t =
  match lit with
  | LitInt (_, span) -> span
  | LitFloat (_, span) -> span
  | LitBool (_, span) -> span
  | LitChar (_, span) -> span
  | LitString (_, span) -> span
  | LitUnit span -> span

and pattern_span (pat : pattern) : Span.t =
  match pat with
  | PatWildcard span -> span
  | PatVar id -> id.span
  | PatLit lit -> lit_span lit
  | PatCon (id, _) -> id.span
  | PatTuple pats ->
    begin match pats with
      | p :: _ -> pattern_span p
      | [] -> Span.dummy
    end
  | PatRecord ((id, _) :: _, _) -> id.span
  | PatRecord ([], _) -> Span.dummy
  | PatOr (p1, _) -> pattern_span p1
  | PatAs (id, _) -> id.span

(** Lookup a symbol by name, searching all_symbols table *)
(** This is needed because scopes are exited after resolution *)
let lookup_symbol_by_name (symbols : Symbol.t) (name : string) : Symbol.symbol option =
  (* Search all_symbols table for most recent symbol with this name *)
  let matching_symbols = Hashtbl.fold (fun _id sym acc ->
    if sym.Symbol.sym_name = name && sym.Symbol.sym_kind = Symbol.SKVariable then
      sym :: acc
    else
      acc
  ) symbols.Symbol.all_symbols [] in
  (* Sort by symbol ID (descending) to get most recent *)
  let sorted_symbols = List.sort (fun a b -> compare b.Symbol.sym_id a.Symbol.sym_id) matching_symbols in
  match sorted_symbols with
  | sym :: _ -> Some sym
  | [] -> None

(** If [expr] creates a reference to a place ([&p] / [&mut p], optionally
    wrapped in spans), return that place. Used to build the borrow-graph
    edge for [let r = &p]. CORE-01 / #177. *)
let rec ref_target (symbols : Symbol.t) (expr : expr) : place option =
  match expr with
  | ExprSpan (e, _) -> ref_target symbols e
  | ExprUnary ((OpRef | OpMutRef), e) -> expr_to_place symbols e
  | _ -> None

(** Convert an expression to a place (if it's an l-value) *)
and expr_to_place (symbols : Symbol.t) (expr : expr) : place option =
  match expr with
  | ExprVar id ->
    begin match lookup_symbol_by_name symbols id.name with
      | Some sym -> Some (PlaceVar (id.name, sym.sym_id))
      | None -> None
    end
  | ExprField (base, field) ->
    begin match expr_to_place symbols base with
      | Some base_place -> Some (PlaceField (base_place, field.name))
      | None -> None
    end
  | ExprIndex (base, _) ->
    begin match expr_to_place symbols base with
      | Some base_place -> Some (PlaceIndex (base_place, None))
      | None -> None
    end
  | ExprSpan (e, _) ->
    expr_to_place symbols e
  | _ -> None

(** Look up the live borrow that [expr] denotes, after the expression has
    been checked. Handles three cases:
      - [&p] / [&mut p]: find the borrow on [p] in [state.borrows];
      - [r] where [r] is a ref-binder symbol: return its [state.ref_bindings]
        entry (the same borrow it already aliases) — this is ref-to-ref
        binding / reborrow through indirection;
      - anything else: None.
    Used by [record_ref_binding] (let path) and [StmtAssign] (assign path)
    so [let r2 = r] and [r2 = r] both extend the borrow-graph correctly.
    CORE-01 / #177 ref-to-ref. *)
let rec ref_source_borrow (state : state) (symbols : Symbol.t)
    (expr : expr) : borrow option =
  match expr with
  | ExprSpan (e, _) -> ref_source_borrow state symbols e
  | ExprUnary ((OpRef | OpMutRef), _) ->
    (match ref_target symbols expr with
     | Some target ->
       List.find_opt (fun b -> places_overlap b.b_place target) state.borrows
     | None -> None)
  | ExprVar id ->
    (match lookup_symbol_by_name symbols id.name with
     | Some sym -> List.assoc_opt sym.Symbol.sym_id state.ref_bindings
     | None -> None)
  | _ -> None

(** Cheap structural test: would [expr] supply a reborrow source on
    [StmtAssign]? Used *before* the RHS check, so it does not consult
    live borrow state — only the structural shape of [expr] and the
    pre-existing [state.ref_bindings] (for the ref-var path). *)
let rec is_reborrow_source (state : state) (symbols : Symbol.t)
    (expr : expr) : bool =
  match expr with
  | ExprSpan (e, _) -> is_reborrow_source state symbols e
  | ExprUnary ((OpRef | OpMutRef), _) -> true
  | ExprVar id ->
    (match lookup_symbol_by_name symbols id.name with
     | Some sym -> List.mem_assoc sym.Symbol.sym_id state.ref_bindings
     | None -> false)
  | _ -> false

(** Record a borrow-graph edge for [let <pat> = <ref-source>].

    [<ref-source>] is either a direct [&p]/[&mut p] *or* another ref-binder
    variable [r] (ref-to-ref binding). Call *after* the value expression has
    been checked, so the underlying borrow is already on [state.borrows]
    (or already aliased via [state.ref_bindings] for the ref-var path).
    CORE-01 / #177. *)
let record_ref_binding (state : state) (symbols : Symbol.t)
    (pat : pattern) (value : expr) : unit =
  match pat with
  | PatVar id ->
    begin match lookup_symbol_by_name symbols id.name with
    | None -> ()
    | Some sym ->
      let rec peel = function ExprSpan (e, _) -> peel e | e -> e in
      begin match peel value with
      | ExprApp _ when state.result_borrows <> [] ->
        (* [let r = f(a)] where the call result borrows one or more of its
           arguments: claim those escaping borrows (left live by the
           [ExprApp] handler) as r's ref-graph edges, so NLL last-use expiry
           and return-escape treat r exactly like [let r = &a]. #554. *)
        List.iter (fun b ->
          state.ref_bindings <- (sym.Symbol.sym_id, b) :: state.ref_bindings)
          state.result_borrows;
        state.result_borrows <- []
      | _ ->
        begin match ref_source_borrow state symbols value with
        | Some b -> state.ref_bindings <- (sym.Symbol.sym_id, b) :: state.ref_bindings
        | None -> ()
        end
      end
    end
  | _ -> ()

(** The borrow a *returned* expression denotes, if any: either a direct
    [&place] / [&mut place], or a reference binder [r] (from [let r = &p])
    looked up in the live borrow-graph. Returning a *value* (incl. [*r]) is
    not a borrow and yields [None]. CORE-01 pt2 / #177 (return-escape). *)
let returned_borrow (state : state) (symbols : Symbol.t)
    (e : expr) : borrow option =
  let rec peel = function ExprSpan (x, _) -> peel x | x -> x in
  match ref_target symbols e with
  | Some target ->
    Some (match List.find_opt (fun b ->
            places_overlap b.b_place target) state.borrows with
          | Some b -> b
          | None -> { b_place = target; b_kind = Shared;
                      b_span = expr_span e; b_id = -1 })
  | None ->
    (match peel e with
     | ExprVar id ->
       (match lookup_symbol_by_name symbols id.name with
        | Some sym ->
          List.assoc_opt sym.Symbol.sym_id state.ref_bindings
        | None -> None)
     | _ -> None)

(** Return-escape (CORE-01 pt2 / #177): a [return e] (or fn-tail) whose
    value is a reference rooted at a *callee-owned* binding — a function
    local or a by-value/[own] parameter — dangles once the callee frame is
    gone. Mirrors [BorrowOutlivesOwner]'s block-escape rationale, extended
    to the return position (the let-only graph in pt1 only saw block tails,
    so [return r] slipped through). Sound and non-over-rejecting: returning
    a borrow of a dying callee binding is always wrong; valid code returns
    the value, or a borrow of a [ref]/[mut] (caller-owned) parameter — the
    latter has no callee-owned root and is intentionally not flagged. *)
let check_return_escape (state : state) (symbols : Symbol.t)
    (e : expr) : unit result =
  match returned_borrow state symbols e with
  | None -> Ok ()
  | Some b ->
    (match root_var b.b_place with
     | Some owner
       when List.mem owner state.callee_owned_params
            || List.mem owner state.block_local_syms ->
       Error (BorrowOutlivesOwner (b, owner))
     | _ -> Ok ())

(** NLL last-use pre-pass (CORE-01 pt3 / #177 Slice A).

    Returns a map [sym_id -> max statement index at which the symbol is
    mentioned anywhere inside [blk] (including nested blocks, lambda
    bodies, handler arms, etc.)]. Statement indices are 0-based; the
    block's tail expression, if any, is treated as the [n]-th "statement"
    (where [n = List.length blk.blk_stmts]).

    Used by [check_block] to expire ref-bindings introduced in this block
    once their binder is dead, instead of holding them until lexical block
    exit. The expiry releases the underlying borrow, so subsequent
    statements may legally read or assign through the (no-longer-borrowed)
    owner — the canonical Rust-style "non-lexical lifetime" win.

    A symbol that never appears is simply absent from the table; callers
    treat that as [-1] (last-use precedes the first statement → expire
    immediately after the declaration). *)
let compute_last_use_index (symbols : Symbol.t) (blk : block)
    : (Symbol.symbol_id, int) Hashtbl.t =
  let tbl : (Symbol.symbol_id, int) Hashtbl.t = Hashtbl.create 8 in
  let bump (sym : Symbol.symbol_id) (idx : int) : unit =
    match Hashtbl.find_opt tbl sym with
    | Some cur when cur >= idx -> ()
    | _ -> Hashtbl.replace tbl sym idx
  in
  let mark_var (idx : int) (id : ident) : unit =
    match lookup_symbol_by_name symbols id.name with
    | Some s -> bump s.Symbol.sym_id idx
    | None -> ()
  in
  let rec visit_expr (idx : int) (e : expr) : unit =
    match e with
    | ExprVar id -> mark_var idx id
    | ExprLit _ | ExprVariant _ -> ()
    | ExprSpan (e, _) -> visit_expr idx e
    | ExprApp (f, args) ->
      visit_expr idx f; List.iter (visit_expr idx) args
    | ExprField (b, _) | ExprTupleIndex (b, _) -> visit_expr idx b
    | ExprIndex (b, i) -> visit_expr idx b; visit_expr idx i
    | ExprTuple es | ExprArray es -> List.iter (visit_expr idx) es
    | ExprRecord r ->
      List.iter (fun (_, eo) -> match eo with Some e -> visit_expr idx e | None -> ()) r.er_fields;
      (match r.er_spread with Some e -> visit_expr idx e | None -> ())
    | ExprRowRestrict (e, _) -> visit_expr idx e
    | ExprBinary (l, _, r) -> visit_expr idx l; visit_expr idx r
    | ExprUnary (_, e) -> visit_expr idx e
    | ExprLet lb ->
      visit_expr idx lb.el_value;
      (match lb.el_body with Some b -> visit_expr idx b | None -> ())
    | ExprIf ei ->
      visit_expr idx ei.ei_cond; visit_expr idx ei.ei_then;
      (match ei.ei_else with Some e -> visit_expr idx e | None -> ())
    | ExprMatch em ->
      visit_expr idx em.em_scrutinee;
      List.iter (fun arm ->
        (match arm.ma_guard with Some g -> visit_expr idx g | None -> ());
        visit_expr idx arm.ma_body
      ) em.em_arms
    | ExprBlock blk -> visit_block idx blk
    | ExprLambda lam -> visit_expr idx lam.elam_body
    | ExprReturn (Some e) -> visit_expr idx e
    | ExprReturn None -> ()
    | ExprBreak _ -> ()
    | ExprContinue _ -> ()
    | ExprHandle eh ->
      visit_expr idx eh.eh_body;
      List.iter (fun arm ->
        match arm with
        | HandlerReturn (_, b) -> visit_expr idx b
        | HandlerOp (_, _, b) -> visit_expr idx b
      ) eh.eh_handlers
    | ExprResume (Some e) -> visit_expr idx e
    | ExprResume None -> ()
    | ExprTry et ->
      visit_block idx et.et_body;
      (match et.et_catch with
       | Some arms ->
         List.iter (fun arm ->
           (match arm.ma_guard with Some g -> visit_expr idx g | None -> ());
           visit_expr idx arm.ma_body) arms
       | None -> ());
      (match et.et_finally with Some b -> visit_block idx b | None -> ())
    | ExprUnsafe ops ->
      List.iter (fun op ->
        match op with
        | UnsafeRead e -> visit_expr idx e
        | UnsafeWrite (a, b) -> visit_expr idx a; visit_expr idx b
        | UnsafeOffset (a, b) -> visit_expr idx a; visit_expr idx b
        | UnsafeTransmute (_, _, e) -> visit_expr idx e
        | UnsafeForget e -> visit_expr idx e
      ) ops
  and visit_stmt (idx : int) (s : stmt) : unit =
    match s with
    | StmtLet sl -> visit_expr idx sl.sl_value
    | StmtExpr e -> visit_expr idx e
    | StmtAssign (lhs, _, rhs) -> visit_expr idx lhs; visit_expr idx rhs
    | StmtWhile (c, b) -> visit_expr idx c; visit_block idx b
    | StmtFor (_, it, b) -> visit_expr idx it; visit_block idx b
  and visit_block (idx : int) (b : block) : unit =
    List.iter (visit_stmt idx) b.blk_stmts;
    match b.blk_expr with Some e -> visit_expr idx e | None -> ()
  in
  List.iteri (fun i s -> visit_stmt i s) blk.blk_stmts;
  let tail_idx = List.length blk.blk_stmts in
  (match blk.blk_expr with
   | Some e -> visit_expr tail_idx e
   | None -> ());
  tbl

(** Merge per-arm post-states after a multi-arm CFG-join construct
    ([ExprHandle], [ExprTry] catch). Each [arm_result] is a triple
    [(result, post_arm_borrows, post_arm_moved)] captured immediately
    after running the arm against the snapshotted base state.

    - Errors: propagates the first error seen (left-to-right).
    - Borrows: intersection across arms — a borrow is alive post-join
      only if it is alive at the end of every arm. (Branch-local
      borrows naturally die at arm exit because the arm body is
      typically a block whose [check_block] clears them; this just
      formalises that semantics across the join.)
    - Moves: union — a place is moved post-join if any arm moved it,
      deduplicated against the base so we don't double-count moves
      that already existed before the arms.

    Mirrors the inlined logic in [ExprMatch]'s arm-merge so all CFG-
    join sites share one notion of "what state survives the join".
    CORE-01 pt3 Slice C / #177 (CFG-join semantics for non-match
    join constructs). *)
let merge_arm_results (state : state)
    (base_borrows : borrow list) (base_moved : move_record list)
    (arm_results : (unit result * borrow list * move_record list) list)
    : unit result =
  let errors = List.filter_map (fun (r, _, _) ->
    match r with Error e -> Some e | Ok () -> None) arm_results in
  match errors with
  | e :: _ -> Error e
  | [] ->
    let all_borrows = List.map (fun (_, bs, _) -> bs) arm_results in
    let merged_borrows = match all_borrows with
      | [] -> base_borrows
      | first :: rest ->
        List.fold_left (fun acc arm_borrows ->
          List.filter (fun b ->
            List.exists (fun b' -> b.b_id = b'.b_id) arm_borrows
          ) acc
        ) first rest
    in
    let all_moves = List.concat_map (fun (_, _, ms) ->
      List.filter (fun mr ->
        not (List.exists (fun base_mr ->
          places_overlap mr.m_place base_mr.m_place
        ) base_moved)
      ) ms
    ) arm_results in
    let unique_moves = List.fold_left (fun acc mr ->
      if List.exists (fun mr' -> places_overlap mr.m_place mr'.m_place) acc
      then acc
      else mr :: acc
    ) [] all_moves in
    state.borrows <- merged_borrows;
    state.moved <- base_moved @ unique_moves;
    Ok ()

(** Check borrows in an expression *)
let rec check_expr (ctx : context) (state : state) (symbols : Symbol.t) (expr : expr) : unit result =
  match expr with
  | ExprVar id ->
    begin match expr_to_place symbols expr with
      | Some place ->
        (* Only check use, don't move - moves happen explicitly at call sites *)
        check_use state place id.span
      | None -> Ok ()
    end

  | ExprLit _ -> Ok ()

  | ExprApp (func, args) ->
    let* () = check_expr ctx state symbols func in
    (* Check if this is a direct function call and get ownership info.
       Peel ExprSpan: the parser wraps the callee in spans, so a bare
       `ExprVar` match silently skipped ownership for essentially every
       real (parsed) call — meaning the affine/borrow discipline on
       owned/ref/mut parameters was not enforced from source. CORE-01. *)
    let rec unwrap_callee = function
      | ExprSpan (e, _) -> unwrap_callee e
      | e -> e
    in
    let param_ownerships =
      match unwrap_callee func with
      | ExprVar fn_id ->
        begin match Hashtbl.find_opt ctx.fn_sigs fn_id.name with
        | Some sig_ -> sig_.fn_param_ownerships
        | None -> List.map (fun _ -> None) args  (* Unknown function, assume no ownership *)
        end
      | _ -> List.map (fun _ -> None) args  (* Higher-order call, assume no ownership *)
    in
    (* The callee's return-borrow summary: argument indices whose borrow
       flows out through the result.  Empty for value-returning and for
       higher-order/unknown callees — so nothing below changes for them,
       which is what keeps the whole valid corpus unaffected. #554. *)
    let ret_origins =
      match unwrap_callee func with
      | ExprVar fn_id ->
        (match Hashtbl.find_opt ctx.fn_sigs fn_id.name with
         | Some sig_ -> sig_.fn_ret_borrow_params
         | None -> [])
      | _ -> []
    in
    (* Check each argument according to parameter ownership.

       Ref/Mut argument borrows are *temporary*: they exist for the duration
       of the call expression only.  AffineScript's value model does not let
       a callee retain a borrow past return, so a call-argument borrow must
       be released once every argument has been checked.  Keeping them live
       until block exit (the old behaviour) was why shared-XOR-exclusive
       could not be enforced at use sites without spuriously rejecting the
       perfectly valid `f(mut x); g(x)`.  We collect the borrows created
       here and end them after the fold. CORE-01 / #177. *)
    (* [escaping] = argument borrows the result carries out (indices in
       [ret_origins]); [temp] = ordinary call-argument borrows released when
       the call completes (the pre-existing behaviour). *)
    let* (escaping, temp) =
      List.fold_left2 (fun acc (i, arg) param_own ->
        let* (esc, tmp) = acc in
        (* First check the argument expression itself *)
        let* () = check_expr ctx state symbols arg in
        (* Then check ownership constraints *)
        match param_own with
        | Some Own ->
          (* Owned parameter - argument must be moved *)
          begin match expr_to_place symbols arg with
          | Some place ->
            let span = expr_span arg in
            let* () = record_move state place span in
            Ok (esc, tmp)
          | None -> Ok (esc, tmp)  (* literals etc. are fine *)
          end
        | Some Ref ->
          (* Borrowed parameter - shared borrow; escaping iff returned *)
          begin match expr_to_place symbols arg with
          | Some place ->
            let span = expr_span arg in
            let* b = record_borrow state place Shared span in
            if List.mem i ret_origins then Ok (b :: esc, tmp)
            else Ok (esc, b :: tmp)
          | None -> Ok (esc, tmp)
          end
        | Some Mut ->
          (* Mutable borrow - exclusive borrow; escaping iff returned *)
          begin match expr_to_place symbols arg with
          | Some place ->
            let span = expr_span arg in
            let* b = record_borrow state place Exclusive span in
            if List.mem i ret_origins then Ok (b :: esc, tmp)
            else Ok (esc, b :: tmp)
          | None -> Ok (esc, tmp)
          end
        | None ->
          (* No ownership annotation - just check usage *)
          Ok (esc, tmp)
      ) (Ok ([], [])) (List.mapi (fun i a -> (i, a)) args) param_ownerships
    in
    (* Release the temporary (non-escaping) call-argument borrows.  The
       escaping ones stay live on [state.borrows] — exactly like a plain `&`
       borrow — and are offered to the result binder via
       [state.result_borrows]; if the result is not bound to a ref-binder
       they linger on [state.borrows] until block exit (the conservative,
       `&`-symmetric outcome). #554. *)
    List.iter (fun b -> end_borrow state b) temp;
    state.result_borrows <- escaping;
    Ok ()

  | ExprLambda lam ->
    (* Collect every free variable referenced in the lambda body —
       variables that appear in the body but are NOT bound by the lambda's
       own parameter list.  Each free variable is "captured" by the closure.

       Borrow-checker contract for captures:
         - We create a Shared borrow for each captured place.  This
           prevents the caller from moving the variable out from under the
           closure while it is still in scope (use-after-move).
         - The borrows expire at the end of the enclosing block thanks to
           the lexical-lifetime clearing in check_block.
         - Ownership / linear duplications are caught by the quantity
           checker (quantity.ml ExprLambda scales captures by QOmega),
           so the borrow checker only needs to prevent structural misuse. *)
    let param_names =
      List.map (fun (p : param) -> p.p_name.name) lam.elam_params
    in
    (* Walk the body collecting every ExprVar name not bound by params. *)
    let rec collect_free (acc : string list) (expr : expr) : string list =
      match expr with
      | ExprVar id ->
        if List.mem id.name param_names || List.mem id.name acc
        then acc
        else id.name :: acc
      | ExprLambda inner ->
        (* For nested lambdas, shadow the inner params too. *)
        let inner_params = List.map (fun (p : param) -> p.p_name.name) inner.elam_params in
        let outer_free = collect_free acc inner.elam_body in
        List.filter (fun n -> not (List.mem n inner_params)) outer_free
      | ExprLit _ | ExprVariant _ -> acc
      | ExprApp (f, args) ->
        List.fold_left collect_free (collect_free acc f) args
      | ExprLet lb ->
        let acc' = collect_free acc lb.el_value in
        (match lb.el_body with Some b -> collect_free acc' b | None -> acc')
      | ExprIf ei ->
        let acc' = collect_free (collect_free acc ei.ei_cond) ei.ei_then in
        (match ei.ei_else with Some e -> collect_free acc' e | None -> acc')
      | ExprMatch em ->
        let acc' = collect_free acc em.em_scrutinee in
        List.fold_left (fun a arm ->
          let a' = match arm.ma_guard with Some g -> collect_free a g | None -> a in
          collect_free a' arm.ma_body
        ) acc' em.em_arms
      | ExprBlock blk ->
        let acc' = List.fold_left (fun a stmt ->
          match stmt with
          | StmtLet sl -> collect_free a sl.sl_value
          | StmtExpr e -> collect_free a e
          | StmtAssign (lhs, _, rhs) -> collect_free (collect_free a lhs) rhs
          | StmtWhile (cond, body) -> collect_free (collect_free a cond) (ExprBlock body)
          | StmtFor (_, iter, body) -> collect_free (collect_free a iter) (ExprBlock body)
        ) acc blk.blk_stmts in
        (match blk.blk_expr with Some e -> collect_free acc' e | None -> acc')
      | ExprBinary (l, _, r) -> collect_free (collect_free acc l) r
      | ExprUnary (_, e) | ExprReturn (Some e) | ExprField (e, _)
      | ExprTupleIndex (e, _) | ExprRowRestrict (e, _) | ExprSpan (e, _) ->
        collect_free acc e
      | ExprReturn None | ExprResume None | ExprUnsafe [] -> acc
      | ExprBreak _ | ExprContinue _ -> acc
      | ExprResume (Some e) -> collect_free acc e
      | ExprTuple es | ExprArray es -> List.fold_left collect_free acc es
      | ExprRecord er ->
        let acc' = List.fold_left (fun a (_, e_opt) ->
          match e_opt with Some e -> collect_free a e | None -> a
        ) acc er.er_fields in
        (match er.er_spread with Some e -> collect_free acc' e | None -> acc')
      | ExprIndex (a, i) -> collect_free (collect_free acc a) i
      | ExprTry et ->
        let acc' = collect_free acc (ExprBlock et.et_body) in
        let acc'' = match et.et_catch with
          | Some arms -> List.fold_left (fun a arm ->
              let a' = match arm.ma_guard with Some g -> collect_free a g | None -> a in
              collect_free a' arm.ma_body) acc' arms
          | None -> acc'
        in
        (match et.et_finally with Some b -> collect_free acc'' (ExprBlock b) | None -> acc'')
      | ExprHandle eh ->
        let acc' = collect_free acc eh.eh_body in
        List.fold_left (fun a arm ->
          match arm with
          | HandlerReturn (_, b) | HandlerOp (_, _, b) -> collect_free a b
        ) acc' eh.eh_handlers
      | ExprUnsafe ops ->
        List.fold_left (fun a op ->
          match op with
          | UnsafeRead e | UnsafeForget e -> collect_free a e
          | UnsafeWrite (e1, e2) | UnsafeOffset (e1, e2) -> collect_free (collect_free a e1) e2
          | UnsafeTransmute (_, _, e) -> collect_free a e
        ) acc ops
    in
    let free_names = collect_free [] lam.elam_body in
    let borrow_span = expr_span (ExprLambda lam) in
    (* CORE-01 pt3 Slice D / #177: reject capture of a @linear binding.
       A closure may be called 0..N times, so capturing a linear
       binding lifts its consumption count out of what the borrow
       checker can prove finite-once.  The quantity checker also
       catches this via [QOmega] scaling, but the borrow-side error
       (a) fires earlier in the pipeline and (b) names the lambda
       span — the actual capture site — rather than a downstream
       "used multiple times" diagnostic. *)
    let* () = List.fold_left (fun acc name ->
      let* () = acc in
      match lookup_symbol_by_name symbols name with
      | Some sym when List.mem sym.Symbol.sym_id state.linear_bindings ->
        Error (LinearCapturedByClosure (name, borrow_span))
      | _ -> Ok ()
    ) (Ok ()) free_names in
    (* Create a Shared borrow for each captured free variable.  If a borrow
       conflict or use-after-move is detected, fail immediately. *)
    let* () = List.fold_left (fun acc name ->
      let* () = acc in
      match expr_to_place symbols (ExprVar { name; span = borrow_span }) with
      | Some place ->
        (* Check the place is not already moved before we borrow it. *)
        let* _borrow = record_borrow state place Shared borrow_span in
        Ok ()
      | None -> Ok ()
    ) (Ok ()) free_names in
    (* Walk the body for structural checking (nested borrows / moves). *)
    check_expr ctx state symbols lam.elam_body

  | ExprLet lb ->
    let* () = check_expr ctx state symbols lb.el_value in
    record_ref_binding state symbols lb.el_pat lb.el_value;
    begin match lb.el_pat with
    | PatVar id ->
      begin match lookup_symbol_by_name symbols id.name with
      | Some sym ->
        state.block_local_syms <- sym.Symbol.sym_id :: state.block_local_syms;
        (* Slice D / #177: record @linear let-bindings for the
           capture-rejection check on subsequent ExprLambda's. *)
        if let_is_linear lb.el_quantity lb.el_ty then
          state.linear_bindings <- sym.Symbol.sym_id :: state.linear_bindings
      | None -> ()
      end
    | _ -> ()
    end;
    begin match lb.el_body with
      | Some body -> check_expr ctx state symbols body
      | None -> Ok ()
    end

  | ExprIf ei ->
    let* () = check_expr ctx state symbols ei.ei_cond in
    (* Save state before branches *)
    let saved_borrows = state.borrows in
    let saved_moved = state.moved in
    (* Check then branch *)
    let* () = check_expr ctx state symbols ei.ei_then in
    let then_borrows = state.borrows in
    let then_moved = state.moved in
    (* Restore state for else branch *)
    state.borrows <- saved_borrows;
    state.moved <- saved_moved;
    (* Check else branch if present *)
    let* () = match ei.ei_else with
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    in
    (* Merge branch states: borrows must be from both branches, moves from either *)
    let else_borrows = state.borrows in
    let else_moved = state.moved in
    (* A borrow is active after if-then-else only if active in both branches *)
    state.borrows <- List.filter (fun b ->
      List.exists (fun b' -> b.b_id = b'.b_id) then_borrows
    ) else_borrows;
    (* A place is moved after if-then-else if moved in either branch *)
    state.moved <- then_moved @ else_moved;
    Ok ()

  | ExprMatch em ->
    let* () = check_expr ctx state symbols em.em_scrutinee in
    (* Each arm is independent: run each against the post-scrutinee state,
       then merge.  A place is moved after the match if it is moved in any
       arm (conservative: we require moves to happen in all arms before
       assuming the value is gone from the outer scope).  Borrows from
       individual arms expire at arm exit. *)
    let base_borrows = state.borrows in
    let base_moved   = state.moved in
    let arm_results = List.map (fun arm ->
      (* Reset to post-scrutinee state for each arm *)
      state.borrows <- base_borrows;
      state.moved   <- base_moved;
      let r =
        let open Result in
        bind (match arm.ma_guard with
          | Some g -> check_expr ctx state symbols g
          | None   -> Ok ())
          (fun () -> check_expr ctx state symbols arm.ma_body)
      in
      (r, state.borrows, state.moved)
    ) em.em_arms in
    (* Propagate the first error, or merge successful states *)
    let errors = List.filter_map (fun (r, _, _) ->
      match r with Error e -> Some e | Ok () -> None) arm_results in
    begin match errors with
    | e :: _ -> Error e
    | [] ->
      (* Borrows: active after match only if active in ALL arms *)
      let all_borrows = List.map (fun (_, bs, _) -> bs) arm_results in
      let merged_borrows = match all_borrows with
        | [] -> base_borrows
        | first :: rest ->
          List.fold_left (fun acc arm_borrows ->
            List.filter (fun b ->
              List.exists (fun b' -> b.b_id = b'.b_id) arm_borrows
            ) acc
          ) first rest
      in
      (* Moves: conservative union — moved in any arm *)
      let all_moves = List.concat_map (fun (_, _, ms) ->
        List.filter (fun mr ->
          not (List.exists (fun base_mr ->
            places_overlap mr.m_place base_mr.m_place
          ) base_moved)
        ) ms
      ) arm_results in
      (* Deduplicate moves by place *)
      let unique_moves = List.fold_left (fun acc mr ->
        if List.exists (fun mr' -> places_overlap mr.m_place mr'.m_place) acc
        then acc
        else mr :: acc
      ) [] all_moves in
      state.borrows <- merged_borrows;
      state.moved   <- base_moved @ unique_moves;
      Ok ()
    end

  | ExprTuple exprs ->
    List.fold_left (fun acc e ->
      let* () = acc in
      check_expr ctx state symbols e
    ) (Ok ()) exprs

  | ExprArray exprs ->
    List.fold_left (fun acc e ->
      let* () = acc in
      check_expr ctx state symbols e
    ) (Ok ()) exprs

  | ExprRecord er ->
    let* () = List.fold_left (fun acc (_id, e_opt) ->
      let* () = acc in
      match e_opt with
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    ) (Ok ()) er.er_fields in
    begin match er.er_spread with
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    end

  | ExprField (base, _) ->
    check_expr ctx state symbols base

  | ExprTupleIndex (base, _) ->
    check_expr ctx state symbols base

  | ExprIndex (arr, idx) ->
    let* () = check_expr ctx state symbols arr in
    check_expr ctx state symbols idx

  | ExprRowRestrict (base, _) ->
    check_expr ctx state symbols base

  | ExprBlock blk ->
    check_block ctx state symbols blk

  | ExprBinary (left, _, right) ->
    let* () = check_expr ctx state symbols left in
    check_expr ctx state symbols right

  | ExprUnary (op, e) ->
    begin match op with
      | OpRef | OpMutRef ->
        (* `&expr` is a shared borrow; `&mut expr` an *exclusive* one.
           This is the surface that finally makes an exclusive borrow
           expressible — shared-XOR-exclusive then enforces it at use
           sites (CORE-01 pt1 `UseWhileExclusivelyBorrowed`). *)
        let kind = (match op with OpMutRef -> Exclusive | _ -> Shared) in
        begin match expr_to_place symbols e with
        | Some place ->
          let span = expr_span e in
          let* _borrow = record_borrow state place kind span in
          Ok ()
        | None ->
          (* Can't borrow non-place expressions, but check the expression *)
          check_expr ctx state symbols e
        end
      | OpDeref ->
        (* Dereferencing: *ptr - just check the pointer expression *)
        check_expr ctx state symbols e
      | _ ->
        check_expr ctx state symbols e
    end

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e ->
        let* () = check_expr ctx state symbols e in
        (* CORE-01 pt2 / #177: a returned reference rooted at a callee-owned
           binding (local or by-value/own param) escapes the frame. The
           let-graph is live here, so `return r` (r = &local/&byval-param)
           is caught — the pt1 tail-only check missed it. *)
        check_return_escape state symbols e
      | None -> Ok ()
    end

  | ExprHandle eh ->
    let* () = check_expr ctx state symbols eh.eh_body in
    (* CFG-join (CORE-01 pt3 Slice C / #177): handler arms are
       mutually exclusive continuations dispatched on whether the
       body returned normally ([HandlerReturn]) or performed an
       effect-op ([HandlerOp]). Each arm runs against the post-body
       state independently; moves/borrows from one arm must NOT
       leak into the next. Mirrors [ExprMatch]'s arm-isolation
       pattern via [merge_arm_results]. *)
    let base_borrows = state.borrows in
    let base_moved = state.moved in
    let arm_results = List.map (fun arm ->
      state.borrows <- base_borrows;
      state.moved <- base_moved;
      let body = match arm with
        | HandlerReturn (_pat, b) -> b
        | HandlerOp (_op, _pats, b) -> b
      in
      let r = check_expr ctx state symbols body in
      (r, state.borrows, state.moved)
    ) eh.eh_handlers in
    merge_arm_results state base_borrows base_moved arm_results

  | ExprResume e_opt ->
    begin match e_opt with
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    end

  (* #459: break/continue carry no expression and own no borrows; safe
     no-ops for the borrow checker (the typecheck loop-context guard is
     what enforces well-formedness). *)
  | ExprBreak _ | ExprContinue _ -> Ok ()

  | ExprTry et ->
    (* CFG-join (CORE-01 pt3 Slice C / #177): body runs first, then
       either succeeds (no-exception path → post-body state
       propagates) or raises (one catch arm runs).  Catch arms are
       alternative continuations from the post-body state, so each
       arm runs against that state independently — moves/borrows
       must not leak between catch arms.  Finally runs
       deterministically against the merged post-catch state. *)
    let* () = check_block ctx state symbols et.et_body in
    let base_borrows = state.borrows in
    let base_moved = state.moved in
    let* () = match et.et_catch with
      | None -> Ok ()
      | Some arms ->
        let arm_results = List.map (fun arm ->
          state.borrows <- base_borrows;
          state.moved <- base_moved;
          let r =
            let open Result in
            bind (match arm.ma_guard with
              | Some g -> check_expr ctx state symbols g
              | None -> Ok ())
              (fun () -> check_expr ctx state symbols arm.ma_body)
          in
          (r, state.borrows, state.moved)
        ) arms in
        merge_arm_results state base_borrows base_moved arm_results
    in
    begin match et.et_finally with
      | Some blk -> check_block ctx state symbols blk
      | None -> Ok ()
    end

  | ExprUnsafe ops ->
    List.fold_left (fun acc op ->
      let* () = acc in
      match op with
      | UnsafeRead e -> check_expr ctx state symbols e
      | UnsafeWrite (e1, e2) ->
        let* () = check_expr ctx state symbols e1 in
        check_expr ctx state symbols e2
      | UnsafeOffset (e1, e2) ->
        let* () = check_expr ctx state symbols e1 in
        check_expr ctx state symbols e2
      | UnsafeTransmute (_, _, e) -> check_expr ctx state symbols e
      | UnsafeForget e -> check_expr ctx state symbols e
    ) (Ok ()) ops

  | ExprVariant _ -> Ok ()

  | ExprSpan (e, _) ->
    check_expr ctx state symbols e

and check_block (ctx : context) (state : state) (symbols : Symbol.t) (blk : block) : unit result =
  (* Snapshot borrows at block entry.  Borrows created inside this block for
     block-local variables expire at block exit (lexical lifetimes).
     Moves persist — a moved value stays moved after the block. *)
  let borrows_at_entry = state.borrows in
  (* Snapshot the borrow-graph scope state so block-local symbols and
     ref-bindings introduced here do not leak past block exit. CORE-01. *)
  let block_locals_at_entry = state.block_local_syms in
  let ref_bindings_at_entry = state.ref_bindings in
  (* NLL last-use pre-pass (CORE-01 pt3 / #177 Slice A): compute the last
     statement index at which each symbol is mentioned. After each
     statement we expire any ref-binding introduced in *this* block whose
     binder's last use has already passed, releasing the underlying
     borrow. This is the non-lexical-lifetimes win — patterns like
     [let r = &x; print( *r); x = 2] now type-check, while the
     anti-aliasing rules remain sound because the borrow is still live
     across statements that *do* use the binder. *)
  let last_use = compute_last_use_index symbols blk in
  let last_use_of (sym : Symbol.symbol_id) : int =
    match Hashtbl.find_opt last_use sym with
    | Some i -> i
    | None -> -1
  in
  let is_outer_binding (sym : Symbol.symbol_id) : bool =
    List.exists (fun (sym', _) -> sym' = sym) ref_bindings_at_entry
  in
  let expire_dead_ref_bindings (stmt_idx : int) : unit =
    let dying, still_live =
      List.partition (fun (sym, _b) ->
        (not (is_outer_binding sym)) && last_use_of sym <= stmt_idx
      ) state.ref_bindings
    in
    (* CORE-01 pt3 ref-to-ref / #177: only end a borrow if no surviving
       ref-binding still aliases it.  Pre-fix, `let r = &x; let r2 = r;`
       would end the borrow on x when r died, even though r2 still held
       it — silently dropping x's protection.  Reference-count the
       borrow by [b_id] across [still_live] before deciding. *)
    List.iter (fun (_sym, b) ->
      if not (List.exists (fun (_, b') -> b'.b_id = b.b_id) still_live)
      then end_borrow state b
    ) dying;
    state.ref_bindings <- still_live
  in
  let* () =
    let indexed = List.mapi (fun i s -> (i, s)) blk.blk_stmts in
    List.fold_left (fun acc (i, stmt) ->
      let* () = acc in
      let* () = check_stmt ctx state symbols stmt in
      expire_dead_ref_bindings i;
      Ok ()
    ) (Ok ()) indexed
  in
  let* () = match blk.blk_expr with
    | Some e -> check_expr ctx state symbols e
    | None   -> Ok ()
  in
  (* Borrow-graph validation (CORE-01 / #177): if the block's value is a
     reference to a place whose owner was declared *inside* this block, the
     reference escapes upward while its owner dies at block exit — a dangling
     borrow.  This is the canonical "returns a reference to a local" bug and
     the trigger that finally emits the long-dead [BorrowOutlivesOwner].
     Sound and non-over-rejecting: a block whose value is `&local` is always
     wrong; valid code returns the value, not a borrow of a dying local. *)
  let symbols_declared_here sym_id =
    List.mem sym_id state.block_local_syms
    && not (List.mem sym_id block_locals_at_entry)
  in
  let* () =
    match blk.blk_expr with
    | Some tail ->
      begin match ref_target symbols tail with
        | Some target ->
          begin match root_var target with
            | Some owner when symbols_declared_here owner ->
              let b =
                match List.find_opt (fun b ->
                  places_overlap b.b_place target) state.borrows with
                | Some b -> b
                | None ->
                  { b_place = target; b_kind = Shared;
                    b_span = expr_span tail; b_id = -1 }
              in
              Error (BorrowOutlivesOwner (b, owner))
            | _ -> Ok ()
          end
        | None -> Ok ()
      end
    | None -> Ok ()
  in
  (* End borrows for places bound in this block: restore to pre-block borrows,
     keeping only those that existed before the block (i.e., borrow outer-scope
     variables that the block merely uses — these are unaffected).
     This is a conservative lexical-lifetime approximation. *)
  state.borrows <- borrows_at_entry;
  state.block_local_syms <- block_locals_at_entry;
  state.ref_bindings <- ref_bindings_at_entry;
  (* Any escaping call-result borrow not claimed by a ref-binder lingered on
     [state.borrows] (like a plain `&` borrow) and is dropped by the restore
     above; clear the transient pointer too. #554. *)
  state.result_borrows <- [];
  Ok ()

and check_stmt (ctx : context) (state : state) (symbols : Symbol.t) (stmt : stmt) : unit result =
  match stmt with
  | StmtLet sl ->
    (* Track mutable bindings *)
    begin match sl.sl_pat with
    | PatVar id ->
      if sl.sl_mut then
        begin match expr_to_place symbols (ExprVar id) with
        | Some place -> state.mutable_bindings <- place :: state.mutable_bindings
        | None -> ()
        end
      else ()
    | _ -> ()
    end;
    let* () = check_expr ctx state symbols sl.sl_value in
    (* Borrow-graph: bind the let symbol to the borrow if RHS is a ref;
       and record the symbol as block-local for outlives validation. *)
    record_ref_binding state symbols sl.sl_pat sl.sl_value;
    begin match sl.sl_pat with
    | PatVar id ->
      begin match lookup_symbol_by_name symbols id.name with
      | Some sym ->
        state.block_local_syms <- sym.Symbol.sym_id :: state.block_local_syms;
        (* Slice D / #177: track @linear let-bindings for the
           capture-rejection check on subsequent ExprLambda's. *)
        if let_is_linear sl.sl_quantity sl.sl_ty then
          state.linear_bindings <- sym.Symbol.sym_id :: state.linear_bindings
      | None -> ()
      end
    | _ -> ()
    end;
    Ok ()
  | StmtExpr e ->
    check_expr ctx state symbols e
  | StmtAssign (lhs, _, rhs) ->
    (* For assignment, LHS must be a mutable place *)
    begin match expr_to_place symbols lhs with
    | Some place ->
      if not (is_mutable state place) then
        Error (CannotBorrowAsMutable (place, expr_span lhs))
      else
        (* CORE-01 pt3 / #177 deferred-items (lib/borrow.ml:1483):
           assignment-clears-move.  A whole-place write (`x = e`,
           LHS is [PlaceVar]) REVIVES the place — the LHS is a write,
           not a read, so a prior move on it must not raise
           [UseAfterMove], and after the RHS lands the move-record
           rooted here is dropped so subsequent uses succeed.
           Sub-place writes (`x.f = e`, `x[i] = e`) keep [check_use]
           because they navigate through the parent place, which must
           still be live.  Unblocks Slice C' loop-soundness. *)
        let is_whole_place_write =
          match place with PlaceVar _ -> true | _ -> false
        in
        let* () =
          if is_whole_place_write then
            match find_aliasing_exclusive state place with
            | Some b ->
              Error (UseWhileExclusivelyBorrowed (place, b, expr_span lhs))
            | None -> Ok ()
          else
            check_use state place (expr_span lhs)
        in
        (* Check for any active borrows of this place *)
        begin match List.find_opt (fun b -> places_overlap place b.b_place) state.borrows with
        | Some borrow ->
          (* Can't assign while borrowed *)
          Error (MoveWhileBorrowed (place, borrow))
        | None ->
          (* Slice B (CORE-01 pt3 / #177): flow-sensitive escape via
             `outer = &y`.  If LHS is a ref-binder symbol that already
             holds a borrow and RHS is a fresh `&y`/`&mut y` reference,
             the assignment *replaces* the held borrow.  We pre-release
             the old held borrow before checking the RHS so the same-
             target reborrow case (`r = &mut x` while `r` already holds
             `&mut x`) does not trip [ConflictingBorrow] on the
             about-to-be-replaced exclusive borrow; we then re-bind the
             ref-graph entry to the freshly-created borrow.  Mirrors
             [record_ref_binding]'s let-graph contract for the
             assignment path so the NLL last-use + return-escape
             analyses see the *current* referent after re-assignment,
             not the stale original. *)
          (* Self-assign `r = r` guard (#177 follow-up to #395): without
             this, [is_reborrow_source] reports true for the ref-binder
             LHS=RHS case, pre_release ends `r`'s borrow and removes the
             binding, then post-rebind calls [ref_source_borrow] which
             now finds `r` unbound and returns None — net effect is `r`
             silently stripped from the borrow-graph. *)
          let rhs_is_self_binder =
            match root_var place with
            | Some binder_sym ->
              let rec peel = function ExprSpan (x, _) -> peel x | x -> x in
              (match peel rhs with
               | ExprVar id ->
                 (match lookup_symbol_by_name symbols id.name with
                  | Some sym -> sym.Symbol.sym_id = binder_sym
                  | None -> false)
               | _ -> false)
            | None -> false
          in
          let pre_release =
            match root_var place with
            | Some binder_sym
              when not rhs_is_self_binder
                && is_reborrow_source state symbols rhs
                && List.mem_assoc binder_sym state.ref_bindings ->
              (* Ref-count the released loan(s) by [b_id]: only [end_borrow] an
                 old borrow if no surviving ref-binding (e.g. a [let r2 = r]
                 alias) still holds it — mirrors [expire_dead_ref_bindings] and
                 the #554 (c) call-result reassign block.  Pre-fix this ended
                 the borrow unconditionally, so [let r2 = r; r = &b] dropped the
                 loan [r2] still names and accepted a later use-after-move of
                 the old target.  (Found by #554 round-3 adversarial review;
                 the bug predates #554 but the (c) block made the asymmetry
                 visible.) *)
              let old_entries, remaining =
                List.partition (fun (s, _) -> s = binder_sym) state.ref_bindings
              in
              List.iter (fun (_, ob) ->
                if not (List.exists (fun (_, b') -> b'.b_id = ob.b_id) remaining)
                then end_borrow state ob) old_entries;
              state.ref_bindings <- remaining;
              Some binder_sym
            | _ -> None
          in
          let* () = check_expr ctx state symbols rhs in
          if is_whole_place_write then
            state.moved <-
              List.filter (fun mr -> not (places_overlap mr.m_place place))
                state.moved;
          (match pre_release with
           | Some binder_sym ->
             (match ref_source_borrow state symbols rhs with
              | Some new_b ->
                state.ref_bindings <-
                  (binder_sym, new_b) :: state.ref_bindings
              | None -> ())
           | None -> ());
          (* #554 residual (c): [r = f(b)] reassigning an existing ref-binder
             to a call whose result borrows [b].  The `&p`/ref-var reborrow
             path above does not fire ([is_reborrow_source] only matches those
             shapes), so release the binder's OLD loan here (ref-counted by
             [b_id] so a borrow another binder still aliases is kept) and
             rebind it to the escaping call-result borrows — mirroring
             [record_ref_binding]'s claim and the Slice-B `&` reborrow, so a
             later use of the old target is no longer spuriously rejected. *)
          (match root_var place with
           | Some binder_sym ->
             let rec peel = function ExprSpan (x, _) -> peel x | x -> x in
             (match peel rhs with
              | ExprApp _ when state.result_borrows <> [] ->
                let old, remaining =
                  List.partition (fun (s, _) -> s = binder_sym) state.ref_bindings
                in
                List.iter (fun (_, ob) ->
                  if not (List.exists (fun (_, b') -> b'.b_id = ob.b_id) remaining)
                  then end_borrow state ob) old;
                state.ref_bindings <-
                  List.fold_left (fun acc b -> (binder_sym, b) :: acc)
                    remaining state.result_borrows;
                state.result_borrows <- []
              | _ -> ())
           | None -> ());
          Ok ()
        end
    | None ->
      (* LHS is not a place (e.g., function call result), check both sides *)
      let* () = check_expr ctx state symbols lhs in
      check_expr ctx state symbols rhs
    end
  | StmtWhile (cond, body) ->
    (* CORE-01 pt3 Slice C' / #177 — loop soundness via 2-iteration.
       A single body pass misses multi-iter conflicts: a move at iter
       1 that the body never restores would, at iter 2, be a
       UseAfterMove.  We run cond+body twice; iter 2 starts from the
       post-iter-1 state, so an unrestored move surfaces as
       UseAfterMove on the second pass.  Pairs with the StmtAssign
       clear-on-rewrite fix (#399, [is_whole_place_write]) so loops
       that legitimately re-initialise a moved-out variable per
       iteration still converge — iter-2's read of the freshly-
       reassigned place is not a UseAfterMove because iter 1's
       assignment cleared the move record.  After both passes we
       restore state to the iter-1-post snapshot so any analysis
       past the loop sees a single iter's worth of state (the loop
       may execute 0..N times — using iter-1-post is the sound
       choice when iter-2 doesn't add new conflicts). *)
    let* () = check_expr ctx state symbols cond in
    let* () = check_block ctx state symbols body in
    let snap_borrows = state.borrows in
    let snap_moved = state.moved in
    let snap_ref_bindings = state.ref_bindings in
    let snap_block_locals = state.block_local_syms in
    let snap_mutable = state.mutable_bindings in
    let r =
      let* () = check_expr ctx state symbols cond in
      check_block ctx state symbols body
    in
    state.borrows <- snap_borrows;
    state.moved <- snap_moved;
    state.ref_bindings <- snap_ref_bindings;
    state.block_local_syms <- snap_block_locals;
    state.mutable_bindings <- snap_mutable;
    r
  | StmtFor (_pat, iter, body) ->
    (* CORE-01 pt3 Slice C' / #177 — same 2-iteration pass as
       [StmtWhile]; see comment there for rationale. *)
    let* () = check_expr ctx state symbols iter in
    let* () = check_block ctx state symbols body in
    let snap_borrows = state.borrows in
    let snap_moved = state.moved in
    let snap_ref_bindings = state.ref_bindings in
    let snap_block_locals = state.block_local_syms in
    let snap_mutable = state.mutable_bindings in
    let r =
      let* () = check_expr ctx state symbols iter in
      check_block ctx state symbols body
    in
    state.borrows <- snap_borrows;
    state.moved <- snap_moved;
    state.ref_bindings <- snap_ref_bindings;
    state.block_local_syms <- snap_block_locals;
    state.mutable_bindings <- snap_mutable;
    r

(** Check a function

    Parameters declared with [mut] ownership are seeded into
    [mutable_bindings] so the body may assign through them (e.g.
    [out[i] = expr] for a buffer parameter). Without this, the assignment
    check at [StmtAssign] rejects all writes through parameters even when
    the surface syntax explicitly opted in with [mut]. *)
let check_function ?(extra_mut_bindings = []) (ctx : context) (symbols : Symbol.t) (fd : fn_decl) : unit result =
  let state = create () in
  (* #548: seed module-level `const mut` bindings so the borrow checker
     treats them as mutable. Empty for callers that don't supply them
     so existing call sites (tests, lsp, repl) keep their semantics. *)
  state.mutable_bindings <- extra_mut_bindings @ state.mutable_bindings;
  List.iter (fun (p : param) ->
    let own = param_ownership p in
    (match lookup_symbol_by_name symbols p.p_name.name with
     | Some sym ->
       if own = Some Mut then
         state.mutable_bindings <-
           PlaceVar (p.p_name.name, sym.sym_id) :: state.mutable_bindings;
       (* Callee-owned params (by-value [None] or [Some Own]); [ref]/[mut]
          are caller-owned referents and stay out. CORE-01 pt2 / #177. *)
       (match own with
        | None | Some Own ->
          state.callee_owned_params <-
            sym.sym_id :: state.callee_owned_params
        | Some Ref | Some Mut -> ());
       (* Slice D / #177: track @linear-annotated params for the
          capture-rejection check on subsequent ExprLambda's. *)
       if p.p_quantity = Some QOne then
         state.linear_bindings <-
           sym.sym_id :: state.linear_bindings
     | None -> ())
  ) fd.fd_params;
  match fd.fd_body with
  | FnBlock blk ->
    let* () = check_block ctx state symbols blk in
    (* Implicit-tail return of a *direct* `&param` (no `let`, no `return`):
       the pt1 block-tail check only knew block-locals, not by-value params.
       `return`-statement escapes are caught inline in ExprReturn. *)
    (match blk.blk_expr with
     | Some tail -> check_return_escape state symbols tail
     | None -> Ok ())
  | FnExpr e ->
    let* () = check_expr ctx state symbols e in
    check_return_escape state symbols e
  | FnExtern -> Ok ()  (* No body to borrow-check *)

(** Check a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  (* Build context with all function signatures *)
  let ctx = build_context program in
  (* #548: collect module-level `const mut <name>` bindings so each
     function's borrow state knows they are mutable. Reads and writes
     to these go through the same {!StmtAssign} path used for
     function-scope `let mut`; without this seed every write to a
     module-mut binding would be rejected by [is_mutable] just as
     function-scope writes to non-mut [let] bindings are.
     [lookup_symbol_by_name] reaches the global symbol table populated
     by {!Symbol} during resolution, so the [sym_id] matches the one
     [expr_to_place] resolves an [ExprVar] to in {!check_expr}. *)
  let module_mut_bindings : place list =
    List.fold_left (fun acc decl ->
      match decl with
      | TopConst { tc_mut = true; tc_name; _ } ->
        (match lookup_symbol_by_name symbols tc_name.name with
         | Some sym -> PlaceVar (tc_name.name, sym.sym_id) :: acc
         | None -> acc)
      | _ -> acc
    ) [] program.prog_decls
  in
  (* Check each function *)
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | TopFn fd -> check_function ~extra_mut_bindings:module_mut_bindings ctx symbols fd
      | _ -> Ok ()
  ) (Ok ()) program.prog_decls

(* CORE-01 / #177 — borrow-graph validation landed (2026-05-19):
   - call-argument Ref/Mut borrows are now temporary (released after the
     call), making the lexical model precise instead of over-conservative;
   - shared-XOR-exclusive is enforced at *use* sites (find_aliasing_exclusive
     in check_use), not only at borrow creation;
   - the reference-binding graph (state.ref_bindings) is tracked for
     [let r = &p] and scoped to its block;
   - [BorrowOutlivesOwner] is finally emitted: a block whose value is a
     reference to an owner declared inside that block is a dangling borrow.
   record_move / record_borrow / end_borrow are all live now.

   CORE-01 pt2 (2026-05-19): return-escape. A `return e` (or fn-tail)
   whose value is a reference rooted at a callee-owned binding — a function
   local or a by-value/[own] parameter — is now [BorrowOutlivesOwner]
   (check_return_escape, state.callee_owned_params). pt1 only inspected
   block tails, so `return r` (r = &local/&by-value-param) slipped through.
   [ref]/[mut] params are caller-owned referents and are intentionally not
   flagged (probed: `fn ok(x: ref Int) -> ref Int { return x; }` passes).
   Sound + non-over-rejecting (full stdlib AOT + borrow suite green).

   CORE-01 pt2 parser surface (2026-05-19): `&mut e` now parses
   (ExprUnary OpMutRef; `AMP MUT e`, zero Menhir conflict delta) and is an
   *Exclusive* borrow here — so shared-XOR-exclusive + return-escape are
   reachable from real source for the first time. `&`-in-`#{` literals and
   bare block-statements already parsed; the `-> &T`/`&T` *type* sigil was
   deliberately not added (`ref T`/`mut T` keyword types already exist).

   CORE-01 pt3 Slice A (NLL last-use): ref-bindings introduced in a
   block now expire at the binder's *last use*, not at block exit. A
   forward pre-pass (compute_last_use_index) maps each symbol to the
   greatest statement index that mentions it (the tail expression
   counts as the n-th statement); after each statement, check_block
   releases the underlying borrow of any in-block ref-binding whose
   binder is now dead. Unblocks the canonical NLL patterns
   ([let r = &x; print( *r); x = 2] and [let m = &mut x; let y = *m; x]),
   while still rejecting real conflicts (the anti-aliasing rules fire
   against statements that *do* use the binder, before expiry).
   Outer-block ref-bindings are deliberately preserved — they expire
   only at their own block's exit.

   CORE-01 pt3 Slice B (flow-sensitive escape via re-assignment):
   `outer = &y` now updates the borrow graph the way `let outer = &y`
   does. In StmtAssign, when LHS is a ref-binder symbol with a held
   borrow and RHS is a direct `&p`/`&mut p`, the old borrow is
   pre-released (so a same-target reborrow like `r = &mut x` while
   `r` already holds `&mut x` does not trip ConflictingBorrow on the
   about-to-be-replaced exclusive borrow), then after the RHS check
   the (binder -> new_borrow) ref-graph entry is re-bound to the
   freshly-created borrow. NLL last-use + return-escape now see the
   *current* referent after re-assignment, not the stale original.

   CORE-01 pt3 Slice C (CFG-join semantics for non-match join
   constructs): [ExprHandle] handler arms and [ExprTry] catch arms
   are mutually exclusive continuations from the post-body state.
   Previously both were checked sequentially against a shared
   state, so moves/borrows from arm i polluted arm i+1 — a
   spurious UseAfterMove on the second of two arms that
   independently move the same value, etc.  Both now snapshot the
   post-body state, run each arm independently, and merge via
   [merge_arm_results] (same intersection-borrows / union-moves
   semantics already used inline by [ExprMatch], now extracted to
   one helper so all CFG-join sites agree).  Finally runs after
   the merge, deterministically, against the merged state.

   CORE-01 pt3 ref-to-ref binding (2026-05-26): [let r2 = r] and
   [r2 = r] (where [r] is itself a ref-binder) now extend the
   borrow-graph by aliasing [r2] to the same borrow [r] holds.  The
   [ref_source_borrow] helper unifies the &p / &mut p / ref-var cases
   so [record_ref_binding] (let path) and the [StmtAssign] reborrow
   block both reach through one extra level of indirection.  Symmetric
   pre-release on the assign path uses the same shape test
   ([is_reborrow_source]) so the existing `r = &x` reborrow continues
   to work; the new path is `r = r_other`.  Closes the
   reborrow-through-indirection gap in #177 / CORE-01 pt3.

   CORE-01 pt3 Slice C' (2026-05-27): loop soundness via a 2-iteration
   check on [StmtWhile]/[StmtFor].  The body runs once; then state-
   fields snapshot; then cond+body run again from the post-iter-1
   state.  Any move that the body didn't restore (no rebinding
   assignment) surfaces as UseAfterMove on the 2nd pass.  Pairs with
   the StmtAssign clear-on-rewrite from #399 ([is_whole_place_write])
   so legitimate re-init loops still accept while loops with an
   unrestored body-move reject.

   CORE-01 pt3 Slice D (2026-05-26): borrow-side rejection of
   @linear-binding capture by a closure.  A closure may be called
   0..N times, so capturing a [@linear] (`QOne`) binding lifts its
   consumption count out of what the borrow checker can prove
   finite-once.  The quantity checker also rejects this via [QOmega]
   scaling of lambda captures (quantity.ml ExprLambda), but the
   borrow-side error fires earlier in the pipeline (Typecheck →
   Borrow → Quantity) and names the *lambda* span as the capture
   site rather than producing a downstream "used multiple times"
   diagnostic. [state.linear_bindings] is populated from
   [@linear] / `Cmd _` annotations on params, `let`-statements, and
   `let`-expressions; [ExprLambda] then rejects free-vars whose
   sym-id is in that set with [LinearCapturedByClosure].

   Callee-returned-borrow soundness (2026-06-14, #554): a call whose
   result borrows one of its arguments now registers that borrow-graph
   edge.  Each function carries a *return-borrow summary*
   ([fn_ret_borrow_params], computed by [compute_ret_borrow_params]):
   the parameter indices whose borrow may flow out through the return
   value (a returned [&p] / [return p] for a [ref]/[mut] parameter [p],
   directly, via a let-bound ref-local chain, or via an [if]/[match]/block
   *tail* in return position — see [walk_tail]).  At a call site the
   argument borrows named by the summary are kept *live* instead of being
   released at call end.  They behave in every respect like a plain `&`
   borrow: [record_ref_binding] claims a directly-bound result as the
   binder's ref-graph edge ([let r = pick(a)] → NLL last-use governs it),
   and an unclaimed result (flowed into a tuple/record/array, dereferenced,
   or discarded) simply lingers on [state.borrows] until lexical block exit.
   So [let r = pick(a); consume(a); *r] and the aggregate form
   [let t = (pick(a), 0); consume(a); *(t.0)] are both [MoveWhileBorrowed],
   while NLL still accepts the legitimate reorderings.  Closes the
   probe-verified false negative behind the CORE-01 stamp; adversarially
   re-verified across fields/paths, &mut, branches/loops, reassignment,
   multi-arg, and aggregates (the aggregate + assign-path + branch-tail-
   summary holes a first cut missed were all found and closed).

   Follow-up (also #554) closed two of the three named residuals:
   - *interprocedural-through-call-result* — [compute_ret_borrow_params] now
     resolves a returned call result through the callee's summary, and
     [build_context] drives all summaries to a monotone fixpoint, so
     [fn wrap(x: ref Int) -> ref Int { let t = pick(x); return t; }] inherits
     pick's origin and [let r = wrap(a); consume(a); *r] is rejected
     (transitively, through any depth of wrappers).
   - *reassignment precision* — [r = f(b)] now releases the binder's prior
     loan (ref-counted by [b_id]) and rebinds to the escaping call result,
     so a later use of the old target is no longer spuriously rejected —
     symmetric with the plain-`&` Slice-B reborrow.
   - *round-3 hardening* — the Slice-B `&` reassign path now ref-counts the
     released loan by [b_id] too, so [let r2 = r; r = &b] keeps the borrow
     [r2] still aliases (a *pre-existing* soundness bug the (c) block exposed
     by being more correct); and the summary walker UNIONs a reassigned
     ref-local's origins, so [let mut t = pick(y); t = pick(x); return t]
     summarises {x,y} rather than the stale {y}.

   Still deferred:
   - Origin/region variables (true Polonius surface) — a region
     var on each [TyRef]/[TyMut] with subset constraints and a
     proper datalog-style loan-live-at-point solver.  Architectural
     change to the type system; ADR-gated.  See ADR-022
     (docs/decisions/0022-polonius-origin-variables.adoc) for the
     M1-M4 migration plan; lexical checker is the merge oracle
     through M3.
   - #554 remaining residual (closed properly by the Polonius origins
     above, #553): *branch-merged / copied-out claim* — a borrow threaded
     through an [if]/[match] arm or block tail at the BIND site
     ([let r = if c { pick(a) } else { pick(b) }], [let r = { pick(a) }]),
     or copied out of its binder into an aggregate before the binder's last
     use ([let r = pick(a); let t = (r, 0); …]), is not protected, because
     the arm-merge / block-exit drops the branch-local borrow and NLL
     expires the binder at the copy site.  This is NOT a new asymmetry: the
     byte-identical plain-`&` programs ([let r = if c { &a } else { &b }],
     [let t = (r, 0)] with [r = &a]) behave identically (both accept) — the
     same pre-existing through-branch / copy-out lexical limitation,
     inherited by the call-result path, not introduced by it.  A true
     flow-sensitive borrow graph (Polonius) discharges it uniformly.
   - #554 minor residuals (round-3, both low-severity; Polonius #553 closes
     them):
       * *Divergent self/mutual recursion with no base case* — a function
         whose only ref-return is its own recursive call (e.g.
         [fn f(ref x) -> ref Int { let t = f(x); return t; }]) gets an empty
         summary (the fixpoint cannot bootstrap an origin from a purely
         self-transitive return), so a use-after-move through its result is
         accepted.  Unreachable in practice: such a function never returns,
         so the use never executes.  Any *terminating* recursion has a
         non-recursive return path, which IS summarised.
       * *Reassign inside a loop body to an outer borrow* —
         [let mut r = pick(a); while … { r = pick(b) } consume(b); *r] is
         accepted because the loop-body block drops its borrows at block exit
         (predates #554, `&`-symmetric).
   - Tighter integration with the quantity checker for captured
     linears (Slice D).
*)
