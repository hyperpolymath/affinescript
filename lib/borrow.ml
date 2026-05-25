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
  }

(** Add a function signature to context *)
let add_fn_signature (ctx : context) (fd : fn_decl) : unit =
  let sig_ = {
    fn_name = fd.fd_name.name;
    fn_param_ownerships = List.map param_ownership fd.fd_params;
  } in
  Hashtbl.replace ctx.fn_sigs fd.fd_name.name sig_

(** Build context from program *)
let build_context (program : program) : context =
  let ctx = create_context () in
  List.iter (fun decl ->
    match decl with
    | TopFn fd -> add_fn_signature ctx fd
    | _ -> ()
  ) program.prog_decls;
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

(** Record a borrow-graph edge for [let <pat> = &p] (or [&mut p]).

    Call *after* the value expression has been checked, so the borrow it
    created is already on [state.borrows]. Binds the let-bound symbol to
    that borrow so block-exit validation can detect a reference outliving
    a block-local owner. CORE-01 / #177. *)
let record_ref_binding (state : state) (symbols : Symbol.t)
    (pat : pattern) (value : expr) : unit =
  match pat with
  | PatVar id ->
    begin match ref_target symbols value with
    | Some target ->
      begin match
        List.find_opt (fun b -> places_overlap b.b_place target) state.borrows,
        lookup_symbol_by_name symbols id.name
      with
      | Some b, Some sym ->
        state.ref_bindings <- (sym.Symbol.sym_id, b) :: state.ref_bindings
      | _ -> ()
      end
    | None -> ()
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
    (* Check each argument according to parameter ownership.

       Ref/Mut argument borrows are *temporary*: they exist for the duration
       of the call expression only.  AffineScript's value model does not let
       a callee retain a borrow past return, so a call-argument borrow must
       be released once every argument has been checked.  Keeping them live
       until block exit (the old behaviour) was why shared-XOR-exclusive
       could not be enforced at use sites without spuriously rejecting the
       perfectly valid `f(mut x); g(x)`.  We collect the borrows created
       here and end them after the fold. CORE-01 / #177. *)
    let* call_borrows =
      List.fold_left2 (fun acc arg param_own ->
        let* created = acc in
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
            Ok created
          | None -> Ok created  (* literals etc. are fine *)
          end
        | Some Ref ->
          (* Borrowed parameter - temporary shared borrow *)
          begin match expr_to_place symbols arg with
          | Some place ->
            let span = expr_span arg in
            let* b = record_borrow state place Shared span in
            Ok (b :: created)
          | None -> Ok created
          end
        | Some Mut ->
          (* Mutable borrow - temporary exclusive borrow *)
          begin match expr_to_place symbols arg with
          | Some place ->
            let span = expr_span arg in
            let* b = record_borrow state place Exclusive span in
            Ok (b :: created)
          | None -> Ok created
          end
        | None ->
          (* No ownership annotation - just check usage *)
          Ok created
      ) (Ok []) args param_ownerships
    in
    (* Release the temporary call-argument borrows. *)
    List.iter (fun b -> end_borrow state b) call_borrows;
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
    (* Create a Shared borrow for each captured free variable.  If a borrow
       conflict or use-after-move is detected, fail immediately. *)
    let borrow_span = expr_span (ExprLambda lam) in
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
        state.block_local_syms <- sym.Symbol.sym_id :: state.block_local_syms
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
    List.iter (fun (_sym, b) -> end_borrow state b) dying;
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
        state.block_local_syms <- sym.Symbol.sym_id :: state.block_local_syms
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
        (* Check that the place is not moved and not borrowed *)
        let* () = check_use state place (expr_span lhs) in
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
          let pre_release =
            match root_var place, ref_target symbols rhs with
            | Some binder_sym, Some _
              when List.mem_assoc binder_sym state.ref_bindings ->
              let old_borrow = List.assoc binder_sym state.ref_bindings in
              end_borrow state old_borrow;
              state.ref_bindings <-
                List.filter (fun (s, _) -> s <> binder_sym) state.ref_bindings;
              Some binder_sym
            | _ -> None
          in
          let* () = check_expr ctx state symbols rhs in
          (match pre_release, ref_target symbols rhs with
           | Some binder_sym, Some new_target ->
             (match List.find_opt (fun b ->
                      places_overlap b.b_place new_target) state.borrows with
              | Some new_b ->
                state.ref_bindings <-
                  (binder_sym, new_b) :: state.ref_bindings
              | None -> ())
           | _ -> ());
          Ok ()
        end
    | None ->
      (* LHS is not a place (e.g., function call result), check both sides *)
      let* () = check_expr ctx state symbols lhs in
      check_expr ctx state symbols rhs
    end
  | StmtWhile (cond, body) ->
    let* () = check_expr ctx state symbols cond in
    check_block ctx state symbols body
  | StmtFor (_pat, iter, body) ->
    let* () = check_expr ctx state symbols iter in
    check_block ctx state symbols body

(** Check a function

    Parameters declared with [mut] ownership are seeded into
    [mutable_bindings] so the body may assign through them (e.g.
    [out[i] = expr] for a buffer parameter). Without this, the assignment
    check at [StmtAssign] rejects all writes through parameters even when
    the surface syntax explicitly opted in with [mut]. *)
let check_function (ctx : context) (symbols : Symbol.t) (fd : fn_decl) : unit result =
  let state = create () in
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
        | Some Ref | Some Mut -> ())
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
  (* Check each function *)
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | TopFn fd -> check_function ctx symbols fd
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

   Still deferred:
   - Reborrow through indirection: `r = some_other_ref_var` (RHS not a
     direct `&place`) does not yet copy the other binder's graph
     entry — the ref-binding stays stale.  Same limitation as
     [record_ref_binding] for the let-graph path; would require
     symmetric let/assign handling for ref-to-ref binding.
   - Origin/region variables (true Polonius surface) — a region
     var on each [TyRef]/[TyMut] with subset constraints and a
     proper datalog-style loan-live-at-point solver.  Architectural
     change to the type system; ADR-gated.
   - Loop soundness (`StmtWhile`/`StmtFor`): a single body pass
     misses multi-iteration move conflicts.  A 2-iteration check
     would catch them but requires fixing the assignment-clears-
     move imprecision first (assignment is currently treated as a
     read of LHS, so `x = …` after a prior move spuriously fails).
     Couple Slice C' with the StmtAssign clear-on-rewrite fix.
   - Tighter integration with the quantity checker for captured
     linears (Slice D).
*)
