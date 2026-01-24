(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
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
  | PlaceVar of Symbol.symbol_id
  | PlaceField of place * string
  | PlaceIndex of place * int option  (** None for dynamic index *)
  | PlaceDeref of place
[@@deriving show]

(** Borrow kind *)
type borrow_kind =
  | Shared     (** Immutable borrow (&) *)
  | Exclusive  (** Mutable borrow (&mut) *)
[@@deriving show, eq]

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
}

(** Borrow checker errors *)
type borrow_error =
  | UseAfterMove of place * Span.t * Span.t  (** place, use site, move site *)
  | ConflictingBorrow of borrow * borrow
  | BorrowOutlivesOwner of borrow * Symbol.symbol_id
  | MoveWhileBorrowed of place * borrow
  | CannotMoveOutOfBorrow of place * borrow
  | CannotBorrowAsMutable of place * Span.t
[@@deriving show]

type 'a result = ('a, borrow_error) Result.t

(* Result bind - define before use *)
let ( let* ) = Result.bind

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
  }

(** Add a function signature to context *)
let add_fn_signature (ctx : context) (fd : fn_decl) : unit =
  let sig_ = {
    fn_name = fd.fd_name.name;
    fn_param_ownerships = List.map (fun p -> p.p_ownership) fd.fd_params;
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
      | TyApp (id, _) when id.name = "Ref" -> true  (* Shared references are Copy *)
      | _ -> false  (* Records, arrays, owned types, etc. are not Copy *)
    end

(** Check if an expression has a Copy type (heuristic for literals) *)
let is_copy_expr (expr : expr) : bool =
  match expr with
  | ExprLit (LitInt _) -> true
  | ExprLit (LitBool _) -> true
  | ExprLit (LitChar _) -> true
  | ExprLit (LitUnit _) -> true
  | ExprUnary (OpRef, _) -> true  (* Reference creation produces a Copy pointer *)
  | _ -> false

(** Check if two places overlap *)
let rec places_overlap (p1 : place) (p2 : place) : bool =
  match (p1, p2) with
  | (PlaceVar v1, PlaceVar v2) -> v1 = v2
  | (PlaceField (base1, _), PlaceField (base2, _)) ->
    places_overlap base1 base2
  | (PlaceVar _, PlaceField (base, _))
  | (PlaceField (base, _), PlaceVar _) ->
    places_overlap p1 base || places_overlap base p2
  | (PlaceDeref p1', PlaceDeref p2') ->
    places_overlap p1' p2'
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
    let* () = begin match kind with
    | Exclusive ->
      if not (is_mutable state place) then
        Error (CannotBorrowAsMutable (place, span))
      else
        Ok ()
    | Shared -> Ok ()
    end in
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

(** Check a use of a place *)
let check_use (state : state) (place : place) (span : Span.t) : unit result =
  match find_move state place with
  | Some move_site -> Error (UseAfterMove (place, span, move_site))
  | None -> Ok ()

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

(** Convert an expression to a place (if it's an l-value) *)
let rec expr_to_place (symbols : Symbol.t) (expr : expr) : place option =
  match expr with
  | ExprVar id ->
    begin match lookup_symbol_by_name symbols id.name with
      | Some sym -> Some (PlaceVar sym.sym_id)
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
    (* Check if this is a direct function call and get ownership info *)
    let param_ownerships =
      match func with
      | ExprVar fn_id ->
        begin match Hashtbl.find_opt ctx.fn_sigs fn_id.name with
        | Some sig_ -> sig_.fn_param_ownerships
        | None -> List.map (fun _ -> None) args  (* Unknown function, assume no ownership *)
        end
      | _ -> List.map (fun _ -> None) args  (* Higher-order call, assume no ownership *)
    in
    (* Check each argument according to parameter ownership *)
    List.fold_left2 (fun acc arg param_own ->
      let* () = acc in
      (* First check the argument expression itself *)
      let* () = check_expr ctx state symbols arg in
      (* Then check ownership constraints *)
      match param_own with
      | Some Own ->
        (* Owned parameter - argument must be moved *)
        begin match expr_to_place symbols arg with
        | Some place ->
          let span = expr_span arg in
          record_move state place span
        | None -> Ok ()  (* Non-place expressions (literals, etc.) are fine *)
        end
      | Some Ref ->
        (* Borrowed parameter - create a shared borrow *)
        begin match expr_to_place symbols arg with
        | Some place ->
          let span = expr_span arg in
          let* _borrow = record_borrow state place Shared span in
          Ok ()
        | None -> Ok ()
        end
      | Some Mut ->
        (* Mutable borrow - create an exclusive borrow *)
        begin match expr_to_place symbols arg with
        | Some place ->
          let span = expr_span arg in
          let* _borrow = record_borrow state place Exclusive span in
          Ok ()
        | None -> Ok ()
        end
      | None ->
        (* No ownership annotation - just check usage *)
        Ok ()
    ) (Ok ()) args param_ownerships

  | ExprLambda lam ->
    check_expr ctx state symbols lam.elam_body

  | ExprLet lb ->
    let* () = check_expr ctx state symbols lb.el_value in
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
    List.fold_left (fun acc arm ->
      let* () = acc in
      let* () = match arm.ma_guard with
        | Some g -> check_expr ctx state symbols g
        | None -> Ok ()
      in
      check_expr ctx state symbols arm.ma_body
    ) (Ok ()) em.em_arms

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
      | OpRef ->
        (* Taking a reference: &expr - creates a shared borrow *)
        begin match expr_to_place symbols e with
        | Some place ->
          let span = expr_span e in
          let* _borrow = record_borrow state place Shared span in
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
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    end

  | ExprHandle eh ->
    let* () = check_expr ctx state symbols eh.eh_body in
    List.fold_left (fun acc arm ->
      let* () = acc in
      match arm with
      | HandlerReturn (_pat, body) -> check_expr ctx state symbols body
      | HandlerOp (_op, _pats, body) -> check_expr ctx state symbols body
    ) (Ok ()) eh.eh_handlers

  | ExprResume e_opt ->
    begin match e_opt with
      | Some e -> check_expr ctx state symbols e
      | None -> Ok ()
    end

  | ExprTry et ->
    let* () = check_block ctx state symbols et.et_body in
    let* () = match et.et_catch with
      | Some arms ->
        List.fold_left (fun acc arm ->
          let* () = acc in
          let* () = match arm.ma_guard with
            | Some g -> check_expr ctx state symbols g
            | None -> Ok ()
          in
          check_expr ctx state symbols arm.ma_body
        ) (Ok ()) arms
      | None -> Ok ()
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
      | UnsafeAssume _ -> Ok ()
    ) (Ok ()) ops

  | ExprVariant _ -> Ok ()

  | ExprSpan (e, _) ->
    check_expr ctx state symbols e

and check_block (ctx : context) (state : state) (symbols : Symbol.t) (blk : block) : unit result =
  let* () = List.fold_left (fun acc stmt ->
    let* () = acc in
    check_stmt ctx state symbols stmt
  ) (Ok ()) blk.blk_stmts in
  match blk.blk_expr with
  | Some e -> check_expr ctx state symbols e
  | None -> Ok ()

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
    check_expr ctx state symbols sl.sl_value
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
          (* Assignment is ok, check the RHS *)
          check_expr ctx state symbols rhs
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

(** Check a function *)
let check_function (ctx : context) (symbols : Symbol.t) (fd : fn_decl) : unit result =
  let state = create () in
  match fd.fd_body with
  | FnBlock blk -> check_block ctx state symbols blk
  | FnExpr e -> check_expr ctx state symbols e

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

(* Silence unused warnings for functions that will be used in later phases *)
let _ = record_move
let _ = record_borrow
let _ = end_borrow

(* Phase 3 (borrow checking) partially complete. Future enhancements:
   - Non-lexical lifetimes with region inference (Phase 3)
   - Dataflow analysis for precise tracking (Phase 3)
   - Integration with quantity checking (Phase 3)
*)
