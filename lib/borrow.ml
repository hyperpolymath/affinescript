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

(** Borrow checker state *)
type state = {
  (** Active borrows *)
  mutable borrows : borrow list;

  (** Moved places *)
  mutable moved : place list;

  (** Next borrow ID *)
  mutable next_id : int;
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

(** Create a new borrow checker state *)
let create () : state =
  {
    borrows = [];
    moved = [];
    next_id = 0;
  }

(** Generate a fresh borrow ID *)
let fresh_id (state : state) : int =
  let id = state.next_id in
  state.next_id <- id + 1;
  id

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

(** Check if a place is moved *)
let is_moved (state : state) (place : place) : bool =
  List.exists (fun moved_place -> places_overlap place moved_place) state.moved

(** Check if a borrow conflicts with existing borrows *)
let find_conflicting_borrow (state : state) (new_borrow : borrow) : borrow option =
  List.find_opt (fun existing ->
    places_overlap new_borrow.b_place existing.b_place &&
    (new_borrow.b_kind = Exclusive || existing.b_kind = Exclusive)
  ) state.borrows

(** Record a move *)
let record_move (state : state) (place : place) (_span : Span.t) : unit result =
  (* Check for active borrows *)
  match List.find_opt (fun b -> places_overlap place b.b_place) state.borrows with
  | Some borrow -> Error (MoveWhileBorrowed (place, borrow))
  | None ->
    state.moved <- place :: state.moved;
    Ok ()

(** Record a borrow *)
let record_borrow (state : state) (place : place) (kind : borrow_kind)
    (span : Span.t) : borrow result =
  (* Check if moved *)
  if is_moved state place then
    Error (UseAfterMove (place, span, span))  (* TODO: Track move site *)
  else
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
  if is_moved state place then
    Error (UseAfterMove (place, span, span))
  else
    Ok ()

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

(** Convert an expression to a place (if it's an l-value) *)
let rec expr_to_place (symbols : Symbol.t) (expr : expr) : place option =
  match expr with
  | ExprVar id ->
    begin match Symbol.lookup symbols id.name with
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
let rec check_expr (state : state) (symbols : Symbol.t) (expr : expr) : unit result =
  match expr with
  | ExprVar id ->
    begin match expr_to_place symbols expr with
      | Some place -> check_use state place id.span
      | None -> Ok ()
    end

  | ExprLit _ -> Ok ()

  | ExprApp (func, args) ->
    let* () = check_expr state symbols func in
    List.fold_left (fun acc arg ->
      let* () = acc in
      check_expr state symbols arg
    ) (Ok ()) args

  | ExprLambda lam ->
    check_expr state symbols lam.elam_body

  | ExprLet lb ->
    let* () = check_expr state symbols lb.el_value in
    match lb.el_body with
    | Some body -> check_expr state symbols body
    | None -> Ok ()

  | ExprIf ei ->
    let* () = check_expr state symbols ei.ei_cond in
    (* TODO: Proper branch handling - save/restore state *)
    let* () = check_expr state symbols ei.ei_then in
    begin match ei.ei_else with
      | Some e -> check_expr state symbols e
      | None -> Ok ()
    end

  | ExprMatch em ->
    let* () = check_expr state symbols em.em_scrutinee in
    List.fold_left (fun acc arm ->
      let* () = acc in
      let* () = match arm.ma_guard with
        | Some g -> check_expr state symbols g
        | None -> Ok ()
      in
      check_expr state symbols arm.ma_body
    ) (Ok ()) em.em_arms

  | ExprTuple exprs ->
    List.fold_left (fun acc e ->
      let* () = acc in
      check_expr state symbols e
    ) (Ok ()) exprs

  | ExprArray exprs ->
    List.fold_left (fun acc e ->
      let* () = acc in
      check_expr state symbols e
    ) (Ok ()) exprs

  | ExprRecord er ->
    let* () = List.fold_left (fun acc (_id, e_opt) ->
      let* () = acc in
      match e_opt with
      | Some e -> check_expr state symbols e
      | None -> Ok ()
    ) (Ok ()) er.er_fields in
    begin match er.er_spread with
      | Some e -> check_expr state symbols e
      | None -> Ok ()
    end

  | ExprField (base, _) ->
    check_expr state symbols base

  | ExprTupleIndex (base, _) ->
    check_expr state symbols base

  | ExprIndex (arr, idx) ->
    let* () = check_expr state symbols arr in
    check_expr state symbols idx

  | ExprRowRestrict (base, _) ->
    check_expr state symbols base

  | ExprBlock blk ->
    check_block state symbols blk

  | ExprBinary (left, _, right) ->
    let* () = check_expr state symbols left in
    check_expr state symbols right

  | ExprUnary (_, e) ->
    check_expr state symbols e

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e -> check_expr state symbols e
      | None -> Ok ()
    end

  | ExprHandle eh ->
    let* () = check_expr state symbols eh.eh_body in
    List.fold_left (fun acc arm ->
      let* () = acc in
      match arm with
      | HandlerReturn (_pat, body) -> check_expr state symbols body
      | HandlerOp (_op, _pats, body) -> check_expr state symbols body
    ) (Ok ()) eh.eh_handlers

  | ExprResume e_opt ->
    begin match e_opt with
      | Some e -> check_expr state symbols e
      | None -> Ok ()
    end

  | ExprTry et ->
    let* () = check_block state symbols et.et_body in
    let* () = match et.et_catch with
      | Some arms ->
        List.fold_left (fun acc arm ->
          let* () = acc in
          let* () = match arm.ma_guard with
            | Some g -> check_expr state symbols g
            | None -> Ok ()
          in
          check_expr state symbols arm.ma_body
        ) (Ok ()) arms
      | None -> Ok ()
    in
    begin match et.et_finally with
      | Some blk -> check_block state symbols blk
      | None -> Ok ()
    end

  | ExprUnsafe ops ->
    List.fold_left (fun acc op ->
      let* () = acc in
      match op with
      | UnsafeRead e -> check_expr state symbols e
      | UnsafeWrite (e1, e2) ->
        let* () = check_expr state symbols e1 in
        check_expr state symbols e2
      | UnsafeOffset (e1, e2) ->
        let* () = check_expr state symbols e1 in
        check_expr state symbols e2
      | UnsafeTransmute (_, _, e) -> check_expr state symbols e
      | UnsafeForget e -> check_expr state symbols e
      | UnsafeAssume _ -> Ok ()
    ) (Ok ()) ops

  | ExprVariant _ -> Ok ()

  | ExprSpan (e, _) ->
    check_expr state symbols e

and check_block (state : state) (symbols : Symbol.t) (blk : block) : unit result =
  let* () = List.fold_left (fun acc stmt ->
    let* () = acc in
    check_stmt state symbols stmt
  ) (Ok ()) blk.blk_stmts in
  match blk.blk_expr with
  | Some e -> check_expr state symbols e
  | None -> Ok ()

and check_stmt (state : state) (symbols : Symbol.t) (stmt : stmt) : unit result =
  match stmt with
  | StmtLet sl ->
    check_expr state symbols sl.sl_value
  | StmtExpr e ->
    check_expr state symbols e
  | StmtAssign (lhs, _, rhs) ->
    let* () = check_expr state symbols lhs in
    check_expr state symbols rhs
  | StmtWhile (cond, body) ->
    let* () = check_expr state symbols cond in
    check_block state symbols body
  | StmtFor (_pat, iter, body) ->
    let* () = check_expr state symbols iter in
    check_block state symbols body

(** Check a function *)
let check_function (symbols : Symbol.t) (fd : fn_decl) : unit result =
  let state = create () in
  match fd.fd_body with
  | FnBlock blk -> check_block state symbols blk
  | FnExpr e -> check_expr state symbols e

(** Check a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | TopFn fd -> check_function symbols fd
      | _ -> Ok ()
  ) (Ok ()) program.prog_decls

(* Silence unused warnings for functions that will be used in later phases *)
let _ = record_move
let _ = record_borrow
let _ = end_borrow

(* TODO: Phase 3 implementation
   - [ ] Non-lexical lifetimes
   - [ ] Dataflow analysis for precise tracking
   - [ ] Lifetime inference
   - [ ] Better error messages with suggestions
   - [ ] Integration with quantity checking
   - [ ] Effect interaction with borrows
*)
