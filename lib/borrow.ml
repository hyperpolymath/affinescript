(* SPDX-License-Identifier: Apache-2.0 OR MIT *)
(* Copyright 2024 AffineScript Contributors *)

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
let record_move (state : state) (place : place) (span : Span.t) : unit result =
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

(** Convert an expression to a place (if it's an l-value) *)
let rec expr_to_place (symbols : Symbol.t) (expr : expr) : place option =
  match expr with
  | EVar id ->
    begin match Symbol.lookup symbols id.id_name with
      | Some sym -> Some (PlaceVar sym.sym_id)
      | None -> None
    end
  | ERecordAccess (base, field, _) ->
    begin match expr_to_place symbols base with
      | Some base_place -> Some (PlaceField (base_place, field.id_name))
      | None -> None
    end
  | EIndex (base, _, _) ->
    begin match expr_to_place symbols base with
      | Some base_place -> Some (PlaceIndex (base_place, None))
      | None -> None
    end
  | _ -> None

(** Check borrows in an expression *)
let rec check_expr (state : state) (symbols : Symbol.t) (expr : expr) : unit result =
  match expr with
  | EVar id ->
    begin match expr_to_place symbols expr with
      | Some place -> check_use state place id.id_span
      | None -> Ok ()
    end

  | ELit _ -> Ok ()

  | EApp (func, arg, _) ->
    let* () = check_expr state symbols func in
    check_expr state symbols arg

  | ELam lam ->
    check_expr state symbols lam.lam_body

  | ELet lb ->
    let* () = check_expr state symbols lb.lb_rhs in
    check_expr state symbols lb.lb_body

  | EIf (cond, then_, else_, _) ->
    let* () = check_expr state symbols cond in
    (* TODO: Proper branch handling - save/restore state *)
    let* () = check_expr state symbols then_ in
    check_expr state symbols else_

  | ECase (scrut, branches, _) ->
    let* () = check_expr state symbols scrut in
    List.fold_left (fun acc branch ->
      match acc with
      | Error e -> Error e
      | Ok () -> check_expr state symbols branch.cb_body
    ) (Ok ()) branches

  | ETuple (exprs, _) ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> check_expr state symbols e
    ) (Ok ()) exprs

  | ERecord (fields, _) ->
    List.fold_left (fun acc (_, e) ->
      match acc with
      | Error e -> Error e
      | Ok () -> check_expr state symbols e
    ) (Ok ()) fields

  | ERecordAccess (base, _, _) ->
    check_expr state symbols base

  | ERecordUpdate (base, _, value, _) ->
    let* () = check_expr state symbols base in
    check_expr state symbols value

  | EBlock (exprs, _) ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> check_expr state symbols e
    ) (Ok ()) exprs

  | EBinOp (left, _, right, _) ->
    let* () = check_expr state symbols left in
    check_expr state symbols right

  | _ -> Ok ()

(* Result bind *)
let ( let* ) = Result.bind

(** Check a function *)
let check_function (symbols : Symbol.t) (fd : fun_decl) : unit result =
  let state = create () in
  match fd.fd_body with
  | Some body -> check_expr state symbols body
  | None -> Ok ()

(** Check a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | DFun fd -> check_function symbols fd
      | _ -> Ok ()
  ) (Ok ()) program.prog_decls

(* TODO: Phase 3 implementation
   - [ ] Non-lexical lifetimes
   - [ ] Dataflow analysis for precise tracking
   - [ ] Lifetime inference
   - [ ] Better error messages with suggestions
   - [ ] Integration with quantity checking
   - [ ] Effect interaction with borrows
*)
