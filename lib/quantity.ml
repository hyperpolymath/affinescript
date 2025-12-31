(* SPDX-License-Identifier: Apache-2.0 OR MIT *)
(* Copyright 2024 AffineScript Contributors *)

(** Quantity checking for QTT.

    This module implements quantity checking for Quantitative Type Theory.
    It tracks how many times each variable is used and verifies that usage
    matches the declared quantity annotations.
*)

open Ast

(** Usage count for a variable *)
type usage =
  | UZero    (** Not used *)
  | UOne     (** Used exactly once *)
  | UMany    (** Used multiple times *)
[@@deriving show, eq]

(** Combine two usages (for branching) *)
let join (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (UZero, u) | (u, UZero) -> u
  | (UOne, UOne) -> UOne
  | _ -> UMany

(** Add two usages (for sequencing) *)
let add (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (UZero, u) | (u, UZero) -> u
  | _ -> UMany

(** Quantity errors *)
type quantity_error =
  | LinearVariableUnused of ident
  | LinearVariableUsedMultiple of ident
  | ErasedVariableUsed of ident
  | QuantityMismatch of ident * quantity * usage
[@@deriving show]

type 'a result = ('a, quantity_error * Span.t) Result.t

(** Quantity context: maps variables to their quantities and current usage *)
type context = {
  quantities : (Symbol.symbol_id, quantity) Hashtbl.t;
  usages : (Symbol.symbol_id, usage) Hashtbl.t;
}

(** Create a new quantity context *)
let create () : context =
  {
    quantities = Hashtbl.create 32;
    usages = Hashtbl.create 32;
  }

(** Record a variable's declared quantity *)
let declare (ctx : context) (sym : Symbol.symbol) (q : quantity) : unit =
  Hashtbl.replace ctx.quantities sym.sym_id q;
  Hashtbl.replace ctx.usages sym.sym_id UZero

(** Record a use of a variable *)
let use (ctx : context) (sym : Symbol.symbol) : unit =
  match Hashtbl.find_opt ctx.usages sym.sym_id with
  | Some UZero -> Hashtbl.replace ctx.usages sym.sym_id UOne
  | Some UOne -> Hashtbl.replace ctx.usages sym.sym_id UMany
  | Some UMany -> ()
  | None -> ()

(** Check that a variable's usage matches its quantity *)
let check_variable (ctx : context) (sym : Symbol.symbol) (id : ident)
    : unit result =
  let q = Hashtbl.find_opt ctx.quantities sym.sym_id |> Option.value ~default:QOmega in
  let u = Hashtbl.find_opt ctx.usages sym.sym_id |> Option.value ~default:UZero in
  match (q, u) with
  (* Erased: must not be used *)
  | (QZero, UZero) -> Ok ()
  | (QZero, _) -> Error (ErasedVariableUsed id, id.id_span)
  (* Linear: must be used exactly once (or zero for affine) *)
  | (QOne, UZero) -> Ok ()  (* Affine: can drop *)
  | (QOne, UOne) -> Ok ()
  | (QOne, UMany) -> Error (LinearVariableUsedMultiple id, id.id_span)
  (* Unrestricted: any usage is fine *)
  | (QOmega, _) -> Ok ()

(** Analyze usage in an expression *)
let rec analyze_expr (ctx : context) (symbols : Symbol.t) (expr : expr) : unit =
  match expr with
  | EVar id ->
    begin match Symbol.lookup symbols id.id_name with
      | Some sym -> use ctx sym
      | None -> ()
    end

  | ELit _ -> ()

  | EApp (func, arg, _) ->
    analyze_expr ctx symbols func;
    analyze_expr ctx symbols arg

  | ELam lam ->
    (* Parameters are bound; analyze body *)
    analyze_expr ctx symbols lam.lam_body

  | ELet lb ->
    analyze_expr ctx symbols lb.lb_rhs;
    analyze_expr ctx symbols lb.lb_body

  | EIf (cond, then_, else_, _) ->
    analyze_expr ctx symbols cond;
    (* For branches, we need to join usages *)
    (* TODO: Proper branch handling *)
    analyze_expr ctx symbols then_;
    analyze_expr ctx symbols else_

  | ECase (scrut, branches, _) ->
    analyze_expr ctx symbols scrut;
    List.iter (fun branch ->
      analyze_expr ctx symbols branch.cb_body
    ) branches

  | ETuple (exprs, _) ->
    List.iter (analyze_expr ctx symbols) exprs

  | ERecord (fields, _) ->
    List.iter (fun (_, e) -> analyze_expr ctx symbols e) fields

  | ERecordAccess (e, _, _) ->
    analyze_expr ctx symbols e

  | ERecordUpdate (base, _, value, _) ->
    analyze_expr ctx symbols base;
    analyze_expr ctx symbols value

  | EBlock (exprs, _) ->
    List.iter (analyze_expr ctx symbols) exprs

  | EBinOp (left, _, right, _) ->
    analyze_expr ctx symbols left;
    analyze_expr ctx symbols right

  | EUnaryOp (_, e, _) ->
    analyze_expr ctx symbols e

  | EHandle (body, handler, _) ->
    analyze_expr ctx symbols body;
    begin match handler.h_return with
      | Some (_, e) -> analyze_expr ctx symbols e
      | None -> ()
    end;
    List.iter (fun clause ->
      analyze_expr ctx symbols clause.oc_body
    ) handler.h_ops

  | EPerform (_, arg, _) ->
    analyze_expr ctx symbols arg

  | _ -> ()

(** Check quantities for a function *)
let check_function (symbols : Symbol.t) (fd : fun_decl) : unit result =
  let ctx = create () in
  (* Declare parameter quantities *)
  List.iter (fun (id, _, q_opt) ->
    let q = Option.value q_opt ~default:QOmega in
    match Symbol.lookup symbols id.id_name with
    | Some sym -> declare ctx sym q
    | None -> ()
  ) fd.fd_params;
  (* Analyze body *)
  begin match fd.fd_body with
    | Some body -> analyze_expr ctx symbols body
    | None -> ()
  end;
  (* Check all parameters *)
  List.fold_left (fun acc (id, _, _) ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match Symbol.lookup symbols id.id_name with
      | Some sym -> check_variable ctx sym id
      | None -> Ok ()
  ) (Ok ()) fd.fd_params

(** Check quantities for a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | DFun fd -> check_function symbols fd
      | _ -> Ok ()
  ) (Ok ()) program.prog_decls

(* Semiring operations for quantity algebra *)

(** Addition in the quantity semiring *)
let q_add (q1 : quantity) (q2 : quantity) : quantity =
  match (q1, q2) with
  | (QZero, q) | (q, QZero) -> q
  | (QOne, QOne) -> QOmega
  | (QOmega, _) | (_, QOmega) -> QOmega

(** Multiplication in the quantity semiring *)
let q_mul (q1 : quantity) (q2 : quantity) : quantity =
  match (q1, q2) with
  | (QZero, _) | (_, QZero) -> QZero
  | (QOne, q) | (q, QOne) -> q
  | (QOmega, QOmega) -> QOmega

(** Check if q1 ≤ q2 in the quantity ordering *)
let q_le (q1 : quantity) (q2 : quantity) : bool =
  match (q1, q2) with
  | (QZero, _) -> true
  | (_, QOmega) -> true
  | (QOne, QOne) -> true
  | _ -> false

(* TODO: Phase 2 implementation
   - [ ] Proper branch handling for if/case
   - [ ] Quantity polymorphism
   - [ ] Integration with type checker
   - [ ] Effect interaction with quantities
   - [ ] Better error messages
*)
