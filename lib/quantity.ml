(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

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
  | (QZero, _) -> Error (ErasedVariableUsed id, id.span)
  (* Linear: must be used exactly once (or zero for affine) *)
  | (QOne, UZero) -> Ok ()  (* Affine: can drop *)
  | (QOne, UOne) -> Ok ()
  | (QOne, UMany) -> Error (LinearVariableUsedMultiple id, id.span)
  (* Unrestricted: any usage is fine *)
  | (QOmega, _) -> Ok ()

(** Analyze usage in an expression *)
let rec analyze_expr (ctx : context) (symbols : Symbol.t) (expr : expr) : unit =
  match expr with
  | ExprVar id ->
    begin match Symbol.lookup symbols id.name with
      | Some sym -> use ctx sym
      | None -> ()
    end

  | ExprLit _ -> ()

  | ExprApp (func, args) ->
    analyze_expr ctx symbols func;
    List.iter (analyze_expr ctx symbols) args

  | ExprLambda lam ->
    (* Parameters are bound; analyze body *)
    analyze_expr ctx symbols lam.elam_body

  | ExprLet lb ->
    analyze_expr ctx symbols lb.el_value;
    Option.iter (analyze_expr ctx symbols) lb.el_body

  | ExprIf ei ->
    analyze_expr ctx symbols ei.ei_cond;
    (* For branches, we need to join usages from both branches *)
    (* Save current usages before branches *)
    let saved_usages = Hashtbl.copy ctx.usages in
    (* Analyze then branch *)
    analyze_expr ctx symbols ei.ei_then;
    let then_usages = Hashtbl.copy ctx.usages in
    (* Restore and analyze else branch *)
    Hashtbl.clear ctx.usages;
    Hashtbl.iter (fun k v -> Hashtbl.add ctx.usages k v) saved_usages;
    Option.iter (analyze_expr ctx symbols) ei.ei_else;
    (* Join usages from both branches: max of the two *)
    Hashtbl.iter (fun id then_usage ->
      let else_usage = Hashtbl.find_opt ctx.usages id |> Option.value ~default:UZero in
      Hashtbl.replace ctx.usages id (join then_usage else_usage)
    ) then_usages

  | ExprMatch em ->
    analyze_expr ctx symbols em.em_scrutinee;
    List.iter (fun arm ->
      Option.iter (analyze_expr ctx symbols) arm.ma_guard;
      analyze_expr ctx symbols arm.ma_body
    ) em.em_arms

  | ExprTuple exprs ->
    List.iter (analyze_expr ctx symbols) exprs

  | ExprArray exprs ->
    List.iter (analyze_expr ctx symbols) exprs

  | ExprRecord er ->
    List.iter (fun (_id, e_opt) ->
      Option.iter (analyze_expr ctx symbols) e_opt
    ) er.er_fields;
    Option.iter (analyze_expr ctx symbols) er.er_spread

  | ExprField (e, _) ->
    analyze_expr ctx symbols e

  | ExprTupleIndex (e, _) ->
    analyze_expr ctx symbols e

  | ExprIndex (arr, idx) ->
    analyze_expr ctx symbols arr;
    analyze_expr ctx symbols idx

  | ExprRowRestrict (e, _) ->
    analyze_expr ctx symbols e

  | ExprBlock blk ->
    analyze_block ctx symbols blk

  | ExprBinary (left, _, right) ->
    analyze_expr ctx symbols left;
    analyze_expr ctx symbols right

  | ExprUnary (_, e) ->
    analyze_expr ctx symbols e

  | ExprHandle eh ->
    analyze_expr ctx symbols eh.eh_body;
    List.iter (fun arm ->
      match arm with
      | HandlerReturn (_pat, body) -> analyze_expr ctx symbols body
      | HandlerOp (_op, _pats, body) -> analyze_expr ctx symbols body
    ) eh.eh_handlers

  | ExprResume e_opt ->
    Option.iter (analyze_expr ctx symbols) e_opt

  | ExprReturn e_opt ->
    Option.iter (analyze_expr ctx symbols) e_opt

  | ExprTry et ->
    analyze_block ctx symbols et.et_body;
    Option.iter (fun arms ->
      List.iter (fun arm ->
        Option.iter (analyze_expr ctx symbols) arm.ma_guard;
        analyze_expr ctx symbols arm.ma_body
      ) arms
    ) et.et_catch;
    Option.iter (analyze_block ctx symbols) et.et_finally

  | ExprUnsafe ops ->
    List.iter (fun op ->
      match op with
      | UnsafeRead e -> analyze_expr ctx symbols e
      | UnsafeWrite (e1, e2) ->
        analyze_expr ctx symbols e1;
        analyze_expr ctx symbols e2
      | UnsafeOffset (e1, e2) ->
        analyze_expr ctx symbols e1;
        analyze_expr ctx symbols e2
      | UnsafeTransmute (_, _, e) -> analyze_expr ctx symbols e
      | UnsafeForget e -> analyze_expr ctx symbols e
      | UnsafeAssume _ -> ()
    ) ops

  | ExprVariant _ -> ()

  | ExprSpan (e, _) ->
    analyze_expr ctx symbols e

and analyze_block (ctx : context) (symbols : Symbol.t) (blk : block) : unit =
  List.iter (analyze_stmt ctx symbols) blk.blk_stmts;
  Option.iter (analyze_expr ctx symbols) blk.blk_expr

and analyze_stmt (ctx : context) (symbols : Symbol.t) (stmt : stmt) : unit =
  match stmt with
  | StmtLet sl ->
    analyze_expr ctx symbols sl.sl_value
  | StmtExpr e ->
    analyze_expr ctx symbols e
  | StmtAssign (lhs, _, rhs) ->
    analyze_expr ctx symbols lhs;
    analyze_expr ctx symbols rhs
  | StmtWhile (cond, body) ->
    analyze_expr ctx symbols cond;
    analyze_block ctx symbols body
  | StmtFor (_pat, iter, body) ->
    analyze_expr ctx symbols iter;
    analyze_block ctx symbols body

(** Check quantities for a function *)
let check_function (symbols : Symbol.t) (fd : fn_decl) : unit result =
  let ctx = create () in
  (* Declare parameter quantities *)
  List.iter (fun param ->
    let q = Option.value param.p_quantity ~default:QOmega in
    match Symbol.lookup symbols param.p_name.name with
    | Some sym -> declare ctx sym q
    | None -> ()
  ) fd.fd_params;
  (* Analyze body *)
  begin match fd.fd_body with
    | FnBlock blk -> analyze_block ctx symbols blk
    | FnExpr e -> analyze_expr ctx symbols e
  end;
  (* Check all parameters *)
  List.fold_left (fun acc param ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match Symbol.lookup symbols param.p_name.name with
      | Some sym -> check_variable ctx sym param.p_name
      | None -> Ok ()
  ) (Ok ()) fd.fd_params

(** Check quantities for a program *)
let check_program (symbols : Symbol.t) (program : program) : unit result =
  List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      match decl with
      | TopFn fd -> check_function symbols fd
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

(* Phase 2 (quantity checking) partially complete. Future enhancements:
   - Quantity polymorphism with inference (Phase 2)
   - Integration with type checker bidirectional flow (Phase 2)
   - Effect interaction with quantities (Phase 3)
*)
