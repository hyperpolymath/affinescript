(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Optimization passes for AffineScript AST.

    This module implements various optimization transformations on the AST
    before code generation.
*)

open Ast

(** Constant folding - evaluate constant expressions at compile time *)
let rec fold_constants_expr (expr : expr) : expr =
  match expr with
  | ExprBinary (ExprLit (LitInt (a, _)), op, ExprLit (LitInt (b, _))) ->
    (* Fold integer binary operations *)
    begin match op with
      | OpAdd -> ExprLit (LitInt (a + b, Span.dummy))
      | OpSub -> ExprLit (LitInt (a - b, Span.dummy))
      | OpMul -> ExprLit (LitInt (a * b, Span.dummy))
      | OpDiv when b <> 0 -> ExprLit (LitInt (a / b, Span.dummy))
      | OpMod when b <> 0 -> ExprLit (LitInt (a mod b, Span.dummy))
      | OpEq -> ExprLit (LitBool (a = b, Span.dummy))
      | OpNe -> ExprLit (LitBool (a <> b, Span.dummy))
      | OpLt -> ExprLit (LitBool (a < b, Span.dummy))
      | OpLe -> ExprLit (LitBool (a <= b, Span.dummy))
      | OpGt -> ExprLit (LitBool (a > b, Span.dummy))
      | OpGe -> ExprLit (LitBool (a >= b, Span.dummy))
      | _ -> expr  (* Don't fold other ops or division by zero *)
    end

  | ExprBinary (ExprLit (LitBool (a, _)), op, ExprLit (LitBool (b, _))) ->
    (* Fold boolean binary operations *)
    begin match op with
      | OpAnd -> ExprLit (LitBool (a && b, Span.dummy))
      | OpOr -> ExprLit (LitBool (a || b, Span.dummy))
      | OpEq -> ExprLit (LitBool (a = b, Span.dummy))
      | OpNe -> ExprLit (LitBool (a <> b, Span.dummy))
      | _ -> expr
    end

  | ExprUnary (OpNeg, ExprLit (LitInt (n, _))) ->
    ExprLit (LitInt (-n, Span.dummy))

  | ExprUnary (OpNot, ExprLit (LitBool (b, _))) ->
    ExprLit (LitBool (not b, Span.dummy))

  | ExprBinary (left, op, right) ->
    let left' = fold_constants_expr left in
    let right' = fold_constants_expr right in
    if left == left' && right == right' then
      expr
    else
      ExprBinary (left', op, right')

  | ExprUnary (op, operand) ->
    let operand' = fold_constants_expr operand in
    if operand == operand' then
      expr
    else
      ExprUnary (op, operand')

  | ExprIf ei ->
    let cond' = fold_constants_expr ei.ei_cond in
    let then' = fold_constants_expr ei.ei_then in
    let else' = Option.map fold_constants_expr ei.ei_else in

    (* If condition is constant, select branch at compile time *)
    begin match cond' with
      | ExprLit (LitBool (true, _)) -> then'
      | ExprLit (LitBool (false, _)) ->
        begin match else' with
          | Some e -> e
          | None -> ExprLit (LitUnit Span.dummy)
        end
      | _ -> ExprIf { ei_cond = cond'; ei_then = then'; ei_else = else' }
    end

  | ExprLet el ->
    ExprLet {
      el with
      el_value = fold_constants_expr el.el_value;
      el_body = Option.map fold_constants_expr el.el_body;
    }

  | ExprMatch em ->
    ExprMatch {
      em_scrutinee = fold_constants_expr em.em_scrutinee;
      em_arms = List.map (fun arm -> { arm with ma_body = fold_constants_expr arm.ma_body }) em.em_arms;
    }

  | ExprLambda lam ->
    ExprLambda { lam with elam_body = fold_constants_expr lam.elam_body }

  | ExprApp (func, args) ->
    ExprApp (fold_constants_expr func, List.map fold_constants_expr args)

  | ExprField (e, f) ->
    ExprField (fold_constants_expr e, f)

  | ExprTupleIndex (e, i) ->
    ExprTupleIndex (fold_constants_expr e, i)

  | ExprIndex (arr, idx) ->
    ExprIndex (fold_constants_expr arr, fold_constants_expr idx)

  | ExprTuple exprs ->
    ExprTuple (List.map fold_constants_expr exprs)

  | ExprArray exprs ->
    ExprArray (List.map fold_constants_expr exprs)

  | ExprRecord er ->
    ExprRecord {
      er_fields = List.map (fun (f, e_opt) -> (f, Option.map fold_constants_expr e_opt)) er.er_fields;
      er_spread = Option.map fold_constants_expr er.er_spread;
    }

  | ExprBlock blk ->
    ExprBlock (fold_constants_block blk)

  | ExprReturn e_opt ->
    ExprReturn (Option.map fold_constants_expr e_opt)

  | _ -> expr

and fold_constants_block (blk : block) : block =
  {
    blk_stmts = List.map fold_constants_stmt blk.blk_stmts;
    blk_expr = Option.map fold_constants_expr blk.blk_expr;
  }

and fold_constants_stmt (stmt : stmt) : stmt =
  match stmt with
  | StmtLet sl ->
    StmtLet { sl with sl_value = fold_constants_expr sl.sl_value }
  | StmtAssign (lhs, op, rhs) ->
    StmtAssign (fold_constants_expr lhs, op, fold_constants_expr rhs)
  | StmtWhile (cond, body) ->
    StmtWhile (fold_constants_expr cond, fold_constants_block body)
  | StmtFor (pat, iter, body) ->
    StmtFor (pat, fold_constants_expr iter, fold_constants_block body)
  | StmtExpr e ->
    StmtExpr (fold_constants_expr e)

let fold_constants_decl (decl : top_level) : top_level =
  match decl with
  | TopFn fd ->
    begin match fd.fd_body with
      | FnBlock blk ->
        TopFn { fd with fd_body = FnBlock (fold_constants_block blk) }
      | FnExpr e ->
        TopFn { fd with fd_body = FnExpr (fold_constants_expr e) }
    end
  | _ -> decl

let fold_constants_program (prog : program) : program =
  { prog with prog_decls = List.map fold_constants_decl prog.prog_decls }
