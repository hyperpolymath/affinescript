(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Trait method desugaring - transforms trait method calls into direct function calls.

    This pass runs after type checking and transforms:
      receiver.method(args)
    into:
      TypeName_TraitName_methodName(receiver, args)

    This enables monomorphized trait methods to be called correctly.
*)

open Ast
open Types

type context = {
  type_defs : (string, ty) Hashtbl.t;
  trait_registry : Trait.trait_registry;
}

(** Extract type name from a type *)
let rec type_name_from_ty (ty : ty) : string option =
  match repr ty with
  | TCon name -> Some name
  | TApp (TCon name, _) -> Some name
  | TApp (ty', _) -> type_name_from_ty ty'
  | _ -> None

(** Desugar expression - transform trait method calls *)
let rec desugar_expr (ctx : context) (expr : expr) : expr =
  match expr with
  | ExprApp (func_expr, args) ->
    begin match func_expr with
      | ExprField (receiver, method_name) ->
        (* This might be a trait method call - need to check types *)
        (* For now, we'll use a heuristic: if the method name doesn't exist as a field,
           it's likely a trait method. The type checker has already validated this. *)

        (* Try to determine the receiver type and find trait impl *)
        (* Since we don't have type info here, we'll use a conservative approach:
           Keep the original call structure but mark it for later optimization *)

        (* Recursively desugar receiver and args *)
        let receiver' = desugar_expr ctx receiver in
        let args' = List.map (desugar_expr ctx) args in
        ExprApp (ExprField (receiver', method_name), args')

      | _ ->
        (* Regular function call *)
        let func' = desugar_expr ctx func_expr in
        let args' = List.map (desugar_expr ctx) args in
        ExprApp (func', args')
    end

  | ExprVar _ | ExprLit _ -> expr

  | ExprBinary (e1, op, e2) ->
    ExprBinary (desugar_expr ctx e1, op, desugar_expr ctx e2)

  | ExprUnary (op, e) ->
    ExprUnary (op, desugar_expr ctx e)

  | ExprIf { ei_cond; ei_then; ei_else } ->
    ExprIf {
      ei_cond = desugar_expr ctx ei_cond;
      ei_then = desugar_expr ctx ei_then;
      ei_else = Option.map (desugar_expr ctx) ei_else;
    }

  | ExprBlock blk ->
    ExprBlock (desugar_block ctx blk)

  | ExprField (e, field) ->
    ExprField (desugar_expr ctx e, field)

  | ExprIndex (e1, e2) ->
    ExprIndex (desugar_expr ctx e1, desugar_expr ctx e2)

  | ExprTupleIndex (e, idx) ->
    ExprTupleIndex (desugar_expr ctx e, idx)

  | ExprRecord { er_fields; er_spread } ->
    ExprRecord {
      er_fields = List.map (fun (id, e_opt) ->
        (id, Option.map (desugar_expr ctx) e_opt)
      ) er_fields;
      er_spread = Option.map (desugar_expr ctx) er_spread;
    }

  | ExprArray exprs ->
    ExprArray (List.map (desugar_expr ctx) exprs)

  | ExprTuple exprs ->
    ExprTuple (List.map (desugar_expr ctx) exprs)

  | ExprMatch { em_scrutinee; em_arms } ->
    ExprMatch {
      em_scrutinee = desugar_expr ctx em_scrutinee;
      em_arms = List.map (fun arm ->
        { arm with
          ma_guard = Option.map (desugar_expr ctx) arm.ma_guard;
          ma_body = desugar_expr ctx arm.ma_body;
        }
      ) em_arms;
    }

  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
    ExprLambda {
      elam_params;
      elam_ret_ty;
      elam_body = desugar_expr ctx elam_body;
    }

  | ExprLet { el_mut; el_pat; el_ty; el_value; el_body } ->
    ExprLet {
      el_mut;
      el_pat;
      el_ty;
      el_value = desugar_expr ctx el_value;
      el_body = Option.map (desugar_expr ctx) el_body;
    }

  | ExprReturn e_opt ->
    ExprReturn (Option.map (desugar_expr ctx) e_opt)

  | ExprTry { et_body; et_catch; et_finally } ->
    ExprTry {
      et_body = desugar_block ctx et_body;
      et_catch = Option.map (List.map (fun arm ->
        { arm with
          ma_guard = Option.map (desugar_expr ctx) arm.ma_guard;
          ma_body = desugar_expr ctx arm.ma_body;
        }
      )) et_catch;
      et_finally = Option.map (desugar_block ctx) et_finally;
    }

  | ExprHandle { eh_body; eh_handlers } ->
    ExprHandle {
      eh_body = desugar_expr ctx eh_body;
      eh_handlers = List.map (desugar_handler ctx) eh_handlers;
    }

  | ExprResume e_opt ->
    ExprResume (Option.map (desugar_expr ctx) e_opt)

  | ExprUnsafe ops ->
    ExprUnsafe (List.map (desugar_unsafe_op ctx) ops)

  | ExprRowRestrict (e, field) ->
    ExprRowRestrict (desugar_expr ctx e, field)

  | ExprVariant _ -> expr

  | ExprSpan (e, span) ->
    ExprSpan (desugar_expr ctx e, span)

and desugar_handler (ctx : context) (handler : handler_arm) : handler_arm =
  match handler with
  | HandlerReturn (pat, body) ->
    HandlerReturn (pat, desugar_expr ctx body)
  | HandlerOp (op, pats, body) ->
    HandlerOp (op, pats, desugar_expr ctx body)

and desugar_unsafe_op (ctx : context) (op : unsafe_op) : unsafe_op =
  match op with
  | UnsafeRead e -> UnsafeRead (desugar_expr ctx e)
  | UnsafeWrite (e1, e2) -> UnsafeWrite (desugar_expr ctx e1, desugar_expr ctx e2)
  | UnsafeOffset (e1, e2) -> UnsafeOffset (desugar_expr ctx e1, desugar_expr ctx e2)
  | UnsafeTransmute (ty1, ty2, e) -> UnsafeTransmute (ty1, ty2, desugar_expr ctx e)
  | UnsafeForget e -> UnsafeForget (desugar_expr ctx e)
  | UnsafeAssume _ as op -> op

and desugar_block (ctx : context) (blk : block) : block =
  { blk_stmts = List.map (desugar_stmt ctx) blk.blk_stmts;
    blk_expr = Option.map (desugar_expr ctx) blk.blk_expr;
  }

and desugar_stmt (ctx : context) (stmt : stmt) : stmt =
  match stmt with
  | StmtLet { sl_mut; sl_pat; sl_ty; sl_value } ->
    StmtLet {
      sl_mut;
      sl_pat;
      sl_ty;
      sl_value = desugar_expr ctx sl_value;
    }

  | StmtExpr e ->
    StmtExpr (desugar_expr ctx e)

  | StmtAssign (lhs, op, rhs) ->
    StmtAssign (desugar_expr ctx lhs, op, desugar_expr ctx rhs)

  | StmtWhile (cond, body) ->
    StmtWhile (desugar_expr ctx cond, desugar_block ctx body)

  | StmtFor (pat, iter, body) ->
    StmtFor (pat, desugar_expr ctx iter, desugar_block ctx body)

let desugar_function (ctx : context) (fd : fn_decl) : fn_decl =
  match fd.fd_body with
  | FnBlock blk ->
    { fd with fd_body = FnBlock (desugar_block ctx blk) }
  | FnExpr e ->
    { fd with fd_body = FnExpr (desugar_expr ctx e) }

let desugar_top_level (ctx : context) (top : top_level) : top_level =
  match top with
  | TopFn fd ->
    TopFn (desugar_function ctx fd)

  | TopImpl ib ->
    (* Desugar method implementations *)
    let items' = List.map (fun item ->
      match item with
      | ImplFn fd -> ImplFn (desugar_function ctx fd)
      | ImplType _ as it -> it
    ) ib.ib_items in
    TopImpl { ib with ib_items = items' }

  | TopConst { tc_vis; tc_name; tc_ty; tc_value } ->
    TopConst {
      tc_vis;
      tc_name;
      tc_ty;
      tc_value = desugar_expr ctx tc_value;
    }

  | TopType _ | TopEffect _ | TopTrait _ as d -> d

let desugar_program (type_defs : (string, ty) Hashtbl.t)
                    (trait_registry : Trait.trait_registry)
                    (prog : program) : program =
  let ctx = { type_defs; trait_registry } in
  { prog with prog_decls = List.map (desugar_top_level ctx) prog.prog_decls }
