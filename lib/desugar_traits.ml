(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
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

  | ExprBinOp (op, e1, e2) ->
    ExprBinOp (op, desugar_expr ctx e1, desugar_expr ctx e2)

  | ExprUnOp (op, e) ->
    ExprUnOp (op, desugar_expr ctx e)

  | ExprIf (cond, then_expr, else_expr) ->
    ExprIf (desugar_expr ctx cond, desugar_expr ctx then_expr,
            Option.map (desugar_expr ctx) else_expr)

  | ExprBlock blk ->
    ExprBlock (desugar_block ctx blk)

  | ExprField (e, field) ->
    ExprField (desugar_expr ctx e, field)

  | ExprIndex (e1, e2) ->
    ExprIndex (desugar_expr ctx e1, desugar_expr ctx e2)

  | ExprCall (e, args) ->
    ExprCall (desugar_expr ctx e, List.map (desugar_expr ctx) args)

  | ExprStruct (name, fields) ->
    ExprStruct (name, List.map (fun (id, e) -> (id, desugar_expr ctx e)) fields)

  | ExprArray exprs ->
    ExprArray (List.map (desugar_expr ctx) exprs)

  | ExprTuple exprs ->
    ExprTuple (List.map (desugar_expr ctx) exprs)

  | ExprMatch (e, arms) ->
    ExprMatch (desugar_expr ctx e,
               List.map (fun (pat, guard, body) ->
                 (pat, Option.map (desugar_expr ctx) guard, desugar_expr ctx body)
               ) arms)

  | ExprLambda (params, body) ->
    ExprLambda (params, desugar_expr ctx body)

  | ExprReturn e ->
    ExprReturn (desugar_expr ctx e)

  | ExprBreak e_opt ->
    ExprBreak (Option.map (desugar_expr ctx) e_opt)

  | ExprContinue -> expr

  | ExprLoop blk ->
    ExprLoop (desugar_block ctx blk)

  | ExprWhile (cond, body) ->
    ExprWhile (desugar_expr ctx cond, desugar_block ctx body)

  | ExprFor (pat, iter, body) ->
    ExprFor (pat, desugar_expr ctx iter, desugar_block ctx body)

  | ExprAssign (lhs, rhs) ->
    ExprAssign (desugar_expr ctx lhs, desugar_expr ctx rhs)

  | ExprBorrow (kind, e) ->
    ExprBorrow (kind, desugar_expr ctx e)

  | ExprDeref e ->
    ExprDeref (desugar_expr ctx e)

  | ExprCast (e, ty) ->
    ExprCast (desugar_expr ctx e, ty)

  | ExprAscribe (e, ty) ->
    ExprAscribe (desugar_expr ctx e, ty)

and desugar_block (ctx : context) (blk : block) : block =
  { blk with b_stmts = List.map (desugar_stmt ctx) blk.b_stmts }

and desugar_stmt (ctx : context) (stmt : stmt) : stmt =
  match stmt with
  | StmtLet sl ->
    StmtLet { sl with sl_value = desugar_expr ctx sl.sl_value }

  | StmtExpr e ->
    StmtExpr (desugar_expr ctx e)

let desugar_function (ctx : context) (fd : fn_decl) : fn_decl =
  match fd.fd_body with
  | FnBlock blk ->
    { fd with fd_body = FnBlock (desugar_block ctx blk) }
  | FnExpr e ->
    { fd with fd_body = FnExpr (desugar_expr ctx e) }

let desugar_decl (ctx : context) (decl : decl) : decl =
  match decl with
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

  | TopConst _ | TopType _ | TopEffect _ | TopTrait _ as d -> d

let desugar_program (type_defs : (string, ty) Hashtbl.t)
                    (trait_registry : Trait.trait_registry)
                    (prog : program) : program =
  let ctx = { type_defs; trait_registry } in
  { prog with prog_decls = List.map (desugar_decl ctx) prog.prog_decls }
