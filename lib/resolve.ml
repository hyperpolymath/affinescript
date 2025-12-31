(* SPDX-License-Identifier: Apache-2.0 OR MIT *)
(* Copyright 2024 AffineScript Contributors *)

(** Name resolution pass.

    This module resolves all names in the AST to symbols in the symbol table.
    It runs after parsing and before type checking.
*)

open Ast

(** Resolution errors *)
type resolve_error =
  | UndefinedVariable of ident
  | UndefinedType of ident
  | UndefinedEffect of ident
  | UndefinedModule of ident
  | DuplicateDefinition of ident
  | VisibilityError of ident * string
  | ImportError of string
[@@deriving show]

(** Resolution result *)
type 'a result = ('a, resolve_error * Span.t) Result.t

(** Resolution context *)
type context = {
  symbols : Symbol.t;
  current_module : string list;
  imports : (string * Symbol.symbol) list;
}

(** Create a new resolution context *)
let create_context () : context =
  {
    symbols = Symbol.create ();
    current_module = [];
    imports = [];
  }

(** Resolve an identifier *)
let resolve_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.id_name in
  match Symbol.lookup ctx.symbols name with
  | Some sym -> Ok sym
  | None -> Error (UndefinedVariable id, id.id_span)

(** Resolve a type identifier *)
let resolve_type_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.id_name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKType -> Ok sym
  | Some sym when sym.sym_kind = Symbol.SKTypeVar -> Ok sym
  | Some _ -> Error (UndefinedType id, id.id_span)
  | None -> Error (UndefinedType id, id.id_span)

(** Resolve an effect identifier *)
let resolve_effect_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.id_name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKEffect -> Ok sym
  | Some _ -> Error (UndefinedEffect id, id.id_span)
  | None -> Error (UndefinedEffect id, id.id_span)

(** Resolve a pattern, binding variables *)
let rec resolve_pattern (ctx : context) (pat : pattern) : context result =
  match pat with
  | PWild _ -> Ok ctx
  | PVar id ->
    if Symbol.is_defined_locally ctx.symbols id.id_name then
      Error (DuplicateDefinition id, id.id_span)
    else begin
      let _ = Symbol.define ctx.symbols id.id_name
          Symbol.SKVariable id.id_span Private in
      Ok ctx
    end
  | PLit _ -> Ok ctx
  | PTuple (pats, _) ->
    List.fold_left (fun acc pat ->
      match acc with
      | Error e -> Error e
      | Ok ctx -> resolve_pattern ctx pat
    ) (Ok ctx) pats
  | PRecord (fields, _, _) ->
    List.fold_left (fun acc (_, pat) ->
      match acc with
      | Error e -> Error e
      | Ok ctx -> resolve_pattern ctx pat
    ) (Ok ctx) fields
  | PConstructor (_, pats, _) ->
    List.fold_left (fun acc pat ->
      match acc with
      | Error e -> Error e
      | Ok ctx -> resolve_pattern ctx pat
    ) (Ok ctx) pats
  | POr (p1, p2, _) ->
    (* Both branches must bind the same variables *)
    let* ctx1 = resolve_pattern ctx p1 in
    let* _ctx2 = resolve_pattern ctx p2 in
    Ok ctx1
  | PAs (pat, id, _) ->
    let* ctx = resolve_pattern ctx pat in
    if Symbol.is_defined_locally ctx.symbols id.id_name then
      Error (DuplicateDefinition id, id.id_span)
    else begin
      let _ = Symbol.define ctx.symbols id.id_name
          Symbol.SKVariable id.id_span Private in
      Ok ctx
    end

(** Resolve an expression *)
let rec resolve_expr (ctx : context) (expr : expr) : unit result =
  match expr with
  | EVar id ->
    let* _ = resolve_ident ctx id in
    Ok ()

  | ELit _ -> Ok ()

  | EApp (func, arg, _) ->
    let* () = resolve_expr ctx func in
    resolve_expr ctx arg

  | ELam lam ->
    Symbol.enter_scope ctx.symbols (Symbol.ScopeFunction "lambda");
    (* Bind parameters *)
    List.iter (fun (id, _, _) ->
      let _ = Symbol.define ctx.symbols id.id_name
          Symbol.SKVariable id.id_span Private in
      ()
    ) lam.lam_params;
    let result = resolve_expr ctx lam.lam_body in
    Symbol.exit_scope ctx.symbols;
    result

  | ELet lb ->
    let* () = resolve_expr ctx lb.lb_rhs in
    Symbol.enter_scope ctx.symbols Symbol.ScopeBlock;
    let* _ = resolve_pattern ctx lb.lb_pat in
    let result = resolve_expr ctx lb.lb_body in
    Symbol.exit_scope ctx.symbols;
    result

  | EIf (cond, then_, else_, _) ->
    let* () = resolve_expr ctx cond in
    let* () = resolve_expr ctx then_ in
    resolve_expr ctx else_

  | ECase (scrut, branches, _) ->
    let* () = resolve_expr ctx scrut in
    List.fold_left (fun acc branch ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        Symbol.enter_scope ctx.symbols Symbol.ScopeMatch;
        let result =
          let* _ = resolve_pattern ctx branch.cb_pat in
          let* () = match branch.cb_guard with
            | Some g -> resolve_expr ctx g
            | None -> Ok ()
          in
          resolve_expr ctx branch.cb_body
        in
        Symbol.exit_scope ctx.symbols;
        result
    ) (Ok ()) branches

  | ETuple (exprs, _) ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) exprs

  | ERecord (fields, _) ->
    List.fold_left (fun acc (_, e) ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) fields

  | ERecordAccess (e, _, _) ->
    resolve_expr ctx e

  | ERecordUpdate (base, _, value, _) ->
    let* () = resolve_expr ctx base in
    resolve_expr ctx value

  | EArray (elems, _) ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) elems

  | EIndex (arr, idx, _) ->
    let* () = resolve_expr ctx arr in
    resolve_expr ctx idx

  | EHandle (body, handler, _) ->
    let* () = resolve_expr ctx body in
    resolve_handler ctx handler

  | EPerform (_, arg, _) ->
    resolve_expr ctx arg

  | EResume (arg, _) ->
    resolve_expr ctx arg

  | EBlock (exprs, _) ->
    Symbol.enter_scope ctx.symbols Symbol.ScopeBlock;
    let result = List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) exprs in
    Symbol.exit_scope ctx.symbols;
    result

  | EBinOp (left, _, right, _) ->
    let* () = resolve_expr ctx left in
    resolve_expr ctx right

  | EUnaryOp (_, e, _) ->
    resolve_expr ctx e

  | ETyApp (e, _, _) ->
    resolve_expr ctx e

  | EUnsafe (e, _) ->
    resolve_expr ctx e

  | EUnsafeCoerce (e, _, _) ->
    resolve_expr ctx e

and resolve_handler (ctx : context) (handler : handler) : unit result =
  (* Resolve return clause *)
  let* () = match handler.h_return with
    | Some (id, body) ->
      Symbol.enter_scope ctx.symbols Symbol.ScopeHandler;
      let _ = Symbol.define ctx.symbols id.id_name
          Symbol.SKVariable id.id_span Private in
      let result = resolve_expr ctx body in
      Symbol.exit_scope ctx.symbols;
      result
    | None -> Ok ()
  in
  (* Resolve operation clauses *)
  List.fold_left (fun acc clause ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      Symbol.enter_scope ctx.symbols Symbol.ScopeHandler;
      (* Bind operation parameters and continuation *)
      List.iter (fun (id, _) ->
        let _ = Symbol.define ctx.symbols id.id_name
            Symbol.SKVariable id.id_span Private in
        ()
      ) clause.oc_params;
      let _ = Symbol.define ctx.symbols clause.oc_resume.id_name
          Symbol.SKVariable clause.oc_resume.id_span Private in
      let result = resolve_expr ctx clause.oc_body in
      Symbol.exit_scope ctx.symbols;
      result
  ) (Ok ()) handler.h_ops

(** Resolve a top-level declaration *)
let resolve_decl (ctx : context) (decl : decl) : unit result =
  match decl with
  | DFun fd ->
    (* First, define the function itself for recursion *)
    let _ = Symbol.define ctx.symbols fd.fd_name.id_name
        Symbol.SKFunction fd.fd_name.id_span fd.fd_vis in
    (* Then resolve the body *)
    Symbol.enter_scope ctx.symbols (Symbol.ScopeFunction fd.fd_name.id_name);
    (* Bind type parameters *)
    List.iter (fun tp ->
      let _ = Symbol.define ctx.symbols tp.tp_name.id_name
          Symbol.SKTypeVar tp.tp_name.id_span Private in
      ()
    ) fd.fd_ty_params;
    (* Bind parameters *)
    List.iter (fun (id, _, _) ->
      let _ = Symbol.define ctx.symbols id.id_name
          Symbol.SKVariable id.id_span Private in
      ()
    ) fd.fd_params;
    let result = match fd.fd_body with
      | Some body -> resolve_expr ctx body
      | None -> Ok ()
    in
    Symbol.exit_scope ctx.symbols;
    result

  | DType td ->
    let _ = Symbol.define ctx.symbols td.td_name.id_name
        Symbol.SKType td.td_name.id_span td.td_vis in
    Ok ()

  | DEffect ed ->
    let _ = Symbol.define ctx.symbols ed.ed_name.id_name
        Symbol.SKEffect ed.ed_name.id_span ed.ed_vis in
    (* Define each operation *)
    List.iter (fun op ->
      let _ = Symbol.define ctx.symbols op.eo_name.id_name
          Symbol.SKEffectOp op.eo_name.id_span ed.ed_vis in
      ()
    ) ed.ed_ops;
    Ok ()

  | DTrait td ->
    let _ = Symbol.define ctx.symbols td.trd_name.id_name
        Symbol.SKTrait td.trd_name.id_span td.trd_vis in
    Ok ()

  | DImpl _ ->
    (* TODO: Resolve impl blocks *)
    Ok ()

  | DModule (name, decls, _) ->
    let _ = Symbol.define ctx.symbols name.id_name
        Symbol.SKModule name.id_span Private in
    Symbol.enter_scope ctx.symbols (Symbol.ScopeModule name.id_name);
    let result = List.fold_left (fun acc d ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_decl ctx d
    ) (Ok ()) decls in
    Symbol.exit_scope ctx.symbols;
    result

  | DImport _ ->
    (* TODO: Handle imports *)
    Ok ()

(** Resolve an entire program *)
let resolve_program (program : program) : (context, resolve_error * Span.t) Result.t =
  let ctx = create_context () in
  match List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> resolve_decl ctx decl
  ) (Ok ()) program.prog_decls with
  | Ok () -> Ok ctx
  | Error e -> Error e

(* Helper for Result bind *)
let ( let* ) = Result.bind

(* TODO: Phase 1 implementation
   - [ ] Module qualified lookups
   - [ ] Import resolution (use, use as, use *)
   - [ ] Visibility checking
   - [ ] Forward references in mutual recursion
   - [ ] Type alias expansion during resolution
*)
