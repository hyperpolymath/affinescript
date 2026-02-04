(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

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

(* Helper for Result bind *)
let ( let* ) = Result.bind

(** Create a new resolution context *)
let create_context () : context =
  {
    symbols = Symbol.create ();
    current_module = [];
    imports = [];
  }

(** Resolve an identifier *)
let resolve_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym -> Ok sym
  | None -> Error (UndefinedVariable id, id.span)

(** Resolve a type identifier *)
let resolve_type_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKType -> Ok sym
  | Some sym when sym.sym_kind = Symbol.SKTypeVar -> Ok sym
  | Some _ -> Error (UndefinedType id, id.span)
  | None -> Error (UndefinedType id, id.span)

(** Resolve an effect identifier *)
let resolve_effect_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKEffect -> Ok sym
  | Some _ -> Error (UndefinedEffect id, id.span)
  | None -> Error (UndefinedEffect id, id.span)

(** Resolve a pattern, binding variables *)
let rec resolve_pattern (ctx : context) (pat : pattern) : context result =
  match pat with
  | PatWildcard _ -> Ok ctx
  | PatVar id ->
    if Symbol.is_defined_locally ctx.symbols id.name then
      Error (DuplicateDefinition id, id.span)
    else begin
      let _ = Symbol.define ctx.symbols id.name
          Symbol.SKVariable id.span Private in
      Ok ctx
    end
  | PatLit _ -> Ok ctx
  | PatTuple pats ->
    List.fold_left (fun acc pat ->
      match acc with
      | Error e -> Error e
      | Ok ctx -> resolve_pattern ctx pat
    ) (Ok ctx) pats
  | PatRecord (fields, _has_rest) ->
    List.fold_left (fun acc (_id, pat_opt) ->
      match acc with
      | Error e -> Error e
      | Ok ctx ->
        match pat_opt with
        | Some pat -> resolve_pattern ctx pat
        | None -> Ok ctx
    ) (Ok ctx) fields
  | PatCon (_con, pats) ->
    List.fold_left (fun acc pat ->
      match acc with
      | Error e -> Error e
      | Ok ctx -> resolve_pattern ctx pat
    ) (Ok ctx) pats
  | PatOr (p1, p2) ->
    (* Both branches must bind the same variables *)
    let* ctx1 = resolve_pattern ctx p1 in
    let* _ctx2 = resolve_pattern ctx p2 in
    Ok ctx1
  | PatAs (id, pat) ->
    let* ctx = resolve_pattern ctx pat in
    if Symbol.is_defined_locally ctx.symbols id.name then
      Error (DuplicateDefinition id, id.span)
    else begin
      let _ = Symbol.define ctx.symbols id.name
          Symbol.SKVariable id.span Private in
      Ok ctx
    end

(** Resolve an expression *)
let rec resolve_expr (ctx : context) (expr : expr) : unit result =
  match expr with
  | ExprVar id ->
    let* _ = resolve_ident ctx id in
    Ok ()

  | ExprLit _ -> Ok ()

  | ExprApp (func, args) ->
    let* () = resolve_expr ctx func in
    List.fold_left (fun acc arg ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx arg
    ) (Ok ()) args

  | ExprLambda lam ->
    Symbol.enter_scope ctx.symbols (Symbol.ScopeFunction "lambda");
    (* Bind parameters *)
    List.iter (fun param ->
      let _ = Symbol.define ctx.symbols param.p_name.name
          Symbol.SKVariable param.p_name.span Private in
      ()
    ) lam.elam_params;
    let result = resolve_expr ctx lam.elam_body in
    Symbol.exit_scope ctx.symbols;
    result

  | ExprLet lb ->
    let* () = resolve_expr ctx lb.el_value in
    Symbol.enter_scope ctx.symbols Symbol.ScopeBlock;
    let* _ = resolve_pattern ctx lb.el_pat in
    let result = match lb.el_body with
      | Some body -> resolve_expr ctx body
      | None -> Ok ()
    in
    Symbol.exit_scope ctx.symbols;
    result

  | ExprIf ei ->
    let* () = resolve_expr ctx ei.ei_cond in
    let* () = resolve_expr ctx ei.ei_then in
    (match ei.ei_else with
     | Some e -> resolve_expr ctx e
     | None -> Ok ())

  | ExprMatch em ->
    let* () = resolve_expr ctx em.em_scrutinee in
    List.fold_left (fun acc arm ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        Symbol.enter_scope ctx.symbols Symbol.ScopeMatch;
        let result =
          let* _ = resolve_pattern ctx arm.ma_pat in
          let* () = match arm.ma_guard with
            | Some g -> resolve_expr ctx g
            | None -> Ok ()
          in
          resolve_expr ctx arm.ma_body
        in
        Symbol.exit_scope ctx.symbols;
        result
    ) (Ok ()) em.em_arms

  | ExprTuple exprs ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) exprs

  | ExprArray exprs ->
    List.fold_left (fun acc e ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_expr ctx e
    ) (Ok ()) exprs

  | ExprRecord er ->
    let* () = List.fold_left (fun acc (_id, e_opt) ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        match e_opt with
        | Some e -> resolve_expr ctx e
        | None -> Ok ()
    ) (Ok ()) er.er_fields in
    (match er.er_spread with
     | Some e -> resolve_expr ctx e
     | None -> Ok ())

  | ExprField (e, _field) ->
    resolve_expr ctx e

  | ExprTupleIndex (e, _idx) ->
    resolve_expr ctx e

  | ExprIndex (arr, idx) ->
    let* () = resolve_expr ctx arr in
    resolve_expr ctx idx

  | ExprBinary (left, _op, right) ->
    let* () = resolve_expr ctx left in
    resolve_expr ctx right

  | ExprUnary (_op, e) ->
    resolve_expr ctx e

  | ExprBlock blk ->
    resolve_block ctx blk

  | ExprReturn e_opt ->
    (match e_opt with
     | Some e -> resolve_expr ctx e
     | None -> Ok ())

  | ExprHandle eh ->
    let* () = resolve_expr ctx eh.eh_body in
    List.fold_left (fun acc arm ->
      match acc with
      | Error e -> Error e
      | Ok () ->
        Symbol.enter_scope ctx.symbols Symbol.ScopeHandler;
        let result = match arm with
          | HandlerReturn (pat, body) ->
            let* _ = resolve_pattern ctx pat in
            resolve_expr ctx body
          | HandlerOp (_op, pats, body) ->
            let* _ = List.fold_left (fun acc pat ->
              match acc with
              | Error e -> Error e
              | Ok ctx -> resolve_pattern ctx pat
            ) (Ok ctx) pats in
            resolve_expr ctx body
        in
        Symbol.exit_scope ctx.symbols;
        result
    ) (Ok ()) eh.eh_handlers

  | ExprResume e_opt ->
    (match e_opt with
     | Some e -> resolve_expr ctx e
     | None -> Ok ())

  | ExprRowRestrict (e, _field) ->
    resolve_expr ctx e

  | ExprTry et ->
    let* () = resolve_block ctx et.et_body in
    let* () = match et.et_catch with
      | Some arms ->
        List.fold_left (fun acc arm ->
          match acc with
          | Error e -> Error e
          | Ok () ->
            Symbol.enter_scope ctx.symbols Symbol.ScopeMatch;
            let* _ = resolve_pattern ctx arm.ma_pat in
            let result = resolve_expr ctx arm.ma_body in
            Symbol.exit_scope ctx.symbols;
            result
        ) (Ok ()) arms
      | None -> Ok ()
    in
    (match et.et_finally with
     | Some blk -> resolve_block ctx blk
     | None -> Ok ())

  | ExprUnsafe ops ->
    (* Resolve expressions within unsafe operations *)
    List.fold_left (fun acc op ->
      let* () = acc in
      match op with
      | UnsafeRead e -> resolve_expr ctx e
      | UnsafeWrite (e1, e2) ->
        let* () = resolve_expr ctx e1 in
        resolve_expr ctx e2
      | UnsafeOffset (e1, e2) ->
        let* () = resolve_expr ctx e1 in
        resolve_expr ctx e2
      | UnsafeTransmute (_, _, e) -> resolve_expr ctx e
      | UnsafeForget e -> resolve_expr ctx e
      | UnsafeAssume _ -> Ok ()  (* Predicates don't need resolution *)
    ) (Ok ()) ops

  | ExprVariant (_ty, _variant) ->
    Ok ()

  | ExprSpan (e, _span) ->
    resolve_expr ctx e

and resolve_block (ctx : context) (blk : block) : unit result =
  Symbol.enter_scope ctx.symbols Symbol.ScopeBlock;
  let result =
    let* () = List.fold_left (fun acc stmt ->
      match acc with
      | Error e -> Error e
      | Ok () -> resolve_stmt ctx stmt
    ) (Ok ()) blk.blk_stmts in
    match blk.blk_expr with
    | Some e -> resolve_expr ctx e
    | None -> Ok ()
  in
  Symbol.exit_scope ctx.symbols;
  result

and resolve_stmt (ctx : context) (stmt : stmt) : unit result =
  match stmt with
  | StmtLet sl ->
    let* () = resolve_expr ctx sl.sl_value in
    let* _ = resolve_pattern ctx sl.sl_pat in
    Ok ()
  | StmtExpr e ->
    resolve_expr ctx e
  | StmtAssign (lhs, _op, rhs) ->
    let* () = resolve_expr ctx lhs in
    resolve_expr ctx rhs
  | StmtWhile (cond, body) ->
    let* () = resolve_expr ctx cond in
    resolve_block ctx body
  | StmtFor (pat, iter, body) ->
    let* () = resolve_expr ctx iter in
    Symbol.enter_scope ctx.symbols Symbol.ScopeBlock;
    let* _ = resolve_pattern ctx pat in
    let result = resolve_block ctx body in
    Symbol.exit_scope ctx.symbols;
    result

(** Resolve a top-level declaration *)
let rec resolve_decl (ctx : context) (decl : top_level) : unit result =
  match decl with
  | TopFn fd ->
    (* First, define the function itself for recursion *)
    let _ = Symbol.define ctx.symbols fd.fd_name.name
        Symbol.SKFunction fd.fd_name.span fd.fd_vis in
    (* Then resolve the body *)
    Symbol.enter_scope ctx.symbols (Symbol.ScopeFunction fd.fd_name.name);
    (* Bind type parameters *)
    List.iter (fun tp ->
      let _ = Symbol.define ctx.symbols tp.tp_name.name
          Symbol.SKTypeVar tp.tp_name.span Private in
      ()
    ) fd.fd_type_params;
    (* Bind parameters *)
    List.iter (fun param ->
      let _ = Symbol.define ctx.symbols param.p_name.name
          Symbol.SKVariable param.p_name.span Private in
      ()
    ) fd.fd_params;
    let result = match fd.fd_body with
      | FnBlock blk -> resolve_block ctx blk
      | FnExpr e -> resolve_expr ctx e
    in
    Symbol.exit_scope ctx.symbols;
    result

  | TopType td ->
    let _ = Symbol.define ctx.symbols td.td_name.name
        Symbol.SKType td.td_name.span td.td_vis in
    Ok ()

  | TopEffect ed ->
    let _ = Symbol.define ctx.symbols ed.ed_name.name
        Symbol.SKEffect ed.ed_name.span ed.ed_vis in
    (* Define each operation *)
    List.iter (fun op ->
      let _ = Symbol.define ctx.symbols op.eod_name.name
          Symbol.SKEffectOp op.eod_name.span ed.ed_vis in
      ()
    ) ed.ed_ops;
    Ok ()

  | TopTrait td ->
    let _ = Symbol.define ctx.symbols td.trd_name.name
        Symbol.SKTrait td.trd_name.span td.trd_vis in
    Ok ()

  | TopImpl ib ->
    (* Resolve impl blocks - check trait reference and methods *)
    Symbol.enter_scope ctx.symbols (Symbol.ScopeBlock);
    (* Bind type parameters *)
    List.iter (fun tp ->
      let _ = Symbol.define ctx.symbols tp.tp_name.name
          Symbol.SKTypeVar tp.tp_name.span Private in
      ()
    ) ib.ib_type_params;
    (* Resolve each impl item *)
    let result = List.fold_left (fun acc item ->
      let* () = acc in
      match item with
      | ImplFn fd -> resolve_decl ctx (TopFn fd)
      | ImplType _ -> Ok ()
    ) (Ok ()) ib.ib_items in
    Symbol.exit_scope ctx.symbols;
    result

  | TopConst tc ->
    let _ = Symbol.define ctx.symbols tc.tc_name.name
        Symbol.SKVariable tc.tc_name.span tc.tc_vis in
    resolve_expr ctx tc.tc_value

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

(** Resolve and type-check a loaded module's symbols *)
let resolve_and_typecheck_module (loaded_mod : Module_loader.loaded_module)
    : (Symbol.t * Typecheck.context) result =
  let prog = Module_loader.get_program loaded_mod in
  let symbols = Symbol.create () in
  let mod_ctx = { symbols; current_module = []; imports = [] } in

  (* Resolve all declarations in the module *)
  let* () = List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> resolve_decl mod_ctx decl
  ) (Ok ()) prog.prog_decls in

  (* Type-check all declarations *)
  let type_ctx = Typecheck.create_context symbols in
  let type_result = List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> Typecheck.check_decl type_ctx decl
  ) (Ok ()) prog.prog_decls in

  match type_result with
  | Ok () -> Ok (symbols, type_ctx)
  | Error type_err ->
    (* Convert type error to resolve error *)
    let msg = Typecheck.show_type_error type_err in
    Error (ImportError ("Type checking failed: " ^ msg), Span.dummy)

(** Import symbols from a resolved module into the current context *)
let import_resolved_symbols
    (dest_symbols : Symbol.t)
    (dest_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (source_symbols : Symbol.t)
    (source_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (_alias : string option) : unit =

  (* Get all public symbols from the source *)
  Hashtbl.iter (fun _id sym ->
    match sym.Symbol.sym_visibility with
    | Public | PubCrate ->
      (* Register symbol in destination *)
      let _ = Symbol.register_import dest_symbols sym None in
      (* Copy type information *)
      begin match Hashtbl.find_opt source_types sym.Symbol.sym_id with
        | Some scheme -> Hashtbl.replace dest_types sym.Symbol.sym_id scheme
        | None -> ()
      end
    | _ -> ()  (* Private symbols not imported *)
  ) source_symbols.all_symbols

(** Import specific items from resolved symbols *)
let import_specific_items
    (dest_symbols : Symbol.t)
    (dest_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (source_symbols : Symbol.t)
    (source_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (items : import_item list) : unit result =

  List.fold_left (fun acc item ->
    let* () = acc in
    let item_name = item.ii_name.name in
    match Symbol.lookup source_symbols item_name with
    | Some sym ->
      begin match sym.Symbol.sym_visibility with
        | Public | PubCrate ->
          let alias = Option.map (fun id -> id.name) item.ii_alias in
          let _ = Symbol.register_import dest_symbols sym alias in
          (* Copy type information *)
          begin match Hashtbl.find_opt source_types sym.Symbol.sym_id with
            | Some scheme -> Hashtbl.replace dest_types sym.Symbol.sym_id scheme
            | None -> ()
          end;
          Ok ()
        | _ ->
          Error (VisibilityError (item.ii_name, "Symbol is not public"), item.ii_name.span)
      end
    | None ->
      Error (UndefinedVariable item.ii_name, item.ii_name.span)
  ) (Ok ()) items

(** Resolve imports in a program using module loader *)
let resolve_imports_with_loader
    (ctx : context)
    (type_ctx : Typecheck.context)
    (loader : Module_loader.t)
    (imports : import_decl list) : unit result =
  List.fold_left (fun acc import ->
    let* () = acc in
    match import with
    | ImportSimple (path, alias) ->
      (* use A.B or use A.B as C *)
      let path_strs = List.map (fun id -> id.name) path in
      begin match Module_loader.load_module loader path_strs with
        | Ok loaded_mod ->
          (* Resolve and type-check the module *)
          begin match resolve_and_typecheck_module loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              let alias_str = Option.map (fun id -> id.name) alias in
              import_resolved_symbols ctx.symbols type_ctx.Typecheck.var_types
                mod_symbols mod_type_ctx.Typecheck.var_types alias_str;
              Ok ()
            | Error e -> Error e
          end
        | Error (Module_loader.ModuleNotFound _) ->
          let id = List.hd (List.rev path) in
          Error (UndefinedModule id, id.span)
        | Error e ->
          let id = List.hd (List.rev path) in
          Error (ImportError (Module_loader.show_load_error e), id.span)
      end
    | ImportList (path, items) ->
      (* use A.B::{x, y} *)
      let path_strs = List.map (fun id -> id.name) path in
      begin match Module_loader.load_module loader path_strs with
        | Ok loaded_mod ->
          (* Resolve and type-check the module *)
          begin match resolve_and_typecheck_module loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              import_specific_items ctx.symbols type_ctx.Typecheck.var_types
                mod_symbols mod_type_ctx.Typecheck.var_types items
            | Error e -> Error e
          end
        | Error (Module_loader.ModuleNotFound _) ->
          let id = List.hd (List.rev path) in
          Error (UndefinedModule id, id.span)
        | Error e ->
          let id = List.hd (List.rev path) in
          Error (ImportError (Module_loader.show_load_error e), id.span)
      end
    | ImportGlob path ->
      (* use A.B::* *)
      let path_strs = List.map (fun id -> id.name) path in
      begin match Module_loader.load_module loader path_strs with
        | Ok loaded_mod ->
          (* Resolve and type-check the module *)
          begin match resolve_and_typecheck_module loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              (* Import all public symbols *)
              Hashtbl.iter (fun _id sym ->
                match sym.Symbol.sym_visibility with
                | Public | PubCrate ->
                  let _ = Symbol.register_import ctx.symbols sym None in
                  (* Copy type information *)
                  begin match Hashtbl.find_opt mod_type_ctx.Typecheck.var_types sym.Symbol.sym_id with
                    | Some scheme -> Hashtbl.replace type_ctx.Typecheck.var_types sym.Symbol.sym_id scheme
                    | None -> ()
                  end
                | _ -> ()
              ) mod_symbols.all_symbols;
              Ok ()
            | Error e -> Error e
          end
        | Error (Module_loader.ModuleNotFound _) ->
          let id = List.hd (List.rev path) in
          Error (UndefinedModule id, id.span)
        | Error e ->
          let id = List.hd (List.rev path) in
          Error (ImportError (Module_loader.show_load_error e), id.span)
      end
  ) (Ok ()) imports

(** Resolve imports in a program (legacy, without module loader) *)
let resolve_imports (ctx : context) (imports : import_decl list) : unit result =
  List.fold_left (fun acc import ->
    let* () = acc in
    match import with
    | ImportSimple (path, alias) ->
      (* use A.B or use A.B as C *)
      let path_strs = List.map (fun id -> id.name) path in
      begin match Symbol.lookup_qualified ctx.symbols path_strs with
        | Some sym ->
          let alias_str = Option.map (fun id -> id.name) alias in
          let _ = Symbol.register_import ctx.symbols sym alias_str in
          Ok ()
        | None ->
          let id = List.hd (List.rev path) in
          Error (UndefinedModule id, id.span)
      end
    | ImportList (path, items) ->
      (* use A.B::{x, y} *)
      let _path_strs = List.map (fun id -> id.name) path in
      List.fold_left (fun acc item ->
        let* () = acc in
        match Symbol.lookup ctx.symbols item.ii_name.name with
        | Some sym ->
          let alias_str = Option.map (fun id -> id.name) item.ii_alias in
          let _ = Symbol.register_import ctx.symbols sym alias_str in
          Ok ()
        | None ->
          Error (UndefinedVariable item.ii_name, item.ii_name.span)
      ) (Ok ()) items
    | ImportGlob path ->
      (* use A.B::* - for now, just validate the path exists *)
      let path_strs = List.map (fun id -> id.name) path in
      begin match Symbol.lookup_qualified ctx.symbols path_strs with
        | Some _ -> Ok ()
        | None ->
          let id = List.hd (List.rev path) in
          Error (UndefinedModule id, id.span)
      end
  ) (Ok ()) imports

(** Resolve a complete program with imports *)
let resolve_program_with_imports (program : program) : (context, resolve_error * Span.t) Result.t =
  let ctx = create_context () in
  (* First resolve imports *)
  let* () = resolve_imports ctx program.prog_imports in
  (* Then resolve declarations *)
  match resolve_program program with
  | Ok resolved_ctx -> Ok resolved_ctx
  | Error e -> Error e

(** Resolve a complete program with module loader support *)
let resolve_program_with_loader
    (program : program)
    (loader : Module_loader.t) : (context * Typecheck.context) result =
  let ctx = create_context () in
  let type_ctx = Typecheck.create_context ctx.symbols in
  (* First resolve imports using module loader *)
  let* () = resolve_imports_with_loader ctx type_ctx loader program.prog_imports in
  (* Then resolve declarations *)
  let* () = List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> resolve_decl ctx decl
  ) (Ok ()) program.prog_decls in
  Ok (ctx, type_ctx)

(* Phase 1 complete. Future enhancements (Phase 2+):
   - Full module system with nested namespaces (Phase 2)
   - Forward reference resolution for mutual recursion (Phase 2)
   - Type alias expansion during resolution (Phase 2)
*)
