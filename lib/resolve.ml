(* SPDX-License-Identifier: MPL-2.0 *)
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

(** A recorded reference: a use-site for a symbol.
    Collected during resolution for the LSP's find-references feature. *)
type reference = {
  ref_symbol_id : Symbol.symbol_id;
  ref_span : Span.t;
}

(** Resolution context *)
type context = {
  symbols : Symbol.t;
  current_module : string list;
  imports : (string * Symbol.symbol) list;
  mutable references : reference list;
  (** Phase C: all use-site references collected during resolution.
      Each entry records which symbol was referenced and where. *)
}

(* Helper for Result bind *)
let ( let* ) = Result.bind

(** Seed the builtin functions/constructors into a symbol table.
    Shared by [create_context] (top-level program) and
    [resolve_and_typecheck_module] (imported modules) so that an imported
    stdlib module — e.g. [string.affine] pulled in by [use string::{...}] —
    resolves its own use of builtins like [len] instead of failing with
    [UndefinedVariable]. Must match the interpreter's create_initial_env. *)
let seed_builtins (symbols : Symbol.t) : unit =
  let def name = let _ = Symbol.define symbols name SKFunction Span.dummy Public in () in
  (* Console I/O *)
  def "print"; def "println"; def "eprint"; def "eprintln";
  (* WASI time (ADR-015 S4a, #180) *)
  def "clock_now_ms";
  (* WASI env / argv counts (ADR-015 S4b, #180) *)
  def "env_count"; def "arg_count";
  (* String / char builtins *)
  def "len"; def "slice"; def "string_get"; def "string_sub"; def "string_find";
  def "char_to_int"; def "int_to_char"; def "show";
  def "to_lowercase"; def "to_uppercase"; def "trim";
  def "int_to_string"; def "float_to_string"; def "string_length";
  def "parse_int"; def "parse_float";
  (* Numeric coercions and math *)
  def "int"; def "float";
  def "sqrt"; def "cbrt"; def "pow_float"; def "floor"; def "ceil"; def "round";
  def "trunc";
  def "abs"; def "max"; def "min";
  def "sin"; def "cos"; def "tan"; def "atan"; def "atan2";
  def "exp"; def "log"; def "log10"; def "log2";
  (* I/O — files, process, environment *)
  def "read_line"; def "read_file"; def "write_file"; def "append_file";
  def "file_exists"; def "is_directory";
  def "getenv"; def "setenv"; def "getcwd"; def "chdir";
  def "list_dir"; def "create_dir"; def "remove_file"; def "remove_dir";
  def "panic"; def "exit";
  (* STDLIB-04b (Refs #329): divergent throw with polymorphic return *)
  def "error";
  (* Time *)
  def "time_now";
  (* TEA runtime — The Elm Architecture interpreter loop *)
  def "tea_run";
  (* Cmd Msg — linear side-effect obligation builtins (Stage 11) *)
  def "cmd_none";
  def "cmd_perform";
  (* Option / Result value constructors — seeded as builtin constructors so
     the honest stdlib (stdlib/string.affine `char_at`, stdlib/result.affine,
     …) resolves without importing the legacy stdlib/prelude. `Option`/`Result`
     are already builtin type constructors; this closes the matching value-side
     gap. A user/prelude `type Option`/`type Result` decl cleanly shadows these
     (Hashtbl.replace in Symbol.define). Issue #122. *)
  let defc name =
    let _ = Symbol.define symbols name SKConstructor Span.dummy Public in ()
  in
  (* #138: Some/None/Ok/Err are no longer seeded as flat builtins — the
     stdlib now reaches them through the proper prelude/module path
     (`use prelude::{Some, None, Ok, Err}`; prelude.affine itself
     defines `Option`/`Result`). Removing the b895374 band-aid keeps it
     from becoming load-bearing. *)
  (* Interpreter builtin exception variant, pattern-matched in try/catch
     arms by the honest stdlib (testing.affine, result.affine). It has
     no module home by design, so it remains a genuine builtin. *)
  defc "RuntimeError"

(** Create a new resolution context, pre-seeded with builtins. *)
let create_context () : context =
  let ctx = {
    symbols = Symbol.create ();
    current_module = [];
    imports = [];
    references = [];
  } in
  seed_builtins ctx.symbols;
  ctx

(** Record a use-site reference for a symbol (Phase C: find-references). *)
let record_reference (ctx : context) (sym : Symbol.symbol) (span : Span.t) : unit =
  ctx.references <- { ref_symbol_id = sym.sym_id; ref_span = span } :: ctx.references

(** Resolve an identifier *)
let resolve_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym ->
    record_reference ctx sym id.span;
    Ok sym
  | None -> Error (UndefinedVariable id, id.span)

(** Resolve a type identifier *)
let resolve_type_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKType ->
    record_reference ctx sym id.span;
    Ok sym
  | Some sym when sym.sym_kind = Symbol.SKTypeVar ->
    record_reference ctx sym id.span;
    Ok sym
  | Some _ -> Error (UndefinedType id, id.span)
  | None -> Error (UndefinedType id, id.span)

(** Resolve an effect identifier *)
let resolve_effect_ident (ctx : context) (id : ident) : Symbol.symbol result =
  let name = id.name in
  match Symbol.lookup ctx.symbols name with
  | Some sym when sym.sym_kind = Symbol.SKEffect ->
    record_reference ctx sym id.span;
    Ok sym
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
      | FnExtern -> Ok ()  (* No body to resolve. *)
    in
    Symbol.exit_scope ctx.symbols;
    result

  | TopType td ->
    let _ = Symbol.define ctx.symbols td.td_name.name
        Symbol.SKType td.td_name.span td.td_vis in
    (* Register enum variant constructors so they are reachable as expressions *)
    (match td.td_body with
     | TyEnum variants ->
       List.iter (fun (vd : variant_decl) ->
         let _ = Symbol.define ctx.symbols vd.vd_name.name
             Symbol.SKConstructor vd.vd_name.span td.td_vis in
         ()
       ) variants
     | TyAlias _ | TyStruct _ | TyExtern -> ());
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

(** Pre-register a top-level declaration's *names* (no body resolution),
    so that forward references between top-level declarations resolve
    (issue #135 slice 11).  Previously [resolve_decl] defined a name then
    immediately resolved its body, and [resolve_program] folded in source
    order — so `fn a() { b() } fn b() {}` failed with `UndefinedVariable
    b`.  This is a standard two-pass: pass 1 declares every top-level
    name, pass 2 resolves bodies.  [Symbol.define] is [Hashtbl.replace]
    based, so the re-definition inside [resolve_decl] is idempotent. *)
let pre_register_decl (ctx : context) (decl : top_level) : unit =
  match decl with
  | TopFn fd ->
    ignore (Symbol.define ctx.symbols fd.fd_name.name
              Symbol.SKFunction fd.fd_name.span fd.fd_vis)
  | TopType td ->
    ignore (Symbol.define ctx.symbols td.td_name.name
              Symbol.SKType td.td_name.span td.td_vis);
    (match td.td_body with
     | TyEnum variants ->
       List.iter (fun (vd : variant_decl) ->
         ignore (Symbol.define ctx.symbols vd.vd_name.name
                   Symbol.SKConstructor vd.vd_name.span td.td_vis)
       ) variants
     | TyAlias _ | TyStruct _ | TyExtern -> ())
  | TopEffect ed ->
    ignore (Symbol.define ctx.symbols ed.ed_name.name
              Symbol.SKEffect ed.ed_name.span ed.ed_vis);
    List.iter (fun op ->
      ignore (Symbol.define ctx.symbols op.eod_name.name
                Symbol.SKEffectOp op.eod_name.span ed.ed_vis)
    ) ed.ed_ops
  | TopTrait td ->
    ignore (Symbol.define ctx.symbols td.trd_name.name
              Symbol.SKTrait td.trd_name.span td.trd_vis)
  | TopConst tc ->
    ignore (Symbol.define ctx.symbols tc.tc_name.name
              Symbol.SKVariable tc.tc_name.span tc.tc_vis)
  | TopImpl _ -> ()

(** Run the forward-declaration pass over a whole program. *)
let pre_register_program (ctx : context) (program : program) : unit =
  List.iter (pre_register_decl ctx) program.prog_decls

(** Resolve an entire program *)
let resolve_program (program : program) : (context, resolve_error * Span.t) Result.t =
  let ctx = create_context () in
  pre_register_program ctx program;
  match List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> resolve_decl ctx decl
  ) (Ok ()) program.prog_decls with
  | Ok () -> Ok ctx
  | Error e -> Error e

(* [resolve_and_typecheck_module] is defined below, mutually recursive
   with [resolve_imports_with_loader], so that an imported module's own
   `use` imports are resolved through the loader (no flat builtin seed). *)

(** Look up a scheme for [sym] in [source_types] (sym_id-keyed) first, then
    fall back to [source_name_types] (name-keyed). The fallback is needed
    because [resolve_and_typecheck_module] runs [Typecheck.check_decl], which
    binds via [bind_scheme] (name-keyed) but never writes [var_types]. *)
let lookup_source_scheme
    (source_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (source_name_types : (string, Types.scheme) Hashtbl.t)
    (sym : Symbol.symbol)
  : Types.scheme option =
  match Hashtbl.find_opt source_types sym.Symbol.sym_id with
  | Some sc -> Some sc
  | None    -> Hashtbl.find_opt source_name_types sym.Symbol.sym_name

(** Import symbols from a resolved module into the current context.

    [dest_name_types] is the destination type checker's name-keyed scheme map;
    populating it here is what makes imported functions visible to a freshly
    created [Typecheck.check_program] (which keys lookups on name, not sym_id). *)
let import_resolved_symbols
    (dest_symbols : Symbol.t)
    (dest_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (dest_name_types : (string, Types.scheme) Hashtbl.t)
    (source_symbols : Symbol.t)
    (source_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (source_name_types : (string, Types.scheme) Hashtbl.t)
    (_alias : string option) : unit =

  (* Get all public symbols from the source *)
  Hashtbl.iter (fun _id sym ->
    match sym.Symbol.sym_visibility with
    | Public | PubCrate ->
      (* Register symbol in destination *)
      let _ = Symbol.register_import dest_symbols sym None in
      Option.iter (fun scheme ->
        Hashtbl.replace dest_types sym.Symbol.sym_id scheme;
        Hashtbl.replace dest_name_types sym.Symbol.sym_name scheme
      ) (lookup_source_scheme source_types source_name_types sym)
    | _ -> ()  (* Private symbols not imported *)
  ) source_symbols.all_symbols

(** Import specific items from resolved symbols. See
    [import_resolved_symbols] for the role of [dest_name_types]. *)
let import_specific_items
    (dest_symbols : Symbol.t)
    (dest_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (dest_name_types : (string, Types.scheme) Hashtbl.t)
    (source_symbols : Symbol.t)
    (source_types : (Symbol.symbol_id, Types.scheme) Hashtbl.t)
    (source_name_types : (string, Types.scheme) Hashtbl.t)
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
          let bound_name = Option.value alias ~default:sym.Symbol.sym_name in
          Option.iter (fun scheme ->
            Hashtbl.replace dest_types sym.Symbol.sym_id scheme;
            Hashtbl.replace dest_name_types bound_name scheme
          ) (lookup_source_scheme source_types source_name_types sym);
          Ok ()
        | _ ->
          Error (VisibilityError (item.ii_name, "Symbol is not public"), item.ii_name.span)
      end
    | None ->
      Error (UndefinedVariable item.ii_name, item.ii_name.span)
  ) (Ok ()) items

(** Resolve imports in a program using module loader *)
let rec resolve_and_typecheck_module
    (loader : Module_loader.t)
    (loaded_mod : Module_loader.loaded_module)
    : (Symbol.t * Typecheck.context) result =
  let prog = Module_loader.get_program loaded_mod in
  let symbols = Symbol.create () in
  seed_builtins symbols;
  let mod_ctx = { symbols; current_module = []; imports = []; references = [] } in
  (* Type-check context, created up front so this module's own imports
     can populate its scheme maps before its decls are checked. *)
  let type_ctx = Typecheck.create_context symbols in
  Typecheck.register_builtins type_ctx;

  (* Resolve THIS module's own `use` imports first (#138 / #128
     coherence). A dependency module reaches Some/None/Ok/Err and its
     sibling modules through its own `use prelude::{...}` /
     `use string::{...}`, not a flat builtin seed. Mutually recursive
     with the loader; the stdlib import graph is an acyclic DAG
     (max depth io -> string -> prelude). *)
  let* () =
    resolve_imports_with_loader mod_ctx type_ctx loader prog.prog_imports
  in

  (* Pass 1: forward-declare every top-level name (#135 slice 11). *)
  pre_register_program mod_ctx prog;
  (* Pass 2: resolve all declaration bodies. *)
  let* () = List.fold_left (fun acc decl ->
    match acc with
    | Error e -> Error e
    | Ok () -> resolve_decl mod_ctx decl
  ) (Ok ()) prog.prog_decls in

  (* Type-check via [check_program] (NOT a raw check_decl fold): it runs
     the forward-declaration pass that pre-registers every function
     signature, so a module with internal forward references — e.g.
     collections.affine's `binary_search` calling the later
     `binary_search_helper` — type-checks on the import path exactly as
     it does standalone. The imports this module resolved above were
     written into [type_ctx.name_types]; thread them in as [import_types]
     so [check_program]'s fresh context still sees them. (#128 coherence:
     imported modules must check the same way top-level programs do.) *)
  match
    Typecheck.check_program
      ~import_types:type_ctx.Typecheck.name_types symbols prog
  with
  | Ok final_ctx -> Ok (symbols, final_ctx)
  | Error type_err ->
    let msg = Typecheck.show_type_error type_err in
    Error (ImportError ("Type checking failed: " ^ msg), Span.dummy)

and resolve_imports_with_loader
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
          begin match resolve_and_typecheck_module loader loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              let alias_str = Option.map (fun id -> id.name) alias in
              import_resolved_symbols ctx.symbols
                type_ctx.Typecheck.var_types
                type_ctx.Typecheck.name_types
                mod_symbols
                mod_type_ctx.Typecheck.var_types
                mod_type_ctx.Typecheck.name_types
                alias_str;
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
          begin match resolve_and_typecheck_module loader loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              import_specific_items ctx.symbols
                type_ctx.Typecheck.var_types
                type_ctx.Typecheck.name_types
                mod_symbols
                mod_type_ctx.Typecheck.var_types
                mod_type_ctx.Typecheck.name_types
                items
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
          begin match resolve_and_typecheck_module loader loaded_mod with
            | Ok (mod_symbols, mod_type_ctx) ->
              (* Import all public symbols *)
              Hashtbl.iter (fun _id sym ->
                match sym.Symbol.sym_visibility with
                | Public | PubCrate ->
                  let _ = Symbol.register_import ctx.symbols sym None in
                  Option.iter (fun scheme ->
                    Hashtbl.replace type_ctx.Typecheck.var_types sym.Symbol.sym_id scheme;
                    Hashtbl.replace type_ctx.Typecheck.name_types sym.Symbol.sym_name scheme
                  ) (lookup_source_scheme
                       mod_type_ctx.Typecheck.var_types
                       mod_type_ctx.Typecheck.name_types
                       sym)
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
(* ---- #178 INT-01: lower module-qualified value paths --------------------
   `use Mod;` (ImportSimple) flat-imports Mod's public symbols (see
   [import_resolved_symbols]), so a qualified *value* reference `Mod.fn` /
   `Mod.fn(x)` denotes the same flat symbol `fn`. The parser yields
   `ExprField (ExprVar Mod, fn)` (optionally ExprSpan-wrapped); rewrite it to
   `ExprVar fn` when `Mod` is a bound module qualifier from [prog_imports].
   This is the value-expression analogue of #241/ADR-014 (which handled
   qualified *type/effect* paths in the grammar). Pure; applied once on the
   parsed program before resolve/typecheck/codegen so every backend sees the
   lowered form uniformly. Genuine record access `r.f` is untouched (`r` is
   not an import qualifier). Only ImportSimple binds a qualifier; ImportList
   / ImportGlob bring names unqualified and bind no module name. *)
let import_qualifiers (imports : import_decl list) : (string, unit) Hashtbl.t =
  let h = Hashtbl.create 8 in
  List.iter (fun imp -> match imp with
    | ImportSimple (path, alias) ->
      let name = match alias with
        | Some id -> id.name
        | None -> (match List.rev path with id :: _ -> id.name | [] -> "")
      in
      if name <> "" then Hashtbl.replace h name ()
    | ImportList _ | ImportGlob _ -> ()
  ) imports;
  h

let rec strip_span (e : expr) : expr =
  match e with ExprSpan (e', _) -> strip_span e' | e -> e

let rec lower_expr quals (e : expr) : expr =
  match e with
  | ExprField (base, fld) ->
    (match strip_span base with
     | ExprVar m when Hashtbl.mem quals m.name -> ExprVar fld
     | _ -> ExprField (lower_expr quals base, fld))
  | ExprSpan (e', sp) -> ExprSpan (lower_expr quals e', sp)
  | ExprLit _ | ExprVar _ | ExprVariant _ -> e
  | ExprLet r ->
    ExprLet { r with el_value = lower_expr quals r.el_value;
                     el_body = Option.map (lower_expr quals) r.el_body }
  | ExprIf r ->
    ExprIf { ei_cond = lower_expr quals r.ei_cond;
             ei_then = lower_expr quals r.ei_then;
             ei_else = Option.map (lower_expr quals) r.ei_else }
  | ExprMatch r ->
    ExprMatch { em_scrutinee = lower_expr quals r.em_scrutinee;
                em_arms = List.map (lower_arm quals) r.em_arms }
  | ExprLambda r -> ExprLambda { r with elam_body = lower_expr quals r.elam_body }
  | ExprApp (f, args) ->
    ExprApp (lower_expr quals f, List.map (lower_expr quals) args)
  | ExprTupleIndex (e1, i) -> ExprTupleIndex (lower_expr quals e1, i)
  | ExprIndex (a, i) -> ExprIndex (lower_expr quals a, lower_expr quals i)
  | ExprTuple es -> ExprTuple (List.map (lower_expr quals) es)
  | ExprArray es -> ExprArray (List.map (lower_expr quals) es)
  | ExprRecord r ->
    ExprRecord
      { er_fields =
          List.map (fun (id, eo) -> (id, Option.map (lower_expr quals) eo))
            r.er_fields;
        er_spread = Option.map (lower_expr quals) r.er_spread }
  | ExprRowRestrict (e1, id) -> ExprRowRestrict (lower_expr quals e1, id)
  | ExprBinary (l, op, r) -> ExprBinary (lower_expr quals l, op, lower_expr quals r)
  | ExprUnary (op, e1) -> ExprUnary (op, lower_expr quals e1)
  | ExprBlock b -> ExprBlock (lower_block quals b)
  | ExprReturn eo -> ExprReturn (Option.map (lower_expr quals) eo)
  | ExprTry r ->
    ExprTry { et_body = lower_block quals r.et_body;
              et_catch = Option.map (List.map (lower_arm quals)) r.et_catch;
              et_finally = Option.map (lower_block quals) r.et_finally }
  | ExprHandle r ->
    ExprHandle { eh_body = lower_expr quals r.eh_body;
                 eh_handlers = List.map (lower_handler quals) r.eh_handlers }
  | ExprResume eo -> ExprResume (Option.map (lower_expr quals) eo)
  | ExprUnsafe ops -> ExprUnsafe (List.map (lower_unsafe quals) ops)

and lower_arm quals a =
  { a with ma_guard = Option.map (lower_expr quals) a.ma_guard;
           ma_body = lower_expr quals a.ma_body }

and lower_handler quals = function
  | HandlerReturn (p, e) -> HandlerReturn (p, lower_expr quals e)
  | HandlerOp (id, ps, e) -> HandlerOp (id, ps, lower_expr quals e)

and lower_unsafe quals = function
  | UnsafeRead e -> UnsafeRead (lower_expr quals e)
  | UnsafeWrite (a, b) -> UnsafeWrite (lower_expr quals a, lower_expr quals b)
  | UnsafeOffset (a, b) -> UnsafeOffset (lower_expr quals a, lower_expr quals b)
  | UnsafeTransmute (t1, t2, e) -> UnsafeTransmute (t1, t2, lower_expr quals e)
  | UnsafeForget e -> UnsafeForget (lower_expr quals e)

and lower_block quals b =
  { blk_stmts = List.map (lower_stmt quals) b.blk_stmts;
    blk_expr = Option.map (lower_expr quals) b.blk_expr }

and lower_stmt quals = function
  | StmtLet r -> StmtLet { r with sl_value = lower_expr quals r.sl_value }
  | StmtExpr e -> StmtExpr (lower_expr quals e)
  | StmtAssign (a, op, b) ->
    StmtAssign (lower_expr quals a, op, lower_expr quals b)
  | StmtWhile (e, b) -> StmtWhile (lower_expr quals e, lower_block quals b)
  | StmtFor (p, e, b) -> StmtFor (p, lower_expr quals e, lower_block quals b)

let lower_fn_body quals = function
  | FnBlock b -> FnBlock (lower_block quals b)
  | FnExpr e -> FnExpr (lower_expr quals e)
  | FnExtern -> FnExtern

let lower_top quals = function
  | TopFn fd -> TopFn { fd with fd_body = lower_fn_body quals fd.fd_body }
  | TopConst r -> TopConst { r with tc_value = lower_expr quals r.tc_value }
  | TopImpl ib ->
    TopImpl { ib with ib_items = List.map (function
      | ImplFn fd -> ImplFn { fd with fd_body = lower_fn_body quals fd.fd_body }
      | ImplType _ as it -> it) ib.ib_items }
  | TopTrait td ->
    TopTrait { td with trd_items = List.map (function
      | TraitFnDefault fd ->
        TraitFnDefault { fd with fd_body = lower_fn_body quals fd.fd_body }
      | other -> other) td.trd_items }
  | (TopType _ | TopEffect _ | TopExternType _ | TopExternFn _) as t -> t

(** #178: lower module-qualified value paths. Idempotent, pure. *)
let lower_qualified_value_paths (program : program) : program =
  let quals = import_qualifiers program.prog_imports in
  if Hashtbl.length quals = 0 then program
  else { program with
         prog_decls = List.map (lower_top quals) program.prog_decls }

let resolve_program_with_loader
    (program : program)
    (loader : Module_loader.t) : (context * Typecheck.context) result =
  let ctx = create_context () in
  let type_ctx = Typecheck.create_context ctx.symbols in
  (* First resolve imports using module loader *)
  let* () = resolve_imports_with_loader ctx type_ctx loader program.prog_imports in
  (* Pass 1: forward-declare every top-level name (#135 slice 11). *)
  pre_register_program ctx program;
  (* Pass 2: resolve declaration bodies. *)
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
