(** Name resolution pass for AffineScript *)

open Ast
open Symbol

(** Resolution errors *)
type error =
  | UnboundVariable of string * Span.t
  | UnboundType of string * Span.t
  | UnboundModule of string list * Span.t
  | DuplicateDefinition of string * Span.t * Span.t option
  | InvalidBreak of Span.t
  | InvalidContinue of Span.t
  | InvalidReturn of Span.t
  | PrivateAccess of string * Span.t
  | CyclicDependency of string list

exception Resolution_error of error

let error_to_string = function
  | UnboundVariable (name, _) ->
      Printf.sprintf "Unbound variable: %s" name
  | UnboundType (name, _) ->
      Printf.sprintf "Unbound type: %s" name
  | UnboundModule (path, _) ->
      Printf.sprintf "Unbound module: %s" (String.concat "." path)
  | DuplicateDefinition (name, _, _) ->
      Printf.sprintf "Duplicate definition: %s" name
  | InvalidBreak _ -> "break outside of loop"
  | InvalidContinue _ -> "continue outside of loop"
  | InvalidReturn _ -> "return outside of function"
  | PrivateAccess (name, _) ->
      Printf.sprintf "Cannot access private symbol: %s" name
  | CyclicDependency path ->
      Printf.sprintf "Cyclic dependency: %s" (String.concat " -> " path)

(** Resolved identifier - original ident with symbol reference *)
type resolved_ident = {
  ri_name: string;
  ri_span: Span.t;
  ri_symbol: symbol;
}
[@@deriving show]

(** Resolution context *)
type context = {
  ctx_symtab: symbol_table;
  ctx_errors: error list ref;
  ctx_current_module: module_path;
  ctx_type_params: (string, symbol) Hashtbl.t;  (** In-scope type parameters *)
}

let create_context () = {
  ctx_symtab = Symbol.create ();
  ctx_errors = ref [];
  ctx_current_module = [];
  ctx_type_params = Hashtbl.create 8;
}

let add_error ctx err =
  ctx.ctx_errors := err :: !(ctx.ctx_errors)

(** Convert AST visibility to Symbol visibility *)
let convert_visibility = function
  | Ast.Private -> VisPrivate
  | Ast.Public -> VisPublic
  | Ast.PubCrate -> VisPubCrate
  | Ast.PubSuper -> VisPubSuper
  | Ast.PubIn path -> VisPubIn (List.map (fun id -> id.name) path)

(** Resolve a variable reference *)
let resolve_var ctx (id: ident) =
  match Symbol.lookup ctx.ctx_symtab id.name with
  | Some sym -> Some { ri_name = id.name; ri_span = id.span; ri_symbol = sym }
  | None ->
      add_error ctx (UnboundVariable (id.name, id.span));
      None

(** Resolve a type reference *)
let resolve_type_ref ctx (id: ident) =
  (* First check type parameters *)
  match Hashtbl.find_opt ctx.ctx_type_params id.name with
  | Some sym -> Some sym
  | None ->
      (* Then check regular scope *)
      match Symbol.lookup ctx.ctx_symtab id.name with
      | Some sym when sym.sym_kind = SKType || sym.sym_kind = SKTypeParam ->
          Some sym
      | Some _ ->
          add_error ctx (UnboundType (id.name, id.span));
          None
      | None ->
          add_error ctx (UnboundType (id.name, id.span));
          None

(** Define a variable in current scope *)
let define_var ctx ~name ~span ~mutable_ ~ty ~quantity =
  if Symbol.is_defined_local ctx.ctx_symtab name then begin
    let existing = Symbol.find_local (Symbol.current_scope ctx.ctx_symtab) name in
    let prev_span = Option.bind existing (fun s -> s.sym_span) in
    add_error ctx (DuplicateDefinition (name, span, prev_span))
  end;
  let sym = Symbol.make_symbol
    ~name
    ~kind:SKVariable
    ~span
    ~mutable_
    ?ty
    ?quantity
    ()
  in
  Symbol.register ctx.ctx_symtab sym

(** Define a function *)
let define_function ctx ~name ~span ~vis ~ty =
  if Symbol.is_defined_local ctx.ctx_symtab name then begin
    let existing = Symbol.find_local (Symbol.current_scope ctx.ctx_symtab) name in
    let prev_span = Option.bind existing (fun s -> s.sym_span) in
    add_error ctx (DuplicateDefinition (name, span, prev_span))
  end;
  let sym = Symbol.make_symbol
    ~name
    ~kind:SKFunction
    ~span
    ~vis:(convert_visibility vis)
    ?ty
    ()
  in
  Symbol.register ctx.ctx_symtab sym

(** Define a type *)
let define_type ctx ~name ~span ~vis =
  if Symbol.is_defined_local ctx.ctx_symtab name then begin
    let existing = Symbol.find_local (Symbol.current_scope ctx.ctx_symtab) name in
    let prev_span = Option.bind existing (fun s -> s.sym_span) in
    add_error ctx (DuplicateDefinition (name, span, prev_span))
  end;
  let sym = Symbol.make_symbol
    ~name
    ~kind:SKType
    ~span
    ~vis:(convert_visibility vis)
    ()
  in
  Symbol.register ctx.ctx_symtab sym

(** Define a type parameter *)
let define_type_param ctx (tp: type_param) =
  let sym = Symbol.make_symbol
    ~name:tp.tp_name.name
    ~kind:SKTypeParam
    ~span:tp.tp_name.span
    ?quantity:tp.tp_quantity
    ()
  in
  Hashtbl.replace ctx.ctx_type_params tp.tp_name.name sym;
  Symbol.register ctx.ctx_symtab sym

(** Clear type parameters after leaving scope *)
let clear_type_params ctx =
  Hashtbl.clear ctx.ctx_type_params

(** Enter scope helper *)
let enter_scope ctx kind =
  ignore (Symbol.enter_scope ctx.ctx_symtab kind)

(** Exit scope helper *)
let exit_scope ctx =
  Symbol.exit_scope ctx.ctx_symtab

(** Resolve a pattern, defining bound variables *)
let rec resolve_pattern ctx pat =
  match pat with
  | PatWildcard _ -> ()
  | PatVar id ->
      ignore (define_var ctx
        ~name:id.name
        ~span:id.span
        ~mutable_:false
        ~ty:None
        ~quantity:None)
  | PatLit _ -> ()
  | PatCon (id, pats) ->
      (* Resolve constructor reference *)
      (match Symbol.lookup ctx.ctx_symtab id.name with
       | Some sym when sym.sym_kind = SKVariant -> ()
       | _ -> add_error ctx (UnboundVariable (id.name, id.span)));
      List.iter (resolve_pattern ctx) pats
  | PatTuple pats ->
      List.iter (resolve_pattern ctx) pats
  | PatRecord (fields, _) ->
      List.iter (fun (_, pat_opt) ->
        Option.iter (resolve_pattern ctx) pat_opt
      ) fields
  | PatOr (p1, p2) ->
      (* Both branches should bind same names *)
      resolve_pattern ctx p1;
      resolve_pattern ctx p2
  | PatAs (id, p) ->
      ignore (define_var ctx
        ~name:id.name
        ~span:id.span
        ~mutable_:false
        ~ty:None
        ~quantity:None);
      resolve_pattern ctx p

(** Resolve a type expression *)
let rec resolve_type_expr ctx ty =
  match ty with
  | TyVar id | TyCon id ->
      ignore (resolve_type_ref ctx id)
  | TyApp (id, args) ->
      ignore (resolve_type_ref ctx id);
      List.iter (resolve_type_arg ctx) args
  | TyArrow (t1, t2, eff) ->
      resolve_type_expr ctx t1;
      resolve_type_expr ctx t2;
      Option.iter (resolve_effect_expr ctx) eff
  | TyDepArrow { da_param; da_param_ty; da_ret_ty; da_eff; _ } ->
      resolve_type_expr ctx da_param_ty;
      enter_scope ctx ScopeBlock;
      ignore (define_var ctx
        ~name:da_param.name
        ~span:da_param.span
        ~mutable_:false
        ~ty:(Some da_param_ty)
        ~quantity:None);
      resolve_type_expr ctx da_ret_ty;
      Option.iter (resolve_effect_expr ctx) da_eff;
      exit_scope ctx
  | TyTuple tys ->
      List.iter (resolve_type_expr ctx) tys
  | TyRecord (fields, rest) ->
      List.iter (fun rf -> resolve_type_expr ctx rf.rf_ty) fields;
      Option.iter (fun id -> ignore (resolve_type_ref ctx id)) rest
  | TyOwn t | TyRef t | TyMut t ->
      resolve_type_expr ctx t
  | TyRefined (t, _pred) ->
      resolve_type_expr ctx t
      (* TODO: resolve predicate names *)
  | TyHole -> ()

and resolve_type_arg ctx = function
  | TyArg t -> resolve_type_expr ctx t
  | NatArg _ -> ()  (* TODO: resolve nat expressions *)

and resolve_effect_expr ctx = function
  | EffVar id -> ignore (resolve_type_ref ctx id)
  | EffCon (id, args) ->
      ignore (resolve_type_ref ctx id);
      List.iter (resolve_type_arg ctx) args
  | EffUnion (e1, e2) ->
      resolve_effect_expr ctx e1;
      resolve_effect_expr ctx e2

(** Resolve an expression *)
let rec resolve_expr ctx expr =
  match expr with
  | ExprSpan (e, _) -> resolve_expr ctx e
  | ExprLit _ -> ()
  | ExprVar id ->
      ignore (resolve_var ctx id)
  | ExprLet { el_mut; el_pat; el_ty; el_value; el_body } ->
      resolve_expr ctx el_value;
      Option.iter (resolve_type_expr ctx) el_ty;
      resolve_pattern ctx el_pat;
      Option.iter (resolve_expr ctx) el_body
  | ExprIf { ei_cond; ei_then; ei_else } ->
      resolve_expr ctx ei_cond;
      resolve_expr ctx ei_then;
      Option.iter (resolve_expr ctx) ei_else
  | ExprMatch { em_scrutinee; em_arms } ->
      resolve_expr ctx em_scrutinee;
      List.iter (resolve_match_arm ctx) em_arms
  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
      enter_scope ctx ScopeFunction;
      List.iter (resolve_param ctx) elam_params;
      Option.iter (resolve_type_expr ctx) elam_ret_ty;
      resolve_expr ctx elam_body;
      exit_scope ctx
  | ExprApp (func, args) ->
      resolve_expr ctx func;
      List.iter (resolve_expr ctx) args
  | ExprField (e, _) ->
      resolve_expr ctx e
  | ExprTupleIndex (e, _) ->
      resolve_expr ctx e
  | ExprIndex (e1, e2) ->
      resolve_expr ctx e1;
      resolve_expr ctx e2
  | ExprTuple exprs | ExprArray exprs ->
      List.iter (resolve_expr ctx) exprs
  | ExprRecord { er_fields; er_spread } ->
      List.iter (fun (_, expr_opt) ->
        Option.iter (resolve_expr ctx) expr_opt
      ) er_fields;
      Option.iter (resolve_expr ctx) er_spread
  | ExprRowRestrict (e, _) ->
      resolve_expr ctx e
  | ExprBinary (e1, _, e2) ->
      resolve_expr ctx e1;
      resolve_expr ctx e2
  | ExprUnary (_, e) ->
      resolve_expr ctx e
  | ExprBlock blk ->
      resolve_block ctx blk
  | ExprReturn e_opt ->
      if not (Symbol.in_function (Symbol.current_scope ctx.ctx_symtab)) then
        add_error ctx (InvalidReturn (Span.dummy));  (* TODO: get span *)
      Option.iter (resolve_expr ctx) e_opt
  | ExprTry { et_body; et_catch; et_finally } ->
      resolve_block ctx et_body;
      Option.iter (List.iter (resolve_match_arm ctx)) et_catch;
      Option.iter (resolve_block ctx) et_finally
  | ExprHandle { eh_body; eh_handlers } ->
      resolve_expr ctx eh_body;
      List.iter (resolve_handler_arm ctx) eh_handlers
  | ExprResume e_opt ->
      Option.iter (resolve_expr ctx) e_opt
  | ExprUnsafe ops ->
      List.iter (resolve_unsafe_op ctx) ops
  | ExprVariant (type_id, _variant_id) ->
      ignore (resolve_type_ref ctx type_id)

and resolve_match_arm ctx arm =
  enter_scope ctx ScopeMatch;
  resolve_pattern ctx arm.ma_pat;
  Option.iter (resolve_expr ctx) arm.ma_guard;
  resolve_expr ctx arm.ma_body;
  exit_scope ctx

and resolve_handler_arm ctx = function
  | HandlerReturn (pat, e) ->
      enter_scope ctx ScopeMatch;
      resolve_pattern ctx pat;
      resolve_expr ctx e;
      exit_scope ctx
  | HandlerOp (_, pats, e) ->
      enter_scope ctx ScopeMatch;
      List.iter (resolve_pattern ctx) pats;
      resolve_expr ctx e;
      exit_scope ctx

and resolve_unsafe_op ctx = function
  | UnsafeRead e | UnsafeForget e -> resolve_expr ctx e
  | UnsafeWrite (e1, e2) | UnsafeOffset (e1, e2) ->
      resolve_expr ctx e1;
      resolve_expr ctx e2
  | UnsafeTransmute (t1, t2, e) ->
      resolve_type_expr ctx t1;
      resolve_type_expr ctx t2;
      resolve_expr ctx e
  | UnsafeAssume _ -> ()

and resolve_block ctx { blk_stmts; blk_expr } =
  enter_scope ctx ScopeBlock;
  List.iter (resolve_stmt ctx) blk_stmts;
  Option.iter (resolve_expr ctx) blk_expr;
  exit_scope ctx

and resolve_stmt ctx = function
  | StmtLet { sl_mut; sl_pat; sl_ty; sl_value } ->
      resolve_expr ctx sl_value;
      Option.iter (resolve_type_expr ctx) sl_ty;
      resolve_pattern ctx sl_pat
  | StmtExpr e ->
      resolve_expr ctx e
  | StmtAssign (target, _, value) ->
      resolve_expr ctx target;
      resolve_expr ctx value
  | StmtWhile (cond, body) ->
      resolve_expr ctx cond;
      enter_scope ctx ScopeLoop;
      resolve_block ctx body;
      exit_scope ctx
  | StmtFor (pat, iter, body) ->
      resolve_expr ctx iter;
      enter_scope ctx ScopeLoop;
      resolve_pattern ctx pat;
      resolve_block ctx body;
      exit_scope ctx

and resolve_param ctx (p: param) =
  resolve_type_expr ctx p.p_ty;
  ignore (define_var ctx
    ~name:p.p_name.name
    ~span:p.p_name.span
    ~mutable_:false
    ~ty:(Some p.p_ty)
    ~quantity:p.p_quantity)

(** Resolve a function declaration *)
let resolve_fn_decl ctx (decl: fn_decl) =
  (* Define the function first (for recursion) *)
  let fn_sym = define_function ctx
    ~name:decl.fd_name.name
    ~span:decl.fd_name.span
    ~vis:decl.fd_vis
    ~ty:None
  in
  (* Enter function scope *)
  enter_scope ctx ScopeFunction;
  (* Define type parameters *)
  List.iter (define_type_param ctx) decl.fd_type_params;
  (* Resolve parameters *)
  List.iter (resolve_param ctx) decl.fd_params;
  (* Resolve return type *)
  Option.iter (resolve_type_expr ctx) decl.fd_ret_ty;
  (* Resolve effect annotation *)
  Option.iter (resolve_effect_expr ctx) decl.fd_eff;
  (* Resolve body *)
  (match decl.fd_body with
   | FnBlock blk -> resolve_block ctx blk
   | FnExpr e -> resolve_expr ctx e);
  (* Exit scope *)
  exit_scope ctx;
  clear_type_params ctx;
  fn_sym

(** Resolve a type declaration *)
let resolve_type_decl ctx (decl: type_decl) =
  (* Define the type *)
  let type_sym = define_type ctx
    ~name:decl.td_name.name
    ~span:decl.td_name.span
    ~vis:decl.td_vis
  in
  (* Enter scope for type parameters *)
  enter_scope ctx ScopeBlock;
  List.iter (define_type_param ctx) decl.td_type_params;
  (* Resolve type body *)
  (match decl.td_body with
   | TyAlias ty ->
       resolve_type_expr ctx ty
   | TyStruct fields ->
       List.iter (fun sf -> resolve_type_expr ctx sf.sf_ty) fields
   | TyEnum variants ->
       List.iter (fun vd ->
         (* Define variant constructor *)
         let _ = Symbol.make_symbol
           ~name:vd.vd_name.name
           ~kind:SKVariant
           ~span:vd.vd_name.span
           ()
         in
         (* Register at global scope for constructor access *)
         List.iter (resolve_type_expr ctx) vd.vd_fields;
         Option.iter (resolve_type_expr ctx) vd.vd_ret_ty
       ) variants);
  exit_scope ctx;
  clear_type_params ctx;
  type_sym

(** Resolve an effect declaration *)
let resolve_effect_decl ctx (decl: effect_decl) =
  let effect_sym = Symbol.make_symbol
    ~name:decl.ed_name.name
    ~kind:SKEffect
    ~span:decl.ed_name.span
    ~vis:(convert_visibility decl.ed_vis)
    ()
  in
  ignore (Symbol.register ctx.ctx_symtab effect_sym);
  enter_scope ctx ScopeBlock;
  List.iter (define_type_param ctx) decl.ed_type_params;
  List.iter (fun op ->
    List.iter (resolve_param ctx) op.eod_params;
    Option.iter (resolve_type_expr ctx) op.eod_ret_ty
  ) decl.ed_ops;
  exit_scope ctx;
  clear_type_params ctx;
  effect_sym

(** Resolve a trait declaration *)
let resolve_trait_decl ctx (decl: trait_decl) =
  let trait_sym = Symbol.make_symbol
    ~name:decl.trd_name.name
    ~kind:SKTrait
    ~span:decl.trd_name.span
    ~vis:(convert_visibility decl.trd_vis)
    ()
  in
  ignore (Symbol.register ctx.ctx_symtab trait_sym);
  enter_scope ctx ScopeTrait;
  List.iter (define_type_param ctx) decl.trd_type_params;
  (* Resolve supertraits *)
  List.iter (fun tb ->
    ignore (resolve_type_ref ctx tb.tb_name);
    List.iter (resolve_type_arg ctx) tb.tb_args
  ) decl.trd_super;
  (* Resolve trait items *)
  List.iter (fun item ->
    match item with
    | TraitFn sig_ ->
        List.iter (define_type_param ctx) sig_.fs_type_params;
        List.iter (resolve_param ctx) sig_.fs_params;
        Option.iter (resolve_type_expr ctx) sig_.fs_ret_ty;
        Option.iter (resolve_effect_expr ctx) sig_.fs_eff
    | TraitFnDefault decl ->
        ignore (resolve_fn_decl ctx decl)
    | TraitType { tt_name; tt_default; _ } ->
        ignore (define_type ctx ~name:tt_name.name ~span:tt_name.span ~vis:Private);
        Option.iter (resolve_type_expr ctx) tt_default
  ) decl.trd_items;
  exit_scope ctx;
  clear_type_params ctx;
  trait_sym

(** Resolve an impl block *)
let resolve_impl_block ctx (impl: impl_block) =
  enter_scope ctx ScopeImpl;
  List.iter (define_type_param ctx) impl.ib_type_params;
  (* Resolve trait reference if present *)
  Option.iter (fun tr ->
    ignore (resolve_type_ref ctx tr.tr_name);
    List.iter (resolve_type_arg ctx) tr.tr_args
  ) impl.ib_trait_ref;
  (* Resolve self type *)
  resolve_type_expr ctx impl.ib_self_ty;
  (* Resolve impl items *)
  List.iter (fun item ->
    match item with
    | ImplFn decl -> ignore (resolve_fn_decl ctx decl)
    | ImplType (name, ty) ->
        ignore (define_type ctx ~name:name.name ~span:name.span ~vis:Private);
        resolve_type_expr ctx ty
  ) impl.ib_items;
  exit_scope ctx;
  clear_type_params ctx

(** Resolve an import declaration *)
let resolve_import _ctx _import =
  (* TODO: Module system implementation *)
  ()

(** Resolve a top-level declaration *)
let resolve_top_level ctx = function
  | TopFn decl -> ignore (resolve_fn_decl ctx decl)
  | TopType decl -> ignore (resolve_type_decl ctx decl)
  | TopEffect decl -> ignore (resolve_effect_decl ctx decl)
  | TopTrait decl -> ignore (resolve_trait_decl ctx decl)
  | TopImpl impl -> resolve_impl_block ctx impl
  | TopConst { tc_vis; tc_name; tc_ty; tc_value } ->
      resolve_type_expr ctx tc_ty;
      resolve_expr ctx tc_value;
      ignore (define_var ctx
        ~name:tc_name.name
        ~span:tc_name.span
        ~mutable_:false
        ~ty:(Some tc_ty)
        ~quantity:None)

(** Add built-in symbols to context *)
let add_builtins ctx =
  let add_builtin name kind =
    let sym = Symbol.make_symbol ~name ~kind ~vis:VisPublic () in
    ignore (Symbol.register ctx.ctx_symtab sym)
  in
  (* Built-in types *)
  add_builtin "Int" SKType;
  add_builtin "Float" SKType;
  add_builtin "Bool" SKType;
  add_builtin "Char" SKType;
  add_builtin "String" SKType;
  add_builtin "Unit" SKType;
  add_builtin "Never" SKType;
  add_builtin "Array" SKType;
  add_builtin "Nat" SKType;
  (* Built-in functions *)
  add_builtin "print" SKBuiltin;
  add_builtin "println" SKBuiltin;
  add_builtin "str" SKBuiltin;
  add_builtin "len" SKBuiltin;
  add_builtin "type_of" SKBuiltin;
  add_builtin "range" SKBuiltin;
  add_builtin "push" SKBuiltin;
  add_builtin "head" SKBuiltin;
  add_builtin "tail" SKBuiltin;
  add_builtin "map" SKBuiltin;
  add_builtin "filter" SKBuiltin;
  add_builtin "fold" SKBuiltin;
  add_builtin "assert" SKBuiltin;
  add_builtin "panic" SKBuiltin

(** Resolve a complete program *)
let resolve_program (prog: program) =
  let ctx = create_context () in
  add_builtins ctx;
  (* First pass: collect all top-level definitions *)
  (* (This allows forward references) *)
  List.iter (fun decl ->
    match decl with
    | TopFn fd ->
        ignore (define_function ctx
          ~name:fd.fd_name.name
          ~span:fd.fd_name.span
          ~vis:fd.fd_vis
          ~ty:None)
    | TopType td ->
        ignore (define_type ctx
          ~name:td.td_name.name
          ~span:td.td_name.span
          ~vis:td.td_vis)
    | TopEffect ed ->
        let sym = Symbol.make_symbol
          ~name:ed.ed_name.name
          ~kind:SKEffect
          ~span:ed.ed_name.span
          ()
        in
        ignore (Symbol.register ctx.ctx_symtab sym)
    | TopTrait td ->
        let sym = Symbol.make_symbol
          ~name:td.trd_name.name
          ~kind:SKTrait
          ~span:td.trd_name.span
          ()
        in
        ignore (Symbol.register ctx.ctx_symtab sym)
    | TopImpl _ -> ()
    | TopConst { tc_name; _ } ->
        ignore (define_var ctx
          ~name:tc_name.name
          ~span:tc_name.span
          ~mutable_:false
          ~ty:None
          ~quantity:None)
  ) prog.prog_decls;
  (* Second pass: resolve all definitions *)
  List.iter (resolve_import ctx) prog.prog_imports;
  List.iter (resolve_top_level ctx) prog.prog_decls;
  (* Return context with resolved symbols and any errors *)
  (ctx.ctx_symtab, List.rev !(ctx.ctx_errors))
