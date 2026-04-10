(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> *)

(** S-expression AST dump for AffineScript.

    Converts every AST node type defined in {!Ast} into a human-readable
    S-expression string.  Covers:

    - {!Ast.program}, {!Ast.top_level}
    - {!Ast.fn_decl}, {!Ast.type_decl}, {!Ast.effect_decl}, {!Ast.trait_decl},
      {!Ast.impl_block}
    - {!Ast.expr}, {!Ast.stmt}, {!Ast.block}
    - {!Ast.pattern}, {!Ast.literal}
    - {!Ast.type_expr}, {!Ast.effect_expr}
    - {!Ast.quantity}, {!Ast.ownership}, {!Ast.visibility}, {!Ast.kind}
*)

open Ast

(* ======================================================================
   HELPERS
   ====================================================================== *)

(** Produce a string of [n] spaces for indentation. *)
let indent n = String.make n ' '

(** Wrap a tag and children into an S-expression with newlines. *)
let sexp_block tag d children =
  let child_strs = List.map (fun c ->
    Printf.sprintf "\n%s%s" (indent (d + 2)) c
  ) children in
  Printf.sprintf "(%s%s)" tag (String.concat "" child_strs)

(* ======================================================================
   SIMPLE ENUMS
   ====================================================================== *)

(** Convert a quantity annotation to an S-expression tag. *)
let quantity_to_sexpr = function
  | QZero  -> "0"
  | QOne   -> "1"
  | QOmega -> "ω"

(** Convert an ownership modifier to an S-expression tag. *)
let ownership_to_sexpr = function
  | Own -> "own"
  | Ref -> "ref"
  | Mut -> "mut"

(** Convert a visibility modifier to an S-expression string. *)
let visibility_to_sexpr = function
  | Private   -> "private"
  | Public    -> "public"
  | PubCrate  -> "pub-crate"
  | PubSuper  -> "pub-super"
  | PubIn ids -> Printf.sprintf "(pub-in %s)"
    (String.concat "." (List.map (fun id -> id.name) ids))

(** Convert a kind to an S-expression string. *)
let rec kind_to_sexpr = function
  | KType          -> "Type"
  | KRow           -> "Row"
  | KEffect        -> "Effect"
  | KArrow (a, b)  -> Printf.sprintf "(-> %s %s)" (kind_to_sexpr a) (kind_to_sexpr b)

(** Convert a binary operator to a string. *)
let binary_op_to_string = function
  | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
  | OpEq -> "==" | OpNe -> "!=" | OpLt -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
  | OpAnd -> "and" | OpOr -> "or"
  | OpBitAnd -> "bit-and" | OpBitOr -> "bit-or" | OpBitXor -> "bit-xor"
  | OpShl -> "shl" | OpShr -> "shr"

(** Convert a unary operator to a string. *)
let unary_op_to_string = function
  | OpNeg -> "neg" | OpNot -> "not" | OpBitNot -> "bit-not"
  | OpRef -> "ref" | OpDeref -> "deref"

(** Convert an assignment operator to a string. *)
let assign_op_to_string = function
  | AssignEq  -> "=" | AssignAdd -> "+=" | AssignSub -> "-="
  | AssignMul -> "*=" | AssignDiv -> "/="

(* ======================================================================
   TYPE EXPRESSIONS
   ====================================================================== *)

(** Convert a type argument to S-expression form. *)
let rec type_arg_to_sexpr = function
  | TyArg ty   -> type_expr_to_sexpr ty

(** Convert a type expression to S-expression form. *)
and type_expr_to_sexpr = function
  | TyVar id  -> id.name
  | TyCon id  -> id.name
  | TyApp (id, args) ->
    Printf.sprintf "(%s %s)" id.name
      (String.concat " " (List.map type_arg_to_sexpr args))
  | TyArrow (a, _q, b, eff) ->
    let eff_str = match eff with
      | None -> ""
      | Some e -> Printf.sprintf " / %s" (effect_expr_to_sexpr e)
    in
    Printf.sprintf "(-> %s %s%s)" (type_expr_to_sexpr a) (type_expr_to_sexpr b) eff_str
  | TyTuple tys ->
    Printf.sprintf "(tuple %s)" (String.concat " " (List.map type_expr_to_sexpr tys))
  | TyRecord (fields, rest) ->
    let fields_str = String.concat " " (List.map (fun rf ->
      Printf.sprintf "(%s %s)" rf.rf_name.name (type_expr_to_sexpr rf.rf_ty)
    ) fields) in
    let rest_str = match rest with
      | None -> ""
      | Some id -> Printf.sprintf " ..%s" id.name
    in
    Printf.sprintf "(record %s%s)" fields_str rest_str
  | TyOwn ty   -> Printf.sprintf "(own %s)" (type_expr_to_sexpr ty)
  | TyRef ty   -> Printf.sprintf "(ref %s)" (type_expr_to_sexpr ty)
  | TyMut ty   -> Printf.sprintf "(mut %s)" (type_expr_to_sexpr ty)
  | TyHole     -> "_"

(** Convert an effect expression to S-expression form. *)
and effect_expr_to_sexpr = function
  | EffVar id         -> id.name
  | EffCon (id, args) ->
    if args = [] then id.name
    else Printf.sprintf "(%s %s)" id.name
      (String.concat " " (List.map type_arg_to_sexpr args))
  | EffUnion (a, b)   ->
    Printf.sprintf "(+ %s %s)" (effect_expr_to_sexpr a) (effect_expr_to_sexpr b)

(* ======================================================================
   PATTERNS
   ====================================================================== *)

(** Convert a pattern to S-expression form. *)
let rec pattern_to_sexpr = function
  | PatWildcard _       -> "_"
  | PatVar id           -> id.name
  | PatLit lit          -> literal_to_sexpr lit
  | PatCon (id, pats)   ->
    Printf.sprintf "(%s %s)" id.name
      (String.concat " " (List.map pattern_to_sexpr pats))
  | PatTuple pats       ->
    Printf.sprintf "(tuple %s)" (String.concat " " (List.map pattern_to_sexpr pats))
  | PatRecord (fields, is_open) ->
    let fields_str = String.concat " " (List.map (fun (id, pat) ->
      match pat with
      | None -> id.name
      | Some p -> Printf.sprintf "(%s %s)" id.name (pattern_to_sexpr p)
    ) fields) in
    let open_str = if is_open then " .." else "" in
    Printf.sprintf "(record %s%s)" fields_str open_str
  | PatOr (a, b)  -> Printf.sprintf "(or %s %s)" (pattern_to_sexpr a) (pattern_to_sexpr b)
  | PatAs (id, p) -> Printf.sprintf "(as %s %s)" id.name (pattern_to_sexpr p)

(** Convert a literal to S-expression form. *)
and literal_to_sexpr = function
  | LitInt (n, _)    -> string_of_int n
  | LitFloat (f, _)  -> string_of_float f
  | LitBool (b, _)   -> if b then "#t" else "#f"
  | LitChar (c, _)   -> Printf.sprintf "#\\%c" c
  | LitString (s, _) -> Printf.sprintf "\"%s\"" (String.escaped s)
  | LitUnit _        -> "()"

(* ======================================================================
   EXPRESSIONS
   ====================================================================== *)

(** Convert an expression to S-expression form.
    [d] is the current indentation depth. *)
let rec expr_to_sexpr d = function
  | ExprLit lit       -> literal_to_sexpr lit
  | ExprVar id        -> id.name
  | ExprLet { el_mut; el_quantity; el_pat; el_ty; el_value; el_body } ->
    let tag = if el_mut then "let-mut" else "let" in
    let q_str = match el_quantity with
      | None -> ""
      | Some QZero -> " #@erased"
      | Some QOne -> " #@linear"
      | Some QOmega -> " #@unrestricted"
    in
    let ty_str = match el_ty with
      | None -> ""
      | Some t -> Printf.sprintf " : %s" (type_expr_to_sexpr t)
    in
    let body_str = match el_body with
      | None -> ""
      | Some b -> Printf.sprintf "\n%s%s" (indent (d + 2)) (expr_to_sexpr (d + 2) b)
    in
    Printf.sprintf "(%s%s %s%s %s%s)" tag q_str
      (pattern_to_sexpr el_pat) ty_str (expr_to_sexpr (d + 2) el_value) body_str
  | ExprIf { ei_cond; ei_then; ei_else } ->
    let else_str = match ei_else with
      | None -> ""
      | Some e -> Printf.sprintf "\n%s%s" (indent (d + 2)) (expr_to_sexpr (d + 2) e)
    in
    Printf.sprintf "(if %s\n%s%s%s)"
      (expr_to_sexpr (d + 2) ei_cond)
      (indent (d + 2)) (expr_to_sexpr (d + 4) ei_then) else_str
  | ExprMatch { em_scrutinee; em_arms } ->
    let arms_str = String.concat "" (List.map (fun arm ->
      let guard_str = match arm.ma_guard with
        | None -> ""
        | Some g -> Printf.sprintf " (guard %s)" (expr_to_sexpr (d + 6) g)
      in
      Printf.sprintf "\n%s(%s%s %s)" (indent (d + 2))
        (pattern_to_sexpr arm.ma_pat) guard_str (expr_to_sexpr (d + 4) arm.ma_body)
    ) em_arms) in
    Printf.sprintf "(match %s%s)" (expr_to_sexpr (d + 2) em_scrutinee) arms_str
  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
    let params_str = String.concat " " (List.map (param_to_sexpr) elam_params) in
    let ret_str = match elam_ret_ty with
      | None -> ""
      | Some t -> Printf.sprintf " -> %s" (type_expr_to_sexpr t)
    in
    Printf.sprintf "(lambda (%s)%s\n%s%s)" params_str ret_str
      (indent (d + 2)) (expr_to_sexpr (d + 2) elam_body)
  | ExprApp (fn, args) ->
    Printf.sprintf "(app %s %s)" (expr_to_sexpr (d + 2) fn)
      (String.concat " " (List.map (expr_to_sexpr (d + 2)) args))
  | ExprField (e, id) ->
    Printf.sprintf "(field %s \"%s\")" (expr_to_sexpr (d + 2) e) id.name
  | ExprTupleIndex (e, i) ->
    Printf.sprintf "(tuple-index %s %d)" (expr_to_sexpr (d + 2) e) i
  | ExprIndex (e, idx) ->
    Printf.sprintf "(index %s %s)" (expr_to_sexpr (d + 2) e) (expr_to_sexpr (d + 2) idx)
  | ExprTuple exprs ->
    Printf.sprintf "(tuple %s)" (String.concat " " (List.map (expr_to_sexpr (d + 2)) exprs))
  | ExprArray exprs ->
    Printf.sprintf "(array %s)" (String.concat " " (List.map (expr_to_sexpr (d + 2)) exprs))
  | ExprRecord { er_fields; er_spread } ->
    let fields_str = String.concat " " (List.map (fun (id, e) ->
      match e with
      | None -> id.name
      | Some v -> Printf.sprintf "(%s %s)" id.name (expr_to_sexpr (d + 4) v)
    ) er_fields) in
    let spread_str = match er_spread with
      | None -> ""
      | Some e -> Printf.sprintf " (.. %s)" (expr_to_sexpr (d + 4) e)
    in
    Printf.sprintf "(record %s%s)" fields_str spread_str
  | ExprRowRestrict (e, id) ->
    Printf.sprintf "(row-restrict %s \"%s\")" (expr_to_sexpr (d + 2) e) id.name
  | ExprBinary (l, op, r) ->
    Printf.sprintf "(%s %s %s)" (binary_op_to_string op)
      (expr_to_sexpr (d + 2) l) (expr_to_sexpr (d + 2) r)
  | ExprUnary (op, e) ->
    Printf.sprintf "(%s %s)" (unary_op_to_string op) (expr_to_sexpr (d + 2) e)
  | ExprBlock blk -> block_to_sexpr d blk
  | ExprReturn e ->
    (match e with
     | None -> "(return)"
     | Some v -> Printf.sprintf "(return %s)" (expr_to_sexpr (d + 2) v))
  | ExprTry { et_body; et_catch; et_finally } ->
    let catch_str = match et_catch with
      | None -> ""
      | Some arms ->
        let arms_str = String.concat "" (List.map (fun arm ->
          Printf.sprintf "\n%s(%s %s)" (indent (d + 4))
            (pattern_to_sexpr arm.ma_pat) (expr_to_sexpr (d + 6) arm.ma_body)
        ) arms) in
        Printf.sprintf "\n%s(catch%s)" (indent (d + 2)) arms_str
    in
    let finally_str = match et_finally with
      | None -> ""
      | Some blk -> Printf.sprintf "\n%s(finally %s)" (indent (d + 2)) (block_to_sexpr (d + 4) blk)
    in
    Printf.sprintf "(try %s%s%s)" (block_to_sexpr (d + 2) et_body) catch_str finally_str
  | ExprHandle { eh_body; eh_handlers } ->
    let handlers_str = String.concat "" (List.map (fun h ->
      match h with
      | HandlerReturn (pat, body) ->
        Printf.sprintf "\n%s(return %s %s)" (indent (d + 2))
          (pattern_to_sexpr pat) (expr_to_sexpr (d + 4) body)
      | HandlerOp (id, pats, body) ->
        Printf.sprintf "\n%s(op %s (%s) %s)" (indent (d + 2)) id.name
          (String.concat " " (List.map pattern_to_sexpr pats))
          (expr_to_sexpr (d + 4) body)
    ) eh_handlers) in
    Printf.sprintf "(handle %s%s)" (expr_to_sexpr (d + 2) eh_body) handlers_str
  | ExprResume e ->
    (match e with
     | None -> "(resume)"
     | Some v -> Printf.sprintf "(resume %s)" (expr_to_sexpr (d + 2) v))
  | ExprUnsafe ops ->
    let ops_str = String.concat "" (List.map (fun op ->
      Printf.sprintf "\n%s%s" (indent (d + 2)) (unsafe_op_to_sexpr (d + 2) op)
    ) ops) in
    Printf.sprintf "(unsafe%s)" ops_str
  | ExprVariant (ty_id, var_id) ->
    Printf.sprintf "(variant %s %s)" ty_id.name var_id.name
  | ExprSpan (e, _) -> expr_to_sexpr d e

(** Convert a block to S-expression form. *)
and block_to_sexpr d blk =
  let stmts_str = String.concat "" (List.map (fun s ->
    Printf.sprintf "\n%s%s" (indent (d + 2)) (stmt_to_sexpr (d + 2) s)
  ) blk.blk_stmts) in
  let final_str = match blk.blk_expr with
    | None -> ""
    | Some e -> Printf.sprintf "\n%s%s" (indent (d + 2)) (expr_to_sexpr (d + 2) e)
  in
  Printf.sprintf "(block%s%s)" stmts_str final_str

(** Convert a statement to S-expression form. *)
and stmt_to_sexpr d = function
  | StmtLet { sl_mut; sl_quantity; sl_pat; sl_ty; sl_value } ->
    let tag = if sl_mut then "let-mut" else "let" in
    let q_str = match sl_quantity with
      | None -> ""
      | Some QZero -> " #@erased"
      | Some QOne -> " #@linear"
      | Some QOmega -> " #@unrestricted"
    in
    let ty_str = match sl_ty with
      | None -> ""
      | Some t -> Printf.sprintf " : %s" (type_expr_to_sexpr t)
    in
    Printf.sprintf "(%s%s %s%s %s)" tag q_str
      (pattern_to_sexpr sl_pat) ty_str (expr_to_sexpr (d + 2) sl_value)
  | StmtExpr e -> expr_to_sexpr d e
  | StmtAssign (lhs, op, rhs) ->
    Printf.sprintf "(assign %s %s %s)" (assign_op_to_string op)
      (expr_to_sexpr (d + 2) lhs) (expr_to_sexpr (d + 2) rhs)
  | StmtWhile (cond, body) ->
    Printf.sprintf "(while %s\n%s%s)" (expr_to_sexpr (d + 2) cond)
      (indent (d + 2)) (block_to_sexpr (d + 2) body)
  | StmtFor (pat, iter, body) ->
    Printf.sprintf "(for %s %s\n%s%s)" (pattern_to_sexpr pat)
      (expr_to_sexpr (d + 2) iter) (indent (d + 2)) (block_to_sexpr (d + 2) body)

(** Convert an unsafe operation to S-expression form. *)
and unsafe_op_to_sexpr d = function
  | UnsafeRead e ->
    Printf.sprintf "(unsafe-read %s)" (expr_to_sexpr (d + 2) e)
  | UnsafeWrite (dst, val_) ->
    Printf.sprintf "(unsafe-write %s %s)" (expr_to_sexpr (d + 2) dst) (expr_to_sexpr (d + 2) val_)
  | UnsafeOffset (base, off) ->
    Printf.sprintf "(unsafe-offset %s %s)" (expr_to_sexpr (d + 2) base) (expr_to_sexpr (d + 2) off)
  | UnsafeTransmute (from_ty, to_ty, e) ->
    Printf.sprintf "(unsafe-transmute %s %s %s)"
      (type_expr_to_sexpr from_ty) (type_expr_to_sexpr to_ty) (expr_to_sexpr (d + 2) e)
  | UnsafeForget e ->
    Printf.sprintf "(unsafe-forget %s)" (expr_to_sexpr (d + 2) e)

(** Convert a parameter to S-expression form. *)
and param_to_sexpr p =
  let q_str = match p.p_quantity with
    | None -> ""
    | Some q -> Printf.sprintf "%s " (quantity_to_sexpr q)
  in
  let own_str = match p.p_ownership with
    | None -> ""
    | Some o -> Printf.sprintf "%s " (ownership_to_sexpr o)
  in
  Printf.sprintf "(%s%s%s : %s)" q_str own_str p.p_name.name (type_expr_to_sexpr p.p_ty)

(* ======================================================================
   TYPE PARAMETERS & CONSTRAINTS
   ====================================================================== *)

(** Convert a type parameter to S-expression form. *)
let type_param_to_sexpr tp =
  let q_str = match tp.tp_quantity with
    | None -> ""
    | Some q -> Printf.sprintf "%s " (quantity_to_sexpr q)
  in
  let k_str = match tp.tp_kind with
    | None -> ""
    | Some k -> Printf.sprintf " : %s" (kind_to_sexpr k)
  in
  Printf.sprintf "(%s%s%s)" q_str tp.tp_name.name k_str

(** Convert a trait bound to S-expression form. *)
let trait_bound_to_sexpr tb =
  if tb.tb_args = [] then tb.tb_name.name
  else Printf.sprintf "(%s %s)" tb.tb_name.name
    (String.concat " " (List.map type_arg_to_sexpr tb.tb_args))

(** Convert a constraint to S-expression form. *)
let constraint_to_sexpr = function
  | ConstraintTrait (id, bounds) ->
    Printf.sprintf "(where %s : %s)" id.name
      (String.concat " + " (List.map trait_bound_to_sexpr bounds))

(* ======================================================================
   TOP-LEVEL DECLARATIONS
   ====================================================================== *)

(** Convert a function signature to S-expression form. *)
let fn_sig_to_sexpr d fs =
  let vis_str = visibility_to_sexpr fs.fs_vis in
  let tparams_str = match fs.fs_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let params_str = String.concat " " (List.map param_to_sexpr fs.fs_params) in
  let ret_str = match fs.fs_ret_ty with
    | None -> ""
    | Some t -> Printf.sprintf " -> %s" (type_expr_to_sexpr t)
  in
  let eff_str = match fs.fs_eff with
    | None -> ""
    | Some e -> Printf.sprintf " / %s" (effect_expr_to_sexpr e)
  in
  ignore d;
  Printf.sprintf "(fn-sig %s \"%s\"%s (%s)%s%s)"
    vis_str fs.fs_name.name tparams_str params_str ret_str eff_str

(** Convert a function body to S-expression form. *)
let fn_body_to_sexpr d = function
  | FnBlock blk -> block_to_sexpr d blk
  | FnExpr e    -> expr_to_sexpr d e

(** Convert a function declaration to S-expression form. *)
let fn_decl_to_sexpr d fd =
  let vis_str = visibility_to_sexpr fd.fd_vis in
  let total_str = if fd.fd_total then " total" else "" in
  let tparams_str = match fd.fd_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let params_str = String.concat " " (List.map param_to_sexpr fd.fd_params) in
  let ret_str = match fd.fd_ret_ty with
    | None -> ""
    | Some t -> Printf.sprintf " -> %s" (type_expr_to_sexpr t)
  in
  let eff_str = match fd.fd_eff with
    | None -> ""
    | Some e -> Printf.sprintf " / %s" (effect_expr_to_sexpr e)
  in
  let where_str = match fd.fd_where with
    | [] -> ""
    | cs -> Printf.sprintf "\n%s%s" (indent (d + 2))
      (String.concat " " (List.map constraint_to_sexpr cs))
  in
  Printf.sprintf "(fn %s%s \"%s\"%s (%s)%s%s%s\n%s%s)"
    vis_str total_str fd.fd_name.name tparams_str params_str
    ret_str eff_str where_str
    (indent (d + 2)) (fn_body_to_sexpr (d + 2) fd.fd_body)

(** Convert a type declaration to S-expression form. *)
let type_decl_to_sexpr d td =
  let vis_str = visibility_to_sexpr td.td_vis in
  let tparams_str = match td.td_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let body_str = match td.td_body with
    | TyAlias ty -> Printf.sprintf "(alias %s)" (type_expr_to_sexpr ty)
    | TyStruct fields ->
      let fields_str = String.concat "" (List.map (fun sf ->
        Printf.sprintf "\n%s(%s %s %s)" (indent (d + 4))
          (visibility_to_sexpr sf.sf_vis) sf.sf_name.name (type_expr_to_sexpr sf.sf_ty)
      ) fields) in
      Printf.sprintf "(struct%s)" fields_str
    | TyEnum variants ->
      let variants_str = String.concat "" (List.map (fun vd ->
        let fields_str = match vd.vd_fields with
          | [] -> ""
          | fs -> Printf.sprintf " %s" (String.concat " " (List.map type_expr_to_sexpr fs))
        in
        let ret_str = match vd.vd_ret_ty with
          | None -> ""
          | Some t -> Printf.sprintf " -> %s" (type_expr_to_sexpr t)
        in
        Printf.sprintf "\n%s(%s%s%s)" (indent (d + 4)) vd.vd_name.name fields_str ret_str
      ) variants) in
      Printf.sprintf "(enum%s)" variants_str
  in
  Printf.sprintf "(type %s \"%s\"%s\n%s%s)"
    vis_str td.td_name.name tparams_str (indent (d + 2)) body_str

(** Convert an effect declaration to S-expression form. *)
let effect_decl_to_sexpr d ed =
  let vis_str = visibility_to_sexpr ed.ed_vis in
  let tparams_str = match ed.ed_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let ops_str = String.concat "" (List.map (fun eod ->
    let params_str = String.concat " " (List.map param_to_sexpr eod.eod_params) in
    let ret_str = match eod.eod_ret_ty with
      | None -> ""
      | Some t -> Printf.sprintf " -> %s" (type_expr_to_sexpr t)
    in
    Printf.sprintf "\n%s(op \"%s\" (%s)%s)" (indent (d + 2))
      eod.eod_name.name params_str ret_str
  ) ed.ed_ops) in
  Printf.sprintf "(effect %s \"%s\"%s%s)" vis_str ed.ed_name.name tparams_str ops_str

(** Convert a trait declaration to S-expression form. *)
let trait_decl_to_sexpr d trd =
  let vis_str = visibility_to_sexpr trd.trd_vis in
  let tparams_str = match trd.trd_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let super_str = match trd.trd_super with
    | [] -> ""
    | bs -> Printf.sprintf " : %s" (String.concat " + " (List.map trait_bound_to_sexpr bs))
  in
  let items_str = String.concat "" (List.map (fun item ->
    Printf.sprintf "\n%s%s" (indent (d + 2)) (match item with
      | TraitFn fs -> fn_sig_to_sexpr (d + 2) fs
      | TraitFnDefault fd -> fn_decl_to_sexpr (d + 2) fd
      | TraitType { tt_name; tt_kind; tt_default } ->
        let kind_str = match tt_kind with
          | None -> ""
          | Some k -> Printf.sprintf " : %s" (kind_to_sexpr k)
        in
        let default_str = match tt_default with
          | None -> ""
          | Some t -> Printf.sprintf " = %s" (type_expr_to_sexpr t)
        in
        Printf.sprintf "(assoc-type \"%s\"%s%s)" tt_name.name kind_str default_str)
  ) trd.trd_items) in
  Printf.sprintf "(trait %s \"%s\"%s%s%s)" vis_str trd.trd_name.name tparams_str super_str items_str

(** Convert an impl block to S-expression form. *)
let impl_block_to_sexpr d ib =
  let tparams_str = match ib.ib_type_params with
    | [] -> ""
    | tps -> Printf.sprintf " [%s]" (String.concat " " (List.map type_param_to_sexpr tps))
  in
  let trait_str = match ib.ib_trait_ref with
    | None -> ""
    | Some tr ->
      let args_str = match tr.tr_args with
        | [] -> ""
        | args -> Printf.sprintf " %s" (String.concat " " (List.map type_arg_to_sexpr args))
      in
      Printf.sprintf " %s%s for" tr.tr_name.name args_str
  in
  let where_str = match ib.ib_where with
    | [] -> ""
    | cs -> Printf.sprintf "\n%s%s" (indent (d + 2))
      (String.concat " " (List.map constraint_to_sexpr cs))
  in
  let items_str = String.concat "" (List.map (fun item ->
    Printf.sprintf "\n%s%s" (indent (d + 2)) (match item with
      | ImplFn fd -> fn_decl_to_sexpr (d + 2) fd
      | ImplType (id, ty) ->
        Printf.sprintf "(type \"%s\" %s)" id.name (type_expr_to_sexpr ty))
  ) ib.ib_items) in
  Printf.sprintf "(impl%s%s %s%s%s)" tparams_str trait_str
    (type_expr_to_sexpr ib.ib_self_ty) where_str items_str

(** Convert a top-level declaration to S-expression form. *)
let top_level_to_sexpr d = function
  | TopFn fd     -> fn_decl_to_sexpr d fd
  | TopType td   -> type_decl_to_sexpr d td
  | TopEffect ed -> effect_decl_to_sexpr d ed
  | TopTrait trd -> trait_decl_to_sexpr d trd
  | TopImpl ib   -> impl_block_to_sexpr d ib
  | TopConst { tc_vis; tc_name; tc_ty; tc_value } ->
    Printf.sprintf "(const %s \"%s\" %s %s)"
      (visibility_to_sexpr tc_vis) tc_name.name
      (type_expr_to_sexpr tc_ty) (expr_to_sexpr (d + 2) tc_value)

(* ======================================================================
   IMPORTS & MODULE PATH
   ====================================================================== *)

(** Convert a module path to a dotted string. *)
let module_path_to_string path =
  String.concat "." (List.map (fun id -> id.name) path)

(** Convert an import declaration to S-expression form. *)
let import_to_sexpr = function
  | ImportSimple (path, alias) ->
    let alias_str = match alias with
      | None -> ""
      | Some id -> Printf.sprintf " as %s" id.name
    in
    Printf.sprintf "(import \"%s\"%s)" (module_path_to_string path) alias_str
  | ImportList (path, items) ->
    let items_str = String.concat " " (List.map (fun ii ->
      match ii.ii_alias with
      | None -> ii.ii_name.name
      | Some alias -> Printf.sprintf "(%s as %s)" ii.ii_name.name alias.name
    ) items) in
    Printf.sprintf "(import \"%s\" {%s})" (module_path_to_string path) items_str
  | ImportGlob path ->
    Printf.sprintf "(import \"%s\" *)" (module_path_to_string path)

(* ======================================================================
   COMPLETE PROGRAM
   ====================================================================== *)

(** Convert a complete AffineScript program to S-expression form.

    This is the main entry point for the S-expression dump.  The output
    is a single S-expression string that faithfully represents every
    node in the parse tree. *)
let program_to_sexpr prog =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "(program";
  (* Module declaration *)
  (match prog.prog_module with
   | None -> ()
   | Some path ->
     Buffer.add_string buf
       (Printf.sprintf "\n  (module \"%s\")" (module_path_to_string path)));
  (* Imports *)
  List.iter (fun imp ->
    Buffer.add_string buf "\n  ";
    Buffer.add_string buf (import_to_sexpr imp)
  ) prog.prog_imports;
  (* Declarations *)
  List.iter (fun decl ->
    Buffer.add_string buf "\n  ";
    Buffer.add_string buf (top_level_to_sexpr 2 decl)
  ) prog.prog_decls;
  Buffer.add_char buf ')';
  Buffer.contents buf
