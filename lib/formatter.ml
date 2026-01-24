(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** AffineScript code formatter - AST pretty printer *)

open Ast
open Format

(** Formatting configuration *)
type config = {
  indent_size: int;      (** Spaces per indentation level *)
  max_line_length: int;  (** Maximum line length *)
  space_before_colon: bool;
  space_after_colon: bool;
  space_around_operators: bool;
}

let default_config = {
  indent_size = 2;
  max_line_length = 100;
  space_before_colon = false;
  space_after_colon = true;
  space_around_operators = true;
}

(** Format list with separator *)
let rec format_list f sep fmt = function
  | [] -> ()
  | [x] -> f fmt x
  | x :: xs ->
    f fmt x;
    fprintf fmt "%s" sep;
    format_list f sep fmt xs

(** Indentation helpers *)
let indent fmt level =
  for _ = 1 to level do
    fprintf fmt "%s" (String.make default_config.indent_size ' ')
  done

(** Format identifier *)
let format_ident fmt (id : ident) =
  fprintf fmt "%s" id.name

(** Format visibility *)
let rec format_visibility fmt = function
  | Public -> fprintf fmt "pub "
  | Private -> ()
  | PubCrate -> fprintf fmt "pub(crate) "
  | PubSuper -> fprintf fmt "pub(super) "
  | PubIn ids ->
    fprintf fmt "pub(";
    format_list format_ident "." fmt ids;
    fprintf fmt ") "

(** Format quantity *)
and format_quantity fmt = function
  | QZero -> fprintf fmt "@erased "
  | QOne -> fprintf fmt "@linear "
  | QOmega -> ()

(** Format ownership *)
and format_ownership fmt = function
  | Own -> fprintf fmt "own "
  | Ref -> fprintf fmt "ref "
  | Mut -> fprintf fmt "mut "

(** Format literal *)
and format_lit fmt = function
  | LitInt (i, _) -> fprintf fmt "%d" i
  | LitFloat (f, _) -> fprintf fmt "%f" f
  | LitBool (b, _) -> fprintf fmt "%b" b
  | LitString (s, _) -> fprintf fmt "\"%s\"" (String.escaped s)
  | LitChar (c, _) -> fprintf fmt "'%c'" c
  | LitUnit _ -> fprintf fmt "()"

(** Format binary operator *)
and format_binop fmt = function
  | OpAdd -> fprintf fmt "+"
  | OpSub -> fprintf fmt "-"
  | OpMul -> fprintf fmt "*"
  | OpDiv -> fprintf fmt "/"
  | OpMod -> fprintf fmt "%%"
  | OpEq -> fprintf fmt "=="
  | OpNe -> fprintf fmt "!="
  | OpLt -> fprintf fmt "<"
  | OpGt -> fprintf fmt ">"
  | OpLe -> fprintf fmt "<="
  | OpGe -> fprintf fmt ">="
  | OpAnd -> fprintf fmt "&&"
  | OpOr -> fprintf fmt "||"
  | OpBitAnd -> fprintf fmt "&"
  | OpBitOr -> fprintf fmt "|"
  | OpBitXor -> fprintf fmt "^"
  | OpShl -> fprintf fmt "<<"
  | OpShr -> fprintf fmt ">>"

(** Format unary operator *)
and format_unop fmt = function
  | OpNeg -> fprintf fmt "-"
  | OpNot -> fprintf fmt "!"
  | OpBitNot -> fprintf fmt "~"
  | OpRef -> fprintf fmt "&"
  | OpDeref -> fprintf fmt "*"

(** Format assign operator *)
and format_assignop fmt = function
  | AssignEq -> fprintf fmt "="
  | AssignAdd -> fprintf fmt "+="
  | AssignSub -> fprintf fmt "-="
  | AssignMul -> fprintf fmt "*="
  | AssignDiv -> fprintf fmt "/="

(** Format expression *)
and format_expr level fmt (e : expr) =
  match e with
  | ExprLit lit -> format_lit fmt lit
  | ExprVar id -> format_ident fmt id

  | ExprBinary (e1, op, e2) ->
    fprintf fmt "%a %a %a"
      (format_expr level) e1
      format_binop op
      (format_expr level) e2

  | ExprUnary (op, e) ->
    fprintf fmt "%a%a" format_unop op (format_expr level) e

  | ExprApp (func, args) ->
    fprintf fmt "%a(" (format_expr level) func;
    format_list (format_expr level) ", " fmt args;
    fprintf fmt ")"

  | ExprIf { ei_cond; ei_then; ei_else } ->
    fprintf fmt "if %a {@ " (format_expr level) ei_cond;
    format_expr (level + 1) fmt ei_then;
    (match ei_else with
    | None -> ()
    | Some else_e ->
      fprintf fmt "@ } else {@ ";
      format_expr (level + 1) fmt else_e);
    fprintf fmt "@ }"

  | ExprMatch { em_scrutinee; em_arms } ->
    fprintf fmt "match %a {@ " (format_expr level) em_scrutinee;
    List.iter (fun arm ->
      fprintf fmt "  %a" (format_pattern level) arm.ma_pat;
      (match arm.ma_guard with
      | Some g -> fprintf fmt " if %a" (format_expr level) g
      | None -> ());
      fprintf fmt " => %a,@ " (format_expr (level + 1)) arm.ma_body
    ) em_arms;
    fprintf fmt "}"

  | ExprLet { el_mut; el_pat; el_ty; el_value; el_body } ->
    fprintf fmt "let %s%a"
      (if el_mut then "mut " else "")
      (format_pattern level) el_pat;
    (match el_ty with
    | Some ty -> fprintf fmt ": %a" (format_type_expr level) ty
    | None -> ());
    fprintf fmt " = %a" (format_expr level) el_value;
    (match el_body with
    | Some body -> fprintf fmt ";@ %a" (format_expr level) body
    | None -> ())

  | ExprBlock blk -> format_block fmt level blk

  | ExprTuple exprs ->
    fprintf fmt "(";
    format_list (format_expr level) ", " fmt exprs;
    fprintf fmt ")"

  | ExprRecord { er_fields; er_spread } ->
    fprintf fmt "{@ ";
    List.iter (fun (id, e_opt) ->
      fprintf fmt "  %a" format_ident id;
      (match e_opt with
      | Some e -> fprintf fmt ": %a" (format_expr (level + 1)) e
      | None -> ());
      fprintf fmt ",@ "
    ) er_fields;
    (match er_spread with
    | Some base -> fprintf fmt "  ..%a@ " (format_expr level) base
    | None -> ());
    fprintf fmt "}"

  | ExprField (e, field) ->
    fprintf fmt "%a.%a" (format_expr level) e format_ident field

  | ExprTupleIndex (e, idx) ->
    fprintf fmt "%a.%d" (format_expr level) e idx

  | ExprArray exprs ->
    fprintf fmt "[";
    format_list (format_expr level) ", " fmt exprs;
    fprintf fmt "]"

  | ExprIndex (arr, idx) ->
    fprintf fmt "%a[%a]" (format_expr level) arr (format_expr level) idx

  | ExprLambda { elam_params; elam_ret_ty; elam_body } ->
    fprintf fmt "|";
    format_list (format_param level) ", " fmt elam_params;
    fprintf fmt "|";
    (match elam_ret_ty with
    | Some ty -> fprintf fmt " -> %a" (format_type_expr level) ty
    | None -> ());
    fprintf fmt " ";
    format_expr (level + 1) fmt elam_body

  | ExprHandle { eh_body; eh_handlers } ->
    fprintf fmt "handle {@ ";
    format_expr (level + 1) fmt eh_body;
    fprintf fmt "@ } with {@ ";
    List.iter (fun h -> format_handler fmt (level + 1) h) eh_handlers;
    fprintf fmt "}"

  | ExprResume e_opt ->
    fprintf fmt "resume";
    (match e_opt with
    | Some e -> fprintf fmt " %a" (format_expr level) e
    | None -> ())

  | ExprReturn e_opt ->
    fprintf fmt "return";
    (match e_opt with
    | Some e -> fprintf fmt " %a" (format_expr level) e
    | None -> ())

  | ExprTry { et_body; et_catch; et_finally } ->
    fprintf fmt "try ";
    format_block fmt level et_body;
    (match et_catch with
    | Some arms ->
      fprintf fmt " catch {@ ";
      List.iter (fun arm ->
        fprintf fmt "  %a => %a,@ "
          (format_pattern level) arm.ma_pat
          (format_expr (level + 1)) arm.ma_body
      ) arms;
      fprintf fmt "}"
    | None -> ());
    (match et_finally with
    | Some blk ->
      fprintf fmt " finally ";
      format_block fmt level blk
    | None -> ())

  | ExprUnsafe ops ->
    fprintf fmt "unsafe {@ ";
    List.iter (fun op -> format_unsafe_op fmt (level + 1) op) ops;
    fprintf fmt "@ }"

  | ExprRowRestrict (e, field) ->
    fprintf fmt "%a \\ %a" (format_expr level) e format_ident field

  | ExprVariant (ty, variant) ->
    fprintf fmt "%a::%a" format_ident ty format_ident variant

  | ExprSpan (e, _) -> format_expr level fmt e

and format_handler fmt level (h : handler_arm) =
  match h with
  | HandlerReturn (pat, body) ->
    fprintf fmt "  return %a => %a,@ "
      (format_pattern level) pat
      (format_expr level) body
  | HandlerOp (op, pats, body) ->
    fprintf fmt "  %a(" format_ident op;
    format_list (format_pattern level) ", " fmt pats;
    fprintf fmt ") => %a,@ " (format_expr level) body

and format_unsafe_op fmt level (op : unsafe_op) =
  indent fmt level;
  match op with
  | UnsafeRead e ->
    fprintf fmt "read(%a);@ " (format_expr level) e
  | UnsafeWrite (ptr, val_) ->
    fprintf fmt "write(%a, %a);@ "
      (format_expr level) ptr
      (format_expr level) val_
  | UnsafeOffset (ptr, off) ->
    fprintf fmt "offset(%a, %a);@ "
      (format_expr level) ptr
      (format_expr level) off
  | UnsafeTransmute (ty1, ty2, e) ->
    fprintf fmt "transmute<%a, %a>(%a);@ "
      (format_type_expr level) ty1
      (format_type_expr level) ty2
      (format_expr level) e
  | UnsafeForget e ->
    fprintf fmt "forget(%a);@ " (format_expr level) e
  | UnsafeAssume pred ->
    fprintf fmt "assume(%a);@ " format_predicate pred

and format_block fmt level (blk : block) =
  fprintf fmt "{@ ";
  List.iter (format_stmt (level + 1) fmt) blk.blk_stmts;
  (match blk.blk_expr with
  | Some e -> format_expr (level + 1) fmt e; fprintf fmt "@ "
  | None -> ());
  fprintf fmt "}"

(** Format statement *)
and format_stmt level fmt (stmt : stmt) =
  indent fmt level;
  match stmt with
  | StmtExpr e ->
    format_expr level fmt e;
    fprintf fmt ";@ "

  | StmtLet { sl_mut; sl_pat; sl_ty; sl_value } ->
    fprintf fmt "let %s%a"
      (if sl_mut then "mut " else "")
      (format_pattern level) sl_pat;
    (match sl_ty with
    | Some ty -> fprintf fmt ": %a" (format_type_expr level) ty
    | None -> ());
    fprintf fmt " = %a;@ " (format_expr level) sl_value

  | StmtAssign (lhs, op, rhs) ->
    fprintf fmt "%a %a %a;@ "
      (format_expr level) lhs
      format_assignop op
      (format_expr level) rhs

  | StmtWhile (cond, body) ->
    fprintf fmt "while %a " (format_expr level) cond;
    format_block fmt level body;
    fprintf fmt "@ "

  | StmtFor (pat, iter, body) ->
    fprintf fmt "for %a in %a "
      (format_pattern level) pat
      (format_expr level) iter;
    format_block fmt level body;
    fprintf fmt "@ "

(** Format pattern *)
and format_pattern level fmt (pat : pattern) =
  match pat with
  | PatWildcard _ -> fprintf fmt "_"
  | PatVar id -> format_ident fmt id
  | PatLit lit -> format_lit fmt lit
  | PatTuple pats ->
    fprintf fmt "(";
    format_list (format_pattern level) ", " fmt pats;
    fprintf fmt ")"
  | PatRecord (fields, rest) ->
    fprintf fmt "{@ ";
    List.iter (fun (id, pat_opt) ->
      fprintf fmt "  %a" format_ident id;
      (match pat_opt with
      | Some p -> fprintf fmt ": %a" (format_pattern (level + 1)) p
      | None -> ());
      fprintf fmt ",@ "
    ) fields;
    if rest then fprintf fmt "  ..@ ";
    fprintf fmt "}"
  | PatCon (con, pats) ->
    format_ident fmt con;
    if pats <> [] then (
      fprintf fmt "(";
      format_list (format_pattern level) ", " fmt pats;
      fprintf fmt ")"
    )
  | PatOr (p1, p2) ->
    fprintf fmt "%a | %a" (format_pattern level) p1 (format_pattern level) p2
  | PatAs (id, pat) ->
    fprintf fmt "%a @ %a" format_ident id (format_pattern level) pat

(** Format parameter *)
and format_param level fmt (p : param) =
  (match p.p_quantity with
  | Some q -> format_quantity fmt q
  | None -> ());
  (match p.p_ownership with
  | Some o -> format_ownership fmt o
  | None -> ());
  fprintf fmt "%a: %a"
    format_ident p.p_name
    (format_type_expr level) p.p_ty

(** Format type expression *)
and format_type_expr level fmt (ty : type_expr) =
  match ty with
  | TyCon id -> format_ident fmt id
  | TyVar id -> format_ident fmt id
  | TyApp (id, args) ->
    format_ident fmt id;
    fprintf fmt "[";
    format_list format_type_arg ", " fmt args;
    fprintf fmt "]"
  | TyArrow (param, ret, eff_opt) ->
    fprintf fmt "%a -> %a"
      (format_type_expr level) param
      (format_type_expr level) ret;
    (match eff_opt with
    | Some eff -> fprintf fmt " / %a" (format_effect_expr level) eff
    | None -> ())
  | TyDepArrow { da_quantity; da_param; da_param_ty; da_ret_ty; da_eff } ->
    fprintf fmt "(";
    (match da_quantity with
    | Some q -> format_quantity fmt q
    | None -> ());
    fprintf fmt "%a: %a) -> %a"
      format_ident da_param
      (format_type_expr level) da_param_ty
      (format_type_expr level) da_ret_ty;
    (match da_eff with
    | Some eff -> fprintf fmt " / %a" (format_effect_expr level) eff
    | None -> ())
  | TyTuple tys ->
    fprintf fmt "(";
    format_list (format_type_expr level) ", " fmt tys;
    fprintf fmt ")"
  | TyRecord (fields, row_var) ->
    fprintf fmt "{@ ";
    List.iter (fun field ->
      fprintf fmt "  %a: %a,@ "
        format_ident field.rf_name
        (format_type_expr (level + 1)) field.rf_ty
    ) fields;
    (match row_var with
    | Some id -> fprintf fmt "  ..%a@ " format_ident id
    | None -> ());
    fprintf fmt "}"
  | TyOwn ty -> fprintf fmt "own %a" (format_type_expr level) ty
  | TyRef ty -> fprintf fmt "ref %a" (format_type_expr level) ty
  | TyMut ty -> fprintf fmt "mut %a" (format_type_expr level) ty
  | TyRefined (ty, pred) ->
    fprintf fmt "%a where %a"
      (format_type_expr level) ty
      format_predicate pred
  | TyHole -> fprintf fmt "_"

and format_type_arg fmt = function
  | TyArg ty -> format_type_expr 0 fmt ty
  | NatArg n -> format_nat_expr fmt n

and format_nat_expr fmt = function
  | NatLit (n, _) -> fprintf fmt "%d" n
  | NatVar id -> format_ident fmt id
  | NatAdd (n1, n2) ->
    fprintf fmt "%a + %a" format_nat_expr n1 format_nat_expr n2
  | NatSub (n1, n2) ->
    fprintf fmt "%a - %a" format_nat_expr n1 format_nat_expr n2
  | NatMul (n1, n2) ->
    fprintf fmt "%a * %a" format_nat_expr n1 format_nat_expr n2
  | NatLen id -> fprintf fmt "len(%a)" format_ident id
  | NatSizeof ty -> fprintf fmt "sizeof(%a)" (format_type_expr 0) ty

and format_predicate fmt = function
  | PredCmp (n1, op, n2) ->
    fprintf fmt "%a %a %a"
      format_nat_expr n1
      format_cmp_op op
      format_nat_expr n2
  | PredNot p -> fprintf fmt "!(%a)" format_predicate p
  | PredAnd (p1, p2) ->
    fprintf fmt "%a && %a" format_predicate p1 format_predicate p2
  | PredOr (p1, p2) ->
    fprintf fmt "%a || %a" format_predicate p1 format_predicate p2

and format_cmp_op fmt = function
  | Lt -> fprintf fmt "<"
  | Le -> fprintf fmt "<="
  | Gt -> fprintf fmt ">"
  | Ge -> fprintf fmt ">="
  | Eq -> fprintf fmt "=="
  | Ne -> fprintf fmt "!="

(** Format effect expression *)
and format_effect_expr level fmt (eff : effect_expr) =
  match eff with
  | EffCon (id, args) ->
    format_ident fmt id;
    if args <> [] then (
      fprintf fmt "[";
      format_list format_type_arg ", " fmt args;
      fprintf fmt "]"
    )
  | EffVar id -> format_ident fmt id
  | EffUnion (e1, e2) ->
    fprintf fmt "%a | %a"
      (format_effect_expr level) e1
      (format_effect_expr level) e2

(** Format kind *)
let rec format_kind fmt = function
  | KType -> fprintf fmt "Type"
  | KNat -> fprintf fmt "Nat"
  | KRow -> fprintf fmt "Row"
  | KEffect -> fprintf fmt "Effect"
  | KArrow (k1, k2) -> fprintf fmt "%a -> %a" format_kind k1 format_kind k2

(** Format function declaration *)
let format_fun_decl fmt level (fd : fn_decl) =
  indent fmt level;
  format_visibility fmt fd.fd_vis;
  if fd.fd_total then fprintf fmt "total ";
  fprintf fmt "fn %a" format_ident fd.fd_name;

  (* Type parameters *)
  if fd.fd_type_params <> [] then (
    fprintf fmt "<";
    format_list (fun fmt tp ->
      (match tp.tp_quantity with
      | Some q -> format_quantity fmt q
      | None -> ());
      format_ident fmt tp.tp_name;
      (match tp.tp_kind with
      | Some k -> fprintf fmt ": %a" format_kind k
      | None -> ())
    ) ", " fmt fd.fd_type_params;
    fprintf fmt ">"
  );

  (* Parameters *)
  fprintf fmt "(";
  format_list (format_param level) ", " fmt fd.fd_params;
  fprintf fmt ")";

  (* Return type and effect *)
  (match fd.fd_ret_ty, fd.fd_eff with
  | Some ty, Some eff ->
    fprintf fmt " -> %a / %a" (format_type_expr level) ty (format_effect_expr level) eff
  | Some ty, None ->
    fprintf fmt " -> %a" (format_type_expr level) ty
  | None, Some eff ->
    fprintf fmt " / %a" (format_effect_expr level) eff
  | None, None -> ());

  (* Body *)
  fprintf fmt " ";
  (match fd.fd_body with
  | FnBlock blk ->
    format_block fmt level blk;
    fprintf fmt "@ "
  | FnExpr e ->
    fprintf fmt "= %a@ " (format_expr (level + 1)) e)

(** Format constraint *)
let format_constraint fmt = function
  | ConstraintPred pred ->
    format_predicate fmt pred
  | ConstraintTrait (ty, bounds) ->
    format_ident fmt ty;
    fprintf fmt ": ";
    format_list (fun fmt tb -> format_ident fmt tb.tb_name) " + " fmt bounds

(** Format top-level declaration *)
let format_top_level fmt level (top : top_level) =
  match top with
  | TopFn fd -> format_fun_decl fmt level fd

  | TopType { td_vis; td_name; td_type_params; td_body } ->
    indent fmt level;
    format_visibility fmt td_vis;
    fprintf fmt "type %a" format_ident td_name;
    if td_type_params <> [] then (
      fprintf fmt "<";
      format_list (fun fmt tp -> format_ident fmt tp.tp_name) ", " fmt td_type_params;
      fprintf fmt ">"
    );
    (match td_body with
    | TyAlias ty ->
      fprintf fmt " = %a;@ @ " (format_type_expr level) ty
    | TyStruct fields ->
      fprintf fmt " {@ ";
      List.iter (fun field ->
        fprintf fmt "  ";
        format_visibility fmt field.sf_vis;
        fprintf fmt "%a: %a,@ "
          format_ident field.sf_name
          (format_type_expr (level + 1)) field.sf_ty
      ) fields;
      fprintf fmt "}@ @ "
    | TyEnum variants ->
      fprintf fmt " {@ ";
      List.iter (fun variant ->
        fprintf fmt "  %a" format_ident variant.vd_name;
        if variant.vd_fields <> [] then (
          fprintf fmt "(";
          format_list (format_type_expr (level + 1)) ", " fmt variant.vd_fields;
          fprintf fmt ")"
        );
        fprintf fmt ",@ "
      ) variants;
      fprintf fmt "}@ @ ")

  | TopEffect { ed_vis; ed_name; ed_type_params; ed_ops } ->
    indent fmt level;
    format_visibility fmt ed_vis;
    fprintf fmt "effect %a" format_ident ed_name;
    if ed_type_params <> [] then (
      fprintf fmt "<";
      format_list (fun fmt tp -> format_ident fmt tp.tp_name) ", " fmt ed_type_params;
      fprintf fmt ">"
    );
    if ed_ops <> [] then (
      fprintf fmt " {@ ";
      List.iter (fun op ->
        fprintf fmt "  fn %a(" format_ident op.eod_name;
        format_list (format_param (level + 1)) ", " fmt op.eod_params;
        fprintf fmt ")";
        (match op.eod_ret_ty with
        | Some ty -> fprintf fmt ": %a" (format_type_expr (level + 1)) ty
        | None -> ());
        fprintf fmt ";@ "
      ) ed_ops;
      fprintf fmt "}"
    );
    fprintf fmt ";@ @ "

  | TopTrait { trd_vis; trd_name; trd_type_params; trd_super; trd_items } ->
    indent fmt level;
    format_visibility fmt trd_vis;
    fprintf fmt "trait %a" format_ident trd_name;
    if trd_type_params <> [] then (
      fprintf fmt "<";
      format_list (fun fmt tp -> format_ident fmt tp.tp_name) ", " fmt trd_type_params;
      fprintf fmt ">"
    );
    if trd_super <> [] then (
      fprintf fmt ": ";
      format_list (fun fmt tb -> format_ident fmt tb.tb_name) " + " fmt trd_super
    );
    fprintf fmt " {@ ";
    List.iter (fun item ->
      match item with
      | TraitFn sig_ ->
        fprintf fmt "  fn %a(" format_ident sig_.fs_name;
        format_list (format_param (level + 1)) ", " fmt sig_.fs_params;
        fprintf fmt ")";
        (match sig_.fs_ret_ty with
        | Some ty -> fprintf fmt " -> %a" (format_type_expr (level + 1)) ty
        | None -> ());
        fprintf fmt ";@ "
      | TraitFnDefault fd ->
        format_fun_decl fmt (level + 1) fd
      | TraitType { tt_name; tt_kind; tt_default } ->
        fprintf fmt "  type %a" format_ident tt_name;
        (match tt_kind with
        | Some k -> fprintf fmt ": %a" format_kind k
        | None -> ());
        (match tt_default with
        | Some ty -> fprintf fmt " = %a" (format_type_expr (level + 1)) ty
        | None -> ());
        fprintf fmt ";@ "
    ) trd_items;
    fprintf fmt "}@ @ "

  | TopImpl { ib_type_params; ib_trait_ref; ib_self_ty; ib_where; ib_items } ->
    indent fmt level;
    fprintf fmt "impl";
    if ib_type_params <> [] then (
      fprintf fmt "<";
      format_list (fun fmt tp -> format_ident fmt tp.tp_name) ", " fmt ib_type_params;
      fprintf fmt ">"
    );
    fprintf fmt " ";
    (match ib_trait_ref with
    | Some tr ->
      format_ident fmt tr.tr_name;
      if tr.tr_args <> [] then (
        fprintf fmt "[";
        format_list format_type_arg ", " fmt tr.tr_args;
        fprintf fmt "]"
      );
      fprintf fmt " for "
    | None -> ());
    fprintf fmt "%a" (format_type_expr level) ib_self_ty;
    if ib_where <> [] then (
      fprintf fmt "@ where@ ";
      format_list format_constraint "," fmt ib_where
    );
    fprintf fmt " {@ ";
    List.iter (fun item ->
      match item with
      | ImplFn fd -> format_fun_decl fmt (level + 1) fd
      | ImplType (name, ty) ->
        fprintf fmt "  type %a = %a;@ "
          format_ident name
          (format_type_expr (level + 1)) ty
    ) ib_items;
    fprintf fmt "}@ @ "

  | TopConst { tc_vis; tc_name; tc_ty; tc_value } ->
    indent fmt level;
    format_visibility fmt tc_vis;
    fprintf fmt "const %a: %a = %a;@ @ "
      format_ident tc_name
      (format_type_expr level) tc_ty
      (format_expr level) tc_value

(** Format entire program *)
let format_program fmt (prog : program) =
  (match prog.prog_module with
  | Some path ->
    fprintf fmt "module ";
    format_list format_ident "." fmt path;
    fprintf fmt ";@ @ "
  | None -> ());

  List.iter (fun import ->
    fprintf fmt "use ";
    (match import with
    | ImportSimple (path, alias) ->
      format_list format_ident "." fmt path;
      (match alias with
      | Some id -> fprintf fmt " as %a" format_ident id
      | None -> ())
    | ImportList (path, items) ->
      format_list format_ident "." fmt path;
      fprintf fmt "::{";
      format_list (fun fmt item ->
        format_ident fmt item.ii_name;
        (match item.ii_alias with
        | Some alias -> fprintf fmt " as %a" format_ident alias
        | None -> ())
      ) ", " fmt items;
      fprintf fmt "}"
    | ImportGlob path ->
      format_list format_ident "." fmt path;
      fprintf fmt "::*");
    fprintf fmt ";@ "
  ) prog.prog_imports;

  if prog.prog_imports <> [] then fprintf fmt "@ ";

  List.iter (format_top_level fmt 0) prog.prog_decls;
  fprintf fmt "@."

(** Format program to string *)
let format_to_string (prog : program) : string =
  let buf = Buffer.create 1024 in
  let fmt = formatter_of_buffer buf in
  format_program fmt prog;
  Buffer.contents buf

(** Format file in-place *)
let format_file (path : string) : unit =
  let prog = Parse_driver.parse_file path in
  let formatted = format_to_string prog in
  let oc = open_out path in
  output_string oc formatted;
  close_out oc
