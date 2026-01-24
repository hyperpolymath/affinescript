(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** AffineScript linter - static analysis for code quality *)

open Ast
open Symbol

(** Lint severity levels *)
type severity =
  | Error
  | Warning
  | Hint
  | Info

(** Lint diagnostic *)
type diagnostic = {
  severity: severity;
  code: string;
  message: string;
  span: Span.t;
  help: string option;
}

(** Lint context *)
type context = {
  mutable diagnostics: diagnostic list;
  symbols: Symbol.t;
  mutable used_vars: string list;
  mutable defined_vars: string list;
}

(** Create a new lint context *)
let create_context (symbols : Symbol.t) : context = {
  diagnostics = [];
  symbols;
  used_vars = [];
  defined_vars = [];
}

(** Add a diagnostic *)
let add_diagnostic (ctx : context) (diag : diagnostic) : unit =
  ctx.diagnostics <- diag :: ctx.diagnostics

(** Report unused variable *)
let report_unused_var (ctx : context) (name : string) (span : Span.t) : unit =
  if not (List.mem name ctx.used_vars) && name <> "_" then
    add_diagnostic ctx {
      severity = Warning;
      code = "L001";
      message = Printf.sprintf "Unused variable '%s'" name;
      span;
      help = Some "Remove this variable or prefix with '_' to indicate intentionally unused";
    }

(** Report missing effect annotation *)
let report_missing_effect (ctx : context) (name : string) (span : Span.t) : unit =
  add_diagnostic ctx {
    severity = Error;
    code = "L002";
    message = Printf.sprintf "Function '%s' may perform effects but lacks effect annotation" name;
    span;
    help = Some "Add effect annotation: fn name() -> T / EffectName";
  }

(** Report dead code *)
let report_dead_code (ctx : context) (span : Span.t) : unit =
  add_diagnostic ctx {
    severity = Warning;
    code = "L003";
    message = "Unreachable code detected";
    span;
    help = Some "This code will never execute, consider removing it";
  }

(** Report naming convention violation *)
let report_naming_convention (ctx : context) (kind : string) (name : string) (span : Span.t) : unit =
  let expected = match kind with
    | "function" -> "snake_case"
    | "type" -> "PascalCase"
    | "constant" -> "SCREAMING_SNAKE_CASE"
    | _ -> "unknown"
  in
  add_diagnostic ctx {
    severity = Hint;
    code = "L004";
    message = Printf.sprintf "%s '%s' does not follow %s naming convention"
      (String.capitalize_ascii kind) name expected;
    span;
    help = Some (Printf.sprintf "Consider renaming to match %s convention" expected);
  }

(** Check if identifier follows snake_case *)
let is_snake_case (s : string) : bool =
  String.for_all (fun c ->
    (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_'
  ) s

(** Check if identifier follows PascalCase *)
let is_pascal_case (s : string) : bool =
  if String.length s = 0 then false
  else
    let first = s.[0] in
    (first >= 'A' && first <= 'Z') &&
    String.for_all (fun c ->
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    ) s

(** Check if identifier follows SCREAMING_SNAKE_CASE *)
let is_screaming_snake_case (s : string) : bool =
  String.for_all (fun c ->
    (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
  ) s

(** Extract variable names from pattern *)
let rec pattern_vars (pat : pattern) : (string * Span.t) list =
  match pat with
  | PatWildcard _ -> []
  | PatVar id -> [(id.name, id.span)]
  | PatLit _ -> []
  | PatTuple pats -> List.concat_map pattern_vars pats
  | PatRecord (fields, _) ->
    List.concat_map (fun (id, pat_opt) ->
      match pat_opt with
      | Some p -> pattern_vars p
      | None -> [(id.name, id.span)]
    ) fields
  | PatCon (_, pats) -> List.concat_map pattern_vars pats
  | PatOr (p1, p2) -> pattern_vars p1 @ pattern_vars p2
  | PatAs (id, pat) -> (id.name, id.span) :: pattern_vars pat

(** Check if expression contains effect-performing calls *)
let rec has_effects (ctx : context) (e : expr) : bool =
  match e with
  | ExprApp (ExprVar id, _) ->
    (* Check if called function has effects *)
    (match Symbol.lookup ctx.symbols id.name with
    | Some entry ->
      (match entry.sym_kind with
      | SKFunction -> (* TODO: check function signature for effects *) false
      | _ -> false)
    | None -> false)
  | ExprHandle _ -> true
  | ExprResume _ -> true
  | ExprBinary (e1, _, e2) -> has_effects ctx e1 || has_effects ctx e2
  | ExprUnary (_, e) -> has_effects ctx e
  | ExprApp (func, args) ->
    has_effects ctx func || List.exists (has_effects ctx) args
  | ExprIf { ei_cond; ei_then; ei_else } ->
    has_effects ctx ei_cond || has_effects ctx ei_then ||
    (match ei_else with Some e -> has_effects ctx e | None -> false)
  | ExprLet { el_value; el_body; _ } ->
    has_effects ctx el_value ||
    (match el_body with Some e -> has_effects ctx e | None -> false)
  | ExprBlock blk ->
    List.exists (stmt_has_effects ctx) blk.blk_stmts ||
    (match blk.blk_expr with Some e -> has_effects ctx e | None -> false)
  | ExprTuple exprs -> List.exists (has_effects ctx) exprs
  | ExprRecord { er_fields; er_spread } ->
    List.exists (fun (_, e_opt) ->
      match e_opt with Some e -> has_effects ctx e | None -> false
    ) er_fields ||
    (match er_spread with Some e -> has_effects ctx e | None -> false)
  | ExprField (e, _) -> has_effects ctx e
  | ExprTupleIndex (e, _) -> has_effects ctx e
  | ExprArray exprs -> List.exists (has_effects ctx) exprs
  | ExprIndex (arr, idx) -> has_effects ctx arr || has_effects ctx idx
  | ExprMatch { em_scrutinee; em_arms } ->
    has_effects ctx em_scrutinee ||
    List.exists (fun arm -> has_effects ctx arm.ma_body) em_arms
  | ExprLambda { elam_body; _ } -> has_effects ctx elam_body
  | ExprTry { et_body; et_catch; et_finally } ->
    List.exists (stmt_has_effects ctx) et_body.blk_stmts ||
    (match et_catch with
    | Some arms -> List.exists (fun arm -> has_effects ctx arm.ma_body) arms
    | None -> false) ||
    (match et_finally with
    | Some blk -> List.exists (stmt_has_effects ctx) blk.blk_stmts
    | None -> false)
  | ExprReturn e_opt ->
    (match e_opt with Some e -> has_effects ctx e | None -> false)
  | ExprUnsafe _ -> true
  | ExprRowRestrict (e, _) -> has_effects ctx e
  | ExprSpan (e, _) -> has_effects ctx e
  | ExprLit _ | ExprVar _ | ExprVariant _ -> false

and stmt_has_effects (ctx : context) (stmt : stmt) : bool =
  match stmt with
  | StmtExpr e -> has_effects ctx e
  | StmtLet { sl_value; _ } -> has_effects ctx sl_value
  | StmtAssign (lhs, _, rhs) -> has_effects ctx lhs || has_effects ctx rhs
  | StmtWhile (cond, body) ->
    has_effects ctx cond ||
    List.exists (stmt_has_effects ctx) body.blk_stmts
  | StmtFor (_, iter, body) ->
    has_effects ctx iter ||
    List.exists (stmt_has_effects ctx) body.blk_stmts

(** Detect unreachable code after return *)
let rec check_dead_code (ctx : context) (stmts : stmt list) : unit =
  let rec aux prev_terminal = function
    | [] -> ()
    | stmt :: rest ->
      if prev_terminal then
        report_dead_code ctx (span_of_stmt stmt);

      let is_terminal = match stmt with
        | StmtExpr (ExprReturn _) -> true
        | _ -> false
      in
      aux is_terminal rest
  in
  aux false stmts

and span_of_stmt (stmt : stmt) : Span.t =
  match stmt with
  | StmtExpr e -> span_of_expr e
  | StmtLet { sl_pat; _ } -> span_of_pattern sl_pat
  | StmtAssign (lhs, _, _) -> span_of_expr lhs
  | StmtWhile (cond, _) -> span_of_expr cond
  | StmtFor (pat, _, _) -> span_of_pattern pat

and span_of_expr (e : expr) : Span.t =
  match e with
  | ExprVar id -> id.span
  | ExprSpan (_, span) -> span
  | ExprLit (LitInt (_, s)) -> s
  | ExprLit (LitFloat (_, s)) -> s
  | ExprLit (LitBool (_, s)) -> s
  | ExprLit (LitChar (_, s)) -> s
  | ExprLit (LitString (_, s)) -> s
  | ExprLit (LitUnit s) -> s
  | _ -> Span.dummy (* TODO: add spans to all expressions *)

and span_of_pattern (pat : pattern) : Span.t =
  match pat with
  | PatVar id -> id.span
  | PatWildcard s -> s
  | PatLit lit ->
    (match lit with
    | LitInt (_, s) | LitFloat (_, s) | LitBool (_, s)
    | LitChar (_, s) | LitString (_, s) | LitUnit s -> s)
  | _ -> Span.dummy (* TODO: add spans to all patterns *)

(** Lint expression *)
let rec lint_expr (ctx : context) (e : expr) : unit =
  match e with
  | ExprVar id ->
    if not (List.mem id.name ctx.used_vars) then
      ctx.used_vars <- id.name :: ctx.used_vars

  | ExprLet { el_pat; el_value; el_body; _ } ->
    lint_expr ctx el_value;
    let vars = pattern_vars el_pat in
    List.iter (fun (name, span) ->
      ctx.defined_vars <- name :: ctx.defined_vars;
      (match el_body with
      | Some body -> lint_expr ctx body
      | None -> ());
      report_unused_var ctx name span
    ) vars

  | ExprBinary (e1, _, e2) ->
    lint_expr ctx e1;
    lint_expr ctx e2

  | ExprUnary (_, e) -> lint_expr ctx e

  | ExprApp (func, args) ->
    lint_expr ctx func;
    List.iter (lint_expr ctx) args

  | ExprIf { ei_cond; ei_then; ei_else } ->
    lint_expr ctx ei_cond;
    lint_expr ctx ei_then;
    Option.iter (lint_expr ctx) ei_else

  | ExprMatch { em_scrutinee; em_arms } ->
    lint_expr ctx em_scrutinee;
    List.iter (fun arm ->
      Option.iter (lint_expr ctx) arm.ma_guard;
      lint_expr ctx arm.ma_body
    ) em_arms

  | ExprBlock blk ->
    check_dead_code ctx blk.blk_stmts;
    List.iter (lint_stmt ctx) blk.blk_stmts;
    Option.iter (lint_expr ctx) blk.blk_expr

  | ExprTuple exprs -> List.iter (lint_expr ctx) exprs

  | ExprRecord { er_fields; er_spread } ->
    List.iter (fun (_, e_opt) ->
      Option.iter (lint_expr ctx) e_opt
    ) er_fields;
    Option.iter (lint_expr ctx) er_spread

  | ExprField (e, _) -> lint_expr ctx e
  | ExprTupleIndex (e, _) -> lint_expr ctx e
  | ExprArray exprs -> List.iter (lint_expr ctx) exprs

  | ExprIndex (arr, idx) ->
    lint_expr ctx arr;
    lint_expr ctx idx

  | ExprLambda { elam_params; elam_body; _ } ->
    let param_vars = List.concat_map (fun p -> [(p.p_name.name, p.p_name.span)]) elam_params in
    List.iter (fun (name, _) -> ctx.defined_vars <- name :: ctx.defined_vars) param_vars;
    lint_expr ctx elam_body;
    List.iter (fun (name, span) -> report_unused_var ctx name span) param_vars

  | ExprHandle { eh_body; eh_handlers } ->
    lint_expr ctx eh_body;
    List.iter (fun h ->
      match h with
      | HandlerReturn (_, body) -> lint_expr ctx body
      | HandlerOp (_, _, body) -> lint_expr ctx body
    ) eh_handlers

  | ExprResume e_opt -> Option.iter (lint_expr ctx) e_opt
  | ExprReturn e_opt -> Option.iter (lint_expr ctx) e_opt

  | ExprTry { et_body; et_catch; et_finally } ->
    List.iter (lint_stmt ctx) et_body.blk_stmts;
    Option.iter (List.iter (fun arm -> lint_expr ctx arm.ma_body)) et_catch;
    Option.iter (fun blk -> List.iter (lint_stmt ctx) blk.blk_stmts) et_finally

  | ExprUnsafe _ -> () (* TODO: lint unsafe operations *)
  | ExprRowRestrict (e, _) -> lint_expr ctx e
  | ExprSpan (e, _) -> lint_expr ctx e
  | ExprLit _ | ExprVariant _ -> ()

(** Lint statement *)
and lint_stmt (ctx : context) (stmt : stmt) : unit =
  match stmt with
  | StmtExpr e -> lint_expr ctx e

  | StmtLet { sl_pat; sl_value; _ } ->
    lint_expr ctx sl_value;
    let vars = pattern_vars sl_pat in
    List.iter (fun (name, _) -> ctx.defined_vars <- name :: ctx.defined_vars) vars

  | StmtAssign (lhs, _, rhs) ->
    lint_expr ctx lhs;
    lint_expr ctx rhs

  | StmtWhile (cond, body) ->
    lint_expr ctx cond;
    List.iter (lint_stmt ctx) body.blk_stmts

  | StmtFor (pat, iter, body) ->
    lint_expr ctx iter;
    let vars = pattern_vars pat in
    List.iter (fun (name, _) -> ctx.defined_vars <- name :: ctx.defined_vars) vars;
    List.iter (lint_stmt ctx) body.blk_stmts

(** Lint function declaration *)
let lint_fun_decl (ctx : context) (fd : fn_decl) : unit =
  (* Check naming convention *)
  if not (is_snake_case fd.fd_name.name) && not (String.starts_with ~prefix:"_" fd.fd_name.name) then
    report_naming_convention ctx "function" fd.fd_name.name fd.fd_name.span;

  (* Check for missing effect annotations *)
  let has_body_effects = match fd.fd_body with
    | FnBlock blk ->
      List.exists (stmt_has_effects ctx) blk.blk_stmts ||
      (match blk.blk_expr with Some e -> has_effects ctx e | None -> false)
    | FnExpr e -> has_effects ctx e
  in
  if has_body_effects && fd.fd_eff = None then
    report_missing_effect ctx fd.fd_name.name fd.fd_name.span;

  (* Lint function body *)
  (match fd.fd_body with
  | FnBlock blk ->
    List.iter (lint_stmt ctx) blk.blk_stmts;
    Option.iter (lint_expr ctx) blk.blk_expr
  | FnExpr e -> lint_expr ctx e)

(** Lint top-level declaration *)
let lint_top_level (ctx : context) (top : top_level) : unit =
  match top with
  | TopFn fd -> lint_fun_decl ctx fd

  | TopType { td_name; _ } ->
    if not (is_pascal_case td_name.name) then
      report_naming_convention ctx "type" td_name.name td_name.span

  | TopTrait { trd_name; trd_items; _ } ->
    if not (is_pascal_case trd_name.name) then
      report_naming_convention ctx "type" trd_name.name trd_name.span;
    List.iter (fun item ->
      match item with
      | TraitFnDefault fd -> lint_fun_decl ctx fd
      | _ -> ()
    ) trd_items

  | TopImpl { ib_items; _ } ->
    List.iter (fun item ->
      match item with
      | ImplFn fd -> lint_fun_decl ctx fd
      | _ -> ()
    ) ib_items

  | TopConst { tc_name; tc_value; _ } ->
    if not (is_screaming_snake_case tc_name.name) then
      report_naming_convention ctx "constant" tc_name.name tc_name.span;
    lint_expr ctx tc_value

  | TopEffect _ -> ()

(** Lint program *)
let lint_program (symbols : Symbol.t) (prog : program) : diagnostic list =
  let ctx = create_context symbols in
  List.iter (lint_top_level ctx) prog.prog_decls;
  List.rev ctx.diagnostics

(** Format diagnostic *)
let format_diagnostic (diag : diagnostic) : string =
  let severity_str = match diag.severity with
    | Error -> Error_formatter.colorize Error_formatter.Red "error"
    | Warning -> Error_formatter.colorize Error_formatter.Yellow "warning"
    | Hint -> Error_formatter.colorize Error_formatter.Cyan "hint"
    | Info -> Error_formatter.colorize Error_formatter.Green "info"
  in
  let loc = Format.asprintf "%a" Span.pp_short diag.span in
  let help_str = match diag.help with
    | Some h -> "\n  " ^ Error_formatter.colorize Error_formatter.Green ("help: " ^ h)
    | None -> ""
  in
  Printf.sprintf "%s [%s] %s: %s%s"
    loc diag.code severity_str diag.message help_str

(** Print diagnostics *)
let print_diagnostics (diags : diagnostic list) : unit =
  List.iter (fun diag ->
    Format.printf "%s@." (format_diagnostic diag)
  ) diags
