(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* BetLang Julia Code Generator

   Translates AffineScript AST to Julia source code.

   Phase 1 (MVP): Basic types (Float64, Int64, Bool), functions, arithmetic
   Phase 2: DistnumberNormal, safety checks
   Phase 3: All 11 number systems
   Phase 4: Performance optimizations
*)

open Ast

(* ============================================================================
   Code Generation Context
   ============================================================================ *)

type codegen_ctx = {
  output : Buffer.t;           (* Accumulated Julia code *)
  indent : int;                (* Current indentation level *)
  symbols : Symbol.t;          (* Symbol table *)
  in_function : bool;          (* Track if inside function definition *)
}

let create_ctx symbols =
  {
    output = Buffer.create 1024;
    indent = 0;
    symbols;
    in_function = false;
  }

let emit ctx str =
  Buffer.add_string ctx.output str

let emit_line ctx str =
  let spaces = String.make (ctx.indent * 4) ' ' in
  Buffer.add_string ctx.output spaces;
  Buffer.add_string ctx.output str;
  Buffer.add_char ctx.output '\n'

let increase_indent ctx =
  { ctx with indent = ctx.indent + 1 }

let decrease_indent ctx =
  { ctx with indent = max 0 (ctx.indent - 1) }

(* ============================================================================
   Type Translation (Phase 1: Basic types only)
   ============================================================================ *)

let rec type_expr_to_julia_string (te : type_expr) : string =
  match te with
  | TyCon name when name.name = "Int" -> "Int64"
  | TyCon name when name.name = "Float" -> "Float64"
  | TyCon name when name.name = "Bool" -> "Bool"
  | TyCon name when name.name = "String" -> "String"
  | TyCon name when name.name = "Unit" -> "Nothing"
  | TyCon name -> name.name  (* Custom type names pass through *)
  | TyArrow (_, ret, _) ->
      (* Function types: for now, just use ret type annotation *)
      type_expr_to_julia_string ret
  | TyTuple tys ->
      (* Tuple types: Tuple{T1, T2, ...} *)
      let ty_strs = List.map type_expr_to_julia_string tys in
      "Tuple{" ^ String.concat ", " ty_strs ^ "}"
  | TyRecord _ -> "NamedTuple"  (* Record types map to NamedTuples *)
  | TyVar _ -> "Any"  (* Type variables: Any for now (Phase 1) *)
  | _ -> "Any"  (* Default fallback *)

(* ============================================================================
   Expression Code Generation
   ============================================================================ *)

let rec gen_expr ctx (expr : expr) : string =
  match expr with
  | ExprLit lit -> gen_literal lit
  | ExprVar name ->
      (* Variable reference *)
      name.name
  | ExprApp (func, args) ->
      (* Function application *)
      let func_str = gen_expr ctx func in
      let arg_strs = List.map (gen_expr ctx) args in
      func_str ^ "(" ^ String.concat ", " arg_strs ^ ")"
  | ExprBinary (e1, op, e2) ->
      (* Binary operators *)
      let op_str = match op with
        | OpAdd -> "+"
        | OpSub -> "-"
        | OpMul -> "*"
        | OpDiv -> "/"
        | OpMod -> "%"
        | OpEq -> "=="
        | OpNe -> "!="
        | OpLt -> "<"
        | OpLe -> "<="
        | OpGt -> ">"
        | OpGe -> ">="
        | OpAnd -> "&&"
        | OpOr -> "||"
        | OpBitAnd -> "&"
        | OpBitOr -> "|"
        | OpBitXor -> "⊻"
        | OpShl -> "<<"
        | OpShr -> ">>"
      in
      "(" ^ gen_expr ctx e1 ^ " " ^ op_str ^ " " ^ gen_expr ctx e2 ^ ")"
  | ExprUnary (op, e) ->
      (* Unary operators *)
      let op_str = match op with
        | OpNeg -> "-"
        | OpNot -> "!"
        | OpBitNot -> "~"
        | OpRef -> "Ref("
        | OpDeref -> "[]"
      in
      if op = OpRef then
        op_str ^ gen_expr ctx e ^ ")"
      else if op = OpDeref then
        gen_expr ctx e ^ op_str
      else
        "(" ^ op_str ^ gen_expr ctx e ^ ")"
  | ExprIf { ei_cond; ei_then; ei_else } ->
      (* If-then-else: Julia uses 'if ... end' syntax *)
      let cond_str = gen_expr ctx ei_cond in
      let then_str = gen_expr ctx ei_then in
      (match ei_else with
      | Some else_br ->
          let else_str = gen_expr ctx else_br in
          "(if " ^ cond_str ^ "; " ^ then_str ^ "; else " ^ else_str ^ "; end)"
      | None ->
          "(if " ^ cond_str ^ "; " ^ then_str ^ "; end)")
  | ExprLet { el_pat; el_value; el_body; el_mut = _; el_ty = _ } ->
      (* Let binding: 'local x = val; body' *)
      let pat_str = gen_pattern ctx el_pat in
      let val_str = gen_expr ctx el_value in
      (match el_body with
      | Some body ->
          let body_str = gen_expr ctx body in
          "(local " ^ pat_str ^ " = " ^ val_str ^ "; " ^ body_str ^ ")"
      | None ->
          "local " ^ pat_str ^ " = " ^ val_str)
  | ExprTuple exprs ->
      (* Tuple: (e1, e2, ...) *)
      let expr_strs = List.map (gen_expr ctx) exprs in
      "(" ^ String.concat ", " expr_strs ^ ")"
  | ExprRecord { er_fields; er_spread = _ } ->
      (* Record/NamedTuple: (field1=val1, field2=val2) *)
      let field_strs = List.map (fun (name, e_opt) ->
        let val_str = match e_opt with
          | Some e -> gen_expr ctx e
          | None -> name.name
        in
        name.name ^ "=" ^ val_str
      ) er_fields in
      "(; " ^ String.concat ", " field_strs ^ ")"
  | ExprField (record, field) ->
      (* Field access: record.field *)
      gen_expr ctx record ^ "." ^ field.name
  | ExprMatch { em_scrutinee; em_arms } ->
      (* Pattern matching: translate to if-elseif chain (Phase 1) *)
      gen_match ctx em_scrutinee em_arms
  | ExprBlock block ->
      (* Block: (stmt1; stmt2; ...; result) *)
      let stmt_strs = List.map (gen_stmt ctx) block.blk_stmts in
      let result_str = match block.blk_expr with
        | Some e -> gen_expr ctx e
        | None -> ""
      in
      "(begin\n" ^ String.concat "\n" stmt_strs ^ "\n" ^ result_str ^ "\nend)"
  | ExprReturn (Some e) ->
      "return " ^ gen_expr ctx e
  | ExprReturn None ->
      "return"
  | _ ->
      (* Unsupported expressions in Phase 1 *)
      "error(\"Unsupported expression\")"

and gen_literal (lit : literal) : string =
  match lit with
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) -> string_of_float f
  | LitBool (true, _) -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _) -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _) -> "'" ^ Char.escaped c ^ "'"
  | LitUnit _ -> "nothing"

and gen_pattern ctx (pat : pattern) : string =
  match pat with
  | PatWildcard _ -> "_"
  | PatVar name -> name.name
  | PatLit lit -> gen_literal lit
  | PatTuple pats ->
      let pat_strs = List.map (gen_pattern ctx) pats in
      "(" ^ String.concat ", " pat_strs ^ ")"
  | _ -> "_"  (* Unsupported patterns: wildcard *)

and gen_match ctx scrutinee cases =
  (* Phase 1: Simple pattern matching via if-elseif chain *)
  let scrutinee_str = gen_expr ctx scrutinee in
  let rec gen_cases remaining =
    match remaining with
    | [] -> "error(\"Non-exhaustive pattern match\")"
    | [arm] ->
        (* Last case: else branch *)
        let body_str = gen_expr ctx arm.ma_body in
        body_str
    | arm :: rest ->
        let cond_str = gen_pattern_cond ctx scrutinee_str arm.ma_pat in
        let body_str = gen_expr ctx arm.ma_body in
        let rest_str = gen_cases rest in
        "if " ^ cond_str ^ "; " ^ body_str ^ "; else " ^ rest_str ^ "; end"
  in
  "(" ^ gen_cases cases ^ ")"

and gen_pattern_cond _ctx scrutinee pat =
  match pat with
  | PatWildcard _ -> "true"
  | PatVar _ -> "true"  (* Variable pattern always matches *)
  | PatLit lit ->
      scrutinee ^ " == " ^ gen_literal lit
  | _ -> "true"

and gen_stmt ctx (stmt : stmt) : string =
  (* Statements (for blocks) *)
  match stmt with
  | StmtLet { sl_pat; sl_value; sl_mut = _; sl_ty = _ } ->
      let pat_str = gen_pattern ctx sl_pat in
      let val_str = gen_expr ctx sl_value in
      pat_str ^ " = " ^ val_str
  | StmtExpr e ->
      gen_expr ctx e
  | StmtAssign (lhs, op, rhs) ->
      let lhs_str = gen_expr ctx lhs in
      let rhs_str = gen_expr ctx rhs in
      let op_str = match op with
        | AssignEq -> "="
        | AssignAdd -> "+="
        | AssignSub -> "-="
        | AssignMul -> "*="
        | AssignDiv -> "/="
      in
      lhs_str ^ " " ^ op_str ^ " " ^ rhs_str
  | StmtWhile (cond, body) ->
      (* While loop: while cond; body; end *)
      let cond_str = gen_expr ctx cond in
      let body_strs = List.map (gen_stmt ctx) body.blk_stmts in
      let body_expr_str = match body.blk_expr with
        | Some e -> gen_expr ctx e
        | None -> ""
      in
      "while " ^ cond_str ^ "\n" ^ String.concat "\n" body_strs ^
      (if body_expr_str <> "" then "\n" ^ body_expr_str else "") ^ "\nend"
  | StmtFor (pat, iter, body) ->
      (* For loop: for pat in iter; body; end *)
      let pat_str = gen_pattern ctx pat in
      let iter_str = gen_expr ctx iter in
      let body_strs = List.map (gen_stmt ctx) body.blk_stmts in
      let body_expr_str = match body.blk_expr with
        | Some e -> gen_expr ctx e
        | None -> ""
      in
      "for " ^ pat_str ^ " in " ^ iter_str ^ "\n" ^ String.concat "\n" body_strs ^
      (if body_expr_str <> "" then "\n" ^ body_expr_str else "") ^ "\nend"

(* ============================================================================
   Top-Level Declaration Code Generation
   ============================================================================ *)

let gen_function ctx (fd : fn_decl) : unit =
  (* Function declaration:
     function name(param1::Type1, param2::Type2)::ReturnType
         body
     end
  *)
  let name = fd.fd_name.name in

  (* Parameters with type annotations *)
  let param_strs = List.map (fun param ->
    let param_name = param.p_name.name in
    let param_ty = type_expr_to_julia_string param.p_ty in
    param_name ^ "::" ^ param_ty
  ) fd.fd_params in

  (* Return type annotation *)
  let ret_ty_str = match fd.fd_ret_ty with
    | Some ty -> "::" ^ type_expr_to_julia_string ty
    | None -> ""
  in

  (* Function signature *)
  emit_line ctx ("function " ^ name ^ "(" ^ String.concat ", " param_strs ^ ")" ^ ret_ty_str);

  (* Function body *)
  let ctx_body = increase_indent ctx in
  let ctx_body = { ctx_body with in_function = true } in

  (match fd.fd_body with
  | FnExpr body_expr ->
      let body_str = gen_expr ctx_body body_expr in
      emit_line ctx_body ("return " ^ body_str)
  | FnBlock block ->
      (* Generate block statements *)
      List.iter (fun stmt ->
        let stmt_str = gen_stmt ctx_body stmt in
        emit_line ctx_body stmt_str
      ) block.blk_stmts;
      (* Generate final expression if present *)
      (match block.blk_expr with
      | Some e ->
          let expr_str = gen_expr ctx_body e in
          emit_line ctx_body ("return " ^ expr_str)
      | None -> ()));

  (* End function *)
  emit_line ctx "end";
  emit ctx "\n"

let gen_type_decl ctx (td : type_decl) : unit =
  (* Type declarations:
     Phase 1: Skip complex types, just emit comments
     Phase 2: Implement struct types
     Phase 3: Implement all types
  *)
  let name = td.td_name.name in
  emit_line ctx ("# Type declaration: " ^ name);
  emit_line ctx ("# Body: " ^ match td.td_body with
    | TyAlias _ -> "alias"
    | TyStruct _ -> "struct"
    | TyEnum _ -> "enum"
  );
  emit ctx "\n"

let gen_top_level ctx (top : top_level) : unit =
  match top with
  | TopFn fd -> gen_function ctx fd
  | TopType td -> gen_type_decl ctx td
  | TopConst _ -> emit_line ctx "# Constant (not yet implemented)"
  | TopEffect _ -> emit_line ctx "# Effect declaration (not yet implemented)"
  | TopTrait _ -> emit_line ctx "# Trait (not yet implemented)"
  | TopImpl _ -> emit_line ctx "# Impl block (not yet implemented)"

(* ============================================================================
   Main Code Generation Entry Point
   ============================================================================ *)

let generate (program : program) (symbols : Symbol.t) : string =
  (* Generate Julia code from AST *)
  let ctx = create_ctx symbols in

  (* Header comment *)
  emit_line ctx "# Generated by BetLang compiler (AffineScript)";
  emit_line ctx "# SPDX-License-Identifier: PMPL-1.0-or-later";
  emit ctx "\n";

  (* Generate each top-level declaration *)
  (* Note: Skipping prog_imports for Phase 1 - imports are import_decl, not top_level *)
  List.iter (gen_top_level ctx) program.prog_decls;

  (* Return accumulated output *)
  Buffer.contents ctx.output

(* ============================================================================
   Public API
   ============================================================================ *)

let codegen_julia (program : program) (symbols : Symbol.t) : (string, string) result =
  try
    let code = generate program symbols in
    Ok code
  with
  | Failure msg -> Error ("Julia codegen error: " ^ msg)
  | e -> Error ("Julia codegen error: " ^ Printexc.to_string e)
