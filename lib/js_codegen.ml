(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** JavaScript Code Generator (MVP).

    Translates AffineScript AST to ES2020 JavaScript source code.
    Targets Deno/Node — no Web Audio, no DOM. Output is a single self-contained
    file that includes a minimal runtime prelude (Some/None/Ok/Err builders,
    println). Effects are erased at this layer; IO operations call the prelude.

    Phase 1 (this file): functions, arithmetic, control flow, let, tuples,
    records, simple match, Option/Result constructors, try/catch. Sufficient
    for the conformance subset that does not depend on ownership at runtime.
*)

open Ast

(* ============================================================================
   Code Generation Context
   ============================================================================ *)

type codegen_ctx = {
  output : Buffer.t;
  indent : int;
  symbols : Symbol.t;
  in_function : bool;
}

let create_ctx symbols = {
  output = Buffer.create 1024;
  indent = 0;
  symbols;
  in_function = false;
}

let emit ctx str =
  Buffer.add_string ctx.output str

let emit_line ctx str =
  let spaces = String.make (ctx.indent * 2) ' ' in
  Buffer.add_string ctx.output spaces;
  Buffer.add_string ctx.output str;
  Buffer.add_char ctx.output '\n'

let increase_indent ctx = { ctx with indent = ctx.indent + 1 }
let decrease_indent ctx = { ctx with indent = max 0 (ctx.indent - 1) }

(* ============================================================================
   Runtime prelude

   Emitted once at the top of every output file. Keeps generated code free of
   library dependencies — `deno run foo.js` or `node foo.js` is enough.
   ============================================================================ *)

let prelude = {|// ---- AffineScript JS runtime (MVP) ----
const Some = (value) => ({ tag: "Some", value });
const None = { tag: "None" };
const Ok   = (value) => ({ tag: "Ok",  value });
const Err  = (error) => ({ tag: "Err", error });
const Unit = null;
const print   = (s) => { (typeof Deno !== "undefined" ? Deno.stdout.writeSync(new TextEncoder().encode(String(s))) : process.stdout.write(String(s))); };
const println = (s) => { console.log(String(s)); };
// ---- end runtime ----

|}

(* ============================================================================
   Identifier sanitisation

   AffineScript identifiers are mostly JS-safe. Reserved keywords are renamed
   with a trailing underscore so generated code parses.
   ============================================================================ *)

let js_reserved = [
  "abstract"; "arguments"; "await"; "boolean"; "break"; "byte"; "case";
  "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default";
  "delete"; "do"; "double"; "else"; "enum"; "eval"; "export"; "extends";
  "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if";
  "implements"; "import"; "in"; "instanceof"; "int"; "interface"; "let";
  "long"; "native"; "new"; "null"; "package"; "private"; "protected";
  "public"; "return"; "short"; "static"; "super"; "switch"; "synchronized";
  "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "var";
  "void"; "volatile"; "while"; "with"; "yield";
]

let mangle (name : string) : string =
  if List.mem name js_reserved then name ^ "_"
  else name

(* ============================================================================
   Expression Code Generation
   ============================================================================ *)

let rec gen_expr ctx (expr : expr) : string =
  match expr with
  | ExprLit lit -> gen_literal lit
  | ExprVar name -> mangle name.name
  | ExprApp (func, args) ->
      let func_str = gen_expr ctx func in
      let arg_strs = List.map (gen_expr ctx) args in
      func_str ^ "(" ^ String.concat ", " arg_strs ^ ")"
  | ExprBinary (e1, op, e2) ->
      let op_str = match op with
        | OpAdd -> "+"
        | OpSub -> "-"
        | OpMul -> "*"
        | OpDiv -> "/"
        | OpMod -> "%"
        | OpEq  -> "==="
        | OpNe  -> "!=="
        | OpLt  -> "<"
        | OpLe  -> "<="
        | OpGt  -> ">"
        | OpGe  -> ">="
        | OpAnd -> "&&"
        | OpOr  -> "||"
        | OpBitAnd -> "&"
        | OpBitOr  -> "|"
        | OpBitXor -> "^"
        | OpShl -> "<<"
        | OpShr -> ">>"
        | OpConcat -> "+"  (* JS string/array overload *)
      in
      "(" ^ gen_expr ctx e1 ^ " " ^ op_str ^ " " ^ gen_expr ctx e2 ^ ")"
  | ExprUnary (op, e) ->
      (match op with
      | OpNeg    -> "(-" ^ gen_expr ctx e ^ ")"
      | OpNot    -> "(!" ^ gen_expr ctx e ^ ")"
      | OpBitNot -> "(~" ^ gen_expr ctx e ^ ")"
      | OpRef    -> "({ get: () => " ^ gen_expr ctx e ^ ", set: (_) => {} })"
      | OpDeref  -> "(" ^ gen_expr ctx e ^ ".get())")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let cond_str = gen_expr ctx ei_cond in
      let then_str = gen_expr ctx ei_then in
      let else_str = match ei_else with
        | Some e -> gen_expr ctx e
        | None   -> "Unit"
      in
      "(" ^ cond_str ^ " ? " ^ then_str ^ " : " ^ else_str ^ ")"
  | ExprLet { el_pat; el_value; el_body; el_mut; el_quantity = _; el_ty = _ } ->
      let pat_str = gen_pattern ctx el_pat in
      let val_str = gen_expr ctx el_value in
      let kw = if el_mut then "let" else "const" in
      (match el_body with
      | Some body ->
          let body_str = gen_expr ctx body in
          "((() => { " ^ kw ^ " " ^ pat_str ^ " = " ^ val_str ^ "; return " ^
          body_str ^ "; })())"
      | None ->
          (* Statement-position let folded into expression: emit IIFE returning Unit. *)
          "((() => { " ^ kw ^ " " ^ pat_str ^ " = " ^ val_str ^ "; return Unit; })())")
  | ExprTuple exprs ->
      let expr_strs = List.map (gen_expr ctx) exprs in
      "[" ^ String.concat ", " expr_strs ^ "]"
  | ExprArray exprs ->
      let expr_strs = List.map (gen_expr ctx) exprs in
      "[" ^ String.concat ", " expr_strs ^ "]"
  | ExprIndex (arr, idx) ->
      gen_expr ctx arr ^ "[" ^ gen_expr ctx idx ^ "]"
  | ExprTupleIndex (e, n) ->
      gen_expr ctx e ^ "[" ^ string_of_int n ^ "]"
  | ExprRecord { er_fields; er_spread } ->
      let field_strs = List.map (fun (name, e_opt) ->
        let val_str = match e_opt with
          | Some e -> gen_expr ctx e
          | None   -> mangle name.name  (* punning: { x } -> { x: x } *)
        in
        mangle name.name ^ ": " ^ val_str
      ) er_fields in
      let spread_str = match er_spread with
        | Some e -> "...(" ^ gen_expr ctx e ^ "), "
        | None   -> ""
      in
      "({ " ^ spread_str ^ String.concat ", " field_strs ^ " })"
  | ExprField (record, field) ->
      gen_expr ctx record ^ "." ^ mangle field.name
  | ExprMatch { em_scrutinee; em_arms } ->
      gen_match ctx em_scrutinee em_arms
  | ExprBlock block ->
      gen_block_expr ctx block
  | ExprReturn (Some e) ->
      "(() => { return " ^ gen_expr ctx e ^ "; })()"
  | ExprReturn None ->
      "(() => { return Unit; })()"
  | ExprLambda { elam_params; elam_body; elam_ret_ty = _ } ->
      let param_strs = List.map (fun (p : param) -> mangle p.p_name.name) elam_params in
      "((" ^ String.concat ", " param_strs ^ ") => " ^ gen_expr ctx elam_body ^ ")"
  | ExprTry { et_body; et_catch; et_finally } ->
      gen_try ctx et_body et_catch et_finally
  | ExprVariant (ty, ctor) ->
      (* `Type::Variant` — emit a tagged object factory.  Special-cases for
         the prelude builders so `Option::None` / `Result::Ok` map directly. *)
      (match ty.name, ctor.name with
       | _, "None"  -> "None"
       | _, "Some"  -> "Some"
       | _, "Ok"    -> "Ok"
       | _, "Err"   -> "Err"
       | _, name    -> Printf.sprintf "({ tag: %S })" name)
  | ExprSpan (inner, _) -> gen_expr ctx inner
  | ExprRowRestrict (e, _field) -> gen_expr ctx e  (* runtime no-op *)
  | ExprHandle { eh_body; eh_handlers = _ } ->
      (* Effect handlers are erased at MVP — IO collapses to direct calls. *)
      gen_expr ctx eh_body
  | ExprResume (Some e) -> gen_expr ctx e
  | ExprResume None     -> "Unit"
  | ExprUnsafe _ ->
      "(() => { throw new Error('unsafe op not supported in JS backend'); })()"

and gen_literal (lit : literal) : string =
  match lit with
  | LitInt (n, _)        -> string_of_int n
  | LitFloat (f, _)      ->
      (* OCaml's string_of_float can produce "1." — patch to "1.0" so JS parses
         identically and the output is stable. *)
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _)    -> "true"
  | LitBool (false, _)   -> "false"
  | LitString (s, _)     -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _)       -> "\"" ^ Char.escaped c ^ "\""
  | LitUnit _            -> "Unit"

and gen_pattern ctx (pat : pattern) : string =
  (* Used in binder positions: let x = ..., function params, for x in ... *)
  match pat with
  | PatWildcard _ -> "_"
  | PatVar name   -> mangle name.name
  | PatLit _      -> "_"  (* Literal patterns can't bind; only meaningful in match *)
  | PatTuple pats ->
      let pat_strs = List.map (gen_pattern ctx) pats in
      "[" ^ String.concat ", " pat_strs ^ "]"
  | PatRecord (fields, _) ->
      let strs = List.map (fun (n, sub) ->
        match sub with
        | None     -> mangle n.name
        | Some sub -> mangle n.name ^ ": " ^ gen_pattern ctx sub
      ) fields in
      "{ " ^ String.concat ", " strs ^ " }"
  | PatAs (id, _) -> mangle id.name
  | PatCon (id, _) -> mangle id.name  (* approximate *)
  | PatOr (p, _) -> gen_pattern ctx p

and gen_match ctx scrutinee arms =
  (* Lower `match` to an IIFE that destructures the scrutinee once and runs an
     if-else cascade. Each arm gets a freshly-scoped block so binders don't
     collide. *)
  let scrutinee_str = gen_expr ctx scrutinee in
  let scrut_var = "__scrut" in
  let rec gen_arms = function
    | [] ->
        "throw new Error(\"non-exhaustive match\");"
    | arm :: rest ->
        let cond = gen_pattern_test scrut_var arm.ma_pat in
        let bindings = gen_pattern_bindings scrut_var arm.ma_pat in
        let guard = match arm.ma_guard with
          | Some g -> " && (" ^ gen_expr ctx g ^ ")"
          | None   -> ""
        in
        let body = gen_expr ctx arm.ma_body in
        let inner_bindings =
          if bindings = "" then ""
          else bindings ^ " "
        in
        let prefix =
          (* When we have bindings, emit them inside the branch so they are
             scoped to that arm and visible to the guard check. *)
          if bindings = "" then
            "if (" ^ cond ^ guard ^ ") { return " ^ body ^ "; }"
          else if arm.ma_guard = None then
            "if (" ^ cond ^ ") { " ^ inner_bindings ^ "return " ^ body ^ "; }"
          else
            "if (" ^ cond ^ ") { " ^ inner_bindings ^
            "if (" ^ (match arm.ma_guard with Some g -> gen_expr ctx g | None -> "true") ^
            ") { return " ^ body ^ "; } }"
        in
        prefix ^ " " ^ gen_arms rest
  in
  "((" ^ scrut_var ^ ") => { " ^ gen_arms arms ^ " })(" ^ scrutinee_str ^ ")"

and gen_pattern_test scrut pat =
  match pat with
  | PatWildcard _ | PatVar _ -> "true"
  | PatLit lit -> scrut ^ " === " ^ gen_literal lit
  | PatCon (id, _) ->
      (* Tagged-union variant: { tag: "Some", value: ... } *)
      scrut ^ ".tag === " ^ Printf.sprintf "%S" id.name
  | PatTuple pats ->
      let conds = List.mapi (fun i p ->
        gen_pattern_test (scrut ^ "[" ^ string_of_int i ^ "]") p
      ) pats in
      String.concat " && " (("Array.isArray(" ^ scrut ^ ")") :: conds)
  | PatRecord (fields, _) ->
      let conds = List.map (fun (n, sub) ->
        match sub with
        | None     -> "true"
        | Some sub -> gen_pattern_test (scrut ^ "." ^ mangle n.name) sub
      ) fields in
      String.concat " && " conds
  | PatAs (_, p) -> gen_pattern_test scrut p
  | PatOr (p1, p2) ->
      "((" ^ gen_pattern_test scrut p1 ^ ") || (" ^ gen_pattern_test scrut p2 ^ "))"

and gen_pattern_bindings scrut pat =
  (* Emit `const x = scrut.<path>;` declarations for every binder reachable
     from this pattern, skipping the wildcard and literal cases. *)
  let buf = Buffer.create 64 in
  let rec walk path = function
    | PatWildcard _ | PatLit _ -> ()
    | PatVar id ->
        Buffer.add_string buf
          ("const " ^ mangle id.name ^ " = " ^ path ^ "; ")
    | PatTuple pats ->
        List.iteri (fun i p ->
          walk (path ^ "[" ^ string_of_int i ^ "]") p
        ) pats
    | PatRecord (fields, _) ->
        List.iter (fun (n, sub) ->
          let sub_path = path ^ "." ^ mangle n.name in
          match sub with
          | None     ->
              Buffer.add_string buf
                ("const " ^ mangle n.name ^ " = " ^ sub_path ^ "; ")
          | Some sub -> walk sub_path sub
        ) fields
    | PatCon (_, args) ->
        (* Convention: variant payload lives at .value (single-arg, like Some)
           or .values[i] (multi-arg). Use .value for arity 1 to match prelude. *)
        (match args with
         | [] -> ()
         | [single] -> walk (path ^ ".value") single
         | many ->
             List.iteri (fun i p ->
               walk (path ^ ".values[" ^ string_of_int i ^ "]") p
             ) many)
    | PatAs (id, sub) ->
        Buffer.add_string buf ("const " ^ mangle id.name ^ " = " ^ path ^ "; ");
        walk path sub
    | PatOr (p, _) -> walk path p
  in
  walk scrut pat;
  Buffer.contents buf

and gen_block_expr ctx block =
  (* JS has no block-as-expression. Emit an IIFE; statements run in order, and
     the trailing expression (if any) becomes the return value. *)
  let body = Buffer.create 64 in
  List.iter (fun s ->
    Buffer.add_string body (gen_stmt ctx s);
    Buffer.add_string body " "
  ) block.blk_stmts;
  let result = match block.blk_expr with
    | Some e -> "return " ^ gen_expr ctx e ^ ";"
    | None   -> "return Unit;"
  in
  "(() => { " ^ Buffer.contents body ^ result ^ " })()"

and gen_try ctx body catch finally =
  let body_str = gen_block_expr ctx body in
  let catch_str = match catch with
    | None | Some [] -> "catch (__e) { throw __e; }"
    | Some (arm :: _) ->
        let bind = match arm.ma_pat with
          | PatVar id -> "const " ^ mangle id.name ^ " = __e; "
          | _ -> ""
        in
        "catch (__e) { " ^ bind ^ "return " ^ gen_expr ctx arm.ma_body ^ "; }"
  in
  let finally_str = match finally with
    | None     -> ""
    | Some blk -> " finally { " ^ gen_block_expr ctx blk ^ "; }"
  in
  "(() => { try { return " ^ body_str ^ "; } " ^ catch_str ^ finally_str ^ " })()"

and gen_stmt ctx (stmt : stmt) : string =
  match stmt with
  | StmtLet { sl_pat; sl_value; sl_mut; sl_quantity = _; sl_ty = _ } ->
      let pat_str = gen_pattern ctx sl_pat in
      let val_str = gen_expr ctx sl_value in
      let kw = if sl_mut then "let" else "const" in
      kw ^ " " ^ pat_str ^ " = " ^ val_str ^ ";"
  | StmtExpr e ->
      gen_expr ctx e ^ ";"
  | StmtAssign (lhs, op, rhs) ->
      let op_str = match op with
        | AssignEq  -> "="
        | AssignAdd -> "+="
        | AssignSub -> "-="
        | AssignMul -> "*="
        | AssignDiv -> "/="
      in
      gen_expr ctx lhs ^ " " ^ op_str ^ " " ^ gen_expr ctx rhs ^ ";"
  | StmtWhile (cond, body) ->
      "while (" ^ gen_expr ctx cond ^ ") { " ^
      String.concat " " (List.map (gen_stmt ctx) body.blk_stmts) ^
      (match body.blk_expr with
       | Some e -> " " ^ gen_expr ctx e ^ ";"
       | None   -> "") ^
      " }"
  | StmtFor (pat, iter, body) ->
      "for (const " ^ gen_pattern ctx pat ^ " of " ^ gen_expr ctx iter ^ ") { " ^
      String.concat " " (List.map (gen_stmt ctx) body.blk_stmts) ^
      (match body.blk_expr with
       | Some e -> " " ^ gen_expr ctx e ^ ";"
       | None   -> "") ^
      " }"

(* ============================================================================
   Top-Level Declaration Code Generation
   ============================================================================ *)

let gen_function ctx (fd : fn_decl) : unit =
  let name = mangle fd.fd_name.name in
  let param_strs = List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params in
  let header = Printf.sprintf "function %s(%s) {" name (String.concat ", " param_strs) in
  emit_line ctx header;
  let ctx_body = increase_indent { ctx with in_function = true } in
  (match fd.fd_body with
   | FnExpr body_expr ->
       emit_line ctx_body ("return " ^ gen_expr ctx_body body_expr ^ ";")
   | FnBlock block ->
       List.iter (fun s -> emit_line ctx_body (gen_stmt ctx_body s)) block.blk_stmts;
       (match block.blk_expr with
        | Some e -> emit_line ctx_body ("return " ^ gen_expr ctx_body e ^ ";")
        | None   -> ()));
  emit_line (decrease_indent ctx_body) "}";
  emit ctx "\n"

let gen_type_decl ctx (td : type_decl) : unit =
  (* Phase 1: emit constructor factories for enum variants so pattern matches
     and `Type::Variant` references both work. Structs and aliases are erased. *)
  match td.td_body with
  | TyEnum variants ->
      List.iter (fun (vd : variant_decl) ->
        let name = mangle vd.vd_name.name in
        let arity = List.length vd.vd_fields in
        if arity = 0 then
          emit_line ctx (Printf.sprintf "const %s = { tag: \"%s\" };" name vd.vd_name.name)
        else if arity = 1 then
          emit_line ctx
            (Printf.sprintf "const %s = (value) => ({ tag: \"%s\", value });"
               name vd.vd_name.name)
        else
          let params = List.init arity (fun i -> "v" ^ string_of_int i) in
          emit_line ctx
            (Printf.sprintf "const %s = (%s) => ({ tag: \"%s\", values: [%s] });"
               name (String.concat ", " params)
               vd.vd_name.name (String.concat ", " params))
      ) variants;
      emit ctx "\n"
  | TyStruct _ | TyAlias _ ->
      emit_line ctx (Printf.sprintf "// type %s (erased)" td.td_name.name)

let gen_top_level ctx (top : top_level) : unit =
  match top with
  | TopFn fd     -> gen_function ctx fd
  | TopType td   -> gen_type_decl ctx td
  | TopConst { tc_name; tc_value; _ } ->
      emit_line ctx
        (Printf.sprintf "const %s = %s;" (mangle tc_name.name)
           (gen_expr ctx tc_value))
  | TopEffect _  -> emit_line ctx "// effect declaration (erased)"
  | TopTrait _   -> emit_line ctx "// trait declaration (erased)"
  | TopImpl _    -> emit_line ctx "// impl block (erased)"

(* ============================================================================
   Main Code Generation Entry Point
   ============================================================================ *)

let generate (program : program) (symbols : Symbol.t) : string =
  let ctx = create_ctx symbols in
  emit_line ctx "// Generated by AffineScript compiler";
  emit_line ctx "// SPDX-License-Identifier: PMPL-1.0-or-later";
  emit ctx prelude;
  List.iter (gen_top_level ctx) program.prog_decls;
  (* If a `main` function exists, invoke it so `node foo.js` actually runs. *)
  let has_main = List.exists (function
    | TopFn fd -> fd.fd_name.name = "main"
    | _ -> false
  ) program.prog_decls in
  if has_main then emit_line ctx "main();";
  Buffer.contents ctx.output

let codegen_js (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("JS codegen error: " ^ msg)
  | e           -> Error ("JS codegen error: " ^ Printexc.to_string e)
