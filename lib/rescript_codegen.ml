(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** ReScript emitter (typed-JS path).

    Lowers a subset of AffineScript to ReScript source. ReScript is
    syntactically OCaml-flavoured but compiles to JavaScript via the
    [rescript] compiler. Semantic mapping is close to OCaml; we emit
    ReScript surface syntax (curly-brace functions, [let] expressions,
    typed records). *)

open Ast

let res_reserved = [
  "and"; "as"; "assert"; "constraint"; "else"; "exception"; "external";
  "false"; "for"; "if"; "in"; "include"; "let"; "list"; "match"; "module";
  "mutable"; "of"; "open"; "private"; "rec"; "switch"; "then"; "to"; "true";
  "try"; "type"; "when"; "while"; "with";
]

let mangle s = if List.mem s res_reserved then s ^ "_" else s

(* Forward decl: ReScript type names (and the mangle_ty rule) live below
   gen_function; we mirror OCaml here. *)
let res_mangle_ty s =
  if String.length s = 0 then s
  else String.uncapitalize_ascii s

let rec res_type = function
  | TyCon id when id.name = "Int"    -> "int"
  | TyCon id when id.name = "Float"  -> "float"
  | TyCon id when id.name = "Bool"   -> "bool"
  | TyCon id when id.name = "String" -> "string"
  | TyCon id when id.name = "Unit"   -> "unit"
  | TyCon id -> res_mangle_ty id.name
  | TyTuple [] -> "unit"
  | TyTuple ts -> "(" ^ String.concat ", " (List.map res_type ts) ^ ")"
  | TyOwn t | TyRef t | TyMut t -> res_type t
  | _ -> "_"

let ret_type = function None -> "unit" | Some t -> res_type t

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id -> mangle id.name
  | ExprApp (callee, args) ->
      gen_expr callee ^ "(" ^ String.concat ", " (List.map gen_expr args) ^ ")"
  | ExprBinary (a, op, b) ->
      (* ReScript distinguishes int and float arithmetic operators. We emit
         the int form by default; programs whose lhs is Float should use
         dedicated builtins, which in turn map to [+.], [-.] etc. For MVP
         this matches AS's monomorphic int/float dispatch — the typechecker
         already pinned the operator type by this point. *)
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "mod"
        | OpEq  -> "==" | OpNe -> "!="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "&&" | OpOr -> "||"
        | OpConcat -> "++"
        | OpBitAnd -> "land" | OpBitOr -> "lor" | OpBitXor -> "lxor"
        | OpShl -> "lsl" | OpShr -> "lsr"
      in
      "(" ^ gen_expr a ^ " " ^ s ^ " " ^ gen_expr b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg -> "(- " ^ gen_expr x ^ ")"
       | OpNot -> "(!" ^ gen_expr x ^ ")"
       | OpBitNot -> "(lnot(" ^ gen_expr x ^ "))"
       | OpRef -> "ref(" ^ gen_expr x ^ ")"
       | OpDeref -> gen_expr x ^ ".contents")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let c = gen_expr ei_cond in
      let t = gen_expr ei_then in
      let f = match ei_else with Some e -> gen_expr e | None -> "()" in
      Printf.sprintf "(if %s { %s } else { %s })" c t f
  | ExprLet { el_pat; el_value; el_body; _ } ->
      let var = match el_pat with PatVar id -> mangle id.name | _ -> "_" in
      let v = gen_expr el_value in
      let body = match el_body with Some e -> gen_expr e | None -> "()" in
      Printf.sprintf "{ let %s = %s; %s }" var v body
  | ExprBlock blk -> gen_block blk
  | ExprTuple es -> "(" ^ String.concat ", " (List.map gen_expr es) ^ ")"
  | ExprTupleIndex (e, n) ->
      (* ReScript projections — match what's in scope. For arity-2 use fst/snd. *)
      (match n with
       | 0 -> Printf.sprintf "fst(%s)" (gen_expr e)
       | 1 -> Printf.sprintf "snd(%s)" (gen_expr e)
       | _ -> Printf.sprintf "failwith(\"ReScript: tuple index %d unsupported\")" n)
  | ExprArray es -> "[" ^ String.concat ", " (List.map gen_expr es) ^ "]"
  | ExprIndex (a, i) -> gen_expr a ^ "[" ^ gen_expr i ^ "]"
  | ExprRecord { er_fields; _ } ->
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr e | None -> mangle id.name in
        Printf.sprintf "%s: %s" (mangle id.name) v
      ) er_fields in
      "{ " ^ String.concat ", " fs ^ " }"
  | ExprField (record, field) -> gen_expr record ^ "." ^ mangle field.name
  | ExprVariant (_ty, ctor) -> mangle ctor.name
  | ExprMatch { em_scrutinee; em_arms } ->
      let arms = List.map (fun arm ->
        Printf.sprintf "| %s => %s" (gen_pattern arm.ma_pat) (gen_expr arm.ma_body)
      ) em_arms in
      Printf.sprintf "switch %s { %s }" (gen_expr em_scrutinee)
        (String.concat " " arms)
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> gen_expr e
  | _ -> "(failwith(\"ReScript backend: unsupported expression\"))"

and gen_pattern (p : pattern) : string =
  match p with
  | PatWildcard _ -> "_"
  | PatVar id -> mangle id.name
  | PatLit lit -> gen_lit lit
  | PatCon (id, args) ->
      if args = [] then mangle id.name
      else mangle id.name ^ "(" ^
           String.concat ", " (List.map gen_pattern args) ^ ")"
  | PatTuple ps -> "(" ^ String.concat ", " (List.map gen_pattern ps) ^ ")"
  | PatRecord (fields, _) ->
      let fs = List.map (fun (id, sub) ->
        match sub with
        | None     -> mangle id.name
        | Some sub -> Printf.sprintf "%s: %s" (mangle id.name) (gen_pattern sub)
      ) fields in
      "{ " ^ String.concat ", " fs ^ " }"
  | PatAs (id, _) -> mangle id.name
  | PatOr (p, _) -> gen_pattern p

and gen_lit = function
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _) -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _) -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _) -> "'" ^ Char.escaped c ^ "'"
  | LitUnit _ -> "()"

and gen_block (blk : block) : string =
  let stmts = List.map gen_stmt blk.blk_stmts in
  let tail = match blk.blk_expr with Some e -> gen_expr e | None -> "()" in
  "{ " ^ String.concat " " stmts ^ " " ^ tail ^ " }"

and gen_stmt = function
  | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
      Printf.sprintf "let %s = %s;" (mangle id.name) (gen_expr sl_value)
  | StmtLet _ -> ""
  | StmtExpr e -> gen_expr e ^ ";"
  | StmtAssign (lhs, _, rhs) ->
      Printf.sprintf "%s.contents = %s;" (gen_expr lhs) (gen_expr rhs)
  | StmtWhile (cond, body) ->
      Printf.sprintf "while %s { %s }" (gen_expr cond) (gen_block body)
  | StmtFor _ -> ""

let gen_function (fd : fn_decl) : string =
  let name = mangle fd.fd_name.name in
  let params = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%s: %s" (mangle p.p_name.name) (res_type p.p_ty))
       fd.fd_params) in
  let ret = ret_type fd.fd_ret_ty in
  let body = match fd.fd_body with
    | FnExpr e -> gen_expr e
    | FnBlock b -> gen_block b
  in
  Printf.sprintf "let %s = (%s): %s => %s\n\n" name params ret body

(* ReScript type names, like OCaml's, must be lowercase. *)
let mangle_ty s =
  if String.length s = 0 then s
  else
    let lowered = String.uncapitalize_ascii s in
    if List.mem lowered res_reserved then "_" ^ lowered else lowered

let gen_type_decl (td : type_decl) : string =
  let name = mangle_ty td.td_name.name in
  match td.td_body with
  | TyAlias (TyRecord (fields, _)) ->
      let fs = List.map (fun (rf : row_field) ->
        Printf.sprintf "  %s: %s" (mangle rf.rf_name.name) (res_type rf.rf_ty)
      ) fields in
      Printf.sprintf "type %s = {\n%s,\n}\n\n" name (String.concat ",\n" fs)
  | TyAlias t -> Printf.sprintf "type %s = %s\n\n" name (res_type t)
  | TyStruct fields ->
      let fs = List.map (fun (sf : struct_field) ->
        Printf.sprintf "  %s: %s" (mangle sf.sf_name.name) (res_type sf.sf_ty)
      ) fields in
      Printf.sprintf "type %s = {\n%s,\n}\n\n" name (String.concat ",\n" fs)
  | TyEnum variants ->
      let vs = List.map (fun (vd : variant_decl) ->
        let tys = List.map res_type vd.vd_fields in
        let body = if tys = [] then "" else "(" ^ String.concat ", " tys ^ ")" in
        Printf.sprintf "  | %s%s" (mangle vd.vd_name.name) body
      ) variants in
      Printf.sprintf "type %s =\n%s\n\n" name (String.concat "\n" vs)

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "// Generated by AffineScript compiler\n";
  Buffer.add_string buf "// SPDX-License-Identifier: PMPL-1.0-or-later\n\n";
  List.iter (function
    | TopType td -> Buffer.add_string buf (gen_type_decl td)
    | _ -> ()
  ) program.prog_decls;
  List.iter (function
    | TopFn fd -> Buffer.add_string buf (gen_function fd)
    | _ -> ()
  ) program.prog_decls;
  Buffer.contents buf

let codegen_rescript (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("ReScript codegen error: " ^ msg)
  | e           -> Error ("ReScript codegen error: " ^ Printexc.to_string e)
