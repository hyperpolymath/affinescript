(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** OCaml Self-Target Emitter (MVP).

    Lowers a subset of AffineScript to compilable OCaml source. Useful as
    a self-validation target — we *are* OCaml, so this is effectively a
    pretty-printer with renaming. *)

open Ast

let ocaml_reserved = [
  "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
  "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
  "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer";
  "lazy"; "let"; "match"; "method"; "module"; "mutable"; "new"; "nonrec";
  "object"; "of"; "open"; "or"; "private"; "rec"; "sig"; "struct"; "then";
  "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
]

let mangle s =
  if List.mem s ocaml_reserved then s ^ "_" else s

(* OCaml type names must start lowercase; AS uses TitleCase by convention.
   Lowercase the first letter and dodge reserved words by prefixing. *)
let mangle_ty s =
  if String.length s = 0 then s
  else
    let lowered = String.uncapitalize_ascii s in
    if List.mem lowered ocaml_reserved then "_" ^ lowered else lowered

let rec ml_type = function
  | TyCon id when id.name = "Int"    -> "int"
  | TyCon id when id.name = "Float"  -> "float"
  | TyCon id when id.name = "Bool"   -> "bool"
  | TyCon id when id.name = "String" -> "string"
  | TyCon id when id.name = "Unit"   -> "unit"
  | TyCon id -> mangle_ty id.name
  | TyTuple [] -> "unit"
  | TyTuple ts -> "(" ^ String.concat " * " (List.map ml_type ts) ^ ")"
  | TyOwn t | TyRef t | TyMut t -> ml_type t
  | _ -> "_"

let ret_type = function
  | None    -> "unit"
  | Some t  -> ml_type t

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id  -> mangle id.name
  | ExprApp (callee, args) ->
      let f = gen_expr callee in
      let xs = List.map (fun a -> "(" ^ gen_expr a ^ ")") args in
      f ^ " " ^ String.concat " " xs
  | ExprBinary (a, op, b) ->
      let opstr = match op with
        | OpAdd -> "+"   | OpSub -> "-"   | OpMul -> "*"  | OpDiv -> "/"
        | OpMod -> "mod" | OpEq  -> "="   | OpNe  -> "<>"
        | OpLt  -> "<"   | OpLe  -> "<="  | OpGt  -> ">"  | OpGe  -> ">="
        | OpAnd -> "&&"  | OpOr  -> "||"
        | OpBitAnd -> "land" | OpBitOr -> "lor" | OpBitXor -> "lxor"
        | OpShl -> "lsl" | OpShr -> "lsr"
        | OpConcat -> "^"
      in
      "(" ^ gen_expr a ^ " " ^ opstr ^ " " ^ gen_expr b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg    -> "(- " ^ gen_expr x ^ ")"
       | OpNot    -> "(not " ^ gen_expr x ^ ")"
       | OpBitNot -> "(lnot " ^ gen_expr x ^ ")"
       | OpRef    -> "(ref " ^ gen_expr x ^ ")"
       | OpDeref  -> "(!" ^ gen_expr x ^ ")")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let c = gen_expr ei_cond in
      let t = gen_expr ei_then in
      let f = match ei_else with Some e -> gen_expr e | None -> "()" in
      Printf.sprintf "(if %s then %s else %s)" c t f
  | ExprLet { el_pat; el_value; el_body; _ } ->
      let var = match el_pat with PatVar id -> mangle id.name | _ -> "_" in
      let v = gen_expr el_value in
      let body = match el_body with Some e -> gen_expr e | None -> "()" in
      Printf.sprintf "(let %s = %s in %s)" var v body
  | ExprBlock blk -> gen_block blk
  | ExprTuple es -> "(" ^ String.concat ", " (List.map gen_expr es) ^ ")"
  | ExprTupleIndex (e, n) ->
      (* OCaml has no .N tuple projection. Use fst/snd for the binary case
         (the common one); for higher arities, the codegen would need the
         tuple's static type to know how many wildcards to emit. We don't
         track it through the AST yet, so error loudly on n > 1. *)
      (match n with
       | 0 -> Printf.sprintf "(fst %s)" (gen_expr e)
       | 1 -> Printf.sprintf "(snd %s)" (gen_expr e)
       | _ ->
           Printf.sprintf "(failwith \"OCaml backend: tuple index %d (need static arity)\")" n)
  | ExprArray es -> "[|" ^ String.concat "; " (List.map gen_expr es) ^ "|]"
  | ExprIndex (a, i) ->
      Printf.sprintf "%s.(%s)" (gen_expr a) (gen_expr i)
  | ExprRecord { er_fields; _ } ->
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr e | None -> mangle id.name in
        Printf.sprintf "%s = %s" (mangle id.name) v
      ) er_fields in
      "{ " ^ String.concat "; " fs ^ " }"
  | ExprField (record, field) ->
      gen_expr record ^ "." ^ mangle field.name
  | ExprVariant (_ty, ctor) -> mangle ctor.name
  | ExprMatch { em_scrutinee; em_arms } ->
      let arms = List.map (fun arm ->
        Printf.sprintf "| %s -> %s" (gen_pattern arm.ma_pat) (gen_expr arm.ma_body)
      ) em_arms in
      Printf.sprintf "(match %s with %s)" (gen_expr em_scrutinee)
        (String.concat " " arms)
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> gen_expr e
  | _ -> "(failwith \"OCaml backend: unsupported expression\")"

and gen_pattern (p : pattern) : string =
  match p with
  | PatWildcard _ -> "_"
  | PatVar id -> mangle id.name
  | PatLit lit -> gen_literal lit
  | PatCon (id, args) ->
      if args = [] then mangle id.name
      else
        let aps = List.map gen_pattern args in
        if List.length aps = 1 then mangle id.name ^ " " ^ List.hd aps
        else mangle id.name ^ " (" ^ String.concat ", " aps ^ ")"
  | PatTuple ps -> "(" ^ String.concat ", " (List.map gen_pattern ps) ^ ")"
  | PatRecord (fields, _) ->
      let fs = List.map (fun (id, sub) ->
        match sub with
        | None     -> mangle id.name
        | Some sub -> Printf.sprintf "%s = %s" (mangle id.name) (gen_pattern sub)
      ) fields in
      "{ " ^ String.concat "; " fs ^ " }"
  | PatAs (id, _) -> mangle id.name
  | PatOr (p, _) -> gen_pattern p

and gen_literal lit =
  match lit with
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _) -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _) -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _) -> "'" ^ Char.escaped c ^ "'"
  | LitUnit _ -> "()"

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
  match stmts with
  | [] -> tail
  | _  -> "(" ^ String.concat "; " stmts ^ "; " ^ tail ^ ")"

and gen_stmt = function
  | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
      Printf.sprintf "let %s = %s in ()" (mangle id.name) (gen_expr sl_value)
      (* Note: this fragment is then sequenced by gen_block. For trivial MVP
         we approximate with `let _ = ... in ()` style sequencing, which
         compiles even though it's not idiomatic. *)
  | StmtLet _ -> "()"
  | StmtExpr e -> gen_expr e
  | StmtAssign (lhs, _, rhs) ->
      Printf.sprintf "%s := %s" (gen_expr lhs) (gen_expr rhs)
  | StmtWhile (cond, body) ->
      Printf.sprintf "while %s do %s done" (gen_expr cond) (gen_block body)
  | StmtFor _ -> "()"

let gen_function (fd : fn_decl) : string =
  let name = mangle fd.fd_name.name in
  let params = match fd.fd_params with
    | [] -> "()"
    | _  -> String.concat " " (List.map
              (fun (p : param) -> "(" ^ mangle p.p_name.name ^ " : " ^ ml_type p.p_ty ^ ")")
              fd.fd_params)
  in
  let ret = ret_type fd.fd_ret_ty in
  let body = match fd.fd_body with
    | FnExpr e -> gen_expr e
    | FnBlock b -> gen_block b
  in
  Printf.sprintf "let %s %s : %s =\n  %s\n" name params ret body

let gen_type_decl (td : type_decl) : string =
  let name = mangle_ty td.td_name.name in
  match td.td_body with
  | TyAlias (TyRecord (fields, _)) ->
      let fs = List.map (fun (rf : row_field) ->
        Printf.sprintf "  %s : %s" (mangle rf.rf_name.name) (ml_type rf.rf_ty)
      ) fields in
      Printf.sprintf "type %s = {\n%s\n}\n\n" name (String.concat ";\n" fs)
  | TyAlias t -> Printf.sprintf "type %s = %s\n\n" name (ml_type t)
  | TyStruct fields ->
      let fs = List.map (fun (sf : struct_field) ->
        Printf.sprintf "  %s : %s" (mangle sf.sf_name.name) (ml_type sf.sf_ty)
      ) fields in
      Printf.sprintf "type %s = {\n%s\n}\n\n" name (String.concat ";\n" fs)
  | TyEnum variants ->
      let vs = List.map (fun (vd : variant_decl) ->
        let tys = List.map ml_type vd.vd_fields in
        let body = if tys = [] then "" else " of " ^ String.concat " * " tys in
        Printf.sprintf "  | %s%s" (mangle vd.vd_name.name) body
      ) variants in
      Printf.sprintf "type %s =\n%s\n\n" name (String.concat "\n" vs)

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "(* Generated by AffineScript compiler *)\n";
  Buffer.add_string buf "(* SPDX-License-Identifier: PMPL-1.0-or-later *)\n\n";
  (* Type decls precede functions so the typechecker sees the schema. *)
  List.iter (function
    | TopType td -> Buffer.add_string buf (gen_type_decl td)
    | _ -> ()
  ) program.prog_decls;
  List.iter (fun decl ->
    match decl with
    | TopFn fd -> Buffer.add_string buf (gen_function fd); Buffer.add_char buf '\n'
    | TopConst { tc_name; tc_ty; tc_value; _ } ->
        Buffer.add_string buf
          (Printf.sprintf "let %s : %s = %s\n\n"
             (mangle tc_name.name) (ml_type tc_ty) (gen_expr tc_value))
    | _ -> ()
  ) program.prog_decls;
  let has_main = List.exists (function
    | TopFn fd -> fd.fd_name.name = "main"
    | _ -> false) program.prog_decls in
  if has_main then
    Buffer.add_string buf "let () = ignore (main ())\n";
  Buffer.contents buf

let codegen_ocaml (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("OCaml codegen error: " ^ msg)
  | e           -> Error ("OCaml codegen error: " ^ Printexc.to_string e)
