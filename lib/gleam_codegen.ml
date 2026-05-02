(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Gleam emitter (BEAM target).

    Lowers a subset of AffineScript to Gleam, which is a Hindley-Milner
    typed language compiling to Erlang (BEAM) and JavaScript. *)

open Ast

let gleam_reserved = [
  "as"; "assert"; "case"; "const"; "external"; "fn"; "if"; "import";
  "let"; "opaque"; "panic"; "pub"; "todo"; "type"; "use";
]

let mangle s = if List.mem s gleam_reserved then s ^ "_" else s

let rec gleam_type = function
  | TyCon id when id.name = "Int"    -> "Int"
  | TyCon id when id.name = "Float"  -> "Float"
  | TyCon id when id.name = "Bool"   -> "Bool"
  | TyCon id when id.name = "String" -> "String"
  | TyCon id when id.name = "Unit"   -> "Nil"
  | TyCon id -> mangle id.name
  | TyTuple [] -> "Nil"
  | TyTuple ts -> "#(" ^ String.concat ", " (List.map gleam_type ts) ^ ")"
  | TyOwn t | TyRef t | TyMut t -> gleam_type t
  | _ -> "Dynamic"

let ret_type = function None -> "Nil" | Some t -> gleam_type t

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id -> mangle id.name
  | ExprApp (callee, args) ->
      gen_expr callee ^ "(" ^ String.concat ", " (List.map gen_expr args) ^ ")"
  | ExprBinary (a, op, b) ->
      (* Gleam has separate Int/Float operators: +, +., -, -., *, *., /, /. *)
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
        | OpEq  -> "==" | OpNe -> "!="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "&&" | OpOr -> "||"
        | OpConcat -> "<>"
        | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr ->
            failwith "Gleam backend: bitwise ops need int_bitwise stdlib"
      in
      "{ " ^ gen_expr a ^ " " ^ s ^ " " ^ gen_expr b ^ " }"
  | ExprUnary (OpNeg, x) -> "{ 0 - " ^ gen_expr x ^ " }"
  | ExprUnary (OpNot, x) -> "!" ^ gen_expr x
  | ExprUnary _ -> "panic as \"Gleam backend: unsupported unary\""
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let c = gen_expr ei_cond in
      let t = gen_expr ei_then in
      let f = match ei_else with Some e -> gen_expr e | None -> "Nil" in
      Printf.sprintf "case %s { True -> %s False -> %s }" c t f
  | ExprLet { el_pat; el_value; el_body; _ } ->
      let var = match el_pat with PatVar id -> mangle id.name | _ -> "_" in
      let v = gen_expr el_value in
      let body = match el_body with Some e -> gen_expr e | None -> "Nil" in
      Printf.sprintf "{ let %s = %s\n  %s }" var v body
  | ExprBlock blk -> gen_block blk
  | ExprTuple es -> "#(" ^ String.concat ", " (List.map gen_expr es) ^ ")"
  | ExprTupleIndex (e, n) -> gen_expr e ^ "." ^ string_of_int n
  | ExprRecord { er_fields; _ } ->
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr e | None -> mangle id.name in
        Printf.sprintf "%s: %s" (mangle id.name) v
      ) er_fields in
      "(" ^ String.concat ", " fs ^ ")"  (* placeholder — real form needs ctor name *)
  | ExprField (record, field) -> gen_expr record ^ "." ^ mangle field.name
  | ExprVariant (_ty, ctor) -> ctor.name  (* Gleam variants keep TitleCase *)
  | ExprMatch { em_scrutinee; em_arms } ->
      let arms = List.map (fun arm ->
        Printf.sprintf "%s -> %s" (gen_pattern arm.ma_pat) (gen_expr arm.ma_body)
      ) em_arms in
      Printf.sprintf "case %s { %s }" (gen_expr em_scrutinee) (String.concat " " arms)
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> gen_expr e
  | _ -> "panic as \"Gleam backend: unsupported expression\""

and gen_pattern (p : pattern) : string =
  match p with
  | PatWildcard _ -> "_"
  | PatVar id -> mangle id.name
  | PatLit lit -> gen_lit lit
  | PatCon (id, args) ->
      if args = [] then id.name
      else id.name ^ "(" ^ String.concat ", " (List.map gen_pattern args) ^ ")"
  | PatTuple ps -> "#(" ^ String.concat ", " (List.map gen_pattern ps) ^ ")"
  | PatRecord _ -> "_"  (* Gleam record patterns need the constructor name *)
  | PatAs (id, _) -> mangle id.name
  | PatOr (p, _) -> gen_pattern p

and gen_lit = function
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _) -> "True"
  | LitBool (false, _) -> "False"
  | LitString (s, _) -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _) -> "\"" ^ Char.escaped c ^ "\""
  | LitUnit _ -> "Nil"

and gen_block (blk : block) : string =
  let stmts = List.map gen_stmt blk.blk_stmts in
  let tail = match blk.blk_expr with Some e -> gen_expr e | None -> "Nil" in
  "{\n  " ^ String.concat "\n  " stmts ^ "\n  " ^ tail ^ "\n}"

and gen_stmt = function
  | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
      Printf.sprintf "let %s = %s" (mangle id.name) (gen_expr sl_value)
  | StmtLet _ -> ""
  | StmtExpr e -> gen_expr e
  | _ -> ""

let gen_function (fd : fn_decl) : string =
  let name = mangle fd.fd_name.name in
  let params = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%s: %s" (mangle p.p_name.name) (gleam_type p.p_ty))
       fd.fd_params) in
  let ret = ret_type fd.fd_ret_ty in
  let body = match fd.fd_body with
    | FnExpr e -> gen_expr e
    | FnBlock b -> gen_block b
  in
  Printf.sprintf "pub fn %s(%s) -> %s {\n  %s\n}\n\n" name params ret body

let gen_type_decl (td : type_decl) : string =
  let name = td.td_name.name in
  match td.td_body with
  | TyAlias (TyRecord (fields, _)) ->
      (* Gleam's record-shape: `pub type Point { Point(x: Int, y: Int) }`. *)
      let fs = List.map (fun (rf : row_field) ->
        Printf.sprintf "%s: %s" (mangle rf.rf_name.name) (gleam_type rf.rf_ty)
      ) fields in
      Printf.sprintf "pub type %s { %s(%s) }\n\n" name name (String.concat ", " fs)
  | TyAlias t -> Printf.sprintf "pub type %s = %s\n\n" name (gleam_type t)
  | TyStruct fields ->
      let fs = List.map (fun (sf : struct_field) ->
        Printf.sprintf "%s: %s" (mangle sf.sf_name.name) (gleam_type sf.sf_ty)
      ) fields in
      Printf.sprintf "pub type %s { %s(%s) }\n\n" name name (String.concat ", " fs)
  | TyEnum variants ->
      let vs = List.map (fun (vd : variant_decl) ->
        let tys = List.map gleam_type vd.vd_fields in
        let body = if tys = [] then "" else "(" ^ String.concat ", " tys ^ ")" in
        Printf.sprintf "  %s%s" vd.vd_name.name body
      ) variants in
      Printf.sprintf "pub type %s {\n%s\n}\n\n" name (String.concat "\n" vs)

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

let codegen_gleam (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("Gleam codegen error: " ^ msg)
  | e           -> Error ("Gleam codegen error: " ^ Printexc.to_string e)
