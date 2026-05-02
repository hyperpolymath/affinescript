(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Lua 5.x Emitter (MVP).

    Lowers a subset of AffineScript to Lua source. Numbers are unified as
    Lua's native [number] type — Int and Float both map to it. *)

open Ast

let lua_reserved = [
  "and"; "break"; "do"; "else"; "elseif"; "end"; "false"; "for";
  "function"; "goto"; "if"; "in"; "local"; "nil"; "not"; "or"; "repeat";
  "return"; "then"; "true"; "until"; "while";
]

let mangle s = if List.mem s lua_reserved then s ^ "_" else s

let prelude = {|-- AffineScript Lua runtime (MVP)
local function println(s) print(tostring(s)) end
local function as_print(s) io.write(tostring(s)) end
local Some = function(v) return { tag = "Some", value = v } end
local None = { tag = "None" }
local Ok   = function(v) return { tag = "Ok",  value = v } end
local Err  = function(e) return { tag = "Err", error = e } end

|}

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id -> mangle id.name
  | ExprApp (callee, args) ->
      gen_expr callee ^ "(" ^ String.concat ", " (List.map gen_expr args) ^ ")"
  | ExprBinary (a, op, b) ->
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
        | OpEq  -> "==" | OpNe -> "~="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "and" | OpOr -> "or"
        | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "~"  (* Lua 5.3+ *)
        | OpShl -> "<<" | OpShr -> ">>"
        | OpConcat -> ".."
      in
      "(" ^ gen_expr a ^ " " ^ s ^ " " ^ gen_expr b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg -> "(-" ^ gen_expr x ^ ")"
       | OpNot -> "(not " ^ gen_expr x ^ ")"
       | OpBitNot -> "(~" ^ gen_expr x ^ ")"
       | OpRef -> gen_expr x
       | OpDeref -> gen_expr x)
  | ExprIf { ei_cond; ei_then; ei_else } ->
      (* Lua has no expression-form if; emit a do-block-as-expression via
         IIFE. *)
      let c = gen_expr ei_cond in
      let t = gen_expr ei_then in
      let f = match ei_else with Some e -> gen_expr e | None -> "nil" in
      Printf.sprintf "((function() if %s then return %s else return %s end end)())" c t f
  | ExprLet { el_pat; el_value; el_body; _ } ->
      let var = match el_pat with PatVar id -> mangle id.name | _ -> "_" in
      let v = gen_expr el_value in
      let body = match el_body with Some e -> gen_expr e | None -> "nil" in
      Printf.sprintf "((function() local %s = %s; return %s end)())" var v body
  | ExprBlock blk -> gen_block_expr blk
  | ExprTuple es -> "{" ^ String.concat ", " (List.map gen_expr es) ^ "}"
  | ExprTupleIndex (e, n) ->
      (* Lua tables are 1-indexed; AS tuple indices are 0-based. *)
      Printf.sprintf "%s[%d]" (gen_expr e) (n + 1)
  | ExprArray es -> "{" ^ String.concat ", " (List.map gen_expr es) ^ "}"
  | ExprIndex (a, i) -> gen_expr a ^ "[" ^ gen_expr i ^ "]"
  | ExprRecord { er_fields; _ } ->
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr e | None -> mangle id.name in
        Printf.sprintf "%s = %s" (mangle id.name) v
      ) er_fields in
      "{ " ^ String.concat ", " fs ^ " }"
  | ExprField (record, field) ->
      gen_expr record ^ "." ^ mangle field.name
  | ExprVariant (_ty, ctor) -> mangle ctor.name
  | ExprMatch { em_scrutinee; em_arms } ->
      let scrut_str = gen_expr em_scrutinee in
      let rec arms = function
        | [] -> "error(\"non-exhaustive match\")"
        | arm :: rest ->
            let cond = gen_pattern_test "__scrut" arm.ma_pat in
            let bindings = gen_pattern_bindings "__scrut" arm.ma_pat in
            let body = gen_expr arm.ma_body in
            Printf.sprintf "if %s then %s return %s else %s end"
              cond bindings body (arms rest)
      in
      Printf.sprintf "((function(__scrut) %s end)(%s))" (arms em_arms) scrut_str
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> gen_expr e
  | _ -> "error(\"Lua backend: unsupported expression\")"

and gen_pattern_test scrut pat =
  match pat with
  | PatWildcard _ | PatVar _ -> "true"
  | PatLit lit -> Printf.sprintf "%s == %s" scrut (gen_lit lit)
  | PatCon (id, _) -> Printf.sprintf "%s.tag == %S" scrut id.name
  | PatTuple _ -> "true"  (* arity match by structure, not tag *)
  | PatRecord _ -> "true"
  | PatAs (_, p) -> gen_pattern_test scrut p
  | PatOr (p, _) -> gen_pattern_test scrut p

and gen_pattern_bindings scrut pat =
  let buf = Buffer.create 64 in
  let rec walk path = function
    | PatWildcard _ | PatLit _ -> ()
    | PatVar id ->
        Buffer.add_string buf
          (Printf.sprintf "local %s = %s; " (mangle id.name) path)
    | PatTuple ps ->
        List.iteri (fun i p -> walk (Printf.sprintf "%s[%d]" path (i + 1)) p) ps
    | PatRecord (fields, _) ->
        List.iter (fun (id, sub) ->
          let p = path ^ "." ^ mangle id.name in
          match sub with
          | None -> Buffer.add_string buf (Printf.sprintf "local %s = %s; " (mangle id.name) p)
          | Some sub -> walk p sub
        ) fields
    | PatCon (_, args) ->
        (match args with
         | [] -> ()
         | [single] -> walk (path ^ ".value") single
         | many ->
             List.iteri (fun i p ->
               walk (Printf.sprintf "%s.values[%d]" path (i + 1)) p
             ) many)
    | PatAs (id, sub) ->
        Buffer.add_string buf (Printf.sprintf "local %s = %s; " (mangle id.name) path);
        walk path sub
    | PatOr (p, _) -> walk path p
  in
  walk scrut pat;
  Buffer.contents buf

and gen_lit = function
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _) -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _) -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _) -> "\"" ^ Char.escaped c ^ "\""
  | LitUnit _ -> "nil"

and gen_block_expr (blk : block) : string =
  let stmts = String.concat " " (List.map gen_stmt blk.blk_stmts) in
  let tail = match blk.blk_expr with
    | Some e -> "return " ^ gen_expr e
    | None   -> "return nil"
  in
  Printf.sprintf "((function() %s %s end)())" stmts tail

and gen_stmt = function
  | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
      Printf.sprintf "local %s = %s;" (mangle id.name) (gen_expr sl_value)
  | StmtLet _ -> ""
  | StmtExpr e -> gen_expr e ^ ";"
  | StmtAssign (lhs, _, rhs) ->
      Printf.sprintf "%s = %s;" (gen_expr lhs) (gen_expr rhs)
  | StmtWhile (cond, body) ->
      Printf.sprintf "while %s do %s end"
        (gen_expr cond)
        (String.concat " " (List.map gen_stmt body.blk_stmts))
  | StmtFor _ -> ""

let gen_function (fd : fn_decl) : string =
  let name = mangle fd.fd_name.name in
  let params = String.concat ", "
    (List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params) in
  let body = match fd.fd_body with
    | FnExpr e -> "return " ^ gen_expr e
    | FnBlock b ->
        let stmts = String.concat " " (List.map gen_stmt b.blk_stmts) in
        let tail = match b.blk_expr with
          | Some e -> "return " ^ gen_expr e
          | None   -> ""
        in
        stmts ^ " " ^ tail
  in
  Printf.sprintf "function %s(%s)\n  %s\nend\n" name params body

let gen_type_decl (td : type_decl) : string =
  match td.td_body with
  | TyEnum variants ->
      let buf = Buffer.create 64 in
      List.iter (fun (vd : variant_decl) ->
        let arity = List.length vd.vd_fields in
        let name = mangle vd.vd_name.name in
        if arity = 0 then
          Buffer.add_string buf (Printf.sprintf "%s = { tag = %S }\n" name vd.vd_name.name)
        else if arity = 1 then
          Buffer.add_string buf
            (Printf.sprintf "%s = function(v) return { tag = %S, value = v } end\n"
               name vd.vd_name.name)
        else
          let ps = List.init arity (fun i -> "v" ^ string_of_int i) in
          Buffer.add_string buf
            (Printf.sprintf "%s = function(%s) return { tag = %S, values = {%s} } end\n"
               name (String.concat ", " ps) vd.vd_name.name (String.concat ", " ps))
      ) variants;
      Buffer.add_char buf '\n';
      Buffer.contents buf
  | _ -> ""  (* records erased — Lua tables are duck-typed *)

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "-- Generated by AffineScript compiler\n";
  Buffer.add_string buf "-- SPDX-License-Identifier: PMPL-1.0-or-later\n";
  Buffer.add_string buf prelude;
  List.iter (function
    | TopType td -> Buffer.add_string buf (gen_type_decl td)
    | _ -> ()
  ) program.prog_decls;
  List.iter (function
    | TopFn fd -> Buffer.add_string buf (gen_function fd); Buffer.add_char buf '\n'
    | _ -> ()
  ) program.prog_decls;
  let has_main = List.exists (function
    | TopFn fd -> fd.fd_name.name = "main"
    | _ -> false) program.prog_decls in
  if has_main then Buffer.add_string buf "print(main())\n";
  Buffer.contents buf

let codegen_lua (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("Lua codegen error: " ^ msg)
  | e           -> Error ("Lua codegen error: " ^ Printexc.to_string e)
