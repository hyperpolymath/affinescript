(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Rust emitter (MVP).

    Lowers a subset of AffineScript to safe Rust. AS's affine semantics map
    naturally to Rust's ownership system, but in MVP we don't lean on that
    yet — every Float/Int/Bool is by-value (Copy), strings are [&'static str].

    This is enough to run [add(40, 2) = 42]-style programs and validate via
    [rustc --edition=2021]. *)

open Ast

let rust_reserved = [
  "as"; "async"; "await"; "box"; "break"; "const"; "continue"; "crate";
  "do"; "dyn"; "else"; "enum"; "extern"; "false"; "final"; "fn"; "for";
  "if"; "impl"; "in"; "let"; "loop"; "macro"; "match"; "mod"; "move";
  "mut"; "override"; "priv"; "pub"; "ref"; "return"; "self"; "Self";
  "static"; "struct"; "super"; "trait"; "true"; "try"; "type"; "typeof";
  "union"; "unsafe"; "unsized"; "use"; "virtual"; "where"; "while"; "yield";
]

let mangle s = if List.mem s rust_reserved then "r#" ^ s else s

let rec rust_type = function
  | TyCon id when id.name = "Int"    -> "i64"
  | TyCon id when id.name = "Float"  -> "f64"
  | TyCon id when id.name = "Bool"   -> "bool"
  | TyCon id when id.name = "String" -> "&'static str"
  | TyCon id when id.name = "Unit"   -> "()"
  | TyCon id -> mangle id.name
  | TyTuple [] -> "()"
  | TyTuple ts -> "(" ^ String.concat ", " (List.map rust_type ts) ^ ")"
  | TyOwn t | TyRef t | TyMut t -> rust_type t
  | _ -> "()"

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id -> mangle id.name
  | ExprApp (callee, args) ->
      gen_expr callee ^ "(" ^ String.concat ", " (List.map gen_expr args) ^ ")"
  | ExprBinary (a, op, b) ->
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
        | OpEq  -> "==" | OpNe -> "!="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "&&" | OpOr -> "||"
        | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "^"
        | OpShl -> "<<" | OpShr -> ">>"
        | OpConcat -> "+"  (* approximate *)
      in
      "(" ^ gen_expr a ^ " " ^ s ^ " " ^ gen_expr b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg -> "(-" ^ gen_expr x ^ ")"
       | OpNot -> "(!" ^ gen_expr x ^ ")"
       | OpBitNot -> "(!" ^ gen_expr x ^ ")"
       | OpRef -> "(&" ^ gen_expr x ^ ")"
       | OpDeref -> "(*" ^ gen_expr x ^ ")")
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
  | ExprTupleIndex (e, n) -> Printf.sprintf "%s.%d" (gen_expr e) n
  | ExprArray es -> "[" ^ String.concat ", " (List.map gen_expr es) ^ "]"
  | ExprIndex (a, i) ->
      Printf.sprintf "%s[%s as usize]" (gen_expr a) (gen_expr i)
  | ExprRecord { er_fields; er_spread = _ } ->
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr e | None -> mangle id.name in
        Printf.sprintf "%s: %s" (mangle id.name) v
      ) er_fields in
      "{ " ^ String.concat ", " fs ^ " }"
      (* Rust requires a struct name on the literal. The TopType decl emits
         a struct; for body-position records we wrap with `Self {...}` only
         inside an `impl`, which we don't use. The bare brace-form is enough
         when the destination is a typed `let` whose type is known by Rust. *)
  | ExprField (record, field) -> gen_expr record ^ "." ^ mangle field.name
  | ExprVariant (ty, ctor) ->
      Printf.sprintf "%s::%s" (mangle ty.name) (mangle ctor.name)
  | ExprMatch { em_scrutinee; em_arms } ->
      let scrut = gen_expr em_scrutinee in
      let arms = List.map (fun arm ->
        let pat = gen_pattern arm.ma_pat in
        let body = gen_expr arm.ma_body in
        Printf.sprintf "%s => %s," pat body
      ) em_arms in
      Printf.sprintf "match %s { %s }" scrut (String.concat " " arms)
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> "return " ^ gen_expr e
  | ExprReturn None -> "return"
  | _ -> "unimplemented!(\"Rust backend: unsupported expression\")"

and gen_pattern (p : pattern) : string =
  match p with
  | PatWildcard _ -> "_"
  | PatVar id -> mangle id.name
  | PatLit lit -> gen_lit lit
  | PatCon (id, args) ->
      let arg_pats = List.map gen_pattern args in
      if arg_pats = [] then mangle id.name
      else mangle id.name ^ "(" ^ String.concat ", " arg_pats ^ ")"
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
  | LitInt (n, _) -> Printf.sprintf "%di64" n
  | LitFloat (f, _) ->
      let s = string_of_float f in
      let s = if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s in
      s ^ "f64"
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
  | StmtLet { sl_pat = PatVar id; sl_value; sl_mut; sl_ty; _ } ->
      let kw = if sl_mut then "let mut" else "let" in
      (* Rust struct literals need a name; if the source said `let p: T = { ... }`,
         lift T onto the record literal so it parses. *)
      let v = gen_value_with_hint sl_ty sl_value in
      Printf.sprintf "%s %s = %s;" kw (mangle id.name) v
  | StmtLet _ -> ""
  | StmtExpr e -> gen_expr e ^ ";"
  | StmtAssign (lhs, op, rhs) ->
      let s = match op with
        | AssignEq -> "=" | AssignAdd -> "+=" | AssignSub -> "-="
        | AssignMul -> "*=" | AssignDiv -> "/=" in
      Printf.sprintf "%s %s %s;" (gen_expr lhs) s (gen_expr rhs)
  | StmtWhile (cond, body) ->
      Printf.sprintf "while %s %s" (gen_expr cond) (gen_block body)
  | StmtFor _ -> ""

(* When we know the destination type for a record literal (from a let
   annotation), prepend the struct name so Rust's parser accepts the
   `Name { ... }` form. *)
and gen_value_with_hint (ty_hint : type_expr option) (e : expr) : string =
  match e, ty_hint with
  | ExprRecord _, Some (TyCon id) ->
      mangle id.name ^ " " ^ gen_expr e
  | _ -> gen_expr e

(* Rust requires [fn main()] to return [()] (or Termination). If the AS
   source defines a [main], we rename it to [__as_main] and emit a real
   Rust [main] that propagates its result as an exit code. *)
let gen_function ?(rename_to = None) (fd : fn_decl) : string =
  let name = match rename_to with
    | Some n -> n
    | None   -> mangle fd.fd_name.name
  in
  let params = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%s: %s" (mangle p.p_name.name) (rust_type p.p_ty))
       fd.fd_params) in
  let ret = match fd.fd_ret_ty with
    | None -> ""
    | Some t -> " -> " ^ rust_type t
  in
  let body = match fd.fd_body with
    | FnExpr e -> gen_expr e
    | FnBlock b -> gen_block b
  in
  Printf.sprintf "fn %s(%s)%s %s\n\n" name params ret body

(* Heuristic: a struct can derive Copy if every field's Rust type is Copy.
   Our scalar map (i64/f64/bool, plain tuples of those) is uniformly Copy;
   referenced types (`void *` fallback) are also Copy at the type level.
   AS's affine semantics permit moves through function calls in current
   surface code; deriving Copy here keeps the Rust output usable without
   forcing every record into clone-ceremony. *)
let rec is_copyable = function
  | TyCon id when id.name = "Int" || id.name = "Float" || id.name = "Bool"
              || id.name = "Char" || id.name = "Unit" -> true
  | TyTuple ts -> List.for_all is_copyable ts
  | TyOwn t | TyRef t | TyMut t -> is_copyable t
  | _ -> false

let emit_struct (name : string) (fields : (string * type_expr) list) : string =
  let derives =
    if List.for_all (fun (_, t) -> is_copyable t) fields
    then "#[derive(Copy, Clone, Debug)]"
    else "#[derive(Clone, Debug)]"
  in
  let fs = List.map (fun (n, ty) ->
    Printf.sprintf "  pub %s: %s," (mangle n) (rust_type ty)) fields in
  Printf.sprintf "%s\nstruct %s {\n%s\n}\n\n"
    derives name (String.concat "\n" fs)

let gen_type_decl (td : type_decl) : string =
  let name = mangle td.td_name.name in
  match td.td_body with
  (* `type Foo = { ... }` parses as TyAlias-of-TyRecord. We treat it as a
     nominal struct rather than a literal type alias because Rust requires
     a name for record-shaped types. *)
  | TyAlias (TyRecord (fields, _)) ->
      let pairs = List.map (fun (rf : row_field) -> (rf.rf_name.name, rf.rf_ty)) fields in
      emit_struct name pairs
  | TyAlias t -> Printf.sprintf "type %s = %s;\n\n" name (rust_type t)
  | TyStruct fields ->
      let pairs = List.map (fun (sf : struct_field) -> (sf.sf_name.name, sf.sf_ty)) fields in
      emit_struct name pairs
  | TyEnum variants ->
      let vs = List.map (fun (vd : variant_decl) ->
        let tys = List.map rust_type vd.vd_fields in
        let body = if tys = [] then "" else "(" ^ String.concat ", " tys ^ ")" in
        Printf.sprintf "  %s%s," (mangle vd.vd_name.name) body
      ) variants in
      Printf.sprintf "#[derive(Clone, Debug)]\nenum %s {\n%s\n}\n\n"
        name (String.concat "\n" vs)

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "// Generated by AffineScript compiler\n";
  Buffer.add_string buf "// SPDX-License-Identifier: PMPL-1.0-or-later\n";
  Buffer.add_string buf "#![allow(unused, dead_code, non_snake_case, unused_parens, non_camel_case_types)]\n\n";
  (* Type decls come first so functions can reference them. *)
  List.iter (function
    | TopType td -> Buffer.add_string buf (gen_type_decl td)
    | _ -> ()
  ) program.prog_decls;
  (* `use <Enum>::*` opens variant constructors so user code can write
     [Circle(5)] instead of [Shape::Circle(5)] — matches AffineScript's
     surface where variant names are resolved without the type prefix. *)
  List.iter (function
    | TopType { td_name; td_body = TyEnum _; _ } ->
        Buffer.add_string buf
          (Printf.sprintf "use %s::*;\n" (mangle td_name.name))
    | _ -> ()
  ) program.prog_decls;
  Buffer.add_char buf '\n';
  let has_main = ref false in
  List.iter (function
    | TopFn fd when fd.fd_name.name = "main" ->
        has_main := true;
        Buffer.add_string buf (gen_function ~rename_to:(Some "__as_main") fd)
    | TopFn fd ->
        Buffer.add_string buf (gen_function fd)
    | _ -> ()
  ) program.prog_decls;
  if !has_main then
    Buffer.add_string buf "fn main() { std::process::exit(__as_main() as i32) }\n";
  Buffer.contents buf

let codegen_rust (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("Rust codegen error: " ^ msg)
  | e           -> Error ("Rust codegen error: " ^ Printexc.to_string e)
