(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** C Code Generator (MVP).

    Translates a typed AffineScript program to a single self-contained C99
    source file. The generated file links nothing beyond libc and a small
    inline runtime emitted at the top of every output, so the round-trip is

      affinescript compile foo.affine -o foo.c
      cc foo.c -o foo && ./foo

    Phase 1 (this file): functions, primitive arithmetic, control flow, let,
    string println, simple match. Tuples / records / variants / ownership are
    not lowered — they emit an explicit error stub so a regression is loud
    rather than silent.

    Compatibility: relies on GCC/Clang "statement expressions" ({ ... }) so
    block expressions can appear inside larger expressions. This is the same
    trade the WASM backend implicitly makes (its blocks lower to wasm blocks).
    Both gcc and clang accept it; tcc does too. msvc does not.
*)

open Ast

(* ============================================================================
   Code Generation Context
   ============================================================================ *)

type codegen_ctx = {
  output : Buffer.t;
  indent : int;
  symbols : Symbol.t;
  fwd_decls : Buffer.t;  (* Forward declarations, written before bodies. *)
}

let create_ctx symbols = {
  output = Buffer.create 1024;
  indent = 0;
  symbols;
  fwd_decls = Buffer.create 256;
}

let emit ctx str = Buffer.add_string ctx.output str

let emit_line ctx str =
  let spaces = String.make (ctx.indent * 4) ' ' in
  Buffer.add_string ctx.output spaces;
  Buffer.add_string ctx.output str;
  Buffer.add_char ctx.output '\n'

let increase_indent ctx = { ctx with indent = ctx.indent + 1 }
let decrease_indent ctx = { ctx with indent = max 0 (ctx.indent - 1) }

(* ============================================================================
   Runtime prelude

   Inlined into every output so generated code links against libc only.
   ============================================================================ *)

let prelude = {|/* ---- AffineScript C runtime (MVP) ---- */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef long      as_int_t;
typedef double    as_float_t;
typedef int       as_bool_t;
typedef const char *as_str_t;

static inline void print(as_str_t s)   { fputs(s, stdout); }
static inline void println(as_str_t s) { puts(s); }
/* ---- end runtime ---- */

|}

(* ============================================================================
   Identifier sanitisation
   ============================================================================ *)

let c_reserved = [
  "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do";
  "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if"; "inline";
  "int"; "long"; "register"; "restrict"; "return"; "short"; "signed";
  "sizeof"; "static"; "struct"; "switch"; "typedef"; "union"; "unsigned";
  "void"; "volatile"; "while"; "_Bool"; "_Complex"; "_Imaginary";
  (* runtime collisions *)
  "main"; "exit"; "abort"; "free"; "malloc"; "calloc"; "realloc";
  "printf"; "puts"; "fputs"; "stdin"; "stdout"; "stderr";
]

let mangle (name : string) : string =
  if List.mem name c_reserved then name ^ "_" else name

(* ============================================================================
   Type lowering

   AS type -> concrete C type. Anything unknown becomes `void *` so the
   generated code at least parses; the WASM backend remains the source of
   truth for full type-driven lowering.
   ============================================================================ *)

let rec c_type_of_ty (te : type_expr) : string =
  match te with
  | TyCon name when name.name = "Int"    -> "as_int_t"
  | TyCon name when name.name = "Float"  -> "as_float_t"
  | TyCon name when name.name = "Bool"   -> "as_bool_t"
  | TyCon name when name.name = "String" -> "as_str_t"
  | TyCon name when name.name = "Unit"   -> "void"
  | TyCon name                           -> mangle name.name  (* user-declared typedef *)
  | TyApp _                              -> "void *"
  | TyArrow (_, _, _, _)                 -> "void *"
  | TyTuple _ | TyRecord _               -> "void *"
  | TyOwn t | TyRef t | TyMut t          -> c_type_of_ty t
  | TyVar _ | TyHole                     -> "void *"

let c_type_of_ret = function
  | None    -> "void"
  | Some ty -> c_type_of_ty ty

(* ============================================================================
   Expression Code Generation

   Returned strings are valid C expressions. Statement-shaped constructs use
   GCC statement expressions: ({ stmt; stmt; expr; }).
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
        | OpAdd -> "+"  | OpSub -> "-"  | OpMul -> "*"  | OpDiv -> "/"
        | OpMod -> "%"
        | OpEq -> "=="  | OpNe -> "!="
        | OpLt -> "<"   | OpLe -> "<="  | OpGt -> ">"   | OpGe -> ">="
        | OpAnd -> "&&" | OpOr  -> "||"
        | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "^"
        | OpShl -> "<<" | OpShr -> ">>"
        | OpConcat ->
            (* C has no string-concat operator; defer to a runtime call.
               The runtime stub is intentionally absent so a use-site shows
               up at link time rather than producing wrong output. *)
            "@CONCAT@"
      in
      if op = OpConcat then
        Printf.sprintf "as_concat(%s, %s)" (gen_expr ctx e1) (gen_expr ctx e2)
      else
        "(" ^ gen_expr ctx e1 ^ " " ^ op_str ^ " " ^ gen_expr ctx e2 ^ ")"
  | ExprUnary (op, e) ->
      (match op with
       | OpNeg    -> "(-" ^ gen_expr ctx e ^ ")"
       | OpNot    -> "(!" ^ gen_expr ctx e ^ ")"
       | OpBitNot -> "(~" ^ gen_expr ctx e ^ ")"
       | OpRef    -> "(&" ^ gen_expr ctx e ^ ")"
       | OpDeref  -> "(*" ^ gen_expr ctx e ^ ")")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let cond_str = gen_expr ctx ei_cond in
      let then_str = gen_expr ctx ei_then in
      let else_str = match ei_else with
        | Some e -> gen_expr ctx e
        | None   -> "((void)0)"
      in
      "(" ^ cond_str ^ " ? " ^ then_str ^ " : " ^ else_str ^ ")"
  | ExprLet { el_pat; el_value; el_body; el_mut = _; el_quantity = _; el_ty } ->
      let var = match el_pat with
        | PatVar id   -> mangle id.name
        | PatWildcard _ -> "_unused"
        | _ -> "_unsupported_pat"
      in
      let ty_str = match el_ty with
        | Some t -> c_type_of_ty t
        | None   -> "long"  (* MVP: untyped binders default to long *)
      in
      let val_str = gen_expr ctx el_value in
      (match el_body with
       | Some body ->
           let body_str = gen_expr ctx body in
           Printf.sprintf "({ %s %s = %s; %s; })" ty_str var val_str body_str
       | None ->
           Printf.sprintf "({ %s %s = %s; (void)0; })" ty_str var val_str)
  | ExprBlock block -> gen_block_expr ctx block
  | ExprReturn (Some e) ->
      Printf.sprintf "({ return %s; })" (gen_expr ctx e)
  | ExprReturn None ->
      "({ return; })"
  | ExprMatch { em_scrutinee; em_arms } ->
      gen_match ctx em_scrutinee em_arms
  | ExprField (record, field) ->
      gen_expr ctx record ^ "." ^ mangle field.name
  | ExprTupleIndex (e, n) ->
      Printf.sprintf "(%s).f%d" (gen_expr ctx e) n
  | ExprIndex (arr, idx) ->
      Printf.sprintf "(%s)[%s]" (gen_expr ctx arr) (gen_expr ctx idx)
  | ExprSpan (inner, _) -> gen_expr ctx inner
  | ExprHandle { eh_body; eh_handlers = _ } -> gen_expr ctx eh_body
  | ExprResume (Some e) -> gen_expr ctx e
  | ExprResume None     -> "((void)0)"
  | ExprRecord { er_fields; _ } ->
      (* Emit just the designated-initializer brace block; the surrounding
         context (gen_let_with_hint) supplies the [(Type)] cast. *)
      let fs = List.map (fun (id, e_opt) ->
        let v = match e_opt with Some e -> gen_expr ctx e | None -> mangle id.name in
        Printf.sprintf ".%s = %s" (mangle id.name) v
      ) er_fields in
      "{ " ^ String.concat ", " fs ^ " }"
  | ExprVariant (_ty, ctor) -> mangle ctor.name  (* refs a generated constant or fn *)
  | ExprTuple _ | ExprArray _ | ExprLambda _ | ExprTry _
  | ExprRowRestrict _ | ExprUnsafe _ ->
      "(__as_unsupported_expr_for_c_backend())"

and gen_literal (lit : literal) : string =
  match lit with
  | LitInt (n, _)        -> "((as_int_t)" ^ string_of_int n ^ ")"
  | LitFloat (f, _)      ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _)    -> "1"
  | LitBool (false, _)   -> "0"
  | LitString (s, _)     -> "\"" ^ String.escaped s ^ "\""
  | LitChar (c, _)       -> "'" ^ Char.escaped c ^ "'"
  | LitUnit _            -> "((void)0)"

and gen_block_expr ctx block =
  (* Emit ({ stmt; stmt; expr; }) — GCC statement expression. *)
  let buf = Buffer.create 64 in
  List.iter (fun s ->
    Buffer.add_string buf (gen_stmt ctx s);
    Buffer.add_char buf ' '
  ) block.blk_stmts;
  let tail = match block.blk_expr with
    | Some e -> gen_expr ctx e ^ ";"
    | None   -> "((void)0);"
  in
  "({ " ^ Buffer.contents buf ^ tail ^ " })"

and gen_match ctx scrutinee arms =
  (* Lowered to a statement-expression: bind the scrutinee, then walk arms.
     Each arm becomes a guarded block that, on tag/literal match, binds
     pattern-variables from the union member and yields the body value. *)
  let scrut_str = gen_expr ctx scrutinee in
  let arm_strs = List.map (fun arm ->
    match arm.ma_pat with
    | PatWildcard _ | PatVar _ ->
        Printf.sprintf "{ __as_match_result = (%s); break; }" (gen_expr ctx arm.ma_body)
    | PatLit lit ->
        Printf.sprintf "if (__scrut == %s) { __as_match_result = (%s); break; }"
          (gen_literal lit) (gen_expr ctx arm.ma_body)
    | PatCon (id, args) ->
        let cond = Printf.sprintf "__scrut.tag == TAG_%s" (mangle id.name) in
        let bindings = List.mapi (fun i p ->
          match p with
          | PatVar pid ->
              Printf.sprintf "%s %s = __scrut.u.%s.f%d;"
                "long" (mangle pid.name) (mangle id.name) i
          | _ -> ""
        ) args |> String.concat " " in
        Printf.sprintf "if (%s) { %s __as_match_result = (%s); break; }"
          cond bindings (gen_expr ctx arm.ma_body)
    | _ ->
        Printf.sprintf "{ __as_match_result = (%s); break; }" (gen_expr ctx arm.ma_body)
  ) arms in
  Printf.sprintf
    "({ __typeof__(%s) __scrut = %s; long __as_match_result = 0; do { %s } while (0); __as_match_result; })"
    scrut_str scrut_str (String.concat " " arm_strs)

and gen_stmt ctx (stmt : stmt) : string =
  match stmt with
  | StmtLet { sl_pat; sl_value; sl_mut = _; sl_quantity = _; sl_ty } ->
      let var = match sl_pat with
        | PatVar id   -> mangle id.name
        | PatWildcard _ -> "_unused"
        | _ -> "_unsupported_pat"
      in
      let ty_str = match sl_ty with
        | Some t -> c_type_of_ty t
        | None   -> "long"
      in
      (* Record literals need a `(Type)` cast in C. When the let has a type
         annotation pointing at a typedef, prepend it so designated braces
         parse as a compound literal. *)
      let value_str =
        match sl_value, sl_ty with
        | ExprRecord _, Some (TyCon id) ->
            Printf.sprintf "(%s)%s" (mangle id.name) (gen_expr ctx sl_value)
        | _ -> gen_expr ctx sl_value
      in
      Printf.sprintf "%s %s = %s;" ty_str var value_str
  | StmtExpr e ->
      gen_expr ctx e ^ ";"
  | StmtAssign (lhs, op, rhs) ->
      let op_str = match op with
        | AssignEq  -> "="  | AssignAdd -> "+="
        | AssignSub -> "-=" | AssignMul -> "*="
        | AssignDiv -> "/="
      in
      Printf.sprintf "%s %s %s;" (gen_expr ctx lhs) op_str (gen_expr ctx rhs)
  | StmtWhile (cond, body) ->
      let body_strs = List.map (gen_stmt ctx) body.blk_stmts in
      let tail = match body.blk_expr with
        | Some e -> gen_expr ctx e ^ ";"
        | None   -> ""
      in
      Printf.sprintf "while (%s) { %s %s }"
        (gen_expr ctx cond) (String.concat " " body_strs) tail
  | StmtFor (_pat, _iter, _body) ->
      (* MVP: AS for-in over iterators has no direct C analogue. Emit a
         placeholder so the build link-fails cleanly. *)
      "{ __as_unsupported_for_loop(); }"

(* ============================================================================
   Top-Level Declaration Code Generation
   ============================================================================ *)

let gen_function ctx (fd : fn_decl) : unit =
  let name = mangle fd.fd_name.name in
  let ret_ty = c_type_of_ret fd.fd_ret_ty in
  let params = List.map (fun (p : param) ->
    Printf.sprintf "%s %s" (c_type_of_ty p.p_ty) (mangle p.p_name.name)
  ) fd.fd_params in
  let params_str =
    if params = [] then "void" else String.concat ", " params
  in
  let signature = Printf.sprintf "%s %s(%s)" ret_ty name params_str in

  (* Forward declaration so any-order calls work. *)
  Buffer.add_string ctx.fwd_decls (signature ^ ";\n");

  emit_line ctx (signature ^ " {");
  let inner = increase_indent ctx in
  (match fd.fd_body with
   | FnExpr body_expr ->
       if ret_ty = "void" then
         emit_line inner ((gen_expr inner body_expr) ^ ";")
       else
         emit_line inner ("return " ^ gen_expr inner body_expr ^ ";")
   | FnBlock block ->
       List.iter (fun s -> emit_line inner (gen_stmt inner s)) block.blk_stmts;
       (match block.blk_expr with
        | Some e ->
            if ret_ty = "void" then
              emit_line inner (gen_expr inner e ^ ";")
            else
              emit_line inner ("return " ^ gen_expr inner e ^ ";")
        | None -> ()));
  emit_line ctx "}";
  emit ctx "\n"

let emit_struct_decl ctx (name : string) (fields : (string * type_expr) list) : unit =
  let lines = List.map (fun (n, ty) ->
    Printf.sprintf "  %s %s;" (c_type_of_ty ty) (mangle n)) fields in
  emit_line ctx (Printf.sprintf "typedef struct {\n%s\n} %s;"
                   (String.concat "\n" lines) name);
  emit ctx "\n"

let emit_enum_decl ctx (name : string) (variants : variant_decl list) : unit =
  (* tag enum *)
  let tags = List.map (fun (vd : variant_decl) ->
    "TAG_" ^ mangle vd.vd_name.name) variants in
  emit_line ctx (Printf.sprintf "typedef enum { %s } %s_tag;"
                   (String.concat ", " tags) name);
  (* tagged union *)
  let union_members = List.map (fun (vd : variant_decl) ->
    let payload =
      if vd.vd_fields = [] then "char _unit;"
      else
        String.concat " "
          (List.mapi (fun i ty ->
            Printf.sprintf "%s f%d;" (c_type_of_ty ty) i) vd.vd_fields)
    in
    Printf.sprintf "    struct { %s } %s;" payload (mangle vd.vd_name.name)
  ) variants in
  emit_line ctx (Printf.sprintf "typedef struct {");
  emit_line ctx (Printf.sprintf "  %s_tag tag;" name);
  emit_line ctx (Printf.sprintf "  union {");
  List.iter (emit_line ctx) union_members;
  emit_line ctx (Printf.sprintf "  } u;");
  emit_line ctx (Printf.sprintf "} %s;" name);
  (* constructor functions / constants *)
  List.iter (fun (vd : variant_decl) ->
    let cname = mangle vd.vd_name.name in
    let arity = List.length vd.vd_fields in
    if arity = 0 then
      emit_line ctx
        (Printf.sprintf "static const %s %s = (%s){ .tag = TAG_%s };"
           name cname name cname)
    else begin
      let params = List.mapi (fun i ty ->
        Printf.sprintf "%s f%d" (c_type_of_ty ty) i) vd.vd_fields in
      let inits = List.mapi (fun i _ -> Printf.sprintf ".f%d = f%d" i i) vd.vd_fields in
      emit_line ctx
        (Printf.sprintf
           "static inline %s %s(%s) { return (%s){ .tag = TAG_%s, .u.%s = { %s } }; }"
           name cname (String.concat ", " params)
           name cname cname (String.concat ", " inits))
    end
  ) variants;
  emit ctx "\n"

let gen_type_decl_c ctx (td : type_decl) : unit =
  let name = mangle td.td_name.name in
  match td.td_body with
  | TyAlias (TyRecord (fields, _)) ->
      let pairs = List.map (fun (rf : row_field) -> (rf.rf_name.name, rf.rf_ty)) fields in
      emit_struct_decl ctx name pairs
  | TyAlias t ->
      emit_line ctx (Printf.sprintf "typedef %s %s;" (c_type_of_ty t) name)
  | TyStruct fields ->
      let pairs = List.map (fun (sf : struct_field) -> (sf.sf_name.name, sf.sf_ty)) fields in
      emit_struct_decl ctx name pairs
  | TyEnum variants ->
      emit_enum_decl ctx name variants

let gen_top_level ctx (top : top_level) : unit =
  match top with
  | TopFn fd     -> gen_function ctx fd
  | TopType td   -> gen_type_decl_c ctx td
  | TopConst { tc_name; tc_ty; tc_value; _ } ->
      emit_line ctx
        (Printf.sprintf "static const %s %s = %s;"
           (c_type_of_ty tc_ty)
           (mangle tc_name.name)
           (gen_expr ctx tc_value))
  | TopEffect _  -> emit_line ctx "/* effect declaration (erased) */"
  | TopTrait _   -> emit_line ctx "/* trait declaration (erased) */"
  | TopImpl _    -> emit_line ctx "/* impl block (erased) */"

(* ============================================================================
   Driver

   AffineScript's `main` returns Int but C's `main` returns `int`. If the
   program defines `main`, emit a C `main` that calls it and propagates the
   exit code. If `main` returns Unit, exit 0.
   ============================================================================ *)

let main_entry_for (program : program) : string =
  let main_fn = List.find_map (function
    | TopFn fd when fd.fd_name.name = "main" -> Some fd
    | _ -> None
  ) program.prog_decls in
  match main_fn with
  | None -> ""
  | Some fd ->
      let ret_ty = c_type_of_ret fd.fd_ret_ty in
      if ret_ty = "void" then
        "int main(void) { main_(); return 0; }\n"
      else
        Printf.sprintf "int main(void) { return (int)main_(); }\n"

let generate (program : program) (symbols : Symbol.t) : string =
  let ctx = create_ctx symbols in
  emit_line ctx "/* Generated by AffineScript compiler */";
  emit_line ctx "/* SPDX-License-Identifier: PMPL-1.0-or-later */";
  emit ctx prelude;

  (* Three-pass emission so forward declarations and body code see all
     types: (1) type decls (typedefs, tagged unions, ctor inlines) into
     types_buf; (2) function bodies into bodies_buf, accumulating fn
     forward decls into fwd_decls. Final layout is types → fwd → bodies. *)
  let types_buf  = Buffer.create 512 in
  let bodies_buf = Buffer.create 1024 in
  let types_ctx  = { ctx with output = types_buf } in
  let body_ctx   = { ctx with output = bodies_buf } in
  List.iter (function
    | TopType td -> gen_type_decl_c types_ctx td
    | _ -> ()
  ) program.prog_decls;
  List.iter (function
    | TopType _ -> ()
    | other -> gen_top_level body_ctx other
  ) program.prog_decls;

  Buffer.add_buffer ctx.output types_buf;
  Buffer.add_char ctx.output '\n';
  Buffer.add_buffer ctx.output ctx.fwd_decls;
  Buffer.add_char ctx.output '\n';
  Buffer.add_buffer ctx.output bodies_buf;
  Buffer.add_string ctx.output (main_entry_for program);

  Buffer.contents ctx.output

let codegen_c (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("C codegen error: " ^ msg)
  | e           -> Error ("C codegen error: " ^ Printexc.to_string e)
