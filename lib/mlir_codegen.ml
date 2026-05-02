(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** MLIR text emitter (MVP, [func]/[arith] dialects).

    Lowers Int/Float scalar functions to MLIR's [func.func] +
    [arith.{addi,subi,muli,addf,subf,mulf,...}]. Output is consumable by
    [mlir-opt] and downstream pipelines (StableHLO, IREE, the Linalg
    stack).

    Scope: same as LLVM IR — Int (i64) and Float (f64) only, single-fn
    bodies with arithmetic + branches. Tensor lowering is Phase 2. *)

open Ast

exception Mlir_unsupported of string
let unsupported m = raise (Mlir_unsupported m)

type fstate = {
  body              : Buffer.t;
  mutable next_ssa  : int;
  mutable env       : (string * (string * string)) list;  (* var -> (ssa, ty) *)
}

let new_fstate () = { body = Buffer.create 256; next_ssa = 0; env = [] }
let fresh st = let n = st.next_ssa in st.next_ssa <- n + 1; Printf.sprintf "%%%d" n
let emit st s = Buffer.add_string st.body s; Buffer.add_char st.body '\n'
let bind st name ssa ty = st.env <- (name, (ssa, ty)) :: st.env
let lookup st name =
  try List.assoc name st.env
  with Not_found -> unsupported ("unbound: " ^ name)

let mlir_type = function
  | TyCon id when id.name = "Int"   -> "i64"
  | TyCon id when id.name = "Float" -> "f64"
  | TyCon id when id.name = "Bool"  -> "i1"
  | _ -> unsupported "type not supported in MLIR backend"

let ret_type = function None -> "()" | Some t -> mlir_type t

let rec gen_expr (st : fstate) (e : expr) : string * string =
  match e with
  | ExprLit (LitInt (n, _)) ->
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = arith.constant %d : i64" dst n);
      (dst, "i64")
  | ExprLit (LitFloat (f, _)) ->
      let s = string_of_float f in
      let s = if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s in
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = arith.constant %s : f64" dst s);
      (dst, "f64")
  | ExprLit (LitBool (b, _)) ->
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = arith.constant %d : i1" dst (if b then 1 else 0));
      (dst, "i1")
  | ExprLit _ -> unsupported "non-numeric literal"
  | ExprVar id -> lookup st id.name
  | ExprBinary (a, op, b) ->
      let (av, ty) = gen_expr st a in
      let (bv, _)  = gen_expr st b in
      let opname = match op, ty with
        | OpAdd, "i64" -> "arith.addi" | OpAdd, "f64" -> "arith.addf"
        | OpSub, "i64" -> "arith.subi" | OpSub, "f64" -> "arith.subf"
        | OpMul, "i64" -> "arith.muli" | OpMul, "f64" -> "arith.mulf"
        | OpDiv, "i64" -> "arith.divsi"| OpDiv, "f64" -> "arith.divf"
        | OpMod, "i64" -> "arith.remsi"| OpMod, "f64" -> "arith.remf"
        | OpBitAnd, _  -> "arith.andi"
        | OpBitOr,  _  -> "arith.ori"
        | OpBitXor, _  -> "arith.xori"
        | OpShl,    _  -> "arith.shli"
        | OpShr,    _  -> "arith.shrsi"
        | _ -> unsupported "binop / comparison needs different lowering"
      in
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = %s %s, %s : %s" dst opname av bv ty);
      (dst, ty)
  | ExprUnary (OpNeg, x) ->
      let (xv, ty) = gen_expr st x in
      let dst = fresh st in
      (match ty with
       | "i64" ->
           let zero = fresh st in
           emit st (Printf.sprintf "  %s = arith.constant 0 : i64" zero);
           emit st (Printf.sprintf "  %s = arith.subi %s, %s : i64" dst zero xv)
       | "f64" ->
           emit st (Printf.sprintf "  %s = arith.negf %s : f64" dst xv)
       | _ -> unsupported "negate on non-numeric");
      (dst, ty)
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let (cv, _) = gen_cmp st ei_cond in
      let (tv, ty) = gen_expr st ei_then in
      let (ev, _) = match ei_else with
        | Some e -> gen_expr st e
        | None -> unsupported "if without else has no value"
      in
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = arith.select %s, %s, %s : %s" dst cv tv ev ty);
      (dst, ty)
  | ExprBlock blk ->
      List.iter (fun s ->
        match s with
        | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
            let (v, ty) = gen_expr st sl_value in bind st id.name v ty
        | StmtExpr e -> ignore (gen_expr st e)
        | _ -> unsupported "stmt form not supported in MLIR block"
      ) blk.blk_stmts;
      (match blk.blk_expr with
       | Some e -> gen_expr st e
       | None -> unsupported "block must end with an expression")
  | ExprLet { el_pat = PatVar id; el_value; el_body = Some body; _ } ->
      let (v, ty) = gen_expr st el_value in
      bind st id.name v ty;
      gen_expr st body
  | ExprApp (callee, args) ->
      let name = match callee with
        | ExprVar id -> id.name
        | _ -> unsupported "indirect call"
      in
      let arg_pairs = List.map (gen_expr st) args in
      let arg_str = String.concat ", " (List.map fst arg_pairs) in
      let arg_tys = String.concat ", " (List.map snd arg_pairs) in
      let dst = fresh st in
      let ret_ty = "i64" in  (* MVP: conservative *)
      emit st (Printf.sprintf "  %s = func.call @%s(%s) : (%s) -> %s"
                 dst name arg_str arg_tys ret_ty);
      (dst, ret_ty)
  | ExprSpan (inner, _) -> gen_expr st inner
  | _ -> unsupported "expression form not supported in MLIR backend"

and gen_cmp (st : fstate) (e : expr) : string * string =
  match e with
  | ExprBinary (a, (OpEq|OpNe|OpLt|OpLe|OpGt|OpGe as op), b) ->
      let (av, ty) = gen_expr st a in
      let (bv, _)  = gen_expr st b in
      let pred, opname = match op, ty with
        | OpEq, "i64" -> "eq",  "arith.cmpi"  | OpNe, "i64" -> "ne",  "arith.cmpi"
        | OpLt, "i64" -> "slt", "arith.cmpi"  | OpLe, "i64" -> "sle", "arith.cmpi"
        | OpGt, "i64" -> "sgt", "arith.cmpi"  | OpGe, "i64" -> "sge", "arith.cmpi"
        | OpEq, "f64" -> "oeq", "arith.cmpf"  | OpNe, "f64" -> "one", "arith.cmpf"
        | OpLt, "f64" -> "olt", "arith.cmpf"  | OpLe, "f64" -> "ole", "arith.cmpf"
        | OpGt, "f64" -> "ogt", "arith.cmpf"  | OpGe, "f64" -> "oge", "arith.cmpf"
        | _ -> unsupported "comparison on non-numeric"
      in
      let dst = fresh st in
      emit st (Printf.sprintf "  %s = %s %s, %s, %s : %s" dst opname pred av bv ty);
      (dst, "i1")
  | _ -> gen_expr st e

let gen_function (buf : Buffer.t) (fd : fn_decl) : unit =
  let st = new_fstate () in
  List.iter (fun (p : param) ->
    let ty = mlir_type p.p_ty in
    bind st p.p_name.name ("%" ^ p.p_name.name) ty;
    st.next_ssa <- max st.next_ssa 0
  ) fd.fd_params;
  let body_expr = match fd.fd_body with
    | FnExpr e -> e
    | FnBlock b -> ExprBlock b
  in
  let ret_ty = ret_type fd.fd_ret_ty in
  let (rv, _) = gen_expr st body_expr in
  if ret_ty = "()" then emit st "  func.return"
  else emit st (Printf.sprintf "  func.return %s : %s" rv ret_ty);
  let params_str = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%%%s: %s" p.p_name.name (mlir_type p.p_ty))
       fd.fd_params) in
  Buffer.add_string buf
    (Printf.sprintf "func.func @%s(%s) -> %s {\n" fd.fd_name.name params_str ret_ty);
  Buffer.add_buffer buf st.body;
  Buffer.add_string buf "}\n\n"

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "// Generated by AffineScript compiler (MLIR func/arith)\n";
  Buffer.add_string buf "// SPDX-License-Identifier: PMPL-1.0-or-later\n";
  Buffer.add_string buf "module {\n";
  List.iter (function
    | TopFn fd -> gen_function buf fd
    | _ -> ()
  ) program.prog_decls;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let codegen_mlir (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Mlir_unsupported m -> Error ("MLIR backend: " ^ m)
  | Failure m          -> Error ("MLIR codegen error: " ^ m)
  | e                  -> Error ("MLIR codegen error: " ^ Printexc.to_string e)
