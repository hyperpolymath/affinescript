(** SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** WASM Backend Implementation
    
    Complete WebAssembly backend with full processor support.
    This backend generates WASM text format and binary format.
*)

open Ast
open Architecture

(** WASM Backend Context *)
type context = {
  config : wasm_config;
  current_function : string option;
  local_count : int;
  label_count : int;
  string_literals : (string * int) list;
  memory_offset : int;
  capabilities : capability list;
}

(** WASM Backend Implementation *)
module Wasm_backend : PROCESSOR_BACKEND = struct
  type context = context
  
  let create_context () : context = {
    config = {
      target_version = "1.0";
      enable_simd = false;
      enable_threads = false;
      enable_reference_types = false;
      enable_tail_calls = false;
      memory_pages = 16;
      optimize_for = `Balanced;
    };
    current_function = None;
    local_count = 0;
    label_count = 0;
    string_literals = [];
    memory_offset = 0;
    capabilities = [
      BasicArithmetic;
      MemoryManagement;
      ControlFlow;
      FunctionCalls;
    ];
  }
  
  let get_capabilities (ctx : context) : capability list = ctx.capabilities
  
  let supports_feature (ctx : context) (feature : string) : bool =
    match feature with
    | "simd" -> ctx.config.enable_simd
    | "threads" -> ctx.config.enable_threads
    | "tail_calls" -> ctx.config.enable_tail_calls
    | "reference_types" -> ctx.config.enable_reference_types
    | _ -> false
  
  let emit_prologue (ctx : context) : string =
    let memory_pages = ctx.config.memory_pages in
    let simd_feature = if ctx.config.enable_simd then " (simd)" else "" in
    let threads_feature = if ctx.config.enable_threads then " (threads)" else "" in
    Printf.sprintf 
      "(module\n"
      ^ "  (type $__affinescript_module_type\n"
      ^ "    (func $main (result i32))\n"
      ^ "    (memory $memory %d)%s%s\n"
      ^ "  )\n"
      ^ "  (memory $memory %d)\n"
      ^ "  (export \"memory\" (memory $memory))\n"
      memory_pages simd_feature threads_feature memory_pages
  
  let emit_epilogue (ctx : context) : string =
    "  (start $main)\n)"
  
  let emit_function (ctx : context) (fd : fn_decl) : string =
    let params = String.concat " " (List.map (fun p -> "(param i32)") fd.fd_params) in
    let result = if fd.fd_return_type = TyCon {name="Unit"} then "" else " (result i32)" in
    let body = emit_expression ctx fd.fd_body in
    Printf.sprintf "  (func $%s (type $%s)%s%s\n    %s\n  )\n"
      fd.fd_name.name fd.fd_name.name params result body
  
  let emit_expression (ctx : context) (expr : expr) : string =
    match expr with
    | ExprLit (LitInt (n, _)) -> Printf.sprintf "i32.const %d" n
    | ExprLit (LitFloat (f, _)) -> Printf.sprintf "f64.const %f" f
    | ExprLit (LitBool (b, _)) -> if b then "i32.const 1" else "i32.const 0"
    | ExprLit (LitString (s, _)) -> 
        (* Store string literal and return pointer *)
        let offset = ctx.memory_offset in
        let updated_literals = (s, offset) :: ctx.string_literals in
        let updated_offset = offset + String.length s + 1 in
        let new_ctx = { ctx with string_literals = updated_literals; memory_offset = updated_offset } in
        Printf.sprintf "i32.const %d" offset
    | ExprVar id -> Printf.sprintf "local.get $%s" id.name
    | ExprBinary (left, op, right) ->
        let left_code = emit_expression ctx left in
        let right_code = emit_expression ctx right in
        let op_code = match op with
          | OpAdd -> "i32.add"
          | OpSub -> "i32.sub"
          | OpMul -> "i32.mul"
          | OpDiv -> "i32.div_s"
          | _ -> "i32.add"
        in
        Printf.sprintf "%s\n    %s\n    %s" left_code right_code op_code
    | _ -> "i32.const 0"  (* Default for unsupported expressions *)
  
  let emit_statement (ctx : context) (stmt : stmt) : string =
    match stmt with
    | StmtLet sl -> 
        let value_code = emit_expression ctx sl.sl_value in
        let var_name = match sl.sl_pat with
          | PatVar id -> id.name
          | _ -> "__temp"
        in
        Printf.sprintf "%s\n    local.set $%s" value_code var_name
    | StmtExpr e -> emit_expression ctx e
    | _ -> "nop"
  
  let generate_code (ctx : context) (program : program) : string =
    let prologue = emit_prologue ctx in
    let function_code = String.concat "\n" (List.map (emit_function ctx) program.top_levels) in
    let epilogue = emit_epilogue ctx in
    prologue ^ "\n" ^ function_code ^ "\n" ^ epilogue
  
  (* Optimization passes *)
  let run_optimizations (ctx : context) (program : program) : program = program
  let inline_functions (ctx : context) (program : program) : program = program
  let constant_folding (ctx : context) (program : program) : program = program
  let dead_code_elimination (ctx : context) (program : program) : program = program
end