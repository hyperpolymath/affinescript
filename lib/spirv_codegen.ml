(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** SPIR-V binary emitter (MVP, proof of wire format).

    SPIR-V is a 32-bit-word-oriented binary format. Each instruction is a
    sequence of words: the first word packs [WordCount << 16 | OpCode], and
    operands follow.

    This MVP emits a minimal valid module: capability + memory model + a
    single [GLCompute] entry point [@kernel] that takes one [Int]
    parameter (the index) and does nothing. It is enough to round-trip
    through [spirv-val] and prove the wire format is correct. Real GPU
    work is left to Phase 2.

    Reference: https://www.khronos.org/registry/SPIR-V/specs/unified1/SPIRV.html
*)

open Ast

exception Spirv_unsupported of string
let unsupported m = raise (Spirv_unsupported m)

(* ============================================================================
   Word emission

   SPIR-V words are little-endian 32-bit unsigned. Strings are NUL-terminated
   and zero-padded to 4-byte alignment.
   ============================================================================ *)

let emit_word (buf : Buffer.t) (w : int) : unit =
  Buffer.add_char buf (Char.chr (w land 0xFF));
  Buffer.add_char buf (Char.chr ((w lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((w lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((w lsr 24) land 0xFF))

let emit_string (buf : Buffer.t) (s : string) : int =
  (* Returns the number of words consumed. NUL-terminated, zero-padded. *)
  let n = String.length s in
  let total = n + 1 in
  let padded = ((total + 3) / 4) * 4 in
  for i = 0 to padded - 1 do
    let c = if i < n then Char.code s.[i] else 0 in
    Buffer.add_char buf (Char.chr c)
  done;
  padded / 4

let emit_op (buf : Buffer.t) (opcode : int) (operands : int list) : unit =
  let word_count = 1 + List.length operands in
  emit_word buf ((word_count lsl 16) lor opcode);
  List.iter (emit_word buf) operands

(* String-bearing ops are emitted manually because their length depends on
   the string padding rather than on a fixed operand count. *)
let emit_op_with_string (buf : Buffer.t) (opcode : int) (prefix : int list)
    (s : string) : unit =
  let str_buf = Buffer.create 64 in
  let str_words = emit_string str_buf s in
  let word_count = 1 + List.length prefix + str_words in
  emit_word buf ((word_count lsl 16) lor opcode);
  List.iter (emit_word buf) prefix;
  Buffer.add_string buf (Buffer.contents str_buf)

(* ============================================================================
   Module emission

   Layout (per spec section 2.4):
   1. Magic + version + generator + bound + reserved (header, 5 words)
   2. Capabilities
   3. Extensions
   4. ExtInstImports
   5. MemoryModel
   6. EntryPoints
   7. ExecutionModes
   8. Debug info (Source, OpName, OpString)
   9. Annotations
   10. Type, constant, global declarations
   11. Function declarations
   12. Function definitions
   ============================================================================ *)

let pick_kernel (program : program) : fn_decl =
  let fns = List.filter_map (function TopFn fd -> Some fd | _ -> None) program.prog_decls in
  match List.find_opt (fun fd -> fd.fd_name.name = "kernel") fns with
  | Some fd -> fd
  | None -> match fns with
            | fd :: _ -> fd
            | [] -> unsupported "no function found"

let generate (program : program) (_symbols : Symbol.t) : string =
  let entry = pick_kernel program in
  let entry_name = entry.fd_name.name in
  let buf = Buffer.create 512 in

  (* SPIR-V op codes (subset, from the spec) *)
  let op_capability      = 17 in
  let op_memory_model    = 14 in
  let op_entry_point     = 15 in
  let op_execution_mode  = 16 in
  let op_type_void       = 19 in
  let op_type_function   = 33 in
  let op_function        = 54 in
  let op_function_end    = 56 in
  let op_label           = 248 in
  let op_return          = 253 in

  (* SSA IDs — we hand-allocate just the few we need. *)
  let id_void_ty   = 1 in
  let id_fn_ty     = 2 in
  let id_main      = 3 in
  let id_label     = 4 in
  let bound        = 5 in   (* one past the highest used ID *)

  (* Header: magic, version 1.0, generator (anything; we use 0), bound, schema *)
  emit_word buf 0x07230203;
  emit_word buf 0x00010000;   (* version 1.0 *)
  emit_word buf 0;            (* generator magic — 0 means 'unknown', valid *)
  emit_word buf bound;
  emit_word buf 0;            (* schema *)

  (* Capability Shader (1) — required for GLCompute entry points *)
  emit_op buf op_capability [1];

  (* OpMemoryModel Logical (0) GLSL450 (1) *)
  emit_op buf op_memory_model [0; 1];

  (* OpEntryPoint GLCompute (5) %id_main "kernel" *)
  emit_op_with_string buf op_entry_point [5; id_main] entry_name;

  (* OpExecutionMode %id_main LocalSize 64 1 1 (mode 17) *)
  emit_op buf op_execution_mode [id_main; 17; 64; 1; 1];

  (* %id_void_ty = OpTypeVoid *)
  emit_op buf op_type_void [id_void_ty];
  (* %id_fn_ty = OpTypeFunction %id_void_ty *)
  emit_op buf op_type_function [id_fn_ty; id_void_ty];

  (* %id_main = OpFunction %id_void_ty None %id_fn_ty *)
  emit_op buf op_function [id_void_ty; id_main; 0; id_fn_ty];
  (* %id_label = OpLabel *)
  emit_op buf op_label [id_label];
  (* OpReturn *)
  emit_op buf op_return [];
  (* OpFunctionEnd *)
  emit_op buf op_function_end [];

  Buffer.contents buf

let codegen_spirv (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Spirv_unsupported m -> Error ("SPIR-V backend: " ^ m)
  | Failure m           -> Error ("SPIR-V codegen error: " ^ m)
  | e                   -> Error ("SPIR-V codegen error: " ^ Printexc.to_string e)
