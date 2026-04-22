(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Binary encoder for WasmGC modules.

    Serialises a {!Wasm_gc.gc_module} to the WASM GC binary format.

    Key differences from the WASM 1.0 format ({!Wasm_encode}):
    - Type section entries may be struct types (0x5F) or array types (0x5E),
      in addition to function types (0x60).
    - Value types include reference types (0x63 / 0x64 + heap_type).
    - GC instructions use a 0xFB sub-opcode prefix.
    - Standard WASM 1.0 instructions are encoded by delegating to
      {!Wasm_encode.add_instr}.

    Spec: https://webassembly.github.io/gc/core/binary/
*)

open Wasm_gc

(* ── Low-level byte writers ─────────────────────────────────────────────── *)

let add_u8 buf n =
  Buffer.add_char buf (Char.chr (n land 0xFF))

(** Unsigned LEB128 encoding.  Used for type indices, counts, etc. *)
let add_u32_leb buf n =
  let rec loop v =
    let byte = v land 0x7F in
    let v' = v lsr 7 in
    if v' = 0 then add_u8 buf byte
    else (add_u8 buf (byte lor 0x80); loop v')
  in
  loop n

(** UTF-8 / binary string with LEB128 length prefix. *)
let add_string buf s =
  let b = Bytes.of_string s in
  add_u32_leb buf (Bytes.length b);
  Buffer.add_bytes buf b

(** Encode a WASM vector: LEB128 count followed by elements. *)
let add_vec buf items f =
  add_u32_leb buf (List.length items);
  List.iter (f buf) items

(* ── Value type encodings ────────────────────────────────────────────────── *)

(** Encode a primitive value type (same byte assignments as WASM 1.0). *)
let add_prim_valtype buf = function
  | I32 -> add_u8 buf 0x7F
  | I64 -> add_u8 buf 0x7E
  | F32 -> add_u8 buf 0x7D
  | F64 -> add_u8 buf 0x7C

(** Encode a heap type.

    Abstract heap types use the fixed byte values specified in the GC proposal.
    Concrete type indices (non-negative integers) encode identically to
    unsigned LEB128 — the s33 format is equivalent for non-negative values. *)
let add_heap_type buf = function
  | HtAny      -> add_u8 buf 0x6E  (* any *)
  | HtEq       -> add_u8 buf 0x6D  (* eq *)
  | HtI31      -> add_u8 buf 0x6C  (* i31 *)
  | HtStruct   -> add_u8 buf 0x6B  (* struct *)
  | HtArray    -> add_u8 buf 0x6A  (* array *)
  | HtFunc     -> add_u8 buf 0x70  (* func *)
  | HtNone     -> add_u8 buf 0x71  (* none *)
  | HtNoFunc   -> add_u8 buf 0x73  (* nofunc *)
  | HtNoExtern -> add_u8 buf 0x72  (* noextern *)
  | HtConcrete idx -> add_u32_leb buf idx  (* s33: positive = type index *)

(** Encode a GC-aware value type. *)
let add_gc_valtype buf = function
  | GcPrim p    -> add_prim_valtype buf p
  | GcRef ht    ->
    (* (ref ht) — non-null reference: 0x63 heaptype *)
    add_u8 buf 0x63; add_heap_type buf ht
  | GcRefNull ht ->
    (* (ref null ht) — nullable reference: 0x64 heaptype *)
    add_u8 buf 0x64; add_heap_type buf ht
  | GcAnyref    -> add_u8 buf 0x6E  (* anyref shorthand *)
  | GcEqref     -> add_u8 buf 0x6D  (* eqref  shorthand *)
  | GcI31ref    -> add_u8 buf 0x6C  (* i31ref shorthand *)
  | GcStructref -> add_u8 buf 0x6B  (* structref shorthand *)
  | GcArrayref  -> add_u8 buf 0x6A  (* arrayref shorthand *)

(* ── Type section encodings ─────────────────────────────────────────────── *)

(** Encode the storage type of a struct / array field. *)
let add_storage_type buf = function
  | StVal vt -> add_gc_valtype buf vt
  | StI8     -> add_u8 buf 0x78  (* packed i8  *)
  | StI16    -> add_u8 buf 0x77  (* packed i16 *)

(** Encode a field type: storage type followed by mutability byte. *)
let add_field_type buf ft =
  add_storage_type buf ft.ft_storage;
  add_u8 buf (if ft.ft_mutable then 0x01 else 0x00)

(** Encode a struct type definition: 0x5F + vec(fieldtype). *)
let add_struct_type buf (fields : struct_type) =
  add_u8 buf 0x5F;
  add_vec buf fields add_field_type

(** Encode an array type definition: 0x5E + fieldtype. *)
let add_array_type buf (elem : array_type) =
  add_u8 buf 0x5E;
  add_field_type buf elem

(** Encode a function type definition using GC value types: 0x60 + params + results. *)
let add_gc_func_type buf (ft : gc_func_type) =
  add_u8 buf 0x60;
  add_vec buf ft.gft_params  add_gc_valtype;
  add_vec buf ft.gft_results add_gc_valtype

(** Encode one type definition in the type section.
    Each entry is implicitly a final type with no declared supertypes,
    which is the simplest valid encoding (backward-compatible with the
    pre-GC type section for function types). *)
let add_gc_type_def buf = function
  | GcStructType fields -> add_struct_type buf fields
  | GcArrayType  elem   -> add_array_type  buf elem
  | GcFuncType   ft     -> add_gc_func_type buf ft

(* ── GC block type encoding ─────────────────────────────────────────────── *)

(** Encode a block result type for GC-aware control flow instructions. *)
let add_gc_blocktype buf = function
  | GcBtEmpty  -> add_u8 buf 0x40              (* void *)
  | GcBtPrim p -> add_prim_valtype buf p       (* primitive result *)
  | GcBtRef ht ->
    (* Emit as (ref null ht) — nullable covers both null and non-null *)
    add_u8 buf 0x64; add_heap_type buf ht

(* ── Instruction encoding ───────────────────────────────────────────────── *)

(** Recursively encode a GC instruction.

    Standard WASM 1.0 instructions are delegated to {!Wasm_encode.add_instr}.
    GC instructions use the 0xFB sub-opcode prefix, followed by the specific
    opcode and operands. *)
let rec add_gc_instr buf = function

  (* ── Wrapped standard WASM 1.0 instructions ──────────────────────── *)
  | Std instr ->
    Wasm_encode.add_instr buf instr

  (* ── Struct instructions ─────────────────────────────────────────── *)

  | StructNew typeidx ->
    (* struct.new: 0xFB 0x00 typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x00; add_u32_leb buf typeidx

  | StructNewDefault typeidx ->
    (* struct.new_default: 0xFB 0x01 typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x01; add_u32_leb buf typeidx

  | StructGet (typeidx, fieldidx) ->
    (* struct.get: 0xFB 0x02 typeidx fieldidx *)
    add_u8 buf 0xFB; add_u8 buf 0x02;
    add_u32_leb buf typeidx; add_u32_leb buf fieldidx

  | StructGetS (typeidx, fieldidx) ->
    (* struct.get_s: 0xFB 0x03 typeidx fieldidx *)
    add_u8 buf 0xFB; add_u8 buf 0x03;
    add_u32_leb buf typeidx; add_u32_leb buf fieldidx

  | StructGetU (typeidx, fieldidx) ->
    (* struct.get_u: 0xFB 0x04 typeidx fieldidx *)
    add_u8 buf 0xFB; add_u8 buf 0x04;
    add_u32_leb buf typeidx; add_u32_leb buf fieldidx

  | StructSet (typeidx, fieldidx) ->
    (* struct.set: 0xFB 0x05 typeidx fieldidx *)
    add_u8 buf 0xFB; add_u8 buf 0x05;
    add_u32_leb buf typeidx; add_u32_leb buf fieldidx

  (* ── Array instructions ──────────────────────────────────────────── *)

  | ArrayNew typeidx ->
    (* array.new: 0xFB 0x06 typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x06; add_u32_leb buf typeidx

  | ArrayNewDefault typeidx ->
    (* array.new_default: 0xFB 0x07 typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x07; add_u32_leb buf typeidx

  | ArrayNewFixed (typeidx, n) ->
    (* array.new_fixed: 0xFB 0x08 typeidx n *)
    add_u8 buf 0xFB; add_u8 buf 0x08;
    add_u32_leb buf typeidx; add_u32_leb buf n

  | ArrayGet typeidx ->
    (* array.get: 0xFB 0x0B typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x0B; add_u32_leb buf typeidx

  | ArrayGetS typeidx ->
    (* array.get_s: 0xFB 0x0C typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x0C; add_u32_leb buf typeidx

  | ArrayGetU typeidx ->
    (* array.get_u: 0xFB 0x0D typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x0D; add_u32_leb buf typeidx

  | ArraySet typeidx ->
    (* array.set: 0xFB 0x0E typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x0E; add_u32_leb buf typeidx

  | ArrayLen ->
    (* array.len: 0xFB 0x0F *)
    add_u8 buf 0xFB; add_u8 buf 0x0F

  | ArrayCopy (dst, src) ->
    (* array.copy: 0xFB 0x11 dst_typeidx src_typeidx *)
    add_u8 buf 0xFB; add_u8 buf 0x11;
    add_u32_leb buf dst; add_u32_leb buf src

  (* ── Reference instructions ──────────────────────────────────────── *)

  | RefNull ht ->
    (* ref.null: 0xD0 heaptype *)
    add_u8 buf 0xD0; add_heap_type buf ht

  | RefIsNull ->
    (* ref.is_null: 0xD1 *)
    add_u8 buf 0xD1

  | RefTest ht ->
    (* ref.test (ref ht) non-null: 0xFB 0x14 heaptype *)
    add_u8 buf 0xFB; add_u8 buf 0x14; add_heap_type buf ht

  | RefTestNull ht ->
    (* ref.test (ref null ht) nullable: 0xFB 0x15 heaptype *)
    add_u8 buf 0xFB; add_u8 buf 0x15; add_heap_type buf ht

  | RefCast ht ->
    (* ref.cast (ref ht) non-null: 0xFB 0x16 heaptype *)
    add_u8 buf 0xFB; add_u8 buf 0x16; add_heap_type buf ht

  | RefCastNull ht ->
    (* ref.cast (ref null ht) nullable: 0xFB 0x17 heaptype *)
    add_u8 buf 0xFB; add_u8 buf 0x17; add_heap_type buf ht

  | RefI31 ->
    (* ref.i31: 0xFB 0x1C *)
    add_u8 buf 0xFB; add_u8 buf 0x1C

  | I31GetS ->
    (* i31.get_s: 0xFB 0x1D *)
    add_u8 buf 0xFB; add_u8 buf 0x1D

  | I31GetU ->
    (* i31.get_u: 0xFB 0x1E *)
    add_u8 buf 0xFB; add_u8 buf 0x1E

  (* ── GC-aware control flow ────────────────────────────────────────── *)

  | GcBlock (bt, instrs) ->
    add_u8 buf 0x02;                          (* block opcode *)
    add_gc_blocktype buf bt;
    List.iter (add_gc_instr buf) instrs;
    add_u8 buf 0x0B                           (* end *)

  | GcLoop (bt, instrs) ->
    add_u8 buf 0x03;                          (* loop opcode *)
    add_gc_blocktype buf bt;
    List.iter (add_gc_instr buf) instrs;
    add_u8 buf 0x0B                           (* end *)

  | GcIf (bt, then_instrs, else_instrs) ->
    add_u8 buf 0x04;                          (* if opcode *)
    add_gc_blocktype buf bt;
    List.iter (add_gc_instr buf) then_instrs;
    if else_instrs <> [] then (
      add_u8 buf 0x05;                        (* else *)
      List.iter (add_gc_instr buf) else_instrs
    );
    add_u8 buf 0x0B                           (* end *)

(* ── Section helpers ────────────────────────────────────────────────────── *)

(** Encode a function code entry: locals vector + body + end marker. *)
let add_gc_code buf (gf : gc_func) =
  let body = Buffer.create 256 in
  (* Locals: vec of (count, gc_valtype) pairs *)
  add_vec body gf.gf_locals (fun b (count, vt) ->
    add_u32_leb b count;
    add_gc_valtype b vt
  );
  List.iter (add_gc_instr body) gf.gf_body;
  add_u8 body 0x0B;                           (* end *)
  add_u32_leb buf (Buffer.length body);
  Buffer.add_buffer buf body

let add_gc_import buf (gi : gc_import) =
  add_string buf gi.gi_module;
  add_string buf gi.gi_name;
  match gi.gi_desc with
  | GcImportFunc type_idx ->
    add_u8 buf 0x00; add_u32_leb buf type_idx
  | GcImportMemory ->
    add_u8 buf 0x02;
    add_u8 buf 0x00;   (* limits: no max *)
    add_u32_leb buf 1  (* minimum 1 page *)

let add_gc_export buf (ge : gc_export) =
  add_string buf ge.ge_name;
  match ge.ge_desc with
  | GcExportFunc   idx -> add_u8 buf 0x00; add_u32_leb buf idx
  | GcExportMemory idx -> add_u8 buf 0x02; add_u32_leb buf idx

(** Write a section if it has content.
    section_id: WASM section identifier byte.
    builder: fills a temporary buffer whose length-prefixed content is appended. *)
let add_section buf section_id builder =
  let sec = Buffer.create 256 in
  builder sec;
  if Buffer.length sec > 0 then (
    add_u8 buf section_id;
    add_u32_leb buf (Buffer.length sec);
    Buffer.add_buffer buf sec
  )

(* ── Top-level writer ───────────────────────────────────────────────────── *)

(** Serialise a {!Wasm_gc.gc_module} to a WASM GC binary file.

    Sections emitted in the required order:
    {ol
    {- Type (1) — GC type definitions}
    {- Import (2) — imports}
    {- Function (3) — function type index list}
    {- Export (7) — named exports}
    {- Code (10) — function bodies}
    }

    Browsers and runtimes that support the GC proposal (V8 ≥ Chrome 119,
    SpiderMonkey ≥ Firefox 120, Wasmtime with --wasm-features gc) accept
    this format. *)
let write_gc_module_to_file (path : string) (m : gc_module) : unit =
  let buf = Buffer.create 4096 in

  (* WASM magic number: \0asm *)
  Buffer.add_string buf "\x00\x61\x73\x6D";
  (* WASM version: 1 (little-endian u32) *)
  Buffer.add_string buf "\x01\x00\x00\x00";

  (* Section 1: Types *)
  add_section buf 1 (fun b -> add_vec b m.gc_types add_gc_type_def);

  (* Section 2: Imports *)
  add_section buf 2 (fun b -> add_vec b m.gc_imports add_gc_import);

  (* Section 3: Functions (type index for each function) *)
  add_section buf 3 (fun b ->
    add_vec b m.gc_funcs (fun b gf -> add_u32_leb b gf.gf_type)
  );

  (* Section 7: Exports *)
  add_section buf 7 (fun b -> add_vec b m.gc_exports add_gc_export);

  (* Section 10: Code (function bodies) *)
  add_section buf 10 (fun b -> add_vec b m.gc_funcs add_gc_code);

  let oc = open_out_bin path in
  output_bytes oc (Bytes.of_string (Buffer.contents buf));
  close_out oc
