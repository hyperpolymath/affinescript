(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WebAssembly binary encoder.

    This module encodes WASM IR into the binary format according to
    the WebAssembly 1.0 specification.
*)

open Wasm

(** Byte buffer for building binary output *)
type buffer = {
  mutable bytes : int list;  (** Bytes in reverse order *)
}

(** Create a new buffer *)
let create_buffer () : buffer = {
  bytes = [];
}

(** Emit a single byte *)
let emit_byte (buf : buffer) (b : int) : unit =
  buf.bytes <- b :: buf.bytes

(** Emit multiple bytes *)
let emit_bytes (buf : buffer) (bs : int list) : unit =
  List.iter (emit_byte buf) bs

(** Get buffer contents as bytes list *)
let buffer_contents (buf : buffer) : int list =
  List.rev buf.bytes

(** Encode unsigned LEB128 *)
let rec encode_u32 (buf : buffer) (n : int) : unit =
  let byte = n land 0x7f in
  let rest = n lsr 7 in
  if rest = 0 then
    emit_byte buf byte
  else begin
    emit_byte buf (byte lor 0x80);
    encode_u32 buf rest
  end

(** Encode signed LEB128 *)
let rec encode_i32 (buf : buffer) (n : int32) : unit =
  let n_int = Int32.to_int n in
  let byte = n_int land 0x7f in
  let rest = n_int asr 7 in
  let sign_bit = n_int land 0x40 in
  if (rest = 0 && sign_bit = 0) || (rest = -1 && sign_bit <> 0) then
    emit_byte buf byte
  else begin
    emit_byte buf (byte lor 0x80);
    encode_i32 buf (Int32.of_int rest)
  end

(** Encode signed 64-bit LEB128 *)
let rec encode_i64 (buf : buffer) (n : int64) : unit =
  let n_int = Int64.to_int n in
  let byte = n_int land 0x7f in
  let rest = n_int asr 7 in
  let sign_bit = n_int land 0x40 in
  if (rest = 0 && sign_bit = 0) || (rest = -1 && sign_bit <> 0) then
    emit_byte buf byte
  else begin
    emit_byte buf (byte lor 0x80);
    encode_i64 buf (Int64.of_int rest)
  end

(** Encode f32 (IEEE 754 single precision) *)
let encode_f32 (buf : buffer) (f : float) : unit =
  let bits = Int32.bits_of_float f in
  emit_byte buf (Int32.to_int (Int32.logand bits 0xFFl));
  emit_byte buf (Int32.to_int (Int32.logand (Int32.shift_right_logical bits 8) 0xFFl));
  emit_byte buf (Int32.to_int (Int32.logand (Int32.shift_right_logical bits 16) 0xFFl));
  emit_byte buf (Int32.to_int (Int32.logand (Int32.shift_right_logical bits 24) 0xFFl))

(** Encode f64 (IEEE 754 double precision) *)
let encode_f64 (buf : buffer) (f : float) : unit =
  let bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xFFL) in
    emit_byte buf byte
  done

(** Encode a name (UTF-8 string) *)
let encode_name (buf : buffer) (s : string) : unit =
  encode_u32 buf (String.length s);
  String.iter (fun c -> emit_byte buf (Char.code c)) s

(** Encode value type *)
let encode_value_type (buf : buffer) (vt : value_type) : unit =
  let code = match vt with
    | I32 -> 0x7f
    | I64 -> 0x7e
    | F32 -> 0x7d
    | F64 -> 0x7c
  in
  emit_byte buf code

(** Encode block type *)
let encode_block_type (buf : buffer) (bt : block_type) : unit =
  match bt with
  | BtEmpty -> emit_byte buf 0x40
  | BtType vt -> encode_value_type buf vt

(** Encode instruction *)
let rec encode_instr (buf : buffer) (instr : instr) : unit =
  match instr with
  | Unreachable -> emit_byte buf 0x00
  | Nop -> emit_byte buf 0x01
  | Block (bt, instrs) ->
    emit_byte buf 0x02;
    encode_block_type buf bt;
    List.iter (encode_instr buf) instrs;
    emit_byte buf 0x0b  (* end *)
  | Loop (bt, instrs) ->
    emit_byte buf 0x03;
    encode_block_type buf bt;
    List.iter (encode_instr buf) instrs;
    emit_byte buf 0x0b  (* end *)
  | If (bt, then_instrs, else_instrs) ->
    emit_byte buf 0x04;
    encode_block_type buf bt;
    List.iter (encode_instr buf) then_instrs;
    if List.length else_instrs > 0 then begin
      emit_byte buf 0x05;  (* else *)
      List.iter (encode_instr buf) else_instrs
    end;
    emit_byte buf 0x0b  (* end *)
  | Br idx -> emit_byte buf 0x0c; encode_u32 buf idx
  | BrIf idx -> emit_byte buf 0x0d; encode_u32 buf idx
  | Return -> emit_byte buf 0x0f
  | Call idx -> emit_byte buf 0x10; encode_u32 buf idx
  | CallIndirect idx -> emit_byte buf 0x11; encode_u32 buf idx; emit_byte buf 0x00

  | Drop -> emit_byte buf 0x1a
  | Select -> emit_byte buf 0x1b

  | LocalGet idx -> emit_byte buf 0x20; encode_u32 buf idx
  | LocalSet idx -> emit_byte buf 0x21; encode_u32 buf idx
  | LocalTee idx -> emit_byte buf 0x22; encode_u32 buf idx
  | GlobalGet idx -> emit_byte buf 0x23; encode_u32 buf idx
  | GlobalSet idx -> emit_byte buf 0x24; encode_u32 buf idx

  | I32Load (align, offset) -> emit_byte buf 0x28; encode_u32 buf align; encode_u32 buf offset
  | I64Load (align, offset) -> emit_byte buf 0x29; encode_u32 buf align; encode_u32 buf offset
  | F32Load (align, offset) -> emit_byte buf 0x2a; encode_u32 buf align; encode_u32 buf offset
  | F64Load (align, offset) -> emit_byte buf 0x2b; encode_u32 buf align; encode_u32 buf offset
  | I32Store (align, offset) -> emit_byte buf 0x36; encode_u32 buf align; encode_u32 buf offset
  | I64Store (align, offset) -> emit_byte buf 0x37; encode_u32 buf align; encode_u32 buf offset
  | F32Store (align, offset) -> emit_byte buf 0x38; encode_u32 buf align; encode_u32 buf offset
  | F64Store (align, offset) -> emit_byte buf 0x39; encode_u32 buf align; encode_u32 buf offset
  | MemorySize -> emit_byte buf 0x3f; emit_byte buf 0x00
  | MemoryGrow -> emit_byte buf 0x40; emit_byte buf 0x00

  | I32Const n -> emit_byte buf 0x41; encode_i32 buf n
  | I64Const n -> emit_byte buf 0x42; encode_i64 buf n
  | F32Const f -> emit_byte buf 0x43; encode_f32 buf f
  | F64Const f -> emit_byte buf 0x44; encode_f64 buf f

  (* I32 operations *)
  | I32Eqz -> emit_byte buf 0x45
  | I32Eq -> emit_byte buf 0x46
  | I32Ne -> emit_byte buf 0x47
  | I32LtS -> emit_byte buf 0x48
  | I32LtU -> emit_byte buf 0x49
  | I32GtS -> emit_byte buf 0x4a
  | I32GtU -> emit_byte buf 0x4b
  | I32LeS -> emit_byte buf 0x4c
  | I32LeU -> emit_byte buf 0x4d
  | I32GeS -> emit_byte buf 0x4e
  | I32GeU -> emit_byte buf 0x4f

  | I32Clz -> emit_byte buf 0x67
  | I32Ctz -> emit_byte buf 0x68
  | I32Popcnt -> emit_byte buf 0x69
  | I32Add -> emit_byte buf 0x6a
  | I32Sub -> emit_byte buf 0x6b
  | I32Mul -> emit_byte buf 0x6c
  | I32DivS -> emit_byte buf 0x6d
  | I32DivU -> emit_byte buf 0x6e
  | I32RemS -> emit_byte buf 0x6f
  | I32RemU -> emit_byte buf 0x70
  | I32And -> emit_byte buf 0x71
  | I32Or -> emit_byte buf 0x72
  | I32Xor -> emit_byte buf 0x73
  | I32Shl -> emit_byte buf 0x74
  | I32ShrS -> emit_byte buf 0x75
  | I32ShrU -> emit_byte buf 0x76
  | I32Rotl -> emit_byte buf 0x77
  | I32Rotr -> emit_byte buf 0x78

  (* I64 operations *)
  | I64Eqz -> emit_byte buf 0x50
  | I64Eq -> emit_byte buf 0x51
  | I64Ne -> emit_byte buf 0x52
  | I64LtS -> emit_byte buf 0x53
  | I64LtU -> emit_byte buf 0x54
  | I64GtS -> emit_byte buf 0x55
  | I64GtU -> emit_byte buf 0x56
  | I64LeS -> emit_byte buf 0x57
  | I64LeU -> emit_byte buf 0x58
  | I64GeS -> emit_byte buf 0x59
  | I64GeU -> emit_byte buf 0x5a

  | I64Clz -> emit_byte buf 0x79
  | I64Ctz -> emit_byte buf 0x7a
  | I64Popcnt -> emit_byte buf 0x7b
  | I64Add -> emit_byte buf 0x7c
  | I64Sub -> emit_byte buf 0x7d
  | I64Mul -> emit_byte buf 0x7e
  | I64DivS -> emit_byte buf 0x7f
  | I64DivU -> emit_byte buf 0x80
  | I64RemS -> emit_byte buf 0x81
  | I64RemU -> emit_byte buf 0x82
  | I64And -> emit_byte buf 0x83
  | I64Or -> emit_byte buf 0x84
  | I64Xor -> emit_byte buf 0x85
  | I64Shl -> emit_byte buf 0x86
  | I64ShrS -> emit_byte buf 0x87
  | I64ShrU -> emit_byte buf 0x88
  | I64Rotl -> emit_byte buf 0x89
  | I64Rotr -> emit_byte buf 0x8a

  (* F32 operations *)
  | F32Eq -> emit_byte buf 0x5b
  | F32Ne -> emit_byte buf 0x5c
  | F32Lt -> emit_byte buf 0x5d
  | F32Gt -> emit_byte buf 0x5e
  | F32Le -> emit_byte buf 0x5f
  | F32Ge -> emit_byte buf 0x60

  | F32Abs -> emit_byte buf 0x8b
  | F32Neg -> emit_byte buf 0x8c
  | F32Ceil -> emit_byte buf 0x8d
  | F32Floor -> emit_byte buf 0x8e
  | F32Trunc -> emit_byte buf 0x8f
  | F32Nearest -> emit_byte buf 0x90
  | F32Sqrt -> emit_byte buf 0x91
  | F32Add -> emit_byte buf 0x92
  | F32Sub -> emit_byte buf 0x93
  | F32Mul -> emit_byte buf 0x94
  | F32Div -> emit_byte buf 0x95
  | F32Min -> emit_byte buf 0x96
  | F32Max -> emit_byte buf 0x97
  | F32Copysign -> emit_byte buf 0x98

  (* F64 operations *)
  | F64Eq -> emit_byte buf 0x61
  | F64Ne -> emit_byte buf 0x62
  | F64Lt -> emit_byte buf 0x63
  | F64Gt -> emit_byte buf 0x64
  | F64Le -> emit_byte buf 0x65
  | F64Ge -> emit_byte buf 0x66

  | F64Abs -> emit_byte buf 0x99
  | F64Neg -> emit_byte buf 0x9a
  | F64Ceil -> emit_byte buf 0x9b
  | F64Floor -> emit_byte buf 0x9c
  | F64Trunc -> emit_byte buf 0x9d
  | F64Nearest -> emit_byte buf 0x9e
  | F64Sqrt -> emit_byte buf 0x9f
  | F64Add -> emit_byte buf 0xa0
  | F64Sub -> emit_byte buf 0xa1
  | F64Mul -> emit_byte buf 0xa2
  | F64Div -> emit_byte buf 0xa3
  | F64Min -> emit_byte buf 0xa4
  | F64Max -> emit_byte buf 0xa5
  | F64Copysign -> emit_byte buf 0xa6

  (* Conversions *)
  | I32WrapI64 -> emit_byte buf 0xa7
  | I64ExtendI32S -> emit_byte buf 0xac
  | I64ExtendI32U -> emit_byte buf 0xad
  | I32TruncF32S -> emit_byte buf 0xa8
  | I32TruncF32U -> emit_byte buf 0xa9
  | I32TruncF64S -> emit_byte buf 0xaa
  | I32TruncF64U -> emit_byte buf 0xab
  | I64TruncF32S -> emit_byte buf 0xae
  | I64TruncF32U -> emit_byte buf 0xaf
  | I64TruncF64S -> emit_byte buf 0xb0
  | I64TruncF64U -> emit_byte buf 0xb1
  | F32ConvertI32S -> emit_byte buf 0xb2
  | F32ConvertI32U -> emit_byte buf 0xb3
  | F32ConvertI64S -> emit_byte buf 0xb4
  | F32ConvertI64U -> emit_byte buf 0xb5
  | F32DemoteF64 -> emit_byte buf 0xb6
  | F64ConvertI32S -> emit_byte buf 0xb7
  | F64ConvertI32U -> emit_byte buf 0xb8
  | F64ConvertI64S -> emit_byte buf 0xb9
  | F64ConvertI64U -> emit_byte buf 0xba
  | F64PromoteF32 -> emit_byte buf 0xbb
  | I32ReinterpretF32 -> emit_byte buf 0xbc
  | I64ReinterpretF64 -> emit_byte buf 0xbd
  | F32ReinterpretI32 -> emit_byte buf 0xbe
  | F64ReinterpretI64 -> emit_byte buf 0xbf

(** Encode function type *)
let encode_func_type (buf : buffer) (ft : func_type) : unit =
  emit_byte buf 0x60;  (* func type tag *)
  encode_u32 buf (List.length ft.ft_params);
  List.iter (encode_value_type buf) ft.ft_params;
  encode_u32 buf (List.length ft.ft_results);
  List.iter (encode_value_type buf) ft.ft_results

(** Encode function *)
let encode_func (buf : buffer) (f : func) : unit =
  (* Encode locals *)
  let locals_buf = create_buffer () in
  encode_u32 locals_buf (List.length f.f_locals);
  List.iter (fun l ->
    encode_u32 locals_buf l.l_count;
    encode_value_type locals_buf l.l_type
  ) f.f_locals;

  (* Encode body *)
  List.iter (encode_instr locals_buf) f.f_body;
  emit_byte locals_buf 0x0b;  (* end *)

  (* Emit size and body *)
  let body_bytes = buffer_contents locals_buf in
  encode_u32 buf (List.length body_bytes);
  emit_bytes buf body_bytes

(** Encode export *)
let encode_export (buf : buffer) (e : export) : unit =
  encode_name buf e.e_name;
  match e.e_desc with
  | ExportFunc idx -> emit_byte buf 0x00; encode_u32 buf idx
  | ExportTable idx -> emit_byte buf 0x01; encode_u32 buf idx
  | ExportMemory idx -> emit_byte buf 0x02; encode_u32 buf idx
  | ExportGlobal idx -> emit_byte buf 0x03; encode_u32 buf idx

(** Encode memory limits *)
let encode_limits (buf : buffer) (lim : limits) : unit =
  match lim.lim_max with
  | None ->
    emit_byte buf 0x00;
    encode_u32 buf lim.lim_min
  | Some max ->
    emit_byte buf 0x01;
    encode_u32 buf lim.lim_min;
    encode_u32 buf max

(** Encode section *)
let encode_section (buf : buffer) (id : int) (contents : int list) : unit =
  if List.length contents > 0 then begin
    emit_byte buf id;
    encode_u32 buf (List.length contents);
    emit_bytes buf contents
  end

(** Encode table *)
let encode_table (buf : buffer) (tab : table) : unit =
  emit_byte buf 0x70;  (* anyfunc *)
  encode_limits buf tab.tab_type

(** Encode global type (value type + mutability) *)
let encode_global_type (buf : buffer) (vt : value_type) (mutable_ : bool) : unit =
  encode_value_type buf vt;
  emit_byte buf (if mutable_ then 1 else 0)

(** Encode global definition *)
let encode_global (buf : buffer) (g : global) : unit =
  encode_global_type buf g.g_type g.g_mutable;
  (* Encode init expression *)
  List.iter (encode_instr buf) g.g_init;
  emit_byte buf 0x0b  (* end *)

(** Encode element segment (for initializing function tables) *)
let encode_element (buf : buffer) (table_idx : int) (offset : int) (func_indices : int list) : unit =
  encode_u32 buf table_idx;  (* table index *)
  (* Offset expression (i32.const 0) *)
  emit_byte buf 0x41;  (* i32.const *)
  encode_i32 buf (Int32.of_int offset);
  emit_byte buf 0x0b;  (* end *)
  (* Function indices *)
  encode_u32 buf (List.length func_indices);
  List.iter (encode_u32 buf) func_indices

(** Encode WASM module *)
let encode_module (m : wasm_module) : int list =
  let buf = create_buffer () in

  (* Magic number *)
  emit_bytes buf [0x00; 0x61; 0x73; 0x6d];

  (* Version *)
  emit_bytes buf [0x01; 0x00; 0x00; 0x00];

  (* Type section *)
  if List.length m.types > 0 then begin
    let type_buf = create_buffer () in
    encode_u32 type_buf (List.length m.types);
    List.iter (encode_func_type type_buf) m.types;
    encode_section buf 1 (buffer_contents type_buf)
  end;

  (* Function section (type indices) *)
  if List.length m.funcs > 0 then begin
    let func_buf = create_buffer () in
    encode_u32 func_buf (List.length m.funcs);
    List.iter (fun f -> encode_u32 func_buf f.f_type) m.funcs;
    encode_section buf 3 (buffer_contents func_buf)
  end;

  (* Table section *)
  if List.length m.tables > 0 then begin
    let table_buf = create_buffer () in
    encode_u32 table_buf (List.length m.tables);
    List.iter (encode_table table_buf) m.tables;
    encode_section buf 4 (buffer_contents table_buf)
  end;

  (* Memory section *)
  if List.length m.mems > 0 then begin
    let mem_buf = create_buffer () in
    encode_u32 mem_buf (List.length m.mems);
    List.iter (fun mem -> encode_limits mem_buf mem.mem_type) m.mems;
    encode_section buf 5 (buffer_contents mem_buf)
  end;

  (* Global section *)
  if List.length m.globals > 0 then begin
    let global_buf = create_buffer () in
    encode_u32 global_buf (List.length m.globals);
    List.iter (encode_global global_buf) m.globals;
    encode_section buf 6 (buffer_contents global_buf)
  end;

  (* Export section *)
  if List.length m.exports > 0 then begin
    let export_buf = create_buffer () in
    encode_u32 export_buf (List.length m.exports);
    List.iter (encode_export export_buf) m.exports;
    encode_section buf 7 (buffer_contents export_buf)
  end;

  (* Element section (initialize function tables) *)
  if List.length m.elems > 0 then begin
    let elem_buf = create_buffer () in
    encode_u32 elem_buf (List.length m.elems);
    List.iter (fun elem ->
      encode_element elem_buf elem.e_table elem.e_offset elem.e_funcs
    ) m.elems;
    encode_section buf 9 (buffer_contents elem_buf)
  end;

  (* Code section (function bodies) *)
  if List.length m.funcs > 0 then begin
    let code_buf = create_buffer () in
    encode_u32 code_buf (List.length m.funcs);
    List.iter (encode_func code_buf) m.funcs;
    encode_section buf 10 (buffer_contents code_buf)
  end;

  (* Data section (memory initialization) *)
  if List.length m.datas > 0 then begin
    let data_buf = create_buffer () in
    encode_u32 data_buf (List.length m.datas);
    List.iter (fun data ->
      (* Data segment format: flags(0=active with offset), offset_expr, data_bytes *)
      encode_u32 data_buf 0;  (* flags: 0 = active segment *)
      (* Offset expression (i32.const offset; end) *)
      emit_byte data_buf 0x41;  (* i32.const *)
      encode_i32 data_buf (Int32.of_int data.d_offset);
      emit_byte data_buf 0x0b;  (* end *)
      (* Data bytes *)
      encode_u32 data_buf (Bytes.length data.d_data);
      Bytes.iter (fun byte -> emit_byte data_buf (Char.code byte)) data.d_data;
    ) m.datas;
    encode_section buf 11 (buffer_contents data_buf)
  end;

  buffer_contents buf

(** Write module to file *)
let write_module_to_file (filename : string) (m : wasm_module) : unit =
  let bytes = encode_module m in
  let oc = open_out_bin filename in
  List.iter (fun b -> output_byte oc b) bytes;
  close_out oc
