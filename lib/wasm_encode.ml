(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Minimal WASM Binary Encoder *)

open Wasm

let add_u8 buf n =
  Buffer.add_char buf (Char.chr (n land 0xFF))

let add_u32_leb buf n =
  let rec loop v =
    let byte = v land 0x7F in
    let v' = v lsr 7 in
    if v' = 0 then
      add_u8 buf byte
    else (
      add_u8 buf (byte lor 0x80);
      loop v'
    )
  in
  loop n

let add_sleb32 buf n =
  let rec loop v =
    let byte = Int32.to_int (Int32.logand v 0x7Fl) in
    let sign_bit = byte land 0x40 in
    let v' = Int32.shift_right v 7 in
    let is_done = (v' = 0l && sign_bit = 0) || (v' = -1l && sign_bit <> 0) in
    if is_done then
      add_u8 buf byte
    else (
      add_u8 buf (byte lor 0x80);
      loop v'
    )
  in
  loop n

let add_sleb64 buf n =
  let rec loop v =
    let byte = Int64.to_int (Int64.logand v 0x7FL) in
    let sign_bit = byte land 0x40 in
    let v' = Int64.shift_right v 7 in
    let is_done = (v' = 0L && sign_bit = 0) || (v' = -1L && sign_bit <> 0) in
    if is_done then
      add_u8 buf byte
    else (
      add_u8 buf (byte lor 0x80);
      loop v'
    )
  in
  loop n

let add_bytes buf b =
  Buffer.add_bytes buf b

let add_string buf s =
  let b = Bytes.of_string s in
  add_u32_leb buf (Bytes.length b);
  add_bytes buf b

let add_vec buf items f =
  add_u32_leb buf (List.length items);
  List.iter (f buf) items

let add_valtype buf = function
  | I32 -> add_u8 buf 0x7F
  | I64 -> add_u8 buf 0x7E
  | F32 -> add_u8 buf 0x7D
  | F64 -> add_u8 buf 0x7C

let add_blocktype buf = function
  | BtEmpty -> add_u8 buf 0x40
  | BtType t -> add_valtype buf t

let add_limits buf lim =
  match lim.lim_max with
  | None ->
      add_u8 buf 0x00;
      add_u32_leb buf lim.lim_min
  | Some max ->
      add_u8 buf 0x01;
      add_u32_leb buf lim.lim_min;
      add_u32_leb buf max

let add_memarg buf align offset =
  add_u32_leb buf align;
  add_u32_leb buf offset

let rec add_instr buf = function
  | Unreachable -> add_u8 buf 0x00
  | Nop -> add_u8 buf 0x01
  | Block (bt, instrs) ->
      add_u8 buf 0x02;
      add_blocktype buf bt;
      List.iter (add_instr buf) instrs;
      add_u8 buf 0x0B
  | Loop (bt, instrs) ->
      add_u8 buf 0x03;
      add_blocktype buf bt;
      List.iter (add_instr buf) instrs;
      add_u8 buf 0x0B
  | If (bt, then_instrs, else_instrs) ->
      add_u8 buf 0x04;
      add_blocktype buf bt;
      List.iter (add_instr buf) then_instrs;
      if else_instrs <> [] then (
        add_u8 buf 0x05;
        List.iter (add_instr buf) else_instrs
      );
      add_u8 buf 0x0B
  | Br idx -> add_u8 buf 0x0C; add_u32_leb buf idx
  | BrIf idx -> add_u8 buf 0x0D; add_u32_leb buf idx
  | Return -> add_u8 buf 0x0F
  | Call idx -> add_u8 buf 0x10; add_u32_leb buf idx
  | CallIndirect idx -> add_u8 buf 0x11; add_u32_leb buf idx; add_u8 buf 0x00
  | Drop -> add_u8 buf 0x1A
  | Select -> add_u8 buf 0x1B
  | LocalGet idx -> add_u8 buf 0x20; add_u32_leb buf idx
  | LocalSet idx -> add_u8 buf 0x21; add_u32_leb buf idx
  | LocalTee idx -> add_u8 buf 0x22; add_u32_leb buf idx
  | GlobalGet idx -> add_u8 buf 0x23; add_u32_leb buf idx
  | GlobalSet idx -> add_u8 buf 0x24; add_u32_leb buf idx
  | I32Load (align, offset) -> add_u8 buf 0x28; add_memarg buf align offset
  | I64Load (align, offset) -> add_u8 buf 0x29; add_memarg buf align offset
  | F32Load (align, offset) -> add_u8 buf 0x2A; add_memarg buf align offset
  | F64Load (align, offset) -> add_u8 buf 0x2B; add_memarg buf align offset
  | I32Store (align, offset) -> add_u8 buf 0x36; add_memarg buf align offset
  | I64Store (align, offset) -> add_u8 buf 0x37; add_memarg buf align offset
  | F32Store (align, offset) -> add_u8 buf 0x38; add_memarg buf align offset
  | F64Store (align, offset) -> add_u8 buf 0x39; add_memarg buf align offset
  | MemorySize -> add_u8 buf 0x3F; add_u8 buf 0x00
  | MemoryGrow -> add_u8 buf 0x40; add_u8 buf 0x00
  | I32Const v -> add_u8 buf 0x41; add_sleb32 buf v
  | I64Const v -> add_u8 buf 0x42; add_sleb64 buf v
  | F32Const v ->
      add_u8 buf 0x43;
      let bits = Int32.bits_of_float v in
      let b = Bytes.create 4 in
      Bytes.set_int32_le b 0 bits;
      add_bytes buf b
  | F64Const v ->
      add_u8 buf 0x44;
      let bits = Int64.bits_of_float v in
      let b = Bytes.create 8 in
      Bytes.set_int64_le b 0 bits;
      add_bytes buf b
  | I32Eqz -> add_u8 buf 0x45
  | I32Eq -> add_u8 buf 0x46
  | I32Ne -> add_u8 buf 0x47
  | I32LtS -> add_u8 buf 0x48
  | I32LtU -> add_u8 buf 0x49
  | I32GtS -> add_u8 buf 0x4A
  | I32GtU -> add_u8 buf 0x4B
  | I32LeS -> add_u8 buf 0x4C
  | I32LeU -> add_u8 buf 0x4D
  | I32GeS -> add_u8 buf 0x4E
  | I32GeU -> add_u8 buf 0x4F
  | I64Eqz -> add_u8 buf 0x50
  | I64Eq -> add_u8 buf 0x51
  | I64Ne -> add_u8 buf 0x52
  | I64LtS -> add_u8 buf 0x53
  | I64LtU -> add_u8 buf 0x54
  | I64GtS -> add_u8 buf 0x55
  | I64GtU -> add_u8 buf 0x56
  | I64LeS -> add_u8 buf 0x57
  | I64LeU -> add_u8 buf 0x58
  | I64GeS -> add_u8 buf 0x59
  | I64GeU -> add_u8 buf 0x5A
  | F32Eq -> add_u8 buf 0x5B
  | F32Ne -> add_u8 buf 0x5C
  | F32Lt -> add_u8 buf 0x5D
  | F32Gt -> add_u8 buf 0x5E
  | F32Le -> add_u8 buf 0x5F
  | F32Ge -> add_u8 buf 0x60
  | F64Eq -> add_u8 buf 0x61
  | F64Ne -> add_u8 buf 0x62
  | F64Lt -> add_u8 buf 0x63
  | F64Gt -> add_u8 buf 0x64
  | F64Le -> add_u8 buf 0x65
  | F64Ge -> add_u8 buf 0x66
  | I32Clz -> add_u8 buf 0x67
  | I32Ctz -> add_u8 buf 0x68
  | I32Popcnt -> add_u8 buf 0x69
  | I32Add -> add_u8 buf 0x6A
  | I32Sub -> add_u8 buf 0x6B
  | I32Mul -> add_u8 buf 0x6C
  | I32DivS -> add_u8 buf 0x6D
  | I32DivU -> add_u8 buf 0x6E
  | I32RemS -> add_u8 buf 0x6F
  | I32RemU -> add_u8 buf 0x70
  | I32And -> add_u8 buf 0x71
  | I32Or -> add_u8 buf 0x72
  | I32Xor -> add_u8 buf 0x73
  | I32Shl -> add_u8 buf 0x74
  | I32ShrS -> add_u8 buf 0x75
  | I32ShrU -> add_u8 buf 0x76
  | I32Rotl -> add_u8 buf 0x77
  | I32Rotr -> add_u8 buf 0x78
  | I64Clz -> add_u8 buf 0x79
  | I64Ctz -> add_u8 buf 0x7A
  | I64Popcnt -> add_u8 buf 0x7B
  | I64Add -> add_u8 buf 0x7C
  | I64Sub -> add_u8 buf 0x7D
  | I64Mul -> add_u8 buf 0x7E
  | I64DivS -> add_u8 buf 0x7F
  | I64DivU -> add_u8 buf 0x80
  | I64RemS -> add_u8 buf 0x81
  | I64RemU -> add_u8 buf 0x82
  | I64And -> add_u8 buf 0x83
  | I64Or -> add_u8 buf 0x84
  | I64Xor -> add_u8 buf 0x85
  | I64Shl -> add_u8 buf 0x86
  | I64ShrS -> add_u8 buf 0x87
  | I64ShrU -> add_u8 buf 0x88
  | I64Rotl -> add_u8 buf 0x89
  | I64Rotr -> add_u8 buf 0x8A
  | F32Abs -> add_u8 buf 0x8B
  | F32Neg -> add_u8 buf 0x8C
  | F32Ceil -> add_u8 buf 0x8D
  | F32Floor -> add_u8 buf 0x8E
  | F32Trunc -> add_u8 buf 0x8F
  | F32Nearest -> add_u8 buf 0x90
  | F32Sqrt -> add_u8 buf 0x91
  | F32Add -> add_u8 buf 0x92
  | F32Sub -> add_u8 buf 0x93
  | F32Mul -> add_u8 buf 0x94
  | F32Div -> add_u8 buf 0x95
  | F32Min -> add_u8 buf 0x96
  | F32Max -> add_u8 buf 0x97
  | F32Copysign -> add_u8 buf 0x98
  | F64Abs -> add_u8 buf 0x99
  | F64Neg -> add_u8 buf 0x9A
  | F64Ceil -> add_u8 buf 0x9B
  | F64Floor -> add_u8 buf 0x9C
  | F64Trunc -> add_u8 buf 0x9D
  | F64Nearest -> add_u8 buf 0x9E
  | F64Sqrt -> add_u8 buf 0x9F
  | F64Add -> add_u8 buf 0xA0
  | F64Sub -> add_u8 buf 0xA1
  | F64Mul -> add_u8 buf 0xA2
  | F64Div -> add_u8 buf 0xA3
  | F64Min -> add_u8 buf 0xA4
  | F64Max -> add_u8 buf 0xA5
  | F64Copysign -> add_u8 buf 0xA6
  | I32WrapI64 -> add_u8 buf 0xA7
  | I32TruncF32S -> add_u8 buf 0xA8
  | I32TruncF32U -> add_u8 buf 0xA9
  | I32TruncF64S -> add_u8 buf 0xAA
  | I32TruncF64U -> add_u8 buf 0xAB
  | I64ExtendI32S -> add_u8 buf 0xAC
  | I64ExtendI32U -> add_u8 buf 0xAD
  | I64TruncF32S -> add_u8 buf 0xAE
  | I64TruncF32U -> add_u8 buf 0xAF
  | I64TruncF64S -> add_u8 buf 0xB0
  | I64TruncF64U -> add_u8 buf 0xB1
  | F32ConvertI32S -> add_u8 buf 0xB2
  | F32ConvertI32U -> add_u8 buf 0xB3
  | F32ConvertI64S -> add_u8 buf 0xB4
  | F32ConvertI64U -> add_u8 buf 0xB5
  | F32DemoteF64 -> add_u8 buf 0xB6
  | F64ConvertI32S -> add_u8 buf 0xB7
  | F64ConvertI32U -> add_u8 buf 0xB8
  | F64ConvertI64S -> add_u8 buf 0xB9
  | F64ConvertI64U -> add_u8 buf 0xBA
  | F64PromoteF32 -> add_u8 buf 0xBB
  | I32ReinterpretF32 -> add_u8 buf 0xBC
  | I64ReinterpretF64 -> add_u8 buf 0xBD
  | F32ReinterpretI32 -> add_u8 buf 0xBE
  | F64ReinterpretI64 -> add_u8 buf 0xBF

let add_instrs buf instrs =
  List.iter (add_instr buf) instrs

let add_func_type buf ft =
  add_u8 buf 0x60;
  add_vec buf ft.ft_params add_valtype;
  add_vec buf ft.ft_results add_valtype

let add_import buf imp =
  add_string buf imp.i_module;
  add_string buf imp.i_name;
  match imp.i_desc with
  | ImportFunc type_idx ->
      add_u8 buf 0x00;
      add_u32_leb buf type_idx
  | ImportTable ->
      add_u8 buf 0x01;
      add_u8 buf 0x70;
      add_limits buf { lim_min = 0; lim_max = None }
  | ImportMemory ->
      add_u8 buf 0x02;
      add_limits buf { lim_min = 1; lim_max = None }
  | ImportGlobal vt ->
      add_u8 buf 0x03;
      add_valtype buf vt;
      add_u8 buf 0x00

let add_export buf exp =
  add_string buf exp.e_name;
  match exp.e_desc with
  | ExportFunc idx -> add_u8 buf 0x00; add_u32_leb buf idx
  | ExportTable idx -> add_u8 buf 0x01; add_u32_leb buf idx
  | ExportMemory idx -> add_u8 buf 0x02; add_u32_leb buf idx
  | ExportGlobal idx -> add_u8 buf 0x03; add_u32_leb buf idx

let add_global buf g =
  add_valtype buf g.g_type;
  add_u8 buf (if g.g_mutable then 0x01 else 0x00);
  add_instrs buf g.g_init;
  add_u8 buf 0x0B

let add_code buf f =
  let body = Buffer.create 256 in
  add_vec body f.f_locals (fun b l -> add_u32_leb b l.l_count; add_valtype b l.l_type);
  add_instrs body f.f_body;
  add_u8 body 0x0B;
  add_u32_leb buf (Buffer.length body);
  Buffer.add_buffer buf body

let add_elem buf e =
  if e.e_table <> 0 then failwith "Only table 0 supported";
  add_u8 buf 0x00;
  add_u8 buf 0x41; add_sleb32 buf (Int32.of_int e.e_offset); add_u8 buf 0x0B;
  add_vec buf e.e_funcs add_u32_leb

let add_data buf d =
  add_u8 buf 0x00;
  add_u8 buf 0x41; add_sleb32 buf (Int32.of_int d.d_offset); add_u8 buf 0x0B;
  add_u32_leb buf (Bytes.length d.d_data);
  add_bytes buf d.d_data

let add_section buf id builder =
  let sec = Buffer.create 256 in
  builder sec;
  if Buffer.length sec > 0 then (
    add_u8 buf id;
    add_u32_leb buf (Buffer.length sec);
    Buffer.add_buffer buf sec
  )

let write_module_to_file path (m : wasm_module) : unit =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf " asm";
  Buffer.add_string buf "   ";

  add_section buf 1 (fun b -> add_vec b m.types add_func_type);
  add_section buf 2 (fun b -> add_vec b m.imports add_import);
  add_section buf 3 (fun b -> add_vec b m.funcs (fun b f -> add_u32_leb b f.f_type));
  add_section buf 4 (fun b ->
    add_vec b m.tables (fun b t -> add_u8 b 0x70; add_limits b t.tab_type)
  );
  add_section buf 5 (fun b ->
    add_vec b m.mems (fun b mem -> add_limits b mem.mem_type)
  );
  add_section buf 6 (fun b -> add_vec b m.globals add_global);
  add_section buf 7 (fun b -> add_vec b m.exports add_export);
  (match m.start with
   | None -> ()
   | Some idx -> add_section buf 8 (fun b -> add_u32_leb b idx));
  add_section buf 9 (fun b -> add_vec b m.elems add_elem);
  add_section buf 10 (fun b -> add_vec b m.funcs add_code);
  add_section buf 11 (fun b -> add_vec b m.datas add_data);

  let oc = open_out_bin path in
  output_bytes oc (Bytes.of_string (Buffer.contents buf));
  close_out oc
