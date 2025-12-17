(** WebAssembly binary format encoder *)

open Codegen

(** Buffer for building binary output *)
type encoder = {
  mutable buf: bytes;
  mutable pos: int;
}

let create_encoder () = {
  buf = Bytes.create 4096;
  pos = 0;
}

let ensure_capacity enc n =
  let required = enc.pos + n in
  if required > Bytes.length enc.buf then begin
    let new_size = max required (Bytes.length enc.buf * 2) in
    let new_buf = Bytes.create new_size in
    Bytes.blit enc.buf 0 new_buf 0 enc.pos;
    enc.buf <- new_buf
  end

let emit_byte enc b =
  ensure_capacity enc 1;
  Bytes.set_uint8 enc.buf enc.pos b;
  enc.pos <- enc.pos + 1

let emit_bytes enc bs =
  let len = Bytes.length bs in
  ensure_capacity enc len;
  Bytes.blit bs 0 enc.buf enc.pos len;
  enc.pos <- enc.pos + len

let emit_string enc s =
  emit_bytes enc (Bytes.of_string s)

(** LEB128 encoding for unsigned integers *)
let emit_u32_leb128 enc value =
  let rec loop v =
    let byte = v land 0x7f in
    let v' = v lsr 7 in
    if v' = 0 then
      emit_byte enc byte
    else begin
      emit_byte enc (byte lor 0x80);
      loop v'
    end
  in
  loop value

(** LEB128 encoding for signed integers *)
let emit_s32_leb128 enc value =
  let rec loop v =
    let byte = v land 0x7f in
    let v' = v asr 7 in
    let done_ =
      (v' = 0 && (byte land 0x40) = 0) ||
      (v' = -1 && (byte land 0x40) <> 0)
    in
    if done_ then
      emit_byte enc byte
    else begin
      emit_byte enc (byte lor 0x80);
      loop v'
    end
  in
  loop value

let emit_s64_leb128 enc value =
  let rec loop v =
    let byte = Int64.to_int (Int64.logand v 0x7fL) in
    let v' = Int64.shift_right v 7 in
    let done_ =
      (Int64.compare v' 0L = 0 && (byte land 0x40) = 0) ||
      (Int64.compare v' (-1L) = 0 && (byte land 0x40) <> 0)
    in
    if done_ then
      emit_byte enc byte
    else begin
      emit_byte enc (byte lor 0x80);
      loop v'
    end
  in
  loop value

(** Emit a vector (length-prefixed sequence) *)
let emit_vec enc items emit_item =
  emit_u32_leb128 enc (List.length items);
  List.iter (emit_item enc) items

(** Emit a name (length-prefixed UTF-8 string) *)
let emit_name enc name =
  let len = String.length name in
  emit_u32_leb128 enc len;
  emit_string enc name

(** WASM magic number and version *)
let wasm_magic = "\x00\x61\x73\x6d"  (* \0asm *)
let wasm_version = "\x01\x00\x00\x00"  (* version 1 *)

(** Section IDs *)
let section_custom = 0
let section_type = 1
let section_import = 2
let section_function = 3
let section_table = 4
let section_memory = 5
let section_global = 6
let section_export = 7
let section_start = 8
let section_element = 9
let section_code = 10
let section_data = 11

(** Value type encoding *)
let encode_valtype = function
  | I32 -> 0x7f
  | I64 -> 0x7e
  | F32 -> 0x7d
  | F64 -> 0x7c
  | Funcref -> 0x70
  | Externref -> 0x6f

(** Emit a value type *)
let emit_valtype enc ty =
  emit_byte enc (encode_valtype ty)

(** Emit a result type (vector of value types) *)
let emit_resulttype enc types =
  emit_vec enc types (fun e t -> emit_valtype e t)

(** Emit a function type *)
let emit_functype enc ft =
  emit_byte enc 0x60;  (* func type marker *)
  emit_resulttype enc ft.ft_params;
  emit_resulttype enc ft.ft_results

(** Emit limits (for memory/table) *)
let emit_limits enc min max_opt =
  match max_opt with
  | None ->
      emit_byte enc 0x00;
      emit_u32_leb128 enc min
  | Some max ->
      emit_byte enc 0x01;
      emit_u32_leb128 enc min;
      emit_u32_leb128 enc max

(** Emit a memory type *)
let emit_memtype enc (min, max) =
  emit_limits enc min max

(** Emit an import *)
let emit_import enc import =
  match import with
  | ImportFunc (mod_name, func_name, _) ->
      emit_name enc mod_name;
      emit_name enc func_name;
      emit_byte enc 0x00  (* func import *)
      (* Type index would be emitted by caller *)
  | ImportGlobal (mod_name, glob_name, ty, mutable_) ->
      emit_name enc mod_name;
      emit_name enc glob_name;
      emit_byte enc 0x03;  (* global import *)
      emit_valtype enc ty;
      emit_byte enc (if mutable_ then 0x01 else 0x00)
  | ImportMemory (mod_name, mem_name, min, max) ->
      emit_name enc mod_name;
      emit_name enc mem_name;
      emit_byte enc 0x02;  (* memory import *)
      emit_memtype enc (min, max)
  | ImportTable (mod_name, tab_name, min, max) ->
      emit_name enc mod_name;
      emit_name enc tab_name;
      emit_byte enc 0x01;  (* table import *)
      emit_byte enc 0x70;  (* funcref *)
      emit_limits enc min max

(** Emit an export *)
let emit_export enc export =
  match export with
  | ExportFunc (name, idx) ->
      emit_name enc name;
      emit_byte enc 0x00;
      emit_u32_leb128 enc idx
  | ExportGlobal (name, idx) ->
      emit_name enc name;
      emit_byte enc 0x03;
      emit_u32_leb128 enc idx
  | ExportMemory name ->
      emit_name enc name;
      emit_byte enc 0x02;
      emit_u32_leb128 enc 0
  | ExportTable name ->
      emit_name enc name;
      emit_byte enc 0x01;
      emit_u32_leb128 enc 0

(** Instruction opcodes *)
let opcode_unreachable = 0x00
let opcode_nop = 0x01
let opcode_block = 0x02
let opcode_loop = 0x03
let opcode_if = 0x04
let opcode_else = 0x05
let opcode_end = 0x0b
let opcode_br = 0x0c
let opcode_br_if = 0x0d
let opcode_br_table = 0x0e
let opcode_return = 0x0f
let opcode_call = 0x10
let opcode_call_indirect = 0x11

let opcode_drop = 0x1a
let opcode_select = 0x1b

let opcode_local_get = 0x20
let opcode_local_set = 0x21
let opcode_local_tee = 0x22
let opcode_global_get = 0x23
let opcode_global_set = 0x24

let opcode_i32_load = 0x28
let opcode_i64_load = 0x29
let opcode_f32_load = 0x2a
let opcode_f64_load = 0x2b
let opcode_i32_store = 0x36
let opcode_i64_store = 0x37
let opcode_f32_store = 0x38
let opcode_f64_store = 0x39
let opcode_memory_size = 0x3f
let opcode_memory_grow = 0x40

let opcode_i32_const = 0x41
let opcode_i64_const = 0x42
let opcode_f32_const = 0x43
let opcode_f64_const = 0x44

let opcode_i32_eqz = 0x45
let opcode_i32_eq = 0x46
let opcode_i32_ne = 0x47
let opcode_i32_lt_s = 0x48
let opcode_i32_lt_u = 0x49
let opcode_i32_gt_s = 0x4a
let opcode_i32_gt_u = 0x4b
let opcode_i32_le_s = 0x4c
let opcode_i32_le_u = 0x4d
let opcode_i32_ge_s = 0x4e
let opcode_i32_ge_u = 0x4f

let opcode_i64_eqz = 0x50
let opcode_i64_eq = 0x51
let opcode_i64_ne = 0x52
let opcode_i64_lt_s = 0x53
let opcode_i64_lt_u = 0x54
let opcode_i64_gt_s = 0x55
let opcode_i64_gt_u = 0x56
let opcode_i64_le_s = 0x57
let opcode_i64_le_u = 0x58
let opcode_i64_ge_s = 0x59
let opcode_i64_ge_u = 0x5a

let opcode_f32_eq = 0x5b
let opcode_f32_ne = 0x5c
let opcode_f32_lt = 0x5d
let opcode_f32_gt = 0x5e
let opcode_f32_le = 0x5f
let opcode_f32_ge = 0x60

let opcode_f64_eq = 0x61
let opcode_f64_ne = 0x62
let opcode_f64_lt = 0x63
let opcode_f64_gt = 0x64
let opcode_f64_le = 0x65
let opcode_f64_ge = 0x66

let opcode_i32_add = 0x6a
let opcode_i32_sub = 0x6b
let opcode_i32_mul = 0x6c
let opcode_i32_div_s = 0x6d
let opcode_i32_div_u = 0x6e
let opcode_i32_rem_s = 0x6f
let opcode_i32_rem_u = 0x70
let opcode_i32_and = 0x71
let opcode_i32_or = 0x72
let opcode_i32_xor = 0x73
let opcode_i32_shl = 0x74
let opcode_i32_shr_s = 0x75
let opcode_i32_shr_u = 0x76

let opcode_i64_add = 0x7c
let opcode_i64_sub = 0x7d
let opcode_i64_mul = 0x7e
let opcode_i64_div_s = 0x7f
let opcode_i64_div_u = 0x80
let opcode_i64_rem_s = 0x81
let opcode_i64_rem_u = 0x82
let opcode_i64_and = 0x83
let opcode_i64_or = 0x84
let opcode_i64_xor = 0x85
let opcode_i64_shl = 0x86
let opcode_i64_shr_s = 0x87
let opcode_i64_shr_u = 0x88

let opcode_f32_add = 0x92
let opcode_f32_sub = 0x93
let opcode_f32_mul = 0x94
let opcode_f32_div = 0x95
let opcode_f32_neg = 0x8c
let opcode_f32_abs = 0x8b
let opcode_f32_sqrt = 0x91
let opcode_f32_ceil = 0x8d
let opcode_f32_floor = 0x8e
let opcode_f32_trunc = 0x8f

let opcode_f64_add = 0xa0
let opcode_f64_sub = 0xa1
let opcode_f64_mul = 0xa2
let opcode_f64_div = 0xa3
let opcode_f64_neg = 0x9a
let opcode_f64_abs = 0x99
let opcode_f64_sqrt = 0x9f
let opcode_f64_ceil = 0x9b
let opcode_f64_floor = 0x9c
let opcode_f64_trunc = 0x9d

(** Emit block type *)
let emit_blocktype enc = function
  | None -> emit_byte enc 0x40  (* empty block type *)
  | Some I32 -> emit_byte enc 0x7f
  | Some I64 -> emit_byte enc 0x7e
  | Some F32 -> emit_byte enc 0x7d
  | Some F64 -> emit_byte enc 0x7c
  | Some _ -> emit_byte enc 0x40

(** Emit f32 as IEEE 754 *)
let emit_f32 enc f =
  let bits = Int32.bits_of_float f in
  for i = 0 to 3 do
    emit_byte enc (Int32.to_int (Int32.logand (Int32.shift_right_logical bits (i * 8)) 0xffl))
  done

(** Emit f64 as IEEE 754 *)
let emit_f64 enc f =
  let bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    emit_byte enc (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xffL))
  done

(** Emit an instruction *)
let rec emit_instr enc instr =
  match instr with
  | I32Const n -> emit_byte enc opcode_i32_const; emit_s32_leb128 enc n
  | I64Const n -> emit_byte enc opcode_i64_const; emit_s64_leb128 enc n
  | F32Const f -> emit_byte enc opcode_f32_const; emit_f32 enc f
  | F64Const f -> emit_byte enc opcode_f64_const; emit_f64 enc f

  | LocalGet n -> emit_byte enc opcode_local_get; emit_u32_leb128 enc n
  | LocalSet n -> emit_byte enc opcode_local_set; emit_u32_leb128 enc n
  | LocalTee n -> emit_byte enc opcode_local_tee; emit_u32_leb128 enc n
  | GlobalGet n -> emit_byte enc opcode_global_get; emit_u32_leb128 enc n
  | GlobalSet n -> emit_byte enc opcode_global_set; emit_u32_leb128 enc n

  | I32Load (align, offset) ->
      emit_byte enc opcode_i32_load;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | I32Store (align, offset) ->
      emit_byte enc opcode_i32_store;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | I64Load (align, offset) ->
      emit_byte enc opcode_i64_load;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | I64Store (align, offset) ->
      emit_byte enc opcode_i64_store;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | F32Load (align, offset) ->
      emit_byte enc opcode_f32_load;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | F32Store (align, offset) ->
      emit_byte enc opcode_f32_store;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | F64Load (align, offset) ->
      emit_byte enc opcode_f64_load;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset
  | F64Store (align, offset) ->
      emit_byte enc opcode_f64_store;
      emit_u32_leb128 enc align;
      emit_u32_leb128 enc offset

  | MemorySize -> emit_byte enc opcode_memory_size; emit_byte enc 0x00
  | MemoryGrow -> emit_byte enc opcode_memory_grow; emit_byte enc 0x00

  | I32Add -> emit_byte enc opcode_i32_add
  | I32Sub -> emit_byte enc opcode_i32_sub
  | I32Mul -> emit_byte enc opcode_i32_mul
  | I32DivS -> emit_byte enc opcode_i32_div_s
  | I32DivU -> emit_byte enc opcode_i32_div_u
  | I32RemS -> emit_byte enc opcode_i32_rem_s
  | I32RemU -> emit_byte enc opcode_i32_rem_u
  | I32And -> emit_byte enc opcode_i32_and
  | I32Or -> emit_byte enc opcode_i32_or
  | I32Xor -> emit_byte enc opcode_i32_xor
  | I32Shl -> emit_byte enc opcode_i32_shl
  | I32ShrS -> emit_byte enc opcode_i32_shr_s
  | I32ShrU -> emit_byte enc opcode_i32_shr_u
  | I32Eqz -> emit_byte enc opcode_i32_eqz
  | I32Eq -> emit_byte enc opcode_i32_eq
  | I32Ne -> emit_byte enc opcode_i32_ne
  | I32LtS -> emit_byte enc opcode_i32_lt_s
  | I32LtU -> emit_byte enc opcode_i32_lt_u
  | I32GtS -> emit_byte enc opcode_i32_gt_s
  | I32GtU -> emit_byte enc opcode_i32_gt_u
  | I32LeS -> emit_byte enc opcode_i32_le_s
  | I32LeU -> emit_byte enc opcode_i32_le_u
  | I32GeS -> emit_byte enc opcode_i32_ge_s
  | I32GeU -> emit_byte enc opcode_i32_ge_u

  | I64Add -> emit_byte enc opcode_i64_add
  | I64Sub -> emit_byte enc opcode_i64_sub
  | I64Mul -> emit_byte enc opcode_i64_mul
  | I64DivS -> emit_byte enc opcode_i64_div_s
  | I64DivU -> emit_byte enc opcode_i64_div_u
  | I64RemS -> emit_byte enc opcode_i64_rem_s
  | I64RemU -> emit_byte enc opcode_i64_rem_u
  | I64And -> emit_byte enc opcode_i64_and
  | I64Or -> emit_byte enc opcode_i64_or
  | I64Xor -> emit_byte enc opcode_i64_xor
  | I64Shl -> emit_byte enc opcode_i64_shl
  | I64ShrS -> emit_byte enc opcode_i64_shr_s
  | I64ShrU -> emit_byte enc opcode_i64_shr_u
  | I64Eqz -> emit_byte enc opcode_i64_eqz
  | I64Eq -> emit_byte enc opcode_i64_eq
  | I64Ne -> emit_byte enc opcode_i64_ne
  | I64LtS -> emit_byte enc opcode_i64_lt_s
  | I64LtU -> emit_byte enc opcode_i64_lt_u
  | I64GtS -> emit_byte enc opcode_i64_gt_s
  | I64GtU -> emit_byte enc opcode_i64_gt_u
  | I64LeS -> emit_byte enc opcode_i64_le_s
  | I64LeU -> emit_byte enc opcode_i64_le_u
  | I64GeS -> emit_byte enc opcode_i64_ge_s
  | I64GeU -> emit_byte enc opcode_i64_ge_u

  | F32Add -> emit_byte enc opcode_f32_add
  | F32Sub -> emit_byte enc opcode_f32_sub
  | F32Mul -> emit_byte enc opcode_f32_mul
  | F32Div -> emit_byte enc opcode_f32_div
  | F32Eq -> emit_byte enc opcode_f32_eq
  | F32Ne -> emit_byte enc opcode_f32_ne
  | F32Lt -> emit_byte enc opcode_f32_lt
  | F32Gt -> emit_byte enc opcode_f32_gt
  | F32Le -> emit_byte enc opcode_f32_le
  | F32Ge -> emit_byte enc opcode_f32_ge
  | F32Neg -> emit_byte enc opcode_f32_neg
  | F32Abs -> emit_byte enc opcode_f32_abs
  | F32Sqrt -> emit_byte enc opcode_f32_sqrt
  | F32Ceil -> emit_byte enc opcode_f32_ceil
  | F32Floor -> emit_byte enc opcode_f32_floor
  | F32Trunc -> emit_byte enc opcode_f32_trunc

  | F64Add -> emit_byte enc opcode_f64_add
  | F64Sub -> emit_byte enc opcode_f64_sub
  | F64Mul -> emit_byte enc opcode_f64_mul
  | F64Div -> emit_byte enc opcode_f64_div
  | F64Eq -> emit_byte enc opcode_f64_eq
  | F64Ne -> emit_byte enc opcode_f64_ne
  | F64Lt -> emit_byte enc opcode_f64_lt
  | F64Gt -> emit_byte enc opcode_f64_gt
  | F64Le -> emit_byte enc opcode_f64_le
  | F64Ge -> emit_byte enc opcode_f64_ge
  | F64Neg -> emit_byte enc opcode_f64_neg
  | F64Abs -> emit_byte enc opcode_f64_abs
  | F64Sqrt -> emit_byte enc opcode_f64_sqrt
  | F64Ceil -> emit_byte enc opcode_f64_ceil
  | F64Floor -> emit_byte enc opcode_f64_floor
  | F64Trunc -> emit_byte enc opcode_f64_trunc

  (* Conversions *)
  | I32WrapI64 -> emit_byte enc 0xa7
  | I64ExtendI32S -> emit_byte enc 0xac
  | I64ExtendI32U -> emit_byte enc 0xad
  | F32ConvertI32S -> emit_byte enc 0xb2
  | F32ConvertI32U -> emit_byte enc 0xb3
  | F32ConvertI64S -> emit_byte enc 0xb4
  | F32ConvertI64U -> emit_byte enc 0xb5
  | F64ConvertI32S -> emit_byte enc 0xb7
  | F64ConvertI32U -> emit_byte enc 0xb8
  | F64ConvertI64S -> emit_byte enc 0xb9
  | F64ConvertI64U -> emit_byte enc 0xba
  | I32TruncF32S -> emit_byte enc 0xa8
  | I32TruncF32U -> emit_byte enc 0xa9
  | I32TruncF64S -> emit_byte enc 0xaa
  | I32TruncF64U -> emit_byte enc 0xab
  | I64TruncF32S -> emit_byte enc 0xae
  | I64TruncF32U -> emit_byte enc 0xaf
  | I64TruncF64S -> emit_byte enc 0xb0
  | I64TruncF64U -> emit_byte enc 0xb1
  | F32DemoteF64 -> emit_byte enc 0xb6
  | F64PromoteF32 -> emit_byte enc 0xbb
  | I32ReinterpretF32 -> emit_byte enc 0xbc
  | I64ReinterpretF64 -> emit_byte enc 0xbd
  | F32ReinterpretI32 -> emit_byte enc 0xbe
  | F64ReinterpretI64 -> emit_byte enc 0xbf

  (* Control flow *)
  | Unreachable -> emit_byte enc opcode_unreachable
  | Nop -> emit_byte enc opcode_nop

  | Block (ty, instrs) ->
      emit_byte enc opcode_block;
      emit_blocktype enc ty;
      List.iter (emit_instr enc) instrs;
      emit_byte enc opcode_end

  | Loop (ty, instrs) ->
      emit_byte enc opcode_loop;
      emit_blocktype enc ty;
      List.iter (emit_instr enc) instrs;
      emit_byte enc opcode_end

  | If (ty, then_instrs, else_instrs) ->
      emit_byte enc opcode_if;
      emit_blocktype enc ty;
      List.iter (emit_instr enc) then_instrs;
      (match else_instrs with
       | Some instrs ->
           emit_byte enc opcode_else;
           List.iter (emit_instr enc) instrs
       | None -> ());
      emit_byte enc opcode_end

  | Br n -> emit_byte enc opcode_br; emit_u32_leb128 enc n
  | BrIf n -> emit_byte enc opcode_br_if; emit_u32_leb128 enc n
  | BrTable (labels, default) ->
      emit_byte enc opcode_br_table;
      emit_vec enc labels (fun e n -> emit_u32_leb128 e n);
      emit_u32_leb128 enc default
  | Return -> emit_byte enc opcode_return
  | Call n -> emit_byte enc opcode_call; emit_u32_leb128 enc n
  | CallIndirect n -> emit_byte enc opcode_call_indirect; emit_u32_leb128 enc n; emit_byte enc 0x00

  | Drop -> emit_byte enc opcode_drop
  | Select -> emit_byte enc opcode_select

(** Emit a code section entry (function body) *)
let emit_code_entry enc func =
  let body_enc = create_encoder () in
  (* Locals: group by type *)
  let local_groups = if func.fn_locals = [] then [] else
    let rec group acc current_ty count = function
      | [] -> List.rev ((count, current_ty) :: acc)
      | ty :: rest when ty = current_ty ->
          group acc current_ty (count + 1) rest
      | ty :: rest ->
          group ((count, current_ty) :: acc) ty 1 rest
    in
    match func.fn_locals with
    | [] -> []
    | ty :: rest -> group [] ty 1 rest
  in
  emit_vec body_enc local_groups (fun e (count, ty) ->
    emit_u32_leb128 e count;
    emit_valtype e ty
  );
  (* Instructions *)
  List.iter (emit_instr body_enc) func.fn_body;
  emit_byte body_enc opcode_end;
  (* Emit size and body *)
  emit_u32_leb128 enc body_enc.pos;
  emit_bytes enc (Bytes.sub body_enc.buf 0 body_enc.pos)

(** Emit a section *)
let emit_section enc section_id content_fn =
  let content_enc = create_encoder () in
  content_fn content_enc;
  if content_enc.pos > 0 then begin
    emit_byte enc section_id;
    emit_u32_leb128 enc content_enc.pos;
    emit_bytes enc (Bytes.sub content_enc.buf 0 content_enc.pos)
  end

(** Encode a WASM module to binary *)
let encode_module (module_ : wasm_module) =
  let enc = create_encoder () in

  (* Magic and version *)
  emit_string enc wasm_magic;
  emit_string enc wasm_version;

  (* Type section *)
  emit_section enc section_type (fun e ->
    emit_vec e module_.mod_types emit_functype
  );

  (* Import section *)
  if module_.mod_imports <> [] then
    emit_section enc section_import (fun e ->
      emit_u32_leb128 e (List.length module_.mod_imports);
      List.iteri (fun i import ->
        match import with
        | ImportFunc (mod_name, func_name, _) ->
            emit_name e mod_name;
            emit_name e func_name;
            emit_byte e 0x00;  (* func import *)
            emit_u32_leb128 e i  (* type index - simplified *)
        | _ -> emit_import e import
      ) module_.mod_imports
    );

  (* Function section (type indices) *)
  let num_imports = List.length (List.filter (function ImportFunc _ -> true | _ -> false) module_.mod_imports) in
  emit_section enc section_function (fun e ->
    emit_vec e module_.mod_funcs (fun e' _fn ->
      emit_u32_leb128 e' 0  (* type index - simplified *)
    )
  );

  (* Memory section *)
  (match module_.mod_memory with
   | Some (min, max) ->
       emit_section enc section_memory (fun e ->
         emit_u32_leb128 e 1;  (* 1 memory *)
         emit_memtype e (min, max)
       )
   | None -> ());

  (* Export section *)
  if module_.mod_exports <> [] then
    emit_section enc section_export (fun e ->
      emit_vec e module_.mod_exports (fun e' export ->
        match export with
        | ExportFunc (name, idx) ->
            emit_name e' name;
            emit_byte e' 0x00;
            emit_u32_leb128 e' (num_imports + idx)
        | ExportMemory name ->
            emit_name e' name;
            emit_byte e' 0x02;
            emit_u32_leb128 e' 0
        | _ -> emit_export e' export
      )
    );

  (* Code section *)
  emit_section enc section_code (fun e ->
    emit_vec e module_.mod_funcs emit_code_entry
  );

  (* Data section *)
  if module_.mod_data <> [] then
    emit_section enc section_data (fun e ->
      emit_vec e module_.mod_data (fun e' (offset, data) ->
        emit_byte e' 0x00;  (* active, memory 0 *)
        emit_byte e' opcode_i32_const;
        emit_s32_leb128 e' offset;
        emit_byte e' opcode_end;
        emit_u32_leb128 e' (String.length data);
        emit_string e' data
      )
    );

  Bytes.sub enc.buf 0 enc.pos

(** Write module to file *)
let write_to_file filename module_ =
  let binary = encode_module module_ in
  let oc = open_out_bin filename in
  output_bytes oc binary;
  close_out oc
