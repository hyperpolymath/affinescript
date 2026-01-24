(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WebAssembly intermediate representation.

    This module defines a simplified WASM AST for code generation from AffineScript.
    Based on the WebAssembly 1.0 specification.
*)

(** WebAssembly value types *)
type value_type =
  | I32  (** 32-bit integer *)
  | I64  (** 64-bit integer *)
  | F32  (** 32-bit float *)
  | F64  (** 64-bit float *)
[@@deriving show, eq]

(** WebAssembly block types *)
type block_type =
  | BtEmpty
  | BtType of value_type
[@@deriving show, eq]

(** WebAssembly instructions *)
type instr =
  (* Control flow *)
  | Unreachable
  | Nop
  | Block of block_type * instr list
  | Loop of block_type * instr list
  | If of block_type * instr list * instr list  (** then, else *)
  | Br of int  (** branch to label *)
  | BrIf of int  (** conditional branch *)
  | Return
  | Call of int  (** function index *)
  | CallIndirect of int  (** type index *)

  (* Parametric instructions *)
  | Drop
  | Select

  (* Variable access *)
  | LocalGet of int  (** local variable index *)
  | LocalSet of int
  | LocalTee of int  (** set and keep value on stack *)
  | GlobalGet of int
  | GlobalSet of int

  (* Memory instructions *)
  | I32Load of int * int  (** align, offset *)
  | I64Load of int * int
  | F32Load of int * int
  | F64Load of int * int
  | I32Store of int * int
  | I64Store of int * int
  | F32Store of int * int
  | F64Store of int * int
  | MemorySize
  | MemoryGrow

  (* Numeric constants *)
  | I32Const of int32
  | I64Const of int64
  | F32Const of float
  | F64Const of float

  (* I32 operations *)
  | I32Eqz
  | I32Eq | I32Ne
  | I32LtS | I32LtU
  | I32GtS | I32GtU
  | I32LeS | I32LeU
  | I32GeS | I32GeU

  | I32Clz | I32Ctz | I32Popcnt
  | I32Add | I32Sub | I32Mul
  | I32DivS | I32DivU
  | I32RemS | I32RemU
  | I32And | I32Or | I32Xor
  | I32Shl | I32ShrS | I32ShrU
  | I32Rotl | I32Rotr

  (* I64 operations *)
  | I64Eqz
  | I64Eq | I64Ne
  | I64LtS | I64LtU
  | I64GtS | I64GtU
  | I64LeS | I64LeU
  | I64GeS | I64GeU

  | I64Clz | I64Ctz | I64Popcnt
  | I64Add | I64Sub | I64Mul
  | I64DivS | I64DivU
  | I64RemS | I64RemU
  | I64And | I64Or | I64Xor
  | I64Shl | I64ShrS | I64ShrU
  | I64Rotl | I64Rotr

  (* F32 operations *)
  | F32Eq | F32Ne
  | F32Lt | F32Gt
  | F32Le | F32Ge

  | F32Abs | F32Neg | F32Ceil | F32Floor
  | F32Trunc | F32Nearest | F32Sqrt
  | F32Add | F32Sub | F32Mul | F32Div
  | F32Min | F32Max | F32Copysign

  (* F64 operations *)
  | F64Eq | F64Ne
  | F64Lt | F64Gt
  | F64Le | F64Ge

  | F64Abs | F64Neg | F64Ceil | F64Floor
  | F64Trunc | F64Nearest | F64Sqrt
  | F64Add | F64Sub | F64Mul | F64Div
  | F64Min | F64Max | F64Copysign

  (* Conversions *)
  | I32WrapI64
  | I64ExtendI32S | I64ExtendI32U
  | I32TruncF32S | I32TruncF32U
  | I32TruncF64S | I32TruncF64U
  | I64TruncF32S | I64TruncF32U
  | I64TruncF64S | I64TruncF64U
  | F32ConvertI32S | F32ConvertI32U
  | F32ConvertI64S | F32ConvertI64U
  | F32DemoteF64
  | F64ConvertI32S | F64ConvertI32U
  | F64ConvertI64S | F64ConvertI64U
  | F64PromoteF32
  | I32ReinterpretF32
  | I64ReinterpretF64
  | F32ReinterpretI32
  | F64ReinterpretI64
[@@deriving show, eq]

(** Function type *)
type func_type = {
  ft_params : value_type list;
  ft_results : value_type list;
}
[@@deriving show, eq]

(** Function locals *)
type local = {
  l_count : int;
  l_type : value_type;
}
[@@deriving show, eq]

(** Function definition *)
type func = {
  f_type : int;  (** index into type section *)
  f_locals : local list;
  f_body : instr list;
}
[@@deriving show, eq]

(** Export descriptor *)
type export_desc =
  | ExportFunc of int
  | ExportTable of int
  | ExportMemory of int
  | ExportGlobal of int
[@@deriving show, eq]

(** Export *)
type export = {
  e_name : string;
  e_desc : export_desc;
}
[@@deriving show, eq]

(** Import descriptor *)
type import_desc =
  | ImportFunc of int  (** type index *)
  | ImportTable
  | ImportMemory
  | ImportGlobal of value_type
[@@deriving show, eq]

(** Import *)
type import = {
  i_module : string;
  i_name : string;
  i_desc : import_desc;
}
[@@deriving show, eq]

(** Global definition *)
type global = {
  g_type : value_type;
  g_mutable : bool;
  g_init : instr list;
}
[@@deriving show, eq]

(** Memory limits *)
type limits = {
  lim_min : int;
  lim_max : int option;
}
[@@deriving show, eq]

(** Memory definition *)
type memory = {
  mem_type : limits;
}
[@@deriving show, eq]

(** Table definition *)
type table = {
  tab_type : limits;
}
[@@deriving show, eq]

(** Element segment (for initializing tables) *)
type elem = {
  e_table : int;         (** table index *)
  e_offset : int;        (** offset in table *)
  e_funcs : int list;    (** function indices *)
}
[@@deriving show, eq]

(** WebAssembly module *)
type wasm_module = {
  types : func_type list;
  funcs : func list;
  tables : table list;
  mems : memory list;
  globals : global list;
  exports : export list;
  imports : import list;
  elems : elem list;   (** element segments for table initialization *)
  start : int option;  (** optional start function index *)
}
[@@deriving show, eq]

(** Create an empty WASM module *)
let empty_module () : wasm_module = {
  types = [];
  funcs = [];
  tables = [];
  mems = [];
  globals = [];
  exports = [];
  imports = [];
  elems = [];
  start = None;
}
