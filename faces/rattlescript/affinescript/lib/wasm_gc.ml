(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** WebAssembly GC proposal intermediate representation.

    Extends the WASM 1.0 IR ({!Wasm}) with types and instructions defined
    by the WebAssembly GC proposal (phase 4, merged into spec 2023).

    AffineScript types map to GC types as follows:
    - Int / Bool / Char / Nat  →  [GcPrim I32] in struct fields
    - Float                    →  [GcPrim F64] in struct fields
    - String                   →  [GcRef (HtConcrete string_array_idx)]
    - Array[T]                 →  [GcRef (HtConcrete array_type_idx)]
    - Record / Struct          →  [GcRef (HtConcrete struct_type_idx)]
    - Tuple                    →  [GcRef (HtConcrete tuple_type_idx)]
    - Variant                  →  [GcRef (HtConcrete variant_type_idx)]

    Binary encoding:   {!Wasm_gc_encode}
    Code generation:   {!Codegen_gc}
    Spec reference:    https://webassembly.github.io/gc/core/binary/
*)

(** {1 Primitive value types} *)

(** The four primitive WASM value types (same as in {!Wasm}). *)
type prim_valtype =
  | I32  (** 32-bit integer *)
  | I64  (** 64-bit integer *)
  | F32  (** 32-bit float *)
  | F64  (** 64-bit float *)
[@@deriving show, eq]

(** {1 Reference / heap types} *)

(** Heap types classify what a GC reference points to.
    Abstract heap types sit at the top of the type hierarchy;
    concrete heap types are indices into the type section. *)
type heap_type =
  | HtAny             (** any  — root of the reference type hierarchy *)
  | HtEq              (** eq   — types supporting structural equality *)
  | HtI31             (** i31  — unboxed 31-bit integers *)
  | HtStruct          (** struct — any struct (abstract supertype) *)
  | HtArray           (** array  — any array  (abstract supertype) *)
  | HtFunc            (** func   — any function reference *)
  | HtNone            (** none   — bottom of non-func/non-extern hierarchy *)
  | HtNoFunc          (** nofunc — bottom of the func hierarchy *)
  | HtNoExtern        (** noextern — bottom of the extern hierarchy *)
  | HtConcrete of int (** concrete type index (non-negative) *)
[@@deriving show, eq]

(** GC-aware value types — extends primitive types with reference types. *)
type gc_valtype =
  | GcPrim    of prim_valtype  (** i32 / i64 / f32 / f64 *)
  | GcRef     of heap_type     (** (ref ht)      — non-null reference *)
  | GcRefNull of heap_type     (** (ref null ht) — nullable reference *)
  | GcAnyref                   (** anyref    = (ref null any)    shorthand *)
  | GcEqref                    (** eqref     = (ref null eq)     shorthand *)
  | GcI31ref                   (** i31ref    = (ref null i31)    shorthand *)
  | GcStructref                (** structref = (ref null struct) shorthand *)
  | GcArrayref                 (** arrayref  = (ref null array)  shorthand *)
[@@deriving show, eq]

(** {1 Type definitions} *)

(** Storage type for struct / array fields.
    Can be a full value type or a packed integer type (i8 / i16). *)
type storage_type =
  | StVal of gc_valtype  (** full value type *)
  | StI8                 (** packed i8  — sign-extended on struct.get_s *)
  | StI16                (** packed i16 — sign-extended on struct.get_s *)
[@@deriving show, eq]

(** Field type: storage type + mutability flag. *)
type field_type = {
  ft_storage : storage_type;
  ft_mutable : bool;  (** true = mutable (var); false = immutable (const) *)
}
[@@deriving show, eq]

(** Struct type: an ordered sequence of fields.
    Field names are tracked separately in {!Codegen_gc} — the binary
    format only carries field indices. *)
type struct_type = field_type list
[@@deriving show, eq]

(** Array element type (a single field_type). *)
type array_type = field_type
[@@deriving show, eq]

(** Function type using GC value types.
    Used in the type section for GC-aware function signatures. *)
type gc_func_type = {
  gft_params  : gc_valtype list;
  gft_results : gc_valtype list;
}
[@@deriving show, eq]

(** A type definition entry in the GC type section. *)
type gc_type_def =
  | GcStructType of struct_type   (** struct { field* } *)
  | GcArrayType  of array_type    (** array { field }   *)
  | GcFuncType   of gc_func_type  (** func (params) (results) *)
[@@deriving show, eq]

(** {1 Instructions} *)

(** Block result type for GC-aware blocks.
    [GcBtRef ht] covers blocks that return a reference. *)
type gc_blocktype =
  | GcBtEmpty           (** void block — no result *)
  | GcBtPrim of prim_valtype  (** block returning a primitive *)
  | GcBtRef  of heap_type     (** block returning a (ref null ht) *)
[@@deriving show, eq]

(** GC instructions, extending the WASM 1.0 instruction set.

    [Std instr] wraps any standard WASM 1.0 instruction for use in GC
    function bodies.  All GC-specific instructions have dedicated constructors
    and are encoded with a [0xFB] prefix opcode.

    Spec: https://webassembly.github.io/gc/core/binary/instructions.html *)
type gc_instr =

  (* ── Standard WASM 1.0 instructions (wrapped for use in GC bodies) ──── *)

  | Std of Wasm.instr
    (** Any WASM 1.0 instruction: LocalGet, I32Add, Call, Return, etc.
        Encoded identically to the WASM 1.0 binary format. *)

  (* ── Struct instructions (0xFB prefix) ─────────────────────────────── *)

  | StructNew of int
    (** [struct.new typeidx] — allocate struct; all fields from stack
        (TOS = last declared field).  Consumes N values; leaves a ref. *)

  | StructNewDefault of int
    (** [struct.new_default typeidx] — allocate struct with zero / null defaults. *)

  | StructGet of int * int
    (** [struct.get typeidx fieldidx] — read field from struct ref.
        Consumes (ref struct); leaves the field value. *)

  | StructGetS of int * int
    (** [struct.get_s typeidx fieldidx] — read packed field, sign-extended. *)

  | StructGetU of int * int
    (** [struct.get_u typeidx fieldidx] — read packed field, zero-extended. *)

  | StructSet of int * int
    (** [struct.set typeidx fieldidx] — write to a mutable struct field.
        Consumes (ref struct, value); leaves nothing. *)

  (* ── Array instructions (0xFB prefix) ──────────────────────────────── *)

  | ArrayNew of int
    (** [array.new typeidx] — allocate array of given length, all elements
        initialised to a provided value.  Consumes (init_val, length). *)

  | ArrayNewDefault of int
    (** [array.new_default typeidx] — allocate zero / null-initialized array.
        Consumes (length: i32). *)

  | ArrayNewFixed of int * int
    (** [array.new_fixed typeidx n] — allocate array from N stack values.
        Values must be on the stack TOS = element[n-1]. *)

  | ArrayGet of int
    (** [array.get typeidx] — load element at index.
        Consumes (ref array, index: i32); leaves element value. *)

  | ArrayGetS of int
    (** [array.get_s typeidx] — load packed element, sign-extended. *)

  | ArrayGetU of int
    (** [array.get_u typeidx] — load packed element, zero-extended. *)

  | ArraySet of int
    (** [array.set typeidx] — store element at index.
        Consumes (ref array, index: i32, value); leaves nothing. *)

  | ArrayLen
    (** [array.len] — get array length.
        Consumes (ref array); leaves i32 length. *)

  | ArrayCopy of int * int
    (** [array.copy dst_typeidx src_typeidx] — bulk element copy
        between two arrays. *)

  (* ── Reference instructions ─────────────────────────────────────────── *)

  | RefNull of heap_type
    (** [ref.null ht] — push a null reference of given heap type. *)

  | RefIsNull
    (** [ref.is_null] — test if reference is null: ref → i32. *)

  | RefTest of heap_type
    (** [ref.test (ref ht)] — non-null cast test: ref → i32 (1=match). *)

  | RefTestNull of heap_type
    (** [ref.test (ref null ht)] — nullable cast test: ref → i32. *)

  | RefCast of heap_type
    (** [ref.cast (ref ht)] — non-null downcast; traps on failure. *)

  | RefCastNull of heap_type
    (** [ref.cast (ref null ht)] — nullable downcast; traps on non-null mismatch. *)

  | RefI31
    (** [ref.i31] — box i32 into i31ref.  Consumes i32; leaves i31ref. *)

  | I31GetS
    (** [i31.get_s] — unbox i31ref with sign extension: i31ref → i32. *)

  | I31GetU
    (** [i31.get_u] — unbox i31ref with zero extension: i31ref → i32. *)

  (* ── GC-aware control flow wrappers ─────────────────────────────────── *)

  | GcBlock of gc_blocktype * gc_instr list
    (** Block whose result type may be a reference. *)

  | GcLoop of gc_blocktype * gc_instr list
    (** Loop with a GC block type. *)

  | GcIf of gc_blocktype * gc_instr list * gc_instr list
    (** If/else with a GC block type. *)

[@@deriving show, eq]

(** {1 Module-level types} *)

(** A GC-aware function definition. *)
type gc_func = {
  gf_type   : int;                     (** index into type section — must reference GcFuncType *)
  gf_locals : (int * gc_valtype) list; (** (count, type) pairs for additional locals beyond params *)
  gf_body   : gc_instr list;
}
[@@deriving show, eq]

(** Import descriptor for a GC module. *)
type gc_import_desc =
  | GcImportFunc   of int  (** function type index *)
  | GcImportMemory         (** linear memory (optional in GC modules) *)
[@@deriving show, eq]

(** A GC module import. *)
type gc_import = {
  gi_module : string;
  gi_name   : string;
  gi_desc   : gc_import_desc;
}
[@@deriving show, eq]

(** Export descriptor for a GC module. *)
type gc_export_desc =
  | GcExportFunc   of int  (** function index *)
  | GcExportMemory of int  (** memory index *)
[@@deriving show, eq]

(** A GC module export. *)
type gc_export = {
  ge_name : string;
  ge_desc : gc_export_desc;
}
[@@deriving show, eq]

(** A complete WasmGC module.

    Sections emitted by {!Wasm_gc_encode.write_gc_module_to_file}:
    - Type section (1): GC type definitions
    - Import section (2): imports
    - Function section (3): type indices
    - Export section (7): named exports
    - Code section (10): function bodies *)
type gc_module = {
  gc_types   : gc_type_def list;   (** ordered type section entries *)
  gc_imports : gc_import list;     (** imports (functions, memories) *)
  gc_funcs   : gc_func list;       (** function definitions *)
  gc_exports : gc_export list;     (** exports *)
}
[@@deriving show, eq]

(** {1 Convenience constructors} *)

(** Empty GC module. *)
let empty_gc_module () : gc_module = {
  gc_types   = [];
  gc_imports = [];
  gc_funcs   = [];
  gc_exports = [];
}

(** Immutable i32 struct field. *)
let field_i32 : field_type = {
  ft_storage = StVal (GcPrim I32);
  ft_mutable = false;
}

(** Immutable f64 struct field. *)
let field_f64 : field_type = {
  ft_storage = StVal (GcPrim F64);
  ft_mutable = false;
}

(** Mutable anyref struct field (stores any GC value). *)
let field_anyref_mut : field_type = {
  ft_storage = StVal GcAnyref;
  ft_mutable = true;
}

(** Immutable anyref struct field. *)
let field_anyref : field_type = {
  ft_storage = StVal GcAnyref;
  ft_mutable = false;
}
