(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** CharacterSelect TEA Bridge Wasm Generator.

    Generates a valid WebAssembly 1.0 module that implements the
    AffineScript TEA runtime ABI for the CharacterSelectScreen.

    The bridge module stores CharacterSelectModel in linear memory at a fixed
    layout, and exports clean i32 functions that JS can call to drive
    a PixiJS scene without needing a full AffineScript → Wasm compiler.

    {2 Memory layout}

    Model state is stored starting at byte offset 64 (matching the TitleScreen
    layout convention) with the following field layout:

    {v
      Offset  Type  Field         Default
      +0      i32   screen_w      1280
      +4      i32   screen_h      720
      +8      i32   bgm_playing   0    (unused; kept for API compatibility)
      +12     i32   selected_tag  0
    v}

    {2 selected_tag encoding}

    {v
      0 = none (no class chosen yet)
      1 = Assault
      2 = Recon
      3 = Engineer (Combat Engineer)
      4 = Signals
      5 = Medic
      6 = Logistics
      7 = confirmed  (navigate to JessicaCustomise)
    v}

    {2 Msg tag encoding (input to affinescript_update)}

    {v
      0 = SelectAssault
      1 = SelectRecon
      2 = SelectEngineer
      3 = SelectSignals
      4 = SelectMedic
      5 = SelectLogistics
      6 = Confirm
    v}

    The update function is branchless: [selected_tag := msg + 1].
    This maps SelectAssault=0 → 1, …, Confirm=6 → 7 (navigate).

    {2 Exported functions}

    Identical API surface to the TitleScreen bridge so the same
    [AffineTEA.js] / [AffineTEA.res] bindings work without modification:

    {ul
      {li [affinescript_init()]}
      {li [affinescript_update(msg: i32)]}
      {li [affinescript_get_screen_w() -> i32]}
      {li [affinescript_get_screen_h() -> i32]}
      {li [affinescript_get_bgm_playing() -> i32]}
      {li [affinescript_get_selected() -> i32]}
      {li [affinescript_set_screen(w: i32, h: i32)]}
      {li [memory]}
    }

    {2 Ownership annotations}

    The [affinescript.ownership] custom section marks [update]'s [msg]
    parameter as Linear (kind byte 1) — consumed exactly once per TEA
    update cycle — encoding the AffineScript linearity invariant for
    typed-wasm Level 10 verification.

    A companion [affinescript.tea_layout] custom section encodes the
    model field layout for tooling.
*)

open Wasm

(** Base address of the CharacterSelectModel in linear memory. *)
let model_base = 64

(** Field offsets relative to [model_base]. *)
let off_screen_w    = 0
let off_screen_h    = 4
let off_bgm_playing = 8
let off_selected    = 12

(** [load_field off] — Wasm instructions that load an i32 from
    [(model_base + off)], leaving the value on the stack. *)
let load_field off : instr list = [
  I32Const (Int32.of_int (model_base + off));
  I32Load (2, 0);
]

(** [store_const off v] — Wasm instructions that store constant [v]
    to [(model_base + off)]. *)
let store_const off v : instr list = [
  I32Const (Int32.of_int (model_base + off));
  I32Const (Int32.of_int v);
  I32Store (2, 0);
]

(* -------------------------------------------------------------------------
   Type section
   -------------------------------------------------------------------------
   Index  Signature              Used by
   0      () -> ()               fn_init
   1      (i32) -> ()            fn_update
   2      () -> i32              fn_get_screen_w/_h/_bgm/_selected
   3      (i32, i32) -> ()       fn_set_screen
   ------------------------------------------------------------------------- *)

let types : func_type list = [
  { ft_params = [];           ft_results = [] };
  { ft_params = [I32];        ft_results = [] };
  { ft_params = [];           ft_results = [I32] };
  { ft_params = [I32; I32];   ft_results = [] };
]

(* -------------------------------------------------------------------------
   Function bodies
   ------------------------------------------------------------------------- *)

(** fn 0: affinescript_init() — write default CharacterSelectModel to memory. *)
let fn_init : func = {
  f_type   = 0;
  f_locals = [];
  f_body   =
    store_const off_screen_w    1280 @
    store_const off_screen_h    720  @
    store_const off_bgm_playing 0    @
    store_const off_selected    0;
}

(** fn 1: affinescript_update(msg: i32) — branchless update:
    [selected_tag := msg + 1].  msg is Linear (consumed exactly once).

    msg 0 → 1 (Assault selected)
    msg 1 → 2 (Recon selected)
    msg 2 → 3 (Engineer selected)
    msg 3 → 4 (Signals selected)
    msg 4 → 5 (Medic selected)
    msg 5 → 6 (Logistics selected)
    msg 6 → 7 (confirmed — navigate to JessicaCustomise) *)
let fn_update : func = {
  f_type   = 1;
  f_locals = [];
  f_body   = [
    (* address of selected_tag *)
    I32Const (Int32.of_int (model_base + off_selected));
    (* compute msg + 1 *)
    LocalGet 0;
    I32Const 1l;
    I32Add;
    I32Store (2, 0);
  ];
}

(** fn 2: affinescript_get_screen_w() -> i32 *)
let fn_get_screen_w : func = {
  f_type   = 2;
  f_locals = [];
  f_body   = load_field off_screen_w;
}

(** fn 3: affinescript_get_screen_h() -> i32 *)
let fn_get_screen_h : func = {
  f_type   = 2;
  f_locals = [];
  f_body   = load_field off_screen_h;
}

(** fn 4: affinescript_get_bgm_playing() -> i32 *)
let fn_get_bgm_playing : func = {
  f_type   = 2;
  f_locals = [];
  f_body   = load_field off_bgm_playing;
}

(** fn 5: affinescript_get_selected() -> i32 *)
let fn_get_selected : func = {
  f_type   = 2;
  f_locals = [];
  f_body   = load_field off_selected;
}

(** fn 6: affinescript_set_screen(w: i32, h: i32) — store new dimensions.
    Handles PixiJS resize events by updating the model. *)
let fn_set_screen : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = [
    I32Const (Int32.of_int (model_base + off_screen_w));
    LocalGet 0;
    I32Store (2, 0);
    I32Const (Int32.of_int (model_base + off_screen_h));
    LocalGet 1;
    I32Store (2, 0);
  ];
}

(* -------------------------------------------------------------------------
   Custom sections
   ------------------------------------------------------------------------- *)

(** Build the [affinescript.ownership] custom section payload.

    Identical encoding to the TitleScreen bridge — only [fn_update]'s msg
    param is Linear; all other params and returns are Unrestricted. *)
let build_ownership_section () : bytes =
  let buf = Buffer.create 64 in
  let u32 n =
    Buffer.add_char buf (Char.chr  (n         land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr  8) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
  in
  let u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
  u32 7;  (* 7 annotated functions *)
  (* fn 0 init: () → (), no params, Unrestricted return *)
  u32 0; u8 0; u8 0;
  (* fn 1 update: (msg: Linear) → (), return Unrestricted *)
  u32 1; u8 1; u8 1 (* Linear=1 *); u8 0;
  (* fn 2-5 getters: () → i32, Unrestricted *)
  u32 2; u8 0; u8 0;
  u32 3; u8 0; u8 0;
  u32 4; u8 0; u8 0;
  u32 5; u8 0; u8 0;
  (* fn 6 set_screen: (i32, i32) → (), both Unrestricted *)
  u32 6; u8 2; u8 0; u8 0; u8 0;
  Buffer.to_bytes buf

(** Build the [affinescript.tea_layout] custom section.

    Compact binary descriptor for the CharacterSelectModel memory layout:
    {v
      u8  version      = 1
      u8  base_addr    = 64
      u8  field_count  = 4
      per field: u8 name_len, name_bytes, u8 offset, u8 type_tag (0x49=i32)
    v} *)
let build_tea_layout_section () : bytes =
  let buf = Buffer.create 64 in
  let u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
  let field name off =
    u8 (String.length name);
    Buffer.add_string buf name;
    u8 off;
    u8 0x49  (* i32 type tag *)
  in
  u8 1;           (* version 1 *)
  u8 model_base;  (* base = 64 *)
  u8 4;           (* 4 fields *)
  field "screen_w"    off_screen_w;
  field "screen_h"    off_screen_h;
  field "bgm_playing" off_bgm_playing;
  field "selected"    off_selected;
  Buffer.to_bytes buf

(* -------------------------------------------------------------------------
   Module assembly
   ------------------------------------------------------------------------- *)

(** Generate the complete TEA bridge Wasm module for CharacterSelectScreen.

    The resulting module is suitable for use with AffineTEA.js in IDApTIK.
    It shares the same exported function names and memory layout as the
    TitleScreen bridge, so the same ReScript/JS bindings work unchanged.

    selected_tag semantics after update(msg):
    - 1-6: one of the six operative backgrounds is highlighted
    - 7: player confirmed — navigate to JessicaCustomiseScreen

    Write with [Wasm_encode.write_module_to_file]. *)
let generate () : wasm_module = {
  types;
  funcs = [
    fn_init;
    fn_update;
    fn_get_screen_w;
    fn_get_screen_h;
    fn_get_bgm_playing;
    fn_get_selected;
    fn_set_screen;
  ];
  tables  = [];
  mems    = [{ mem_type = { lim_min = 1; lim_max = None } }];
  globals = [];
  imports = [];
  elems   = [];
  datas   = [];
  start   = None;
  exports = [
    { e_name = "affinescript_init";           e_desc = ExportFunc 0 };
    { e_name = "affinescript_update";          e_desc = ExportFunc 1 };
    { e_name = "affinescript_get_screen_w";    e_desc = ExportFunc 2 };
    { e_name = "affinescript_get_screen_h";    e_desc = ExportFunc 3 };
    { e_name = "affinescript_get_bgm_playing"; e_desc = ExportFunc 4 };
    { e_name = "affinescript_get_selected";    e_desc = ExportFunc 5 };
    { e_name = "affinescript_set_screen";      e_desc = ExportFunc 6 };
    { e_name = "memory";                       e_desc = ExportMemory 0 };
  ];
  custom_sections = [
    ("affinescript.ownership",   build_ownership_section ());
    ("affinescript.tea_layout",  build_tea_layout_section ());
  ];
}
