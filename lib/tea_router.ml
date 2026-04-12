(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** TEA Router Wasm Generator.

    Generates a WebAssembly 1.0 module that implements the AffineScript
    Cadre Router ABI for IDApTIK.

    The router holds IDApTIK's screen back-stack as an affine resource.
    Pushing or popping a screen consumes the old stack linearly and produces
    a new one — navigation history as a linear type, making the typed-wasm
    paper claim real.

    {2 Memory layout}

    RouterModel at byte offset 64:
    {v
      Offset   Type        Field         Default
      +0       i32         screen_w      1280
      +4       i32         screen_h      720
      +8       i32         stack_len     0
      +12      i32         popup_tag     -1  (none)
      +16      i32[8]      stack_data    [0 × 8]
    v}

    Maximum stack depth is 8, giving headroom above the 5 IDApTIK screen types.
    Total footprint: 4 × 4 + 8 × 4 = 48 bytes (model_base 64, so up to byte 112).

    {2 Screen tag encoding}

    {v
      0 = Title
      1 = CharacterSelect
      2 = WorldMap
      3 = Load
      4 = Game
    v}

    {2 Popup tag encoding}

    {v
      -1 = none (no active popup)
       0 = Settings
       1 = Inventory
       2 = Hacking
    v}

    {2 RouterMsg encoding (ownership section annotation)}

    Each exported function corresponds to one RouterMsg variant.  The
    payload parameters are annotated Linear (kind byte = 1) in the
    [affinescript.ownership] custom section, encoding that each message is
    consumed exactly once per TEA update cycle:

    {v
      Push(screen_tag)      fn 1 — screen_tag: Linear
      Pop                   fn 2 — no payload
      PresentPopup(popup)   fn 3 — popup_tag: Linear
      DismissPopup          fn 4 — no payload
      Resize(w, h)          fn 5 — w: Linear, h: Linear
    v}

    Getters (fn 6–10) have no payload parameters.

    {2 Exported functions}

    {ul
      {li [affinescript_router_init()]}
      {li [affinescript_router_push(screen_tag: i32)]}
      {li [affinescript_router_pop()]}
      {li [affinescript_router_present_popup(popup_tag: i32)]}
      {li [affinescript_router_dismiss_popup()]}
      {li [affinescript_router_resize(w: i32, h: i32)]}
      {li [affinescript_router_get_screen_w() -> i32]}
      {li [affinescript_router_get_screen_h() -> i32]}
      {li [affinescript_router_get_stack_len() -> i32]}
      {li [affinescript_router_get_stack_top() -> i32]}
      {li [affinescript_router_get_popup_tag() -> i32]}
      {li [memory]}
    }
*)

open Wasm

(** Base address of the RouterModel in linear memory. *)
let model_base     = 64

(** Field offsets relative to [model_base]. *)
let off_screen_w   = 0
let off_screen_h   = 4
let off_stack_len  = 8
let off_popup_tag  = 12
let off_stack_data = 16   (** 8 × i32 = 32 bytes *)

(** Maximum number of screens on the back-stack. *)
let max_stack_depth = 8

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

(** [store_local off local_idx] — Wasm instructions that store
    [local[local_idx]] to [(model_base + off)]. *)
let store_local off local_idx : instr list = [
  I32Const (Int32.of_int (model_base + off));
  LocalGet local_idx;
  I32Store (2, 0);
]

(* -------------------------------------------------------------------------
   Type section
   -------------------------------------------------------------------------
   Index  Signature              Used by
   0      () -> ()               fn_init, fn_pop, fn_dismiss_popup
   1      (i32) -> ()            fn_push, fn_present_popup
   2      (i32, i32) -> ()       fn_resize
   3      () -> i32              fn_get_screen_w/_h/stack_len/stack_top/popup_tag
   ------------------------------------------------------------------------- *)

let types : func_type list = [
  { ft_params = [];           ft_results = [] };
  { ft_params = [I32];        ft_results = [] };
  { ft_params = [I32; I32];   ft_results = [] };
  { ft_params = [];           ft_results = [I32] };
]

(* -------------------------------------------------------------------------
   Function bodies
   ------------------------------------------------------------------------- *)

(** fn 0: affinescript_router_init() — write default RouterModel to memory.

    Initialises all four scalar fields and zeros the eight stack slots. *)
let fn_init : func = {
  f_type   = 0;
  f_locals = [];
  f_body   =
    store_const off_screen_w  1280 @
    store_const off_screen_h  720  @
    store_const off_stack_len 0    @
    store_const off_popup_tag (-1) @
    List.concat (List.init max_stack_depth (fun i ->
      store_const (off_stack_data + i * 4) 0));
}

(** fn 1: affinescript_router_push(screen_tag: i32) — push [screen_tag] onto
    the back-stack if the stack is not full.

    Encoding: [stack_data[stack_len] := screen_tag; stack_len += 1].
    [screen_tag] is Linear — consumed exactly once per TEA update cycle.

    When the stack is full the screen_tag is explicitly dropped via [Drop]
    rather than left in an unused-param else-branch.  This satisfies the
    per-path linearity verifier: both branches consume param 0 exactly once
    (store in the then-branch, explicit drop in the else-branch), giving
    min_uses = 1, max_uses = 1 → OK. *)
let fn_push : func = {
  f_type   = 1;
  f_locals = [];
  f_body   = [
    (* condition: stack_len < max_stack_depth *)
    I32Const (Int32.of_int (model_base + off_stack_len));
    I32Load (2, 0);
    I32Const (Int32.of_int max_stack_depth);
    I32LtS;
    If (BtEmpty,
      (* then: append screen_tag and bump len *)
      [
        (* address = stack_data_base + (stack_len * 4) *)
        I32Const (Int32.of_int (model_base + off_stack_data));
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Load (2, 0);
        I32Const 4l;
        I32Mul;
        I32Add;
        (* value: screen_tag (param 0) *)
        LocalGet 0;
        I32Store (2, 0);
        (* stack_len += 1 *)
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Load (2, 0);
        I32Const 1l;
        I32Add;
        I32Store (2, 0);
      ],
      (* else: stack full — explicitly consume screen_tag (ownership discharged) *)
      [ LocalGet 0; Drop ]
    );
  ];
}

(** fn 2: affinescript_router_pop() — pop the top screen if non-empty.

    Encoding: [if stack_len > 0 then stack_len -= 1].  The popped slot is
    not zeroed (it becomes dead data above the new top). *)
let fn_pop : func = {
  f_type   = 0;
  f_locals = [];
  f_body   = [
    (* condition: stack_len > 0 *)
    I32Const (Int32.of_int (model_base + off_stack_len));
    I32Load (2, 0);
    I32Const 0l;
    I32GtS;
    If (BtEmpty,
      (* then: stack_len -= 1 *)
      [
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Load (2, 0);
        I32Const 1l;
        I32Sub;
        I32Store (2, 0);
      ],
      []
    );
  ];
}

(** fn 3: affinescript_router_present_popup(popup_tag: i32) — activate popup.

    Stores [popup_tag] directly.  Any previous popup is silently replaced
    (caller should [dismiss_popup] first for clean semantics).
    [popup_tag] is Linear — consumed exactly once. *)
let fn_present_popup : func = {
  f_type   = 1;
  f_locals = [];
  f_body   = store_local off_popup_tag 0;
}

(** fn 4: affinescript_router_dismiss_popup() — clear the active popup.

    Stores -1 to [popup_tag] (= no active popup). *)
let fn_dismiss_popup : func = {
  f_type   = 0;
  f_locals = [];
  f_body   = store_const off_popup_tag (-1);
}

(** fn 5: affinescript_router_resize(w: i32, h: i32) — update screen dimensions.

    Both parameters are Linear — the Resize message is consumed exactly once. *)
let fn_resize : func = {
  f_type   = 2;
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

(** fn 6: affinescript_router_get_screen_w() -> i32 *)
let fn_get_screen_w : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = load_field off_screen_w;
}

(** fn 7: affinescript_router_get_screen_h() -> i32 *)
let fn_get_screen_h : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = load_field off_screen_h;
}

(** fn 8: affinescript_router_get_stack_len() -> i32 *)
let fn_get_stack_len : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = load_field off_stack_len;
}

(** fn 9: affinescript_router_get_stack_top() -> i32

    Returns the screen tag at the top of the stack (the current screen),
    or [-1] if the stack is empty.

    Encoding:
    {v
      if stack_len == 0: -1
      else: stack_data[stack_len - 1]
    v} *)
let fn_get_stack_top : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = [
    (* load stack_len *)
    I32Const (Int32.of_int (model_base + off_stack_len));
    I32Load (2, 0);
    (* branch on stack_len == 0 *)
    I32Eqz;
    If (BtType I32,
      (* then: empty stack → -1 *)
      [ I32Const (-1l) ],
      (* else: load stack_data[stack_len - 1] *)
      [
        I32Const (Int32.of_int (model_base + off_stack_data));
        (* compute index: (stack_len - 1) * 4 *)
        I32Const (Int32.of_int (model_base + off_stack_len));
        I32Load (2, 0);
        I32Const 1l;
        I32Sub;
        I32Const 4l;
        I32Mul;
        (* address = stack_data_base + index *)
        I32Add;
        I32Load (2, 0);
      ]
    );
  ];
}

(** fn 10: affinescript_router_get_popup_tag() -> i32

    Returns the active popup tag, or [-1] if no popup is active. *)
let fn_get_popup_tag : func = {
  f_type   = 3;
  f_locals = [];
  f_body   = load_field off_popup_tag;
}

(* -------------------------------------------------------------------------
   Custom sections
   ------------------------------------------------------------------------- *)

(** Build the [affinescript.ownership] custom section payload.

    Encoding (all little-endian):
    {v
      u32  entry_count
      per entry:
        u32  func_index
        u8   param_count
        u8*  param_kind  (0=Unrestricted 1=Linear 2=SharedBorrow 3=ExclBorrow)
        u8   return_kind
    v}

    RouterMsg payload parameters that are Linear:
    - fn 1 (push): screen_tag (1 param, Linear)
    - fn 3 (present_popup): popup_tag (1 param, Linear)
    - fn 5 (resize): w, h (2 params, both Linear)
    All other functions carry no payload (0 params, Unrestricted return). *)
let build_ownership_section () : bytes =
  let buf = Buffer.create 128 in
  let u32 n =
    Buffer.add_char buf (Char.chr  (n         land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr  8) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
  in
  let u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
  u32 11;  (* 11 annotated functions *)
  (* fn 0 init: () → (), no params *)
  u32 0; u8 0; u8 0;
  (* fn 1 push: (screen_tag: Linear) → () *)
  u32 1; u8 1; u8 1; u8 0;
  (* fn 2 pop: () → () *)
  u32 2; u8 0; u8 0;
  (* fn 3 present_popup: (popup_tag: Linear) → () *)
  u32 3; u8 1; u8 1; u8 0;
  (* fn 4 dismiss_popup: () → () *)
  u32 4; u8 0; u8 0;
  (* fn 5 resize: (w: Linear, h: Linear) → () *)
  u32 5; u8 2; u8 1; u8 1; u8 0;
  (* fn 6–10 getters: () → i32, no param annotations *)
  u32 6;  u8 0; u8 0;
  u32 7;  u8 0; u8 0;
  u32 8;  u8 0; u8 0;
  u32 9;  u8 0; u8 0;
  u32 10; u8 0; u8 0;
  Buffer.to_bytes buf

(** Build the [affinescript.tea_layout] custom section.

    Compact binary descriptor for the RouterModel memory layout:
    {v
      u8  version    = 1
      u8  base_addr  = 64
      u8  field_count = 5
      per field: u8 name_len, name_bytes, u8 offset, u8 type_tag
        0x49 = i32 scalar
        0x41 = array of i32
    v} *)
let build_tea_layout_section () : bytes =
  let buf = Buffer.create 64 in
  let u8 n = Buffer.add_char buf (Char.chr (n land 0xff)) in
  let field name off type_tag =
    u8 (String.length name);
    Buffer.add_string buf name;
    u8 off;
    u8 type_tag
  in
  u8 1;           (* version 1 *)
  u8 model_base;  (* base = 64 *)
  u8 5;           (* 5 fields *)
  field "screen_w"    off_screen_w   0x49;   (* i32 *)
  field "screen_h"    off_screen_h   0x49;
  field "stack_len"   off_stack_len  0x49;
  field "popup_tag"   off_popup_tag  0x49;
  field "stack_data"  off_stack_data 0x41;   (* array of i32, 8 slots *)
  Buffer.to_bytes buf

(* -------------------------------------------------------------------------
   Module assembly
   ------------------------------------------------------------------------- *)

(** Generate the complete TEA Router Wasm module for IDApTIK's Cadre Router.

    The resulting module is suitable for use with AffineTEARouter.js.
    Write it with [Wasm_encode.write_module_to_file]. *)
let generate () : wasm_module = {
  types;
  funcs = [
    fn_init;
    fn_push;
    fn_pop;
    fn_present_popup;
    fn_dismiss_popup;
    fn_resize;
    fn_get_screen_w;
    fn_get_screen_h;
    fn_get_stack_len;
    fn_get_stack_top;
    fn_get_popup_tag;
  ];
  tables  = [];
  mems    = [{ mem_type = { lim_min = 1; lim_max = None } }];
  globals = [];
  imports = [];
  elems   = [];
  datas   = [];
  start   = None;
  exports = [
    { e_name = "affinescript_router_init";          e_desc = ExportFunc 0  };
    { e_name = "affinescript_router_push";          e_desc = ExportFunc 1  };
    { e_name = "affinescript_router_pop";           e_desc = ExportFunc 2  };
    { e_name = "affinescript_router_present_popup"; e_desc = ExportFunc 3  };
    { e_name = "affinescript_router_dismiss_popup"; e_desc = ExportFunc 4  };
    { e_name = "affinescript_router_resize";        e_desc = ExportFunc 5  };
    { e_name = "affinescript_router_get_screen_w";  e_desc = ExportFunc 6  };
    { e_name = "affinescript_router_get_screen_h";  e_desc = ExportFunc 7  };
    { e_name = "affinescript_router_get_stack_len"; e_desc = ExportFunc 8  };
    { e_name = "affinescript_router_get_stack_top"; e_desc = ExportFunc 9  };
    { e_name = "affinescript_router_get_popup_tag"; e_desc = ExportFunc 10 };
    { e_name = "memory";                            e_desc = ExportMemory 0 };
  ];
  custom_sections = [
    ("affinescript.ownership",  build_ownership_section ());
    ("affinescript.tea_layout", build_tea_layout_section ());
  ];
}
