(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Shortest round-trip [float_to_string] for core WASM (#643).

    The output matches ECMAScript [Number.prototype.toString] (Bun's
    [String(x)]) and the interpreter ({!Float_format}): the fewest
    significant digits that read back to the same double, the closest
    such digits when several qualify, then the JS layout ("1e+21",
    "0.000001", "1e-7", "NaN", "-Infinity", "0" for both zeros).

    Digits come from Burger & Dybvig's free-format algorithm ("Printing
    Floating-Point Numbers Quickly and Accurately", PLDI 1996) run on
    fixed 40-limb (1280-bit) bignums in linear memory, which holds every
    intermediate for the full double range. No tables and no host import.

    The printer is a handful of helper functions appended once to a
    module that uses it. A call site emits [Call call_sentinel]; the
    module assembler patches that to the real index ({!patch_calls}).

    Memory: the result string is written at the heap pointer B
    ([len:i32][bytes], at most 25 bytes). Scratch digits sit at B+48 and
    the bignums R, S, M+, M-, T at B+80 .. B+880. Only the string is
    kept: the heap pointer ends just past it, so a call leaks nothing. *)

open Wasm

(** Placeholder index a call site uses for the printer's entry point. *)
let call_sentinel = -643

let limbs = 40
let limb_bytes = limbs * 4

(* Helper order: the entry point first, so [base] is its index. *)
let i_f2s = 0
let i_mul10 = 1
let i_add = 2
let i_sub = 3
let i_cmp = 4
let i_set = 5

(* addr = p + i*4, for limb [i] of bignum [p] *)
let limb_addr p i = [LocalGet p; LocalGet i; I32Const 2l; I32Shl; I32Add]

(* p *= 10.  params p; locals i, carry:i64, t:i64 *)
let mul10_body =
  let p, i, carry, t = 0, 1, 2, 3 in
  [ I64Const 0L; LocalSet carry; I32Const 0l; LocalSet i;
    Loop (BtEmpty,
      limb_addr p i
      @ limb_addr p i
      @ [ I64Load32U (2, 0); I64Const 10L; I64Mul; LocalGet carry; I64Add;
          LocalTee t; I64Store32 (2, 0);
          LocalGet t; I64Const 32L; I64ShrU; LocalSet carry;
          LocalGet i; I32Const 1l; I32Add; LocalTee i;
          I32Const (Int32.of_int limbs); I32LtU; BrIf 0 ]) ]

(* dst = a + b (dst may alias a or b).  params dst, a, b; locals i, carry, t *)
let add_body =
  let dst, a, b, i, carry, t = 0, 1, 2, 3, 4, 5 in
  [ I64Const 0L; LocalSet carry; I32Const 0l; LocalSet i;
    Loop (BtEmpty,
      limb_addr dst i
      @ limb_addr a i @ [I64Load32U (2, 0)]
      @ limb_addr b i @ [I64Load32U (2, 0)]
      @ [ I64Add; LocalGet carry; I64Add; LocalTee t; I64Store32 (2, 0);
          LocalGet t; I64Const 32L; I64ShrU; LocalSet carry;
          LocalGet i; I32Const 1l; I32Add; LocalTee i;
          I32Const (Int32.of_int limbs); I32LtU; BrIf 0 ]) ]

(* a -= b, requires a >= b.  params a, b; locals i, borrow, t *)
let sub_body =
  let a, b, i, borrow, t = 0, 1, 2, 3, 4 in
  [ I64Const 0L; LocalSet borrow; I32Const 0l; LocalSet i;
    Loop (BtEmpty,
      limb_addr a i
      @ limb_addr a i @ [I64Load32U (2, 0)]
      @ limb_addr b i @ [I64Load32U (2, 0)]
      @ [ I64Sub; LocalGet borrow; I64Sub; LocalTee t; I64Store32 (2, 0);
          (* t is negative exactly when this limb borrowed *)
          LocalGet t; I64Const 63L; I64ShrU; LocalSet borrow;
          LocalGet i; I32Const 1l; I32Add; LocalTee i;
          I32Const (Int32.of_int limbs); I32LtU; BrIf 0 ]) ]

(* sign(a - b) as -1 / 0 / 1.  params a, b; locals i, x, y *)
let cmp_body =
  let a, b, i, x, y = 0, 1, 2, 3, 4 in
  [ I32Const (Int32.of_int (limbs - 1)); LocalSet i;
    Loop (BtEmpty,
      limb_addr a i @ [I32Load (2, 0); LocalSet x]
      @ limb_addr b i @ [I32Load (2, 0); LocalSet y]
      @ [ LocalGet x; LocalGet y; I32Ne;
          If (BtEmpty,
              [ I32Const 1l; I32Const (-1l);
                LocalGet x; LocalGet y; I32GtU; Select; Return ],
              [Nop]);
          LocalGet i; I32Const 1l; I32Sub; LocalTee i;
          I32Const 0l; I32GeS; BrIf 0 ]);
    I32Const 0l ]

(* p = f * 2^n, f < 2^54.  params p, f:i64, n; locals i, addr, bit *)
let set_body =
  let p, f, n, i, addr, bit = 0, 1, 2, 3, 4, 5 in
  [ I32Const 0l; LocalSet i;
    Loop (BtEmpty,
      limb_addr p i
      @ [ I32Const 0l; I32Store (2, 0);
          LocalGet i; I32Const 1l; I32Add; LocalTee i;
          I32Const (Int32.of_int limbs); I32LtU; BrIf 0 ]);
    LocalGet p; LocalGet n; I32Const 5l; I32ShrU; I32Const 2l; I32Shl;
    I32Add; LocalSet addr;
    LocalGet n; I32Const 31l; I32And; LocalSet bit;
    LocalGet addr; LocalGet f; LocalGet bit; I64ExtendI32U; I64Shl;
    I64Store (3, 0);
    LocalGet bit;
    If (BtEmpty,
        [ LocalGet addr;
          LocalGet f; I64Const 64L; LocalGet bit; I64ExtendI32U; I64Sub;
          I64ShrU; I32WrapI64; I32Store (2, 8) ],
        [Nop]) ]

let f2s_body ~base ~heap =
  let call i = Call (base + i) in
  (* locals: 0 = x (param) *)
  let bits, f = 1, 2 in                          (* i64 *)
  let neg, bexp, e, sh, even, k = 3, 4, 5, 6, 7, 8 in
  let bb, b, r, s, mp, mm, t = 9, 10, 11, 12, 13, 14, 15 in
  let d, nd, w, i, tc1, tc2, ex, dig, hi = 16, 17, 18, 19, 20, 21, 22, 23, 24 in
  let aa = 25 in
  let put_code code =                    (* store the i32 on top, w++ *)
    [LocalGet w] @ code @ [I32Store8 (0, 0);
     LocalGet w; I32Const 1l; I32Add; LocalSet w] in
  let put c = put_code [I32Const (Int32.of_int (Char.code c))] in
  let put_str str = List.concat_map put (List.of_seq (String.to_seq str)) in
  let mul10_all = [LocalGet r; call i_mul10; LocalGet mp; call i_mul10;
                   LocalGet mm; call i_mul10] in
  (* while (cnt > 0) { body; cnt-- } with cnt in local [i] *)
  let repeat body =
    [ Block (BtEmpty, [ Loop (BtEmpty,
        [ LocalGet i; I32Const 0l; I32LeS; BrIf 1 ] @ body
        @ [ LocalGet i; I32Const 1l; I32Sub; LocalSet i; Br 0 ]) ]) ] in
  (* emit digits dig[i .. hi) as ASCII, i and hi already set *)
  let copy_digits =
    [ Block (BtEmpty, [ Loop (BtEmpty,
        [ LocalGet i; LocalGet hi; I32GeS; BrIf 1 ]
        @ put_code [LocalGet dig; LocalGet i; I32Add; I32Load8U (0, 0);
                    I32Const 48l; I32Add]
        @ [ LocalGet i; I32Const 1l; I32Add; LocalSet i; Br 0 ]) ]) ] in
  let digits_range lo hi_code =
    lo @ [LocalSet i] @ hi_code @ [LocalSet hi] @ copy_digits in
  let zeros cnt_code = cnt_code @ [LocalSet i] @ repeat (put '0') in
  (* "is c > 0, or c = 0 when the mantissa is even" for c on the stack *)
  let gt_or_even_eq c =
    [ LocalTee c; I32Const 0l; I32GtS;
      LocalGet c; I32Eqz; LocalGet even; I32And; I32Or ] in
  let general =
    [ LocalGet neg; If (BtEmpty, put '-', [Nop]);
      (* sh: the gap below v is half the gap above (f = 2^52, bexp > 1) *)
      LocalGet f; I64Eqz; LocalGet bexp; I32Const 1l; I32GtU; I32And;
      LocalSet sh;
      LocalGet bexp; I32Eqz;
      If (BtEmpty,
          [ I32Const (-1074l); LocalSet e ],
          [ LocalGet f; I64Const 0x10000000000000L; I64Or; LocalSet f;
            LocalGet bexp; I32Const 1075l; I32Sub; LocalSet e ]);
      LocalGet f; I32WrapI64; I32Const 1l; I32And; I32Eqz; LocalSet even;
      (* aa = max(e, 0), bb = max(-e, 0) *)
      LocalGet e; I32Const 0l; LocalGet e; I32Const 0l; I32GtS; Select;
      LocalSet aa;
      I32Const 0l; LocalGet e; I32Sub; I32Const 0l;
      LocalGet e; I32Const 0l; I32LtS; Select; LocalSet bb;
      (* r = f 2^(aa+1+sh), s = 2^(bb+1+sh), m+ = 2^(aa+sh), m- = 2^aa *)
      LocalGet r; LocalGet f;
      LocalGet aa; I32Const 1l; I32Add; LocalGet sh; I32Add; call i_set;
      LocalGet s; I64Const 1L;
      LocalGet bb; I32Const 1l; I32Add; LocalGet sh; I32Add; call i_set;
      LocalGet mp; I64Const 1L; LocalGet aa; LocalGet sh; I32Add; call i_set;
      LocalGet mm; I64Const 1L; LocalGet aa; call i_set;
      (* k = ceil(floor(log2 v) log10 2 - 1e-10): exact or one too small *)
      LocalGet e; I32Const 63l; I32Add; LocalGet f; I64Clz; I32WrapI64;
      I32Sub; F64ConvertI32S; F64Const 0.30102999566398114; F64Mul;
      F64Const 1e-10; F64Sub; F64Ceil; I32TruncF64S; LocalSet k;
      LocalGet k; I32Const 0l; I32GeS;
      If (BtEmpty,
          [LocalGet k; LocalSet i] @ repeat [LocalGet s; call i_mul10],
          [I32Const 0l; LocalGet k; I32Sub; LocalSet i] @ repeat mul10_all);
      (* fixup: if v's upper bound reaches 10^k the estimate was low *)
      LocalGet t; LocalGet r; LocalGet mp; call i_add;
      LocalGet t; LocalGet s; call i_cmp ] @ gt_or_even_eq tc2 @
    [ If (BtEmpty,
          [ LocalGet k; I32Const 1l; I32Add; LocalSet k ],
          mul10_all);
      (* digit generation *)
      I32Const 0l; LocalSet nd;
      Loop (BtEmpty,
        [ I32Const 0l; LocalSet d;
          Block (BtEmpty, [ Loop (BtEmpty,
            [ LocalGet r; LocalGet s; call i_cmp; I32Const 0l; I32LtS; BrIf 1;
              LocalGet r; LocalGet s; call i_sub;
              LocalGet d; I32Const 1l; I32Add; LocalSet d; Br 0 ]) ]);
          (* tc1: r < m- (<= when even); tc2: r + m+ > s (>= when even) *)
          LocalGet mm; LocalGet r; call i_cmp ]
        @ gt_or_even_eq tc1 @ [ LocalSet tc1;
          LocalGet t; LocalGet r; LocalGet mp; call i_add;
          LocalGet t; LocalGet s; call i_cmp ]
        @ gt_or_even_eq tc2 @ [ LocalSet tc2;
          LocalGet tc1; LocalGet tc2; I32Or; I32Eqz;
          If (BtEmpty,
              [ LocalGet dig; LocalGet nd; I32Add; LocalGet d; I32Store8 (0, 0);
                LocalGet nd; I32Const 1l; I32Add; LocalSet nd ]
              @ mul10_all @ [ Br 1 ],
              [Nop]);
          (* last digit: round up when only the upper bound is near, or
             when both are and r is over half of s (an exact half goes to
             the even digit, as Bun does) *)
          LocalGet tc1; LocalGet tc2; I32And;
          If (BtEmpty,
              [ LocalGet t; LocalGet r; LocalGet r; call i_add;
                LocalGet d; LocalGet t; LocalGet s; call i_cmp; LocalTee hi;
                I32Const 0l; I32GtS; LocalGet hi; I32Eqz;
                LocalGet d; I32Const 1l; I32And; I32And; I32Or;
                I32Add; LocalSet d ],
              [ LocalGet d; LocalGet tc2; I32Add; LocalSet d ]);
          LocalGet dig; LocalGet nd; I32Add; LocalGet d; I32Store8 (0, 0);
          LocalGet nd; I32Const 1l; I32Add; LocalSet nd ]);
      (* layout, per Number::toString with n = k, digit count nd *)
      Block (BtEmpty,
        [ (* nd <= k <= 21: digits then zeros *)
          LocalGet nd; LocalGet k; I32LeS; LocalGet k; I32Const 21l; I32LeS;
          I32And;
          If (BtEmpty,
              digits_range [I32Const 0l] [LocalGet nd]
              @ zeros [LocalGet k; LocalGet nd; I32Sub] @ [Br 1],
              [Nop]);
          (* 0 < k <= 21: digits with a point inside *)
          LocalGet k; I32Const 0l; I32GtS; LocalGet k; I32Const 21l; I32LeS;
          I32And;
          If (BtEmpty,
              digits_range [I32Const 0l] [LocalGet k] @ put '.'
              @ digits_range [LocalGet k] [LocalGet nd] @ [Br 1],
              [Nop]);
          (* -6 < k <= 0: 0.000ddd *)
          LocalGet k; I32Const (-6l); I32GtS; LocalGet k; I32Const 0l; I32LeS;
          I32And;
          If (BtEmpty,
              put_str "0." @ zeros [I32Const 0l; LocalGet k; I32Sub]
              @ digits_range [I32Const 0l] [LocalGet nd] @ [Br 1],
              [Nop]) ]
        (* exponent form: d[.ddd]e±x *)
        @ digits_range [I32Const 0l] [I32Const 1l]
        @ [ LocalGet nd; I32Const 1l; I32GtS;
            If (BtEmpty,
                put '.' @ digits_range [I32Const 1l] [LocalGet nd], [Nop]) ]
        @ put 'e'
        @ [ LocalGet k; I32Const 1l; I32Sub; LocalTee ex; I32Const 0l; I32LtS;
            If (BtEmpty,
                put '-' @ [I32Const 0l; LocalGet ex; I32Sub; LocalSet ex],
                put '+');
            LocalGet ex; I32Const 100l; I32GeS;
            If (BtEmpty,
                put_code [LocalGet ex; I32Const 100l; I32DivU; I32Const 48l;
                          I32Add], [Nop]);
            LocalGet ex; I32Const 10l; I32GeS;
            If (BtEmpty,
                put_code [LocalGet ex; I32Const 10l; I32DivU; I32Const 10l;
                          I32RemU; I32Const 48l; I32Add], [Nop]) ]
        @ put_code [LocalGet ex; I32Const 10l; I32RemU; I32Const 48l; I32Add]) ]
  in
  [ GlobalGet heap; LocalSet b;
    LocalGet b; I32Const 4l; I32Add; LocalSet w;
    LocalGet b; I32Const 48l; I32Add; LocalSet dig;
    LocalGet b; I32Const 80l; I32Add; LocalSet r;
    LocalGet r; I32Const (Int32.of_int limb_bytes); I32Add; LocalSet s;
    LocalGet s; I32Const (Int32.of_int limb_bytes); I32Add; LocalSet mp;
    LocalGet mp; I32Const (Int32.of_int limb_bytes); I32Add; LocalSet mm;
    LocalGet mm; I32Const (Int32.of_int limb_bytes); I32Add; LocalSet t;
    LocalGet 0; I64ReinterpretF64; LocalSet bits;
    LocalGet bits; I64Const 63L; I64ShrU; I32WrapI64; LocalSet neg;
    LocalGet bits; I64Const 52L; I64ShrU; I32WrapI64; I32Const 0x7ffl; I32And;
    LocalSet bexp;
    LocalGet bits; I64Const 0xFFFFFFFFFFFFFL; I64And; LocalSet f;
    Block (BtEmpty,
      [ LocalGet bexp; I32Const 0x7ffl; I32Eq;
        If (BtEmpty,
            [ LocalGet f; I64Eqz;
              If (BtEmpty,
                  [ LocalGet neg; If (BtEmpty, put '-', [Nop]) ]
                  @ put_str "Infinity",
                  put_str "NaN");
              Br 1 ],
            [Nop]);
        (* both zeros print as "0" *)
        LocalGet bits; I64Const 0x7FFFFFFFFFFFFFFFL; I64And; I64Eqz;
        If (BtEmpty, put '0' @ [Br 1], [Nop]) ]
      @ general);
    LocalGet b; LocalGet w; LocalGet b; I32Sub; I32Const 4l; I32Sub;
    I32Store (2, 0);
    LocalGet w; GlobalSet heap;
    LocalGet b ]

(** The printer's functions, to append at function index [base], with
    [heap] the bump-allocator global. [intern] gives a type index. *)
let funcs ~base ~heap ~(intern : func_type -> int) : func list =
  let fn params results n_i32 n_i64 body =
    { f_type = intern { ft_params = params; ft_results = results };
      f_locals =
        List.filter (fun l -> l.l_count > 0)
          [ { l_count = n_i32; l_type = I32 }; { l_count = n_i64; l_type = I64 } ];
      f_body = body } in
  [ (* f2s: 2 i64 locals first (bits, f), then the i32s *)
    { f_type = intern { ft_params = [F64]; ft_results = [I32] };
      f_locals = [ { l_count = 2; l_type = I64 }; { l_count = 23; l_type = I32 } ];
      f_body = f2s_body ~base ~heap };
    fn [I32] [] 1 2 mul10_body;
    fn [I32; I32; I32] [] 1 2 add_body;
    fn [I32; I32] [] 1 2 sub_body;
    fn [I32; I32] [I32] 3 0 cmp_body;
    fn [I32; I64; I32] [] 3 0 set_body ]

(** Replace [Call call_sentinel] with [Call target] throughout [body]. *)
let rec patch_calls target (body : instr list) : instr list =
  List.map (function
    | Call n when n = call_sentinel -> Call target
    | Block (bt, b) -> Block (bt, patch_calls target b)
    | Loop (bt, b) -> Loop (bt, patch_calls target b)
    | If (bt, a, b) -> If (bt, patch_calls target a, patch_calls target b)
    | i -> i) body

let rec body_calls_sentinel (body : instr list) : bool =
  List.exists (function
    | Call n -> n = call_sentinel
    | Block (_, b) | Loop (_, b) -> body_calls_sentinel b
    | If (_, a, b) -> body_calls_sentinel a || body_calls_sentinel b
    | _ -> false) body
