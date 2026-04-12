(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath *)

(** typed-wasm ownership verifier — Stage 7 (per-path analysis added Stage 9).

    Statically verifies typed-wasm Level 7 (aliasing safety) and Level 10
    (linearity) constraints on AffineScript's Wasm IR.

    The verifier runs after codegen on the in-memory [Wasm.wasm_module]
    together with the ownership annotations collected during codegen.
    It is a second line of defence: the QTT quantity checker (Stage 1) already
    enforces these rules at the AffineScript source level; this module
    re-checks them on the emitted Wasm IR to catch any codegen bugs.

    Level 10 — Linearity: a parameter annotated [Linear] (TyOwn) must be
    loaded exactly once on every execution path.  Zero uses = dropped;
    more than one use = possible duplication; non-zero use on some paths
    but zero on others = partial drop (dropped on the zero-count path).

    Level 7 — Aliasing safety: a parameter annotated [ExclBorrow] (TyMut)
    must be loaded at most once on any execution path.

    [SharedBorrow] (TyRef) and [Unrestricted] params are not checked.

    Branch semantics (per-path min/max): for [If] instructions we compute
    [(min_then, max_then)] and [(min_else, max_else)] independently and
    combine as [(min_then, max_then, min_else, max_else) →
    (min min_then min_else, max max_then max_else)].  A Linear param used
    exactly once in BOTH branches gives (1, 1) — OK.  A param used in the
    then-branch only gives (min=0, max=1) — [LinearDroppedOnSomePath].

    @see typed-wasm LEVEL-STATUS.md — Level 7 and Level 10 sections.
*)

open Codegen

(* ============================================================================
   Error types
   ============================================================================ *)

(** An ownership violation found in a Wasm function body. *)
type ownership_error =
  | LinearNotUsed of { func_idx : int; param_idx : int }
  (** Level 10: Linear parameter was never loaded on any path — dropped without
      consumption.  The owned resource leaks unconditionally. *)
  | LinearDroppedOnSomePath of { func_idx : int; param_idx : int }
  (** Level 10: Linear parameter is consumed on some execution paths but
      silently dropped on others (per-path min_uses = 0, max_uses >= 1).
      The caller's ownership guarantee is satisfied only conditionally —
      the drop path leaks the resource. *)
  | LinearUsedMultiple of { func_idx : int; param_idx : int; count : int }
  (** Level 10: Linear parameter was loaded [count] times on some path —
      potential duplication.  Only one consumer is permitted. *)
  | ExclBorrowAliased of { func_idx : int; param_idx : int; count : int }
  (** Level 7: ExclBorrow parameter was loaded [count] times — aliasing violation.
      An exclusive borrow must not have multiple simultaneous references. *)

(* ============================================================================
   Per-path use-range analysis
   ============================================================================ *)

(** Compute [(min_uses, max_uses)] — the minimum and maximum number of times
    [local_idx] is loaded across all execution paths within [instrs].

    Sequential instructions sum their ranges: if instr A uses the param
    (1, 1) times and instr B uses it (0, 1) times, the sequence is (1, 2).

    For [If] nodes only one branch executes, so we take the component-wise
    min/max of the two branch ranges:
      - [min = min(then_min, else_min)]: may be zero if one branch doesn't use it
      - [max = max(then_max, else_max)]: the worst-case duplication count

    Example: [If { LocalGet 0 } { LocalGet 0 }] → [(1, 1)] OK for Linear.
    Example: [If { LocalGet 0 } { [] }]         → [(0, 1)] LinearDroppedOnSomePath.
    Example: [If { LocalGet 0; LocalGet 0 } { LocalGet 0 }] → [(1, 2)]
      → LinearUsedMultiple (max = 2).

    Nested [Block] and [Loop] bodies are descended into (no new paths). *)
let rec count_uses_range (local_idx : int) (instrs : Wasm.instr list) : int * int =
  List.fold_left (fun (a_min, a_max) instr ->
    let (i_min, i_max) = uses_range_in_instr local_idx instr in
    (a_min + i_min, a_max + i_max)
  ) (0, 0) instrs

and uses_range_in_instr (local_idx : int) (instr : Wasm.instr) : int * int =
  match instr with
  | Wasm.LocalGet n -> if n = local_idx then (1, 1) else (0, 0)
  | Wasm.Block (_, body) | Wasm.Loop (_, body) ->
    count_uses_range local_idx body
  | Wasm.If (_, then_, else_) ->
    let (t_min, t_max) = count_uses_range local_idx then_ in
    let (e_min, e_max) = count_uses_range local_idx else_ in
    (min t_min e_min, max t_max e_max)
  | _ -> (0, 0)

(* ============================================================================
   Per-function verification
   ============================================================================ *)

(** Verify ownership constraints for one function.

    [func] is the Wasm function body.
    [param_kinds] are the ownership annotations for each parameter (in order).
    [func_idx] is the global function index (for error reporting).
    Returns all violations found (empty list = clean).

    Uses per-path min/max analysis:
    - [LinearNotUsed]: max_uses = 0 (param dropped on every path)
    - [LinearDroppedOnSomePath]: min_uses = 0, max_uses >= 1 (dropped conditionally)
    - [LinearUsedMultiple]: max_uses > 1 (duplicated on some path)
    Multiple violations can be reported for a single param (e.g. both
    [LinearDroppedOnSomePath] and [LinearUsedMultiple] if min=0, max>1). *)
let verify_function
    (func     : Wasm.func)
    (param_kinds : ownership_kind list)
    (func_idx : int)
  : ownership_error list =
  List.concat_map (fun (param_idx, kind) ->
    let (min_uses, max_uses) = count_uses_range param_idx func.Wasm.f_body in
    match kind with
    | Linear ->
      (* Exactly once on every path: zero everywhere = dropped; zero on some
         path = partial drop; more than one on some path = may duplicate. *)
      let drop_errors =
        if max_uses = 0 then [LinearNotUsed { func_idx; param_idx }]
        else if min_uses = 0 then [LinearDroppedOnSomePath { func_idx; param_idx }]
        else []
      in
      let dup_errors =
        if max_uses > 1 then [LinearUsedMultiple { func_idx; param_idx; count = max_uses }]
        else []
      in
      drop_errors @ dup_errors
    | ExclBorrow ->
      (* At most once on any path: max > 1 creates simultaneous aliases. *)
      if max_uses > 1 then
        [ExclBorrowAliased { func_idx; param_idx; count = max_uses }]
      else
        []
    | Unrestricted | SharedBorrow ->
      (* No constraints on these ownership kinds. *)
      []
  ) (List.mapi (fun i k -> (i, k)) param_kinds)

(* ============================================================================
   Module-level verification
   ============================================================================ *)

(** Verify ownership constraints across an entire Wasm module.

    [wasm_mod] is the compiled module.
    [annots] is the list of [(func_idx, param_kinds, ret_kind)] annotations
    collected by codegen and stored in the [affinescript.ownership] custom
    section (but here provided in structured form directly from [Codegen]).

    Imported functions have no body and are skipped.
    Returns all violations found. *)
let verify_module
    (wasm_mod : Wasm.wasm_module)
    (annots   : (int * ownership_kind list * ownership_kind) list)
  : ownership_error list =
  let import_count = List.length wasm_mod.Wasm.imports in
  List.concat_map (fun (func_idx, param_kinds, _ret_kind) ->
    let local_idx = func_idx - import_count in
    if local_idx < 0 || local_idx >= List.length wasm_mod.Wasm.funcs then
      []   (* Imported function: no IR to inspect *)
    else
      let func = List.nth wasm_mod.Wasm.funcs local_idx in
      verify_function func param_kinds func_idx
  ) annots

(* ============================================================================
   Pipeline integration — parse ownership section from the module
   ============================================================================ *)

(** Parse the [affinescript.ownership] custom section payload into structured
    [(func_idx, param_kinds, ret_kind)] annotations.

    Binary encoding (from [Codegen.build_ownership_section]):
      u32le  count
      for each entry:
        u32le  func_idx
        u8     n_params
        u8[n]  param_kinds  (0=Unrestricted, 1=Linear, 2=SharedBorrow, 3=ExclBorrow)
        u8     ret_kind *)
let parse_ownership_section_payload
    (payload : bytes)
  : (int * ownership_kind list * ownership_kind) list =
  let kind_of_byte = function
    | 1 -> Linear
    | 2 -> SharedBorrow
    | 3 -> ExclBorrow
    | _ -> Unrestricted
  in
  let pos = ref 0 in
  let len = Bytes.length payload in
  let read_u32_le () =
    if !pos + 4 > len then 0
    else begin
      let b0 = Char.code (Bytes.get payload  !pos)      in
      let b1 = Char.code (Bytes.get payload (!pos + 1)) in
      let b2 = Char.code (Bytes.get payload (!pos + 2)) in
      let b3 = Char.code (Bytes.get payload (!pos + 3)) in
      pos := !pos + 4;
      b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
    end
  in
  let read_u8 () =
    if !pos >= len then 0
    else begin
      let b = Char.code (Bytes.get payload !pos) in
      pos := !pos + 1;
      b
    end
  in
  let count = read_u32_le () in
  List.init count (fun _ ->
    let func_idx    = read_u32_le () in
    let n_params    = read_u8 ()     in
    let param_kinds = List.init n_params (fun _ -> kind_of_byte (read_u8 ())) in
    let ret_kind    = kind_of_byte (read_u8 ()) in
    (func_idx, param_kinds, ret_kind)
  )

(** Verify a Wasm module using the embedded [affinescript.ownership] custom
    section.  This is the primary entry point for the pipeline and the CLI.

    Returns [Ok ()] if no violations are found, [Error errs] otherwise. *)
let verify_from_module
    (wasm_mod : Wasm.wasm_module)
  : (unit, ownership_error list) Result.t =
  match List.assoc_opt "affinescript.ownership" wasm_mod.Wasm.custom_sections with
  | None ->
    (* No ownership section: nothing to verify.  This is not an error —
       modules compiled without ownership qualifiers have no constraints. *)
    Ok ()
  | Some payload ->
    let annots = parse_ownership_section_payload payload in
    let errors = verify_module wasm_mod annots in
    if errors = [] then Ok () else Error errors

(* ============================================================================
   Pretty-printing
   ============================================================================ *)

(** Format a single ownership error for human-readable output. *)
let pp_error (fmt : Format.formatter) (err : ownership_error) : unit =
  match err with
  | LinearNotUsed { func_idx; param_idx } ->
    Format.fprintf fmt
      "Level 10 violation: function %d, param %d — Linear (own) param dropped \
       on all paths (must be consumed exactly once)"
      func_idx param_idx
  | LinearDroppedOnSomePath { func_idx; param_idx } ->
    Format.fprintf fmt
      "Level 10 violation: function %d, param %d — Linear (own) param dropped \
       on some paths (per-path min uses = 0; must be consumed on every path)"
      func_idx param_idx
  | LinearUsedMultiple { func_idx; param_idx; count } ->
    Format.fprintf fmt
      "Level 10 violation: function %d, param %d — Linear (own) param loaded \
       %d times on some path (exactly 1 required; possible duplication)"
      func_idx param_idx count
  | ExclBorrowAliased { func_idx; param_idx; count } ->
    Format.fprintf fmt
      "Level 7 violation: function %d, param %d — ExclBorrow (mut) param \
       aliased (%d simultaneous references; at most 1 permitted)"
      func_idx param_idx count

(** Format a full verification report. Prints "OK" if no errors. *)
let pp_report (fmt : Format.formatter) (errs : ownership_error list) : unit =
  match errs with
  | [] ->
    Format.fprintf fmt "typed-wasm ownership verification: OK@."
  | _ ->
    Format.fprintf fmt "typed-wasm ownership verification: %d violation(s)@."
      (List.length errs);
    List.iter (fun e ->
      Format.fprintf fmt "  %a@." pp_error e
    ) errs
