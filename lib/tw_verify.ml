(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath *)

(** typed-wasm ownership verifier — Stage 7.

    Statically verifies typed-wasm Level 7 (aliasing safety) and Level 10
    (linearity) constraints on AffineScript's Wasm IR.

    The verifier runs after codegen on the in-memory [Wasm.wasm_module]
    together with the ownership annotations collected during codegen.
    It is a second line of defence: the QTT quantity checker (Stage 1) already
    enforces these rules at the AffineScript source level; this module
    re-checks them on the emitted Wasm IR to catch any codegen bugs.

    Level 10 — Linearity: a parameter annotated [Linear] (TyOwn) must be
    loaded exactly once in the function body.  Loading zero times means the
    owned value is dropped; loading more than once means it may be duplicated.

    Level 7 — Aliasing safety: a parameter annotated [ExclBorrow] (TyMut)
    must be loaded at most once.  Multiple loads create aliased references to
    the same mutable memory, violating exclusive-borrow invariants.

    [SharedBorrow] (TyRef) and [Unrestricted] params are not checked by this
    pass (no constraints violated by multiple reads of a shared reference).

    Branch semantics: for [If] instructions, we take the {b max} of the
    use-counts across the two branches (since only one branch executes at
    runtime).  This is conservative: a linear param used exactly once in
    each branch of an if/else is counted as 1 (correct), not 2.  Stage 8
    will refine this with a per-path analysis if needed.

    @see typed-wasm LEVEL-STATUS.md — Level 7 and Level 10 sections.
*)

open Codegen

(* ============================================================================
   Error types
   ============================================================================ *)

(** An ownership violation found in a Wasm function body. *)
type ownership_error =
  | LinearNotUsed of { func_idx : int; param_idx : int }
  (** Level 10: Linear parameter was never loaded — dropped without consumption.
      The owned resource leaks. *)
  | LinearUsedMultiple of { func_idx : int; param_idx : int; count : int }
  (** Level 10: Linear parameter was loaded [count] times — potential duplication.
      Only one consumer is permitted. *)
  | ExclBorrowAliased of { func_idx : int; param_idx : int; count : int }
  (** Level 7: ExclBorrow parameter was loaded [count] times — aliasing violation.
      An exclusive borrow must not have multiple simultaneous references. *)

(* ============================================================================
   Use-count analysis
   ============================================================================ *)

(** Count how many times [local_idx] is loaded inside [instrs].

    For [If] nodes the branch counts are combined with [max] rather than
    summed, reflecting that only one branch executes.  All other nodes are
    summed.  Nested [Block] and [Loop] bodies are descended into. *)
let rec count_uses (local_idx : int) (instrs : Wasm.instr list) : int =
  List.fold_left (fun acc instr -> acc + uses_in_instr local_idx instr) 0 instrs

and uses_in_instr (local_idx : int) (instr : Wasm.instr) : int =
  match instr with
  | Wasm.LocalGet n -> if n = local_idx then 1 else 0
  | Wasm.Block (_, body) | Wasm.Loop (_, body) ->
    count_uses local_idx body
  | Wasm.If (_, then_, else_) ->
    (* Only one branch executes: take max so that
       If { then: LocalGet 0 } { else: LocalGet 0 } counts as 1, not 2. *)
    max (count_uses local_idx then_) (count_uses local_idx else_)
  | _ -> 0

(* ============================================================================
   Per-function verification
   ============================================================================ *)

(** Verify ownership constraints for one function.

    [func] is the Wasm function body.
    [param_kinds] are the ownership annotations for each parameter (in order).
    [func_idx] is the global function index (for error reporting).
    Returns all violations found (empty list = clean). *)
let verify_function
    (func     : Wasm.func)
    (param_kinds : ownership_kind list)
    (func_idx : int)
  : ownership_error list =
  List.concat_map (fun (param_idx, kind) ->
    let uses = count_uses param_idx func.Wasm.f_body in
    match kind with
    | Linear ->
      (* Exactly once: zero = dropped, >1 = may be duplicated. *)
      if uses = 0 then
        [LinearNotUsed { func_idx; param_idx }]
      else if uses > 1 then
        [LinearUsedMultiple { func_idx; param_idx; count = uses }]
      else
        []
    | ExclBorrow ->
      (* At most once: >1 creates simultaneous aliases. *)
      if uses > 1 then
        [ExclBorrowAliased { func_idx; param_idx; count = uses }]
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
       (must be consumed exactly once)"
      func_idx param_idx
  | LinearUsedMultiple { func_idx; param_idx; count } ->
    Format.fprintf fmt
      "Level 10 violation: function %d, param %d — Linear (own) param loaded \
       %d times (exactly 1 required; possible duplication)"
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
