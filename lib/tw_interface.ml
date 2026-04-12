(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** typed-wasm multi-module boundary verifier — Stage 10.

    Implements two capabilities:

    1.  {b Interface extraction}: given any Wasm module that contains an
        [affinescript.ownership] custom section, extract the
        ownership-annotated signatures of all exported functions.  This is
        the machine-readable boundary contract that callers must honour.
        Printed by [affinescript interface FILE].

    2.  {b Cross-module call verification}: given a callee module (with
        ownership annotations) and a caller module (which imports functions
        from the callee), verify that the caller's function bodies invoke
        each Linear-param import with consistent per-path call counts:
        - [max_calls > 1] on any path → [LinearImportCalledMultiple]
          (the Linear argument may be duplicated)
        - [min_calls = 0, max_calls ≥ 1] → [LinearImportDroppedOnSomePath]
          (the argument is dropped without transfer on the zero-call path)

    The intra-module verifier ([Tw_verify]) checks function bodies against
    their own params.  This module checks call sites against the callee's
    exported ownership contract — closing the full-stack guarantee loop from
    AffineScript source through to the multi-module Wasm boundary.

    @see typed-wasm LEVEL-STATUS.md — Level 7 and Level 10 sections.
*)

open Codegen

(* ============================================================================
   Interface types
   ============================================================================ *)

(** Ownership-annotated signature for one exported function. *)
type func_interface = {
  fi_name        : string;             (** export name *)
  fi_func_idx    : int;                (** global function index in the callee *)
  fi_param_kinds : ownership_kind list;
  fi_ret_kind    : ownership_kind;
}

(** A cross-module ownership violation found in a caller's function body. *)
type cross_error =
  | LinearImportCalledMultiple of {
      caller_func_idx : int;   (** local function index in the caller module *)
      import_func_idx : int;   (** global import slot index in the caller *)
      import_name     : string;
      count           : int;   (** max calls on any single execution path *)
    }
  (** Level 10: A Linear-param import is invoked [count] times on some
      execution path.  Each call consumes the Linear argument — calling
      twice duplicates it, violating exclusive ownership. *)
  | LinearImportDroppedOnSomePath of {
      caller_func_idx : int;
      import_func_idx : int;
      import_name     : string;
    }
  (** Level 10: A Linear-param import is invoked on some execution paths
      but not others (min_calls = 0, max_calls ≥ 1).  The Linear argument
      is silently dropped on the zero-call paths. *)

(* ============================================================================
   Ownership index extraction
   ============================================================================ *)

(** Build a [(func_idx → (param_kinds, ret_kind))] lookup table from the
    [affinescript.ownership] custom section of [wasm_mod].
    Returns an empty table if the section is absent. *)
let ownership_index_of_module (wasm_mod : Wasm.wasm_module)
    : (int, ownership_kind list * ownership_kind) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  (match List.assoc_opt "affinescript.ownership" wasm_mod.Wasm.custom_sections with
  | None -> ()
  | Some payload ->
    List.iter (fun (idx, param_kinds, ret_kind) ->
      Hashtbl.replace tbl idx (param_kinds, ret_kind)
    ) (Tw_verify.parse_ownership_section_payload payload));
  tbl

(* ============================================================================
   Interface extraction
   ============================================================================ *)

(** Extract the ownership-annotated export interface of [wasm_mod].

    Returns one [func_interface] per exported function (non-function exports
    such as tables, memories, and globals are excluded).  Functions without
    an entry in the ownership section are treated as fully [Unrestricted]. *)
let extract_exports (wasm_mod : Wasm.wasm_module) : func_interface list =
  let idx = ownership_index_of_module wasm_mod in
  List.filter_map (fun export ->
    match export.Wasm.e_desc with
    | Wasm.ExportFunc func_idx ->
      let (param_kinds, ret_kind) =
        match Hashtbl.find_opt idx func_idx with
        | Some ann -> ann
        | None     -> ([], Unrestricted)
      in
      Some {
        fi_name        = export.Wasm.e_name;
        fi_func_idx    = func_idx;
        fi_param_kinds = param_kinds;
        fi_ret_kind    = ret_kind;
      }
    | _ -> None
  ) wasm_mod.Wasm.exports

(* ============================================================================
   Per-path call-count analysis
   ============================================================================ *)

(** Count how many times [Call import_idx] appears across all execution paths
    in [instrs], returning [(min_calls, max_calls)].

    Sequential instructions sum their ranges.  For [If] nodes only one branch
    executes: take [min] of minimums and [max] of maximums.  Nested [Block]
    and [Loop] bodies are descended into.

    Example: [If { Call 0 } { Call 0 }] → [(1, 1)] — called exactly once.
    Example: [If { Call 0 } { [] }]     → [(0, 1)] — dropped on else path.
    Example: [Call 0; Call 0]           → [(2, 2)] — always duplicated. *)
let rec count_calls_range (import_idx : int) (instrs : Wasm.instr list) : int * int =
  List.fold_left (fun (a_min, a_max) instr ->
    let (i_min, i_max) = calls_range_in_instr import_idx instr in
    (a_min + i_min, a_max + i_max)
  ) (0, 0) instrs

and calls_range_in_instr (import_idx : int) (instr : Wasm.instr) : int * int =
  match instr with
  | Wasm.Call n ->
    if n = import_idx then (1, 1) else (0, 0)
  | Wasm.Block (_, body) | Wasm.Loop (_, body) ->
    count_calls_range import_idx body
  | Wasm.If (_, then_, else_) ->
    let (t_min, t_max) = count_calls_range import_idx then_ in
    let (e_min, e_max) = count_calls_range import_idx else_ in
    (min t_min e_min, max t_max e_max)
  | _ -> (0, 0)

(* ============================================================================
   Cross-module verification
   ============================================================================ *)

(** [has_linear_param param_kinds] — true if any param in [param_kinds] is [Linear]. *)
let has_linear_param (kinds : ownership_kind list) : bool =
  List.mem Linear kinds

(** Verify that [caller_mod]'s local function bodies respect the ownership
    annotations of the imports they call.

    [callee_iface] is the ownership-annotated export interface of the callee
    module whose functions are imported by [caller_mod].  Only imports whose
    export names appear in [callee_iface] with at least one [Linear] parameter
    are checked — other imports are not constrained by this pass.

    For each such Linear-annotated import, every local function in [caller_mod]
    is inspected.  If a function calls the import at all (max_calls ≥ 1):
    - [max_calls > 1]: [LinearImportCalledMultiple]
    - [min_calls = 0]: [LinearImportDroppedOnSomePath]

    Functions that never call the import on any path are not flagged — there
    is no obligation for every function to invoke every import. *)
let verify_cross_module
    (callee_iface : func_interface list)
    (caller_mod   : Wasm.wasm_module)
  : (unit, cross_error list) Result.t =
  (* Index callee exports by their name for O(n) lookup *)
  let iface_by_name =
    List.fold_left (fun acc fi -> (fi.fi_name, fi) :: acc) [] callee_iface
  in
  let import_count = List.length caller_mod.Wasm.imports in
  (* Identify imports in caller that correspond to Linear-param exports in callee *)
  let linear_imports =
    List.filter_map (fun (import_slot, import) ->
      match List.assoc_opt import.Wasm.i_name iface_by_name with
      | Some fi when has_linear_param fi.fi_param_kinds ->
        Some (import_slot, import.Wasm.i_name)
      | _ -> None
    ) (List.mapi (fun i imp -> (i, imp)) caller_mod.Wasm.imports)
  in
  if linear_imports = [] then Ok ()
  else begin
    let errors =
      List.concat_map (fun (import_slot, import_name) ->
        List.concat_map (fun (local_idx, func) ->
          let caller_func_idx = local_idx + import_count in
          let (min_calls, max_calls) =
            count_calls_range import_slot func.Wasm.f_body
          in
          if max_calls = 0 then
            [] (* never calls this import — not a violation *)
          else
            let drop_errors =
              if min_calls = 0 then
                [LinearImportDroppedOnSomePath {
                  caller_func_idx;
                  import_func_idx = import_slot;
                  import_name }]
              else []
            in
            let dup_errors =
              if max_calls > 1 then
                [LinearImportCalledMultiple {
                  caller_func_idx;
                  import_func_idx = import_slot;
                  import_name;
                  count = max_calls }]
              else []
            in
            drop_errors @ dup_errors
        ) (List.mapi (fun i f -> (i, f)) caller_mod.Wasm.funcs)
      ) linear_imports
    in
    if errors = [] then Ok () else Error errors
  end

(* ============================================================================
   Pretty-printing
   ============================================================================ *)

let pp_kind (fmt : Format.formatter) (k : ownership_kind) : unit =
  Format.pp_print_string fmt (match k with
    | Linear       -> "own"
    | SharedBorrow -> "ref"
    | ExclBorrow   -> "mut"
    | Unrestricted -> "val")

(** Format the ownership-annotated export interface for human-readable output. *)
let pp_interface (fmt : Format.formatter) (iface : func_interface list) : unit =
  match iface with
  | [] ->
    Format.fprintf fmt "No exported functions with ownership annotations.@."
  | _ ->
    Format.fprintf fmt "Ownership-annotated export interface:@.";
    List.iter (fun fi ->
      let param_strs =
        List.mapi (fun i k ->
          Format.asprintf "p%d:%a" i pp_kind k
        ) fi.fi_param_kinds
      in
      Format.fprintf fmt "  [fn %d] %s(%s) -> %a@."
        fi.fi_func_idx
        fi.fi_name
        (String.concat ", " param_strs)
        pp_kind fi.fi_ret_kind
    ) iface

(** Format a single cross-module error. *)
let pp_cross_error (fmt : Format.formatter) (err : cross_error) : unit =
  match err with
  | LinearImportCalledMultiple { caller_func_idx; import_name; count; _ } ->
    Format.fprintf fmt
      "Level 10 boundary violation: caller fn %d calls import '%s' \
       %d time(s) on some path (Linear param; must be called at most once)"
      caller_func_idx import_name count
  | LinearImportDroppedOnSomePath { caller_func_idx; import_name; _ } ->
    Format.fprintf fmt
      "Level 10 boundary violation: caller fn %d calls import '%s' on \
       some paths but not others (Linear param dropped on zero-call path)"
      caller_func_idx import_name

(** Format a full cross-module verification report. *)
let pp_cross_report (fmt : Format.formatter) (errs : cross_error list) : unit =
  match errs with
  | [] ->
    Format.fprintf fmt "typed-wasm cross-module boundary verification: OK@."
  | _ ->
    Format.fprintf fmt
      "typed-wasm cross-module boundary verification: %d violation(s)@."
      (List.length errs);
    List.iter (fun e ->
      Format.fprintf fmt "  %a@." pp_cross_error e
    ) errs
