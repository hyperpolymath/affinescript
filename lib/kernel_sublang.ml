(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Shared infrastructure for kernel-sublanguage backends.

    The Tier-C backends — WGSL, SPIR-V, CUDA, Metal, OpenCL, Faust, ONNX,
    MLIR — all accept a *deliberately restricted* subset of AffineScript.
    Each had its own copy of:

    - an [<Backend>_unsupported] exception + [unsupported] helper
    - a [pick_kernel] / [pick_entry] function that searches for a canonical
      entry name and falls back to the first [fn]
    - an [array_element] helper that strips ownership and matches
      [Array[T]] for a scalar [T]
    - a math-builtin allowlist for trig / pow / sqrt / etc.
    - an [is_unit_ty] check that accepts both [TyCon "Unit"] and [TyTuple []]

    Per the [docs/guides/frontier-programming-practices/AI.a2ml] backends
    section: "If two new tier-C backends ship with overlapping
    restrictions, factor the validator before adding the third." We're
    well past three. This module is the factoring.

    Per-target lowering of *concrete* type names (i32 vs i64 vs int vs
    long, f32 vs f64 vs float vs double) intentionally stays in each
    backend — that's not duplication, that's correctly different. *)

open Ast

(* ============================================================================
   Common exception
   ============================================================================ *)

(** Raised by any kernel-sublanguage backend when the source falls outside
    its accepted subset. The string is the user-facing reason; backends
    typically prefix it with their name (e.g. "WGSL backend: ..."). *)
exception Unsupported of string

let unsupported (msg : string) : 'a = raise (Unsupported msg)

(* ============================================================================
   Entry-function selection

   Most kernel backends look for a function named [kernel], [process],
   [main], or [graph] and fall back to the first user-defined function in
   the program. The default name list mirrors what the existing backends
   used; callers can override.
   ============================================================================ *)

let default_entry_names = ["kernel"; "process"; "main"; "graph"]

let pick_entry ?(names = default_entry_names) (program : program) : fn_decl =
  let fns = List.filter_map (function TopFn fd -> Some fd | _ -> None)
              program.prog_decls in
  let rec try_names = function
    | n :: rest ->
        (match List.find_opt (fun fd -> fd.fd_name.name = n) fns with
         | Some fd -> fd
         | None -> try_names rest)
    | [] ->
        (match fns with
         | fd :: _ -> fd
         | [] -> unsupported "no function found to lower as kernel")
  in
  try_names names

(* ============================================================================
   Type-shape predicates and helpers
   ============================================================================ *)

let rec strip_ownership (te : type_expr) : type_expr =
  match te with
  | TyOwn t | TyRef t | TyMut t -> strip_ownership t
  | t -> t

(** Accept [Unit] either as the named type or as the empty tuple, since
    [-> ()] parses as [TyTuple []] but [-> Unit] parses as [TyCon "Unit"]. *)
let is_unit_ty (te : type_expr) : bool =
  match strip_ownership te with
  | TyCon id when id.name = "Unit" -> true
  | TyTuple [] -> true
  | _ -> false

(** Standard primitive scalar names — the intersection that every kernel
    sublanguage we currently target supports. Backends restricting further
    (e.g. Faust's Float-only kernels) should layer their own check on top. *)
let is_scalar_type_name (n : string) : bool =
  n = "Int" || n = "Float" || n = "Bool"

let is_scalar_type (te : type_expr) : bool =
  match strip_ownership te with
  | TyCon id -> is_scalar_type_name id.name
  | _ -> false

(** Decompose [Array[T]] for a scalar [T], returning the inner type-con
    name (e.g. ["Int"], ["Float"]). Returns [None] for non-array or
    non-scalar-element shapes. *)
let array_element (te : type_expr) : string option =
  match strip_ownership te with
  | TyApp (id, [TyArg inner]) when id.name = "Array" ->
      (match strip_ownership inner with
       | TyCon id when is_scalar_type_name id.name -> Some id.name
       | _ -> None)
  | _ -> None

(** Same as [array_element] but raises [Unsupported] with a useful message
    on shapes that don't decompose. The expected shape string is included
    in the error so users see e.g. "expected Array[Float]" rather than a
    generic "type not allowed." *)
let require_array_element (expected : string) (te : type_expr) : string =
  match array_element te with
  | Some name -> name
  | None ->
      unsupported (Printf.sprintf "expected %s for kernel buffer" expected)

(* ============================================================================
   Math builtins shared across kernel backends

   Names are taken from the common subset (WGSL spec, GLSL, Metal stdlib,
   OpenCL, CUDA math.h, Faust). [int]/[float] are coercions; the rest are
   real math intrinsics. Backends may add target-specific names (e.g.
   Metal's [metal::float4]) but should start from this list.
   ============================================================================ *)

let math_builtins : string list = [
  "sin"; "cos"; "tan"; "asin"; "acos"; "atan"; "atan2";
  "sinh"; "cosh"; "tanh";
  "exp"; "log"; "log2"; "log10"; "pow"; "sqrt"; "rsqrt";
  "floor"; "ceil"; "round"; "trunc"; "fract"; "fmod";
  "abs"; "min"; "max"; "clamp"; "sign";
  "mix"; "step"; "smoothstep";
  "int"; "float"; "i32"; "u32"; "f32";
]

let is_math_builtin (name : string) : bool = List.mem name math_builtins

(* ============================================================================
   Validation helpers

   These build on [is_scalar_type], [array_element], and [is_unit_ty] to
   produce friendly error messages. They are convenience wrappers — every
   backend can implement its own validation if its rules differ.
   ============================================================================ *)

(** Require the function's return type satisfies [pred]; raise on violation. *)
let validate_return (pred : type_expr -> bool) (expected : string) (fd : fn_decl) : unit =
  match fd.fd_ret_ty with
  | None -> ()  (* no annotation — caller decides whether to accept *)
  | Some t when pred t -> ()
  | _ -> unsupported
           (Printf.sprintf "kernel function must return %s" expected)

(** Require every parameter satisfies [pred]; raise on the first that
    doesn't, naming it. *)
let validate_params (pred : type_expr -> bool) (expected : string)
    (fd : fn_decl) : unit =
  List.iter (fun (p : param) ->
    if not (pred p.p_ty) then
      unsupported
        (Printf.sprintf "parameter %s must be %s" p.p_name.name expected)
  ) fd.fd_params

(** Common shape: first param is [Int] (the global invocation index),
    remaining params are [Array[T]] buffers, return type is [Unit]. Used
    by WGSL / SPIR-V / CUDA / Metal / OpenCL. *)
let validate_compute_kernel_shape (fd : fn_decl) : unit =
  validate_return (fun t -> is_unit_ty t) "Unit or ()" fd;
  match fd.fd_params with
  | [] -> unsupported "kernel must take at least an Int index parameter"
  | first :: rest ->
      (match strip_ownership first.p_ty with
       | TyCon id when id.name = "Int" -> ()
       | _ -> unsupported "first kernel parameter must be Int (the global index)");
      List.iter (fun (p : param) ->
        ignore (require_array_element "Array[Int|Float]" p.p_ty)
      ) rest
