(** SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** AffineScript Backends Module
    
    Main entry point for all backend operations.
    Initializes backends, selects targets, and manages code generation.
*)

(** Load all backend modules *)
module Architecture = Backends_architecture
module Wasm_backend = Backends_wasm_backend
module Native_backend = Backends_native_backend
module Gpu_backend = Backends_gpu_backend
module Audio_backend = Backends_audio_backend
module Npu_backend = Backends_npu_backend
module Fpga_backend = Backends_fpga_backend

(** Load all kernel modules *)
module Audio_kernel = Backends_audio_kernel
module Gpu_kernel = Backends_gpu_kernel
module Npu_kernel = Backends_npu_kernel
module Math_kernel = Backends_math_kernel
module Physics_kernel = Backends_physics_kernel
module Fpga_kernel = Backends_fpga_kernel
module Crypto_kernel = Backends_crypto_kernel
module Vector_kernel = Backends_vector_kernel

(** Initialize all backends and kernels *)
let initialize () : unit =
  Architecture.initialize_backends ();
  ()

(** Compile program to target backend *)
let compile (target : Architecture.backend_target) (program : Ast.program) : string =
  let backend = Architecture.select_backend target in
  Architecture.generate_code backend program

(** Get available backends *)
let available_backends () : string list =
  Architecture.BackendRegistry.list_backends ()

(** Get available kernels *)
let available_kernels () : string list =
  Architecture.KernelRegistry.list_kernels ()

(** Check if backend supports feature *)
let backend_supports (backend_name : string) (feature : string) : bool =
  match Architecture.BackendRegistry.get_backend backend_name with
  | Some backend ->
      let module B = (val backend : Architecture.PROCESSOR_BACKEND) in
      let ctx = B.create_context () in
      B.supports_feature ctx feature
  | None -> false

(** Get backend capabilities *)
let backend_capabilities (backend_name : string) : Architecture.capability list =
  match Architecture.BackendRegistry.get_backend backend_name with
  | Some backend ->
      let module B = (val backend : Architecture.PROCESSOR_BACKEND) in
      let ctx = B.create_context () in
      B.get_capabilities ctx
  | None -> []

(** Execute kernel function *)
let execute_kernel (kernel_name : string) (function_name : string) (args : Ast.expr list) : Ast.expr =
  match Architecture.KernelRegistry.get_kernel kernel_name with
  | Some kernel ->
      let module K = (val kernel : Architecture.KERNEL) in
      let ctx = K.initialize () in
      K.execute ctx function_name args
  | None -> 
      (* Fallback: return unit if kernel not available *)
      Ast.ExprLit (Ast.LitUnit None)

(** Check if kernel is available *)
let kernel_available (kernel_name : string) : bool =
  Architecture.KernelRegistry.has_kernel kernel_name