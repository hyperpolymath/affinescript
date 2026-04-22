(** SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** AffineScript Backend Architecture
    
    This module defines the overall backend architecture including:
    - Processor backends (complete implementations)
    - Kernel stubs (placeholders for hardware acceleration)
    
    The architecture follows a layered approach:
    1. Frontend: AffineScript AST
    2. Middle-end: Optimized IR
    3. Backend: Target-specific code generation
    4. Kernel: Hardware-optimized routines
*)

open Ast
open Types

(** Backend target specification *)
type backend_target =
  | WASM of wasm_config
  | Native of native_config
  | GPU of gpu_config
  | AudioDSP of audio_config
  | NPU of npu_config
  | FPGA of fpga_config

(** Backend capability flags *)
type capability =
  | BasicArithmetic
  | MemoryManagement
  | ControlFlow
  | FunctionCalls
  | SIMDOperations
  | Multithreading
  | HardwareAcceleration
  | RealTimeProcessing

(** Processor backend interface *)
module type PROCESSOR_BACKEND = sig
  type context
  
  val create_context : unit -> context
  val generate_code : context -> Ast.program -> string
  val get_capabilities : context -> capability list
  val supports_feature : context -> string -> bool
  
  (** Target-specific operations *)
  val emit_prologue : context -> string
  val emit_epilogue : context -> string
  val emit_function : context -> Ast.fn_decl -> string
  val emit_expression : context -> Ast.expr -> string
  val emit_statement : context -> Ast.stmt -> string
  
  (** Optimization passes *)
  val run_optimizations : context -> Ast.program -> Ast.program
  val inline_functions : context -> Ast.program -> Ast.program
  val constant_folding : context -> Ast.program -> Ast.program
  val dead_code_elimination : context -> Ast.program -> Ast.program
end

(** Kernel interface for hardware acceleration *)
module type KERNEL = sig
  type kernel_context
  
  val initialize : unit -> kernel_context
  val shutdown : kernel_context -> unit
  val is_available : kernel_context -> bool
  val get_capabilities : kernel_context -> string list
  
  (** Kernel execution *)
  val execute : kernel_context -> string -> Ast.expr list -> Ast.expr
  val compile_kernel : kernel_context -> string -> Ast.fn_decl -> unit
  val load_precompiled : kernel_context -> string -> unit
  
  (** Memory management *)
  val allocate_device_memory : kernel_context -> int -> int
  val free_device_memory : kernel_context -> int -> unit
  val copy_to_device : kernel_context -> int -> bytes -> int -> unit
  val copy_from_device : kernel_context -> int -> bytes -> int -> unit
end

(** WASM Backend Configuration *)
type wasm_config = {
  target_version : string;
  enable_simd : bool;
  enable_threads : bool;
  enable_reference_types : bool;
  enable_tail_calls : bool;
  memory_pages : int;
  optimize_for : [ `Size | `Speed | `Balanced ];
}

(** Native Backend Configuration *)
type native_config = {
  target_arch : [ `X86_64 | `ARM64 | `RISCV64 | `WASM32 ];
  target_os : [ `Linux | `Windows | `MacOS | `WASI ];
  optimization_level : int;
  enable_lto : bool;
  enable_debug : bool;
}

(** GPU Backend Configuration *)
type gpu_config = {
  api : [ `WebGPU | `Vulkan | `Metal | `CUDA | `OpenCL ];
  device_type : [ `Integrated | `Discrete | `Virtual ];
  enable_compute : bool;
  enable_graphics : bool;
  max_workgroups : int * int * int;
  shader_model : string;
}

(** Audio DSP Backend Configuration *)
type audio_config = {
  api : [ `WebAudio | `ALSA | `CoreAudio | `WASAPI | `JACK ];
  sample_rate : int;
  buffer_size : int;
  channels : int;
  latency_ms : float;
  enable_real_time : bool;
}

(** NPU/TPU Backend Configuration *)
type npu_config = {
  framework : [ `TensorFlowLite | `ONNX | `TVM | `Custom ];
  precision : [ `FP32 | `FP16 | `INT8 | `BFLOAT16 ];
  max_ops : int;
  memory_limit : int;
  enable_quantization : bool;
}

(** FPGA Backend Configuration *)
type fpga_config = {
  vendor : [ `Xilinx | `Intel | `AMD | `Lattice ];
  device : string;
  clock_speed : int;
  logic_elements : int;
  block_ram : int;
  dsp_slices : int;
}

(** Backend Registry *)
module BackendRegistry : sig
  val register_backend : string -> (module PROCESSOR_BACKEND) -> unit
  val get_backend : string -> (module PROCESSOR_BACKEND) option
  val list_backends : unit -> string list
  val get_default_backend : unit -> (module PROCESSOR_BACKEND)
end = struct
  let backends : (string, (module PROCESSOR_BACKEND)) Hashtbl.t = Hashtbl.create 16
  
  let register_backend name module_ = Hashtbl.add backends name module_
  let get_backend name = Hashtbl.find_opt backends name
  let list_backends () = Hashtbl.fold (fun k _ acc -> k :: acc) backends []
  
  let get_default_backend () =
    match get_backend "wasm" with
    | Some backend -> backend
    | None -> failwith "WASM backend not registered"
end

(** Kernel Registry *)
module KernelRegistry : sig
  val register_kernel : string -> (module KERNEL) -> unit
  val get_kernel : string -> (module KERNEL) option
  val list_kernels : unit -> string list
  val has_kernel : string -> bool
end = struct
  let kernels : (string, (module KERNEL)) Hashtbl.t = Hashtbl.create 16
  
  let register_kernel name module_ = Hashtbl.add kernels name module_
  let get_kernel name = Hashtbl.find_opt kernels name
  let list_kernels () = Hashtbl.fold (fun k _ acc -> k :: acc) kernels []
  let has_kernel name = Hashtbl.mem kernels name
end

(** Backend Selection and Configuration *)
let select_backend (target : backend_target) : (module PROCESSOR_BACKEND) =
  match target with
  | WASM _ -> (module Wasm_backend : PROCESSOR_BACKEND)
  | Native _ -> (module Native_backend : PROCESSOR_BACKEND)
  | GPU _ -> (module Gpu_backend : PROCESSOR_BACKEND)
  | AudioDSP _ -> (module Audio_backend : PROCESSOR_BACKEND)
  | NPU _ -> (module Npu_backend : PROCESSOR_BACKEND)
  | FPGA _ -> (module Fpga_backend : PROCESSOR_BACKEND)

(** Optimization Pipeline *)
let run_optimization_pipeline (backend : (module PROCESSOR_BACKEND)) (program : Ast.program) : Ast.program =
  let module B = (val backend : PROCESSOR_BACKEND) in
  let ctx = B.create_context () in
  program
  |> B.run_optimizations ctx
  |> B.inline_functions ctx
  |> B.constant_folding ctx
  |> B.dead_code_elimination ctx

(** Code Generation Pipeline *)
let generate_code (backend : (module PROCESSOR_BACKEND)) (program : Ast.program) : string =
  let module B = (val backend : PROCESSOR_BACKEND) in
  let ctx = B.create_context () in
  let optimized = run_optimization_pipeline backend program in
  B.emit_prologue ctx ^
  B.generate_code ctx optimized ^
  B.emit_epilogue ctx

(** Initialize all backends and kernels *)
let initialize_backends () : unit =
  (* Register processor backends *)
  BackendRegistry.register_backend "wasm" (module Wasm_backend);
  BackendRegistry.register_backend "native" (module Native_backend);
  BackendRegistry.register_backend "gpu" (module Gpu_backend);
  BackendRegistry.register_backend "audio" (module Audio_backend);
  BackendRegistry.register_backend "npu" (module Npu_backend);
  BackendRegistry.register_backend "fpga" (module Fpga_backend);
  
  (* Register kernel stubs *)
  KernelRegistry.register_kernel "audio_kernel" (module Audio_kernel);
  KernelRegistry.register_kernel "gpu_kernel" (module Gpu_kernel);
  KernelRegistry.register_kernel "npu_kernel" (module Npu_kernel);
  KernelRegistry.register_kernel "math_kernel" (module Math_kernel);
  KernelRegistry.register_kernel "physics_kernel" (module Physics_kernel);
  KernelRegistry.register_kernel "fpga_kernel" (module Fpga_kernel);
  KernelRegistry.register_kernel "crypto_kernel" (module Crypto_kernel);
  KernelRegistry.register_kernel "vector_kernel" (module Vector_kernel);
  
  ()