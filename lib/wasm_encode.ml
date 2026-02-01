(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* WASM Binary Encoder - Stub Implementation

   TODO: Implement proper WASM binary encoding.
   For Phase 1 MVP, only Julia codegen is supported.
*)

open Wasm

(** Write WASM module to binary file - STUB *)
let write_module_to_file (_path : string) (_module : wasm_module) : unit =
  failwith "WASM encoding not yet implemented. Use .jl extension for Julia output."
