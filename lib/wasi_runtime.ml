(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WASI runtime support - I/O bindings for WebAssembly System Interface.

    This module provides helper functions to generate WASM code that calls
    WASI system calls for I/O operations.
*)

open Wasm

(** WASI file descriptor constants *)
let fd_stdout = 1l
let fd_stderr = 2l

(** Create WASI fd_write import

    fd_write signature: (fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> i32

    Returns (import, func_type_index)
*)
let create_fd_write_import () : import * func_type =
  let func_type = {
    ft_params = [I32; I32; I32; I32];  (* fd, iovs_ptr, iovs_len, nwritten_ptr *)
    ft_results = [I32];                 (* error code *)
  } in
  let import = {
    i_module = "wasi_snapshot_preview1";
    i_name = "fd_write";
    i_desc = ImportFunc 0;  (* Will be adjusted when added to module *)
  } in
  (import, func_type)

(** Generate code to print an integer to stdout

    This function:
    1. Converts int to string in memory
    2. Creates iovec structure pointing to string
    3. Calls WASI fd_write

    Assumes fd_write is imported at index 0
    Requires heap allocation support

    Returns: (context, code) where code leaves 0 on stack for success
*)
let gen_print_int (heap_ptr_global : int) (fd_write_idx : int) (value_local : int)
    : instr list =
  (* For now, we'll use a simplified approach:
     - Allocate 16 bytes for buffer (enough for i32 range)
     - Convert int to ASCII digits
     - Write using fd_write

     TODO: Implement full int_to_string conversion
     For MVP, just write the raw bytes as a hack
  *)

  (* Allocate space for:
     - 16 bytes for string buffer
     - 8 bytes for iovec struct (ptr: i32, len: i32)
     - 4 bytes for nwritten result
  *)
  let total_size = 28 in

  [
    (* Allocate memory *)
    GlobalGet heap_ptr_global;
    I32Const (Int32.of_int total_size);
    I32Add;
    GlobalSet heap_ptr_global;

    (* buf_ptr = heap_ptr - 28 *)
    GlobalGet heap_ptr_global;
    I32Const (Int32.of_int total_size);
    I32Sub;
    LocalTee value_local;  (* Save buf_ptr in value_local *)

    (* TODO: Convert int to string - for now just store raw value *)
    (* This is a placeholder - we'd need proper int-to-string *)
    LocalGet value_local;
    I32Const 48l;  (* ASCII '0' *)
    I32Store (2, 0);

    (* Create iovec structure at buf_ptr + 16 *)
    (* iovec.buf_ptr = buf_ptr *)
    LocalGet value_local;
    I32Const 16l;
    I32Add;
    LocalGet value_local;
    I32Store (2, 0);

    (* iovec.buf_len = 1 (one char for now) *)
    LocalGet value_local;
    I32Const 16l;
    I32Add;
    I32Const 1l;
    I32Store (2, 4);

    (* Call fd_write(stdout, iovec_ptr, 1, nwritten_ptr) *)
    I32Const fd_stdout;           (* fd = stdout *)
    LocalGet value_local;
    I32Const 16l;
    I32Add;                       (* iovs = buf_ptr + 16 *)
    I32Const 1l;                  (* iovs_len = 1 *)
    LocalGet value_local;
    I32Const 24l;
    I32Add;                       (* nwritten = buf_ptr + 24 *)
    Call fd_write_idx;

    (* fd_write returns error code, we'll drop it for now *)
    Drop;
    I32Const 0l;  (* Return success *)
  ]

(** Generate code to print a newline *)
let gen_println (heap_ptr_global : int) (fd_write_idx : int) (temp_local : int)
    : instr list =
  let newline_byte = 10l in  (* ASCII '\n' *)
  [
    (* Allocate space for 1 byte + iovec + nwritten = 13 bytes *)
    GlobalGet heap_ptr_global;
    I32Const 13l;
    I32Add;
    GlobalSet heap_ptr_global;

    GlobalGet heap_ptr_global;
    I32Const 13l;
    I32Sub;
    LocalTee temp_local;

    (* Store newline character *)
    LocalGet temp_local;
    I32Const newline_byte;
    I32Store8 (0, 0);

    (* Create iovec *)
    LocalGet temp_local;
    I32Const 1l;
    I32Add;
    LocalGet temp_local;
    I32Store (2, 0);

    LocalGet temp_local;
    I32Const 1l;
    I32Add;
    I32Const 1l;
    I32Store (2, 4);

    (* Call fd_write *)
    I32Const fd_stdout;
    LocalGet temp_local;
    I32Const 1l;
    I32Add;
    I32Const 1l;
    LocalGet temp_local;
    I32Const 9l;
    I32Add;
    Call fd_write_idx;

    Drop;
    I32Const 0l;
  ]
