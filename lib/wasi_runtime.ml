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
    1. Converts int to string in memory (digit by digit)
    2. Creates iovec structure pointing to string
    3. Calls WASI fd_write

    Algorithm:
    - Handle negative sign separately
    - Extract digits using division and modulo
    - Write digits in reverse order (most significant first)

    Memory layout:
    - [0..15]: digit buffer (max 11 chars: "-2147483648")
    - [16..23]: iovec struct (buf_ptr, buf_len)
    - [24..27]: nwritten result

    Returns: code that leaves 0 on stack for success
*)
let gen_print_int (heap_ptr_global : int) (fd_write_idx : int) (num_local : int)
    : instr list =
  (* We need additional locals for the conversion:
     - buf_ptr: pointer to start of allocated buffer
     - write_ptr: current position while writing digits (counts backwards)
     - n: the number being converted (copy of num_local)
     - digit: current digit being extracted
     - is_negative: 1 if negative, 0 if positive
     - len: final string length
  *)

  (* Allocate 28 bytes total *)
  let total_size = 28 in
  let buf_local = num_local + 1 in       (* reuse locals after num_local *)
  let write_ptr_local = num_local + 2 in
  let n_local = num_local + 3 in
  let digit_local = num_local + 4 in
  let is_neg_local = num_local + 5 in
  let len_local = num_local + 6 in

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
    LocalTee buf_local;

    (* Copy number to n_local, check if negative *)
    LocalGet num_local;
    LocalTee n_local;
    I32Const 0l;
    I32LtS;  (* n < 0 ? *)
    LocalTee is_neg_local;

    (* If negative, negate n to make it positive *)
    If (BtEmpty, [
      I32Const 0l;
      LocalGet n_local;
      I32Sub;  (* n = -n *)
      LocalSet n_local;
    ], []);

    (* write_ptr starts at buf + 15 (end of buffer, write backwards) *)
    LocalGet buf_local;
    I32Const 15l;
    I32Add;
    LocalSet write_ptr_local;

    (* Extract digits loop - write from right to left *)
    Block (BtEmpty, [
      Loop (BtEmpty, [
        (* digit = n % 10 *)
        LocalGet n_local;
        I32Const 10l;
        I32RemU;
        LocalTee digit_local;

        (* *write_ptr = '0' + digit *)
        I32Const 48l;  (* ASCII '0' *)
        I32Add;
        LocalGet write_ptr_local;
        I32Store (0, 0);

        (* write_ptr-- *)
        LocalGet write_ptr_local;
        I32Const 1l;
        I32Sub;
        LocalSet write_ptr_local;

        (* n = n / 10 *)
        LocalGet n_local;
        I32Const 10l;
        I32DivU;
        LocalTee n_local;

        (* Continue if n > 0 *)
        I32Const 0l;
        I32GtU;
        BrIf 0;
      ])
    ]);

    (* If negative, add '-' sign *)
    LocalGet is_neg_local;
    If (BtEmpty, [
      I32Const 45l;  (* ASCII '-' *)
      LocalGet write_ptr_local;
      I32Store (0, 0);
      LocalGet write_ptr_local;
      I32Const 1l;
      I32Sub;
      LocalSet write_ptr_local;
    ], []);

    (* Calculate length: (buf + 15) - write_ptr *)
    LocalGet buf_local;
    I32Const 15l;
    I32Add;
    LocalGet write_ptr_local;
    I32Sub;
    LocalSet len_local;

    (* Actual string starts at write_ptr + 1 *)
    LocalGet write_ptr_local;
    I32Const 1l;
    I32Add;
    LocalSet write_ptr_local;

    (* Create iovec structure at buf + 16 *)
    (* iovec.buf_ptr = write_ptr (where string actually starts) *)
    LocalGet buf_local;
    I32Const 16l;
    I32Add;
    LocalGet write_ptr_local;
    I32Store (2, 0);

    (* iovec.buf_len = len *)
    LocalGet buf_local;
    I32Const 16l;
    I32Add;
    LocalGet len_local;
    I32Store (2, 4);

    (* Call fd_write(stdout, iovec_ptr, 1, nwritten_ptr) *)
    I32Const fd_stdout;
    LocalGet buf_local;
    I32Const 16l;
    I32Add;                       (* iovs = buf + 16 *)
    I32Const 1l;                  (* iovs_len = 1 *)
    LocalGet buf_local;
    I32Const 24l;
    I32Add;                       (* nwritten = buf + 24 *)
    Call fd_write_idx;

    (* Drop error code, return success *)
    Drop;
    I32Const 0l;
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
    I32Store (0, 0);

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
