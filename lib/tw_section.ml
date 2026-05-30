(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

module Encode = struct
  let write_u32_le buf n =
    Buffer.add_char buf (Char.chr  (n         land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr  8) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))

  let write_u8 buf n = Buffer.add_char buf (Char.chr (n land 0xff))

  let ownership (annots : (int * int list * int) list) : bytes =
    if annots = [] then Bytes.empty
    else
      let buf = Buffer.create 64 in
      write_u32_le buf (List.length annots);
      List.iter (fun (func_idx, param_kind_bytes, ret_kind_byte) ->
        write_u32_le buf func_idx;
        write_u8 buf (List.length param_kind_bytes);
        List.iter (fun b -> write_u8 buf b) param_kind_bytes;
        write_u8 buf ret_kind_byte
      ) annots;
      Buffer.to_bytes buf
end
