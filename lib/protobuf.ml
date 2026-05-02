(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Minimal Protocol Buffers wire-format encoder.

    Implements the subset needed to write ONNX [.onnx] files (and any other
    proto2/proto3 message that uses the same wire types):

    - varint (wire type 0) — for ints, enums, and lengths
    - 64-bit fixed (wire type 1) — for [double], [fixed64], [sfixed64]
    - length-delimited (wire type 2) — for strings, bytes, embedded messages,
      and packed-repeated scalars
    - 32-bit fixed (wire type 5) — for [float], [fixed32], [sfixed32]

    Wire tag layout (per Google's spec): [(field_number << 3) | wire_type],
    encoded as a varint. The decoder reads the tag, splits it back into
    field number and wire type, and uses the wire type to know how many
    bytes follow.

    This module is intentionally schema-agnostic: it emits the bytes for a
    single field at a time. Higher layers (e.g. {!Onnx_proto}) compose
    these calls to build typed messages. *)

(* ============================================================================
   Wire types
   ============================================================================ *)

type wire_type =
  | Varint                (* 0 *)
  | Fixed64               (* 1 *)
  | LengthDelimited       (* 2 *)
  | Fixed32               (* 5 *)

let wire_type_int = function
  | Varint           -> 0
  | Fixed64          -> 1
  | LengthDelimited  -> 2
  | Fixed32          -> 5

(* ============================================================================
   Varint encoding

   7 bits of payload per byte; MSB set on every byte except the last. Negative
   ints (two's complement) require 10 bytes; unsigned ints are encoded as the
   smallest number of bytes that fits.
   ============================================================================ *)

let encode_varint (buf : Buffer.t) (n : int) : unit =
  let rec loop n =
    if n < 0x80 then Buffer.add_char buf (Char.chr n)
    else begin
      Buffer.add_char buf (Char.chr ((n land 0x7F) lor 0x80));
      loop (n lsr 7)
    end
  in
  if n < 0 then
    (* Two's complement extension: emit the low 64 bits worth of bytes,
       which is at most 10 7-bit groups. *)
    let rec loop10 n i =
      if i = 9 then Buffer.add_char buf (Char.chr (n land 0x7F))
      else begin
        Buffer.add_char buf (Char.chr ((n land 0x7F) lor 0x80));
        loop10 (n lsr 7) (i + 1)
      end
    in
    loop10 n 0
  else
    loop n

(* ============================================================================
   Field tags

   Combine field number and wire type into a single varint. ONNX field
   numbers stay below 32, so the tag fits in one byte.
   ============================================================================ *)

let encode_tag (buf : Buffer.t) (field_number : int) (wt : wire_type) : unit =
  let tag = (field_number lsl 3) lor (wire_type_int wt) in
  encode_varint buf tag

(* ============================================================================
   Field encoders (one per scalar type)

   Each writes the tag followed by the value in the appropriate wire format.
   Empty/zero values are still emitted: the protobuf default-elision rule
   applies only to *messages with proto3 default-zero suppression*; for our
   purposes (constructing ONNX models from scratch) we always emit fields we
   set explicitly, which is what every ONNX writer in the wild does.
   ============================================================================ *)

let encode_int32_field (buf : Buffer.t) (field : int) (n : int) : unit =
  encode_tag buf field Varint;
  encode_varint buf n

let encode_int64_field = encode_int32_field
let encode_uint32_field = encode_int32_field

let encode_float_field (buf : Buffer.t) (field : int) (f : float) : unit =
  encode_tag buf field Fixed32;
  let bits = Int32.bits_of_float f in
  for i = 0 to 3 do
    let b = Int32.to_int (Int32.logand (Int32.shift_right_logical bits (i * 8)) 0xFFl) in
    Buffer.add_char buf (Char.chr b)
  done

let encode_double_field (buf : Buffer.t) (field : int) (f : float) : unit =
  encode_tag buf field Fixed64;
  let bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    let b = Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xFFL) in
    Buffer.add_char buf (Char.chr b)
  done

let encode_string_field (buf : Buffer.t) (field : int) (s : string) : unit =
  encode_tag buf field LengthDelimited;
  encode_varint buf (String.length s);
  Buffer.add_string buf s

let encode_bytes_field = encode_string_field

(** Embed a sub-message: encode its bytes once into a temporary buffer to
    learn the length, then emit length-delimited. *)
let encode_message_field (buf : Buffer.t) (field : int)
    (encode : Buffer.t -> unit) : unit =
  let inner = Buffer.create 64 in
  encode inner;
  let payload = Buffer.contents inner in
  encode_tag buf field LengthDelimited;
  encode_varint buf (String.length payload);
  Buffer.add_string buf payload

(** Repeated message field: just emit the same field number multiple times. *)
let encode_repeated_message_field (buf : Buffer.t) (field : int)
    (encode_one : Buffer.t -> 'a -> unit) (items : 'a list) : unit =
  List.iter (fun item ->
    encode_message_field buf field (fun b -> encode_one b item)
  ) items

(** Repeated string field: same convention. *)
let encode_repeated_string_field (buf : Buffer.t) (field : int)
    (items : string list) : unit =
  List.iter (encode_string_field buf field) items
