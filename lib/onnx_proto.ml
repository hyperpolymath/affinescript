(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** ONNX schema subset, encoded directly to protobuf wire bytes.

    Implements the messages we need to construct a valid [ModelProto] from
    scratch, in the order they appear in the upstream [onnx.proto]:

    - [TensorShapeProto.Dimension] (`dim_value` or `dim_param`)
    - [TensorShapeProto]
    - [TypeProto.Tensor]
    - [TypeProto]
    - [ValueInfoProto]
    - [NodeProto]
    - [GraphProto]
    - [OperatorSetIdProto]
    - [ModelProto]

    Field numbers and wire types are taken from the canonical
    [https://github.com/onnx/onnx/blob/main/onnx/onnx.proto] schema. The
    ONNX runtime, ORT, tract, and oxionnx-proto all read this exact wire
    format; round-tripping our output through any of them validates we got
    the field numbers right.

    Tensor element-type codes match [TensorProto.DataType]:
    1 = FLOAT, 7 = INT64, 11 = DOUBLE, 9 = BOOL — see ONNX's enum table.
*)

open Protobuf

(* ============================================================================
   TensorShapeProto.Dimension
   ============================================================================ *)

type dimension =
  | DimValue of int
  | DimParam of string

let encode_dimension (buf : Buffer.t) (d : dimension) : unit =
  match d with
  | DimValue n   -> encode_int64_field buf 1 n
  | DimParam s   -> encode_string_field buf 2 s

(* ============================================================================
   TensorShapeProto
   ============================================================================ *)

type shape = dimension list

let encode_shape (buf : Buffer.t) (dims : shape) : unit =
  encode_repeated_message_field buf 1 encode_dimension dims

(* ============================================================================
   TypeProto.Tensor
   ============================================================================ *)

type tensor_type = {
  elem_type : int;     (* TensorProto.DataType *)
  shape     : shape;
}

let encode_tensor_type (buf : Buffer.t) (t : tensor_type) : unit =
  encode_int32_field buf 1 t.elem_type;
  encode_message_field buf 2 (fun b -> encode_shape b t.shape)

(* ============================================================================
   TypeProto

   We only emit the [tensor_type] variant — sequence/map/optional types
   exist in newer ONNX but aren't relevant for the kernel sublanguage.
   ============================================================================ *)

type type_proto =
  | TensorType of tensor_type

let encode_type_proto (buf : Buffer.t) (t : type_proto) : unit =
  match t with
  | TensorType tt -> encode_message_field buf 1 (fun b -> encode_tensor_type b tt)

(* ============================================================================
   ValueInfoProto
   ============================================================================ *)

type value_info = {
  vi_name : string;
  vi_type : type_proto;
}

let encode_value_info (buf : Buffer.t) (v : value_info) : unit =
  encode_string_field buf 1 v.vi_name;
  encode_message_field buf 2 (fun b -> encode_type_proto b v.vi_type)

(* ============================================================================
   NodeProto
   ============================================================================ *)

type node = {
  n_input    : string list;
  n_output   : string list;
  n_name     : string;
  n_op_type  : string;
  n_domain   : string;          (* "" for default opset *)
}

let encode_node (buf : Buffer.t) (n : node) : unit =
  encode_repeated_string_field buf 1 n.n_input;
  encode_repeated_string_field buf 2 n.n_output;
  encode_string_field buf 3 n.n_name;
  encode_string_field buf 4 n.n_op_type;
  if n.n_domain <> "" then
    encode_string_field buf 7 n.n_domain

(* ============================================================================
   GraphProto
   ============================================================================ *)

type graph = {
  g_node    : node list;
  g_name    : string;
  g_input   : value_info list;
  g_output  : value_info list;
}

let encode_graph (buf : Buffer.t) (g : graph) : unit =
  encode_repeated_message_field buf 1 encode_node g.g_node;
  encode_string_field buf 2 g.g_name;
  encode_repeated_message_field buf 11 encode_value_info g.g_input;
  encode_repeated_message_field buf 12 encode_value_info g.g_output

(* ============================================================================
   OperatorSetIdProto
   ============================================================================ *)

type opset = {
  op_domain  : string;     (* "" for the default ONNX opset *)
  op_version : int;
}

let encode_opset (buf : Buffer.t) (o : opset) : unit =
  encode_string_field buf 1 o.op_domain;
  encode_int64_field buf 2 o.op_version

(* ============================================================================
   ModelProto
   ============================================================================ *)

type model = {
  m_ir_version       : int;
  m_producer_name    : string;
  m_producer_version : string;
  m_opset_import     : opset list;
  m_graph            : graph;
}

let encode_model (buf : Buffer.t) (m : model) : unit =
  encode_int64_field buf 1 m.m_ir_version;
  encode_repeated_message_field buf 8 encode_opset m.m_opset_import;
  encode_message_field buf 7 (fun b -> encode_graph b m.m_graph);
  encode_string_field buf 2 m.m_producer_name;
  encode_string_field buf 3 m.m_producer_version

let serialize_model (m : model) : string =
  let buf = Buffer.create 1024 in
  encode_model buf m;
  Buffer.contents buf
