(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Runtime values for the AffineScript interpreter.

    This module defines the value representation used during
    interpretation, including closures, records, and effect handlers.
*)

open Ast

(** Runtime value *)
type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VChar of char
  | VString of string
  | VTuple of value list
  | VArray of value array
  | VRecord of (string * value) list
  | VVariant of string * value option
  | VClosure of closure
  | VBuiltin of string * (value list -> value result)
  | VRef of value ref
  | VMut of value ref
  | VOwn of value ref * bool ref  (** Value + moved flag *)

(** Closure: captures environment *)
and closure = {
  cl_params : param list;
  cl_body : expr;
  cl_env : env;
}

(** Environment: maps names to values *)
and env = (string * value) list

(** Evaluation errors *)
and eval_error =
  | UnboundVariable of string
  | TypeMismatch of string
  | DivisionByZero
  | IndexOutOfBounds of int * int
  | FieldNotFound of string
  | PatternMatchFailure
  | AffineViolation of string  (** Double use of affine value *)
  | RuntimeError of string

and 'a result = ('a, eval_error) Result.t

(** Show function for evaluation errors *)
let show_eval_error (e : eval_error) : string =
  match e with
  | UnboundVariable name -> "Unbound variable: " ^ name
  | TypeMismatch msg -> "Type mismatch: " ^ msg
  | DivisionByZero -> "Division by zero"
  | IndexOutOfBounds (idx, len) ->
    Printf.sprintf "Index out of bounds: %d (length: %d)" idx len
  | FieldNotFound field -> "Field not found: " ^ field
  | PatternMatchFailure -> "Pattern match failure"
  | AffineViolation msg -> "Affine violation: " ^ msg
  | RuntimeError msg -> "Runtime error: " ^ msg

let pp_eval_error fmt e = Format.pp_print_string fmt (show_eval_error e)

(** Empty environment *)
let empty_env : env = []

(** Extend environment with a binding *)
let extend_env (name : string) (value : value) (env : env) : env =
  (name, value) :: env

(** Extend environment with multiple bindings *)
let extend_env_list (bindings : (string * value) list) (env : env) : env =
  bindings @ env

(** Look up a variable in the environment *)
let rec lookup_env (name : string) (env : env) : value result =
  match env with
  | [] -> Error (UnboundVariable name)
  | (n, v) :: rest ->
    if n = name then Ok v
    else lookup_env name rest

(** Check if a value is truthy *)
let is_truthy (v : value) : bool =
  match v with
  | VBool false -> false
  | VUnit -> false
  | _ -> true

(** Convert value to string for display *)
let rec show_value (v : value) : string =
  match v with
  | VUnit -> "()"
  | VBool true -> "true"
  | VBool false -> "false"
  | VInt n -> string_of_int n
  | VFloat f -> string_of_float f
  | VChar c -> "'" ^ Char.escaped c ^ "'"
  | VString s -> "\"" ^ String.escaped s ^ "\""
  | VTuple vs ->
    "(" ^ String.concat ", " (List.map show_value vs) ^ ")"
  | VArray arr ->
    "[" ^ String.concat ", " (Array.to_list (Array.map show_value arr)) ^ "]"
  | VRecord fields ->
    "{" ^ String.concat ", " (List.map (fun (n, v) ->
      n ^ ": " ^ show_value v
    ) fields) ^ "}"
  | VVariant (tag, None) -> tag
  | VVariant (tag, Some v) -> tag ^ "(" ^ show_value v ^ ")"
  | VClosure _ -> "<function>"
  | VBuiltin (name, _) -> "<builtin:" ^ name ^ ">"
  | VRef r -> "ref(" ^ show_value !r ^ ")"
  | VMut r -> "mut(" ^ show_value !r ^ ")"
  | VOwn (r, moved) ->
    if !moved then "<moved>"
    else "own(" ^ show_value !r ^ ")"

(** Pretty printer for values *)
let pp_value fmt v = Format.pp_print_string fmt (show_value v)

(** Check affine ownership - mark as moved *)
let check_and_move_own (v : value) : value result =
  match v with
  | VOwn (r, moved_flag) ->
    if !moved_flag then
      Error (AffineViolation "Value already moved")
    else begin
      moved_flag := true;
      Ok !r
    end
  | _ -> Ok v

(** Dereference a reference *)
let deref (v : value) : value result =
  match v with
  | VRef r | VMut r -> Ok !r
  | VOwn (r, moved_flag) ->
    if !moved_flag then
      Error (AffineViolation "Cannot dereference moved value")
    else
      Ok !r
  | _ -> Error (TypeMismatch "Expected reference type")

(** Assign to a mutable reference *)
let assign (v : value) (new_val : value) : unit result =
  match v with
  | VMut r ->
    r := new_val;
    Ok ()
  | VRef _ -> Error (TypeMismatch "Cannot assign to immutable reference")
  | _ -> Error (TypeMismatch "Expected mutable reference")

(** Binary operation on integers *)
let binop_int (op : binary_op) (a : int) (b : int) : value result =
  match op with
  | OpAdd -> Ok (VInt (a + b))
  | OpSub -> Ok (VInt (a - b))
  | OpMul -> Ok (VInt (a * b))
  | OpDiv ->
    if b = 0 then Error DivisionByZero
    else Ok (VInt (a / b))
  | OpMod ->
    if b = 0 then Error DivisionByZero
    else Ok (VInt (a mod b))
  | OpEq -> Ok (VBool (a = b))
  | OpNe -> Ok (VBool (a <> b))
  | OpLt -> Ok (VBool (a < b))
  | OpLe -> Ok (VBool (a <= b))
  | OpGt -> Ok (VBool (a > b))
  | OpGe -> Ok (VBool (a >= b))
  | OpBitAnd -> Ok (VInt (a land b))
  | OpBitOr -> Ok (VInt (a lor b))
  | OpBitXor -> Ok (VInt (a lxor b))
  | OpShl -> Ok (VInt (a lsl b))
  | OpShr -> Ok (VInt (a lsr b))
  | OpAnd | OpOr -> Error (TypeMismatch "Logical operators require booleans")

(** Binary operation on floats *)
let binop_float (op : binary_op) (a : float) (b : float) : value result =
  match op with
  | OpAdd -> Ok (VFloat (a +. b))
  | OpSub -> Ok (VFloat (a -. b))
  | OpMul -> Ok (VFloat (a *. b))
  | OpDiv -> Ok (VFloat (a /. b))
  | OpEq -> Ok (VBool (a = b))
  | OpNe -> Ok (VBool (a <> b))
  | OpLt -> Ok (VBool (a < b))
  | OpLe -> Ok (VBool (a <= b))
  | OpGt -> Ok (VBool (a > b))
  | OpGe -> Ok (VBool (a >= b))
  | _ -> Error (TypeMismatch "Operation not supported on floats")

(** Binary operation on strings *)
let binop_string (op : binary_op) (a : string) (b : string) : value result =
  match op with
  | OpAdd -> Ok (VString (a ^ b))
  | OpEq -> Ok (VBool (a = b))
  | OpNe -> Ok (VBool (a <> b))
  | OpLt -> Ok (VBool (String.compare a b < 0))
  | OpLe -> Ok (VBool (String.compare a b <= 0))
  | OpGt -> Ok (VBool (String.compare a b > 0))
  | OpGe -> Ok (VBool (String.compare a b >= 0))
  | _ -> Error (TypeMismatch "Operation not supported on strings")

(** Binary operation on booleans *)
let binop_bool (op : binary_op) (a : bool) (b : bool) : value result =
  match op with
  | OpAnd -> Ok (VBool (a && b))
  | OpOr -> Ok (VBool (a || b))
  | OpEq -> Ok (VBool (a = b))
  | OpNe -> Ok (VBool (a <> b))
  | _ -> Error (TypeMismatch "Operation not supported on booleans")

(** Unary operation *)
let unary_op (op : unary_op) (v : value) : value result =
  match (op, v) with
  | (OpNeg, VInt n) -> Ok (VInt (-n))
  | (OpNeg, VFloat f) -> Ok (VFloat (-.f))
  | (OpNot, VBool b) -> Ok (VBool (not b))
  | (OpBitNot, VInt n) -> Ok (VInt (lnot n))
  | (OpRef, v) -> Ok (VRef (ref v))
  | (OpDeref, _) -> deref v
  | _ -> Error (TypeMismatch "Invalid unary operation")

(** Get field from record *)
let get_field (name : string) (fields : (string * value) list) : value result =
  match List.assoc_opt name fields with
  | Some v -> Ok v
  | None -> Error (FieldNotFound name)

(** Get element from tuple *)
let get_tuple_elem (idx : int) (elems : value list) : value result =
  if idx >= 0 && idx < List.length elems then
    Ok (List.nth elems idx)
  else
    Error (IndexOutOfBounds (idx, List.length elems))

(** Get element from array *)
let get_array_elem (idx : int) (arr : value array) : value result =
  if idx >= 0 && idx < Array.length arr then
    Ok (Array.get arr idx)
  else
    Error (IndexOutOfBounds (idx, Array.length arr))

(** Set element in array *)
let set_array_elem (idx : int) (arr : value array) (v : value) : unit result =
  if idx >= 0 && idx < Array.length arr then begin
    Array.set arr idx v;
    Ok ()
  end else
    Error (IndexOutOfBounds (idx, Array.length arr))

(** Value equality (structural) *)
let rec value_eq (v1 : value) (v2 : value) : bool =
  match (v1, v2) with
  | (VUnit, VUnit) -> true
  | (VBool a, VBool b) -> a = b
  | (VInt a, VInt b) -> a = b
  | (VFloat a, VFloat b) -> a = b
  | (VChar a, VChar b) -> a = b
  | (VString a, VString b) -> a = b
  | (VTuple vs1, VTuple vs2) ->
    List.length vs1 = List.length vs2 &&
    List.for_all2 value_eq vs1 vs2
  | (VArray arr1, VArray arr2) ->
    Array.length arr1 = Array.length arr2 &&
    Array.for_all2 value_eq arr1 arr2
  | (VRecord fs1, VRecord fs2) ->
    List.length fs1 = List.length fs2 &&
    List.for_all (fun (n1, v1) ->
      match List.assoc_opt n1 fs2 with
      | Some v2 -> value_eq v1 v2
      | None -> false
    ) fs1
  | (VVariant (t1, v1), VVariant (t2, v2)) ->
    t1 = t2 && (match (v1, v2) with
      | (None, None) -> true
      | (Some a, Some b) -> value_eq a b
      | _ -> false)
  | _ -> false
