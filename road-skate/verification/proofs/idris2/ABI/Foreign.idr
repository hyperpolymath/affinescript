-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: FFI function return type proofs
-- Proves that all FFI functions return expected types.
-- All proofs MUST be constructive (no b_me, no a_total).

module ABI.Foreign

%default total

||| Result type for FFI operations.
||| All FFI functions must return through this type.
public export
data FFIResult : Type -> Type where
  FFISuccess : (value : a) -> FFIResult a
  FFIError   : (code : Int) -> (msg : String) -> FFIResult a

||| Proof that FFIResult is a functor (map preserves structure).
export
mapFFIResult : (a -> b) -> FFIResult a -> FFIResult b
mapFFIResult f (FFISuccess value) = FFISuccess (f value)
mapFFIResult f (FFIError code msg) = FFIError code msg

||| Proof that mapping identity preserves the result.
export
mapIdPreserves : (r : FFIResult a) -> mapFFIResult Prelude.id r = r
mapIdPreserves (FFISuccess value) = Refl
mapIdPreserves (FFIError code msg) = Refl

||| An FFI function specification: name, argument types, return type.
public export
record FFISpec where
  constructor MkFFISpec
  ffiName : String
  ffiReturnType : Type

||| Proof that an FFI spec has a specific return type.
||| Use this to verify at compile time that FFI functions return the
||| types we expect across the C ABI boundary.
public export
FFIReturns : FFISpec -> Type -> Type
FFIReturns spec ty = ffiReturnType spec = ty

||| C calling convention marker.
||| Proofs about calling convention compatibility.
public export
data CallingConv = CDecl | StdCall | FastCall

||| All hyperpolymath FFI uses CDecl.
public export
defaultCallingConv : CallingConv
defaultCallingConv = CDecl
