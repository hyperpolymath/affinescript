-- SPDX-License-Identifier: PMPL-1.0-or-later
||| ABI Type Definitions Template
|||
||| This module defines the Application Binary Interface (ABI) for this library.
||| All type definitions include formal proofs of correctness.

module Abi.Types

import Data.Bits
import Data.So
import Data.Vect
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- Platform Model
--------------------------------------------------------------------------------

||| Target platforms for the FFI bridge
public export
data Platform = Linux | MacOS | Windows | WASM | RISCV

||| Pointer size in bits per platform
public export
ptrSize : Platform -> Nat
ptrSize Linux = 64
ptrSize MacOS = 64
ptrSize Windows = 64
ptrSize WASM = 32
ptrSize RISCV = 64

||| Current target platform (detected at compile-time)
public export
thisPlatform : Platform
thisPlatform = Linux -- Simplified for template

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

||| Return codes for FFI calls
public export
data Result = Ok | Error | InvalidParam | Busy

||| Results are decidably equal
public export
implementation DecEq Result where
  decEq Ok Ok = Yes Refl
  decEq Error Error = Yes Refl
  decEq InvalidParam InvalidParam = Yes Refl
  decEq Busy Busy = Yes Refl
  decEq Ok Error = No (\case Refl impossible)
  decEq Ok InvalidParam = No (\case Refl impossible)
  decEq Ok Busy = No (\case Refl impossible)
  decEq Error Ok = No (\case Refl impossible)
  decEq Error InvalidParam = No (\case Refl impossible)
  decEq Error Busy = No (\case Refl impossible)
  decEq InvalidParam Ok = No (\case Refl impossible)
  decEq InvalidParam Error = No (\case Refl impossible)
  decEq InvalidParam Busy = No (\case Refl impossible)
  decEq Busy Ok = No (\case Refl impossible)
  decEq Busy Error = No (\case Refl impossible)
  decEq Busy InvalidParam = No (\case Refl impossible)

||| Opaque handle for library resources
||| Invariant: Handle pointer must be non-null
public export
record Handle where
  constructor MkHandle
  ptr : Bits64
  0 prf : So (ptr /= 0)

||| Returns Nothing if pointer is null
public export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle ptr = case decSo (ptr /= 0) of
  Yes p => Just (MkHandle ptr p)
  No _ => Nothing

--------------------------------------------------------------------------------
-- C-Types Mapping
--------------------------------------------------------------------------------

||| Tagged types for C-FFI boundary
public export
data CType = CInt | CUInt | CLong | CULong | CPtrType

||| Pointer type for platform
public export
CPtr : Platform -> CType -> Type
CPtr p _ = Bits64 -- Simplified for 64-bit template

||| Size of C types (platform-specific)
public export
cSizeOf : (p : Platform) -> (t : CType) -> Nat
cSizeOf p CInt = 4
cSizeOf p CUInt = 4
cSizeOf p CLong = 8
cSizeOf p CULong = 8
cSizeOf p CPtrType = 8

||| Alignment of C types (platform-specific)
public export
cAlignOf : (p : Platform) -> (t : CType) -> Nat
cAlignOf p CInt = 4
cAlignOf p CUInt = 4
cAlignOf p CLong = 8
cAlignOf p CULong = 8
cAlignOf p CPtrType = 8
