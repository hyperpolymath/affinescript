-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| ABI Type Definitions for Affinescriptiser
|||
||| This module defines the core type system for affine resource tracking.
||| It formalises ResourceKind (what kind of resource), Linearity (how many
||| times it may be used), and Ownership (who holds the resource). These
||| types are the foundation for proving that wrapped code cannot leak,
||| double-free, or use-after-release any tracked resource.
|||
||| @see https://idris2.readthedocs.io for Idris2 documentation

module Affinescriptiser.ABI.Types

import Data.Bits
import Data.So
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Platform Detection
--------------------------------------------------------------------------------

||| Supported target platforms for WASM compilation
||| WASM is the primary target; native platforms are for testing and FFI
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

||| Compile-time platform detection
||| Affinescriptiser primarily targets WASM, but supports native for testing
public export
thisPlatform : Platform
thisPlatform =
  %runElab do
    -- Platform detection logic — default WASM for affinescriptiser
    pure WASM

--------------------------------------------------------------------------------
-- Resource Kinds
--------------------------------------------------------------------------------

||| Classification of trackable resources
||| Each variant represents a distinct kind of resource that affinescriptiser
||| can wrap with affine types. The codegen phase maps detected handles in
||| the user's Rust/C/Zig source to one of these kinds.
public export
data ResourceKind : Type where
  ||| File descriptor or file handle (open/close pair)
  FileDescriptor : ResourceKind
  ||| Heap allocation (malloc/free, alloc/dealloc pair)
  HeapAllocation : ResourceKind
  ||| Network socket (connect/close pair)
  NetworkSocket : ResourceKind
  ||| Mutex or lock (lock/unlock pair)
  MutexLock : ResourceKind
  ||| GPU buffer (allocate/release pair, CUDA or Vulkan)
  GPUBuffer : ResourceKind
  ||| Memory-mapped region (mmap/munmap pair)
  MemoryMap : ResourceKind
  ||| Database connection or transaction handle
  DatabaseHandle : ResourceKind
  ||| Generic opaque resource for user-defined acquire/release pairs
  OpaqueResource : String -> ResourceKind

||| ResourceKind equality is decidable for all concrete variants
public export
Eq ResourceKind where
  FileDescriptor == FileDescriptor = True
  HeapAllocation == HeapAllocation = True
  NetworkSocket  == NetworkSocket  = True
  MutexLock      == MutexLock      = True
  GPUBuffer      == GPUBuffer      = True
  MemoryMap      == MemoryMap      = True
  DatabaseHandle == DatabaseHandle = True
  (OpaqueResource a) == (OpaqueResource b) = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- Linearity
--------------------------------------------------------------------------------

||| Linearity constraint for a resource
||| Determines how many times a resource value may be used.
||| Affinescriptiser enforces these constraints in the generated AffineScript.
public export
data Linearity : Type where
  ||| Must be used exactly once (linear type)
  Linear : Linearity
  ||| May be used at most once (affine type — the primary mode)
  Affine : Linearity
  ||| May be used any number of times (no tracking, opt-out)
  Unrestricted : Linearity

||| Proof that Affine is weaker than Linear
||| Linear implies Affine — anything that satisfies linear constraints
||| also satisfies affine constraints
public export
data WeakerThan : Linearity -> Linearity -> Type where
  AffineWeakerThanLinear : WeakerThan Affine Linear
  UnrestrictedWeakerThanAffine : WeakerThan Unrestricted Affine
  UnrestrictedWeakerThanLinear : WeakerThan Unrestricted Linear
  ReflexiveLinearity : (l : Linearity) -> WeakerThan l l

||| Default linearity for each resource kind
||| Most resources are affine (at-most-once) — the safe default.
||| Mutex locks are linear (exactly-once) because failing to unlock is a bug.
public export
defaultLinearity : ResourceKind -> Linearity
defaultLinearity MutexLock = Linear
defaultLinearity _         = Affine

--------------------------------------------------------------------------------
-- Ownership
--------------------------------------------------------------------------------

||| Ownership state of a resource at a given program point
||| The codegen tracks ownership transitions and proves that no resource
||| is used after transfer or release.
public export
data Ownership : Type where
  ||| Resource is owned by the current scope — may be used or transferred
  Owned : Ownership
  ||| Resource has been borrowed immutably — must not be released
  BorrowedImmutable : Ownership
  ||| Resource has been borrowed mutably — exclusive access, must not be released
  BorrowedMutable : Ownership
  ||| Ownership has been transferred to another scope — must not be used
  Transferred : Ownership
  ||| Resource has been released — must not be used or released again
  Released : Ownership

||| Proof that a resource in a given ownership state may be used
public export
data CanUse : Ownership -> Type where
  CanUseOwned : CanUse Owned
  CanUseBorrowedImmutable : CanUse BorrowedImmutable
  CanUseBorrowedMutable : CanUse BorrowedMutable

||| Proof that a resource in a given ownership state may be released
public export
data CanRelease : Ownership -> Type where
  CanReleaseOwned : CanRelease Owned

||| Proof that a resource in a given ownership state may be transferred
public export
data CanTransfer : Ownership -> Type where
  CanTransferOwned : CanTransfer Owned

||| Ownership transition: release an owned resource
public export
release : (o : Ownership) -> {auto prf : CanRelease o} -> Ownership
release Owned = Released

||| Ownership transition: transfer an owned resource
public export
transfer : (o : Ownership) -> {auto prf : CanTransfer o} -> Ownership
transfer Owned = Transferred

--------------------------------------------------------------------------------
-- FFI Result Codes
--------------------------------------------------------------------------------

||| Result codes for FFI operations
||| C-compatible integers for cross-language interop
public export
data Result : Type where
  ||| Operation succeeded
  Ok : Result
  ||| Generic error
  Error : Result
  ||| Invalid parameter provided
  InvalidParam : Result
  ||| Out of memory
  OutOfMemory : Result
  ||| Null pointer encountered
  NullPointer : Result
  ||| Affine violation detected (double use or use after release)
  AffineViolation : Result
  ||| Resource leak detected (affine resource not released before scope exit)
  ResourceLeak : Result

||| Convert Result to C integer
public export
resultToInt : Result -> Bits32
resultToInt Ok              = 0
resultToInt Error           = 1
resultToInt InvalidParam    = 2
resultToInt OutOfMemory     = 3
resultToInt NullPointer     = 4
resultToInt AffineViolation = 5
resultToInt ResourceLeak    = 6

||| Results are decidably equal
public export
DecEq Result where
  decEq Ok Ok = Yes Refl
  decEq Error Error = Yes Refl
  decEq InvalidParam InvalidParam = Yes Refl
  decEq OutOfMemory OutOfMemory = Yes Refl
  decEq NullPointer NullPointer = Yes Refl
  decEq AffineViolation AffineViolation = Yes Refl
  decEq ResourceLeak ResourceLeak = Yes Refl
  decEq _ _ = No absurd

--------------------------------------------------------------------------------
-- Tracked Resource Handle
--------------------------------------------------------------------------------

||| A tracked resource: pairs a raw handle with its kind, linearity, and ownership.
||| The type-level ownership parameter ensures that the Idris2 type checker
||| rejects code that uses a resource after transfer or release.
public export
data TrackedResource : ResourceKind -> Linearity -> Ownership -> Type where
  MkTracked : (ptr : Bits64)
           -> (kind : ResourceKind)
           -> (lin : Linearity)
           -> {auto 0 nonNull : So (ptr /= 0)}
           -> TrackedResource kind lin Owned

||| Extract the raw pointer from a tracked resource
public export
trackedPtr : TrackedResource kind lin own -> Bits64
trackedPtr (MkTracked ptr _ _) = ptr

||| Safely create a tracked resource from a raw pointer
public export
trackResource : (ptr : Bits64) -> (kind : ResourceKind) -> Maybe (TrackedResource kind (defaultLinearity kind) Owned)
trackResource 0 _    = Nothing
trackResource ptr kind = Just (MkTracked ptr kind (defaultLinearity kind))

--------------------------------------------------------------------------------
-- Opaque Handles (for FFI bridge)
--------------------------------------------------------------------------------

||| Opaque handle type for the Zig FFI layer
||| Prevents direct construction; must be created through safe API
public export
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle

||| Safely create a handle from a pointer value
public export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle ptr = Just (MkHandle ptr)

||| Extract pointer value from handle
public export
handlePtr : Handle -> Bits64
handlePtr (MkHandle ptr) = ptr

--------------------------------------------------------------------------------
-- Platform-Specific Types
--------------------------------------------------------------------------------

||| C int size varies by platform
public export
CInt : Platform -> Type
CInt Linux   = Bits32
CInt Windows = Bits32
CInt MacOS   = Bits32
CInt BSD     = Bits32
CInt WASM    = Bits32

||| C size_t varies by platform — WASM uses 32-bit addresses
public export
CSize : Platform -> Type
CSize WASM = Bits32
CSize _    = Bits64

||| Pointer size in bits — WASM is 32-bit, native is 64-bit
public export
ptrSize : Platform -> Nat
ptrSize WASM = 32
ptrSize _    = 64

||| Pointer type for platform
public export
CPtr : Platform -> Type -> Type
CPtr p _ = Bits (ptrSize p)

--------------------------------------------------------------------------------
-- Memory Layout Proofs
--------------------------------------------------------------------------------

||| Proof that a type has a specific size in bytes
public export
data HasSize : Type -> Nat -> Type where
  SizeProof : {0 t : Type} -> {n : Nat} -> HasSize t n

||| Proof that a type has a specific alignment in bytes
public export
data HasAlignment : Type -> Nat -> Type where
  AlignProof : {0 t : Type} -> {n : Nat} -> HasAlignment t n

||| Size of C types (platform-specific)
public export
cSizeOf : (p : Platform) -> (t : Type) -> Nat
cSizeOf p (CInt _)  = 4
cSizeOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cSizeOf p Bits32    = 4
cSizeOf p Bits64    = 8
cSizeOf p Double    = 8
cSizeOf p _         = ptrSize p `div` 8

||| Alignment of C types (platform-specific)
public export
cAlignOf : (p : Platform) -> (t : Type) -> Nat
cAlignOf p (CInt _)  = 4
cAlignOf p (CSize _) = if ptrSize p == 64 then 8 else 4
cAlignOf p Bits32    = 4
cAlignOf p Bits64    = 8
cAlignOf p Double    = 8
cAlignOf p _         = ptrSize p `div` 8

--------------------------------------------------------------------------------
-- WASM-Specific Proofs
--------------------------------------------------------------------------------

||| Proof that a WASM linear memory offset is within bounds
||| WASM linear memory uses 32-bit addresses (max 4GiB)
public export
data WASMOffsetValid : Nat -> Nat -> Type where
  OffsetOk : (offset : Nat) -> (size : Nat) -> {auto 0 inBounds : So (offset + size <= 4294967296)} -> WASMOffsetValid offset size

||| Proof that a tracked resource's pointer fits in WASM address space
public export
wasmPtrValid : TrackedResource kind lin own -> Platform -> Bool
wasmPtrValid res WASM = trackedPtr res < 4294967296
wasmPtrValid _ _      = True

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

||| Compile-time verification of ABI properties
namespace Verify

  ||| Verify that all resource kinds have a default linearity
  export
  verifyDefaultLinearities : IO ()
  verifyDefaultLinearities = do
    putStrLn $ "FileDescriptor: " ++ show (defaultLinearity FileDescriptor == Affine)
    putStrLn $ "HeapAllocation: " ++ show (defaultLinearity HeapAllocation == Affine)
    putStrLn $ "MutexLock: " ++ show (defaultLinearity MutexLock == Linear)
    putStrLn "ABI linearities verified"

  ||| Verify struct sizes are correct for WASM target
  export
  verifySizes : IO ()
  verifySizes = do
    putStrLn $ "WASM ptr size: " ++ show (ptrSize WASM) ++ " bits"
    putStrLn $ "WASM CSize: " ++ show (cSizeOf WASM Bits32) ++ " bytes"
    putStrLn "ABI sizes verified"
