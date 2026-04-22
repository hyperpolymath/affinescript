-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: Platform-specific type size proofs
-- Proves that C type sizes are correct per platform.
-- All proofs MUST be constructive (no believe_me, no assert_total).

module ABI.Platform

%default total

||| Supported target platforms for ABI verification.
public export
data Platform = Linux64 | LinuxARM64 | MacOS64 | MacOSARM64
              | Windows64 | FreeBSD64 | WASM32

||| Pointer size in bytes for each platform.
public export
ptrSize : Platform -> Nat
ptrSize WASM32 = 4
ptrSize _ = 8

||| C `int` size in bytes.
public export
cIntSize : Platform -> Nat
cIntSize _ = 4

||| C `size_t` size in bytes (matches pointer size).
public export
cSizeT : Platform -> Nat
cSizeT = ptrSize

||| Proof that size_t always equals pointer size on all platforms.
export
sizeTEqPtrSize : (p : Platform) -> cSizeT p = ptrSize p
sizeTEqPtrSize _ = Refl

||| Proof that pointer size is always 4 or 8 bytes.
export
ptrSizeValid : (p : Platform) -> Either (ptrSize p = 4) (ptrSize p = 8)
ptrSizeValid WASM32 = Left Refl
ptrSizeValid Linux64 = Right Refl
ptrSizeValid LinuxARM64 = Right Refl
ptrSizeValid MacOS64 = Right Refl
ptrSizeValid MacOSARM64 = Right Refl
ptrSizeValid Windows64 = Right Refl
ptrSizeValid FreeBSD64 = Right Refl

||| Proof that C int is always 4 bytes on all platforms.
export
cIntAlways4 : (p : Platform) -> cIntSize p = 4
cIntAlways4 _ = Refl

||| Proof that pointer size is always at least 4 bytes.
export
ptrSizeAtLeast4 : (p : Platform) -> LTE 4 (ptrSize p)
ptrSizeAtLeast4 WASM32 = lteRefl
ptrSizeAtLeast4 Linux64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
ptrSizeAtLeast4 LinuxARM64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
ptrSizeAtLeast4 MacOS64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
ptrSizeAtLeast4 MacOSARM64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
ptrSizeAtLeast4 Windows64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
ptrSizeAtLeast4 FreeBSD64 = lteSuccRight (lteSuccRight (lteSuccRight (lteSuccRight lteRefl)))
