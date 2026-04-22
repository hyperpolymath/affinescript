-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: Non-null pointer safety
-- Template proof — customise for your project's pointer types.
-- All proofs MUST be constructive (no b_me, no a_total).

module ABI.Pointers

import Data.So

%default total

||| A pointer value that has been proven non-null.
||| The `So` constraint carries a compile-time witness that `ptr /= 0`.
public export
record SafePtr where
  constructor MkSafePtr
  ptr : Bits64
  {auto 0 nonNull : So (ptr /= 0)}

||| Proof that SafePtr can never hold a null (zero) value.
||| This is enforced by the `So` constraint in the record.
export
safePtrNeverNull : (sp : SafePtr) -> So (sp.ptr /= 0)
safePtrNeverNull sp = sp.nonNull

||| Wrap a raw pointer with a runtime null check.
||| Returns Nothing if the pointer is null.
export
checkPtr : (raw : Bits64) -> Maybe SafePtr
checkPtr 0 = Nothing
checkPtr raw = case choose (raw /= 0) of
  Left prf => Just (MkSafePtr raw)
  Right _ => Nothing

||| Proof that checkPtr 0 always returns Nothing.
export
checkPtrZeroIsNothing : checkPtr 0 = Nothing
checkPtrZeroIsNothing = Refl

||| An opaque handle backed by a non-null pointer.
||| Use this for FFI resource handles (file descriptors, sockets, etc.).
public export
record Handle (tag : String) where
  constructor MkHandle
  safePtr : SafePtr

||| Proof that two handles with equal pointers are equal.
export
handlePtrEq : (h1, h2 : Handle tag) -> h1.safePtr.ptr = h2.safePtr.ptr -> h1 = h2
handlePtrEq (MkHandle (MkSafePtr p)) (MkHandle (MkSafePtr p)) Refl = Refl
