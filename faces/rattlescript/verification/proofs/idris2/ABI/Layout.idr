-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: Memory layout correctness
-- Proves struct size, alignment, and padding properties.
-- All proofs MUST be constructive (no believe_me, no assert_total).

module ABI.Layout

%default total

||| Witness that a type has a known size in bytes at compile time.
public export
interface HasSize (ty : Type) where
  sizeOf : Nat

||| Witness that a type has a known alignment in bytes.
public export
interface HasAlignment (ty : Type) where
  alignOf : Nat

||| Calculate padding needed to reach the next aligned offset.
||| paddingFor offset alignment = bytes to add so (offset + padding) `mod` alignment == 0
public export
paddingFor : (offset : Nat) -> (alignment : Nat) -> {auto 0 ok : NonZero alignment} -> Nat
paddingFor offset alignment = let r = modNatNZ offset alignment ok
                              in case r of
                                   Z => Z
                                   (S _) => minus alignment r

||| Proof that an offset with zero remainder needs zero padding.
export
alignedNeedsPadding : (n : Nat) -> (a : Nat) -> {auto 0 ok : NonZero a} ->
                      modNatNZ n a ok = 0 -> paddingFor n a = 0
alignedNeedsPadding n a prf = rewrite prf in Refl

||| A field within a struct, carrying its offset and size.
public export
record StructField where
  constructor MkField
  fieldName : String
  fieldOffset : Nat
  fieldSize : Nat
  fieldAlignment : Nat

||| Proof that a field is correctly aligned within a struct.
public export
FieldAligned : StructField -> Type
FieldAligned f = modNatNZ (fieldOffset f) (fieldAlignment f) SIsNonZero = 0

||| Proof that a field does not overflow past a given struct size.
public export
FieldInBounds : (structSize : Nat) -> StructField -> Type
FieldInBounds sz f = LTE (fieldOffset f + fieldSize f) sz

||| A struct layout is a list of fields with a total size.
public export
record StructLayout where
  constructor MkLayout
  layoutName : String
  layoutFields : List StructField
  layoutSize : Nat
  layoutAlignment : Nat
