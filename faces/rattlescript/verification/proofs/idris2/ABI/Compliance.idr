-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- ABI Proof: C ABI compliance
-- Proves that struct layouts are C ABI compliant.
-- All proofs MUST be constructive (no believe_me, no assert_total).

module ABI.Compliance

import ABI.Layout
import ABI.Platform

%default total

||| Evidence that every field in a layout is correctly aligned.
public export
data AllFieldsAligned : List StructField -> Type where
  AFANil  : AllFieldsAligned []
  AFACons : FieldAligned f -> AllFieldsAligned fs -> AllFieldsAligned (f :: fs)

||| Evidence that every field is within the struct bounds.
public export
data AllFieldsInBounds : (size : Nat) -> List StructField -> Type where
  AFBNil  : AllFieldsInBounds size []
  AFBCons : FieldInBounds size f -> AllFieldsInBounds size fs -> AllFieldsInBounds size (f :: fs)

||| A struct layout is C ABI compliant when:
||| 1. All fields are aligned to their natural alignment
||| 2. All fields are within bounds of the struct size
||| 3. The struct size is a multiple of the struct alignment
public export
record CABICompliant (layout : StructLayout) where
  constructor MkCompliant
  fieldsAligned  : AllFieldsAligned (layoutFields layout)
  fieldsInBounds : AllFieldsInBounds (layoutSize layout) (layoutFields layout)
  sizeAligned    : modNatNZ (layoutSize layout) (layoutAlignment layout) SIsNonZero = 0

||| An empty struct is trivially compliant (size=1, alignment=1).
export
emptyStructCompliant : CABICompliant (MkLayout "empty" [] 1 1)
emptyStructCompliant = MkCompliant AFANil AFBNil Refl
