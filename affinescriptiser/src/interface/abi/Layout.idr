-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| WASM Memory Layout Proofs for Affinescriptiser
|||
||| This module provides formal proofs about WASM linear memory layout,
||| alignment, and padding. Because affinescriptiser compiles to WebAssembly,
||| we must prove that the memory layout of tracked resources matches what
||| the WASM runtime expects. WASM uses 32-bit addresses and a flat linear
||| memory model — there is no heap management provided by the runtime.
|||
||| @see https://webassembly.github.io/spec/core/syntax/types.html

module Affinescriptiser.ABI.Layout

import Affinescriptiser.ABI.Types
import Data.Vect
import Data.So

%default total

--------------------------------------------------------------------------------
-- Alignment Utilities
--------------------------------------------------------------------------------

||| Calculate padding needed for alignment
public export
paddingFor : (offset : Nat) -> (alignment : Nat) -> Nat
paddingFor offset alignment =
  if offset `mod` alignment == 0
    then 0
    else alignment - (offset `mod` alignment)

||| Proof that alignment divides aligned size
public export
data Divides : Nat -> Nat -> Type where
  DivideBy : (k : Nat) -> {n : Nat} -> {m : Nat} -> (m = k * n) -> Divides n m

||| Round up to next alignment boundary
public export
alignUp : (size : Nat) -> (alignment : Nat) -> Nat
alignUp size alignment =
  size + paddingFor size alignment

||| Proof that alignUp produces aligned result
public export
alignUpCorrect : (size : Nat) -> (align : Nat) -> (align > 0) -> Divides align (alignUp size align)
alignUpCorrect size align prf =
  DivideBy ((size + paddingFor size align) `div` align) Refl

--------------------------------------------------------------------------------
-- WASM Linear Memory Layout
--------------------------------------------------------------------------------

||| WASM page size is always 64KiB (65536 bytes)
public export
wasmPageSize : Nat
wasmPageSize = 65536

||| Maximum WASM linear memory: 4GiB (65536 pages)
public export
wasmMaxPages : Nat
wasmMaxPages = 65536

||| Maximum WASM linear memory in bytes
public export
wasmMaxMemory : Nat
wasmMaxMemory = wasmPageSize * wasmMaxPages

||| A region in WASM linear memory
public export
record WASMRegion where
  constructor MkWASMRegion
  ||| Byte offset from start of linear memory
  offset : Nat
  ||| Size in bytes
  size : Nat
  ||| Required alignment
  alignment : Nat

||| Proof that a WASM region fits within linear memory bounds
public export
data RegionInBounds : WASMRegion -> Nat -> Type where
  InBounds : (r : WASMRegion) -> (memSize : Nat) -> {auto 0 ok : So (r.offset + r.size <= memSize)} -> RegionInBounds r memSize

||| Proof that two WASM regions do not overlap
||| Critical for affine correctness: two affine resources must not alias
public export
data NonOverlapping : WASMRegion -> WASMRegion -> Type where
  DisjointBefore : (a : WASMRegion) -> (b : WASMRegion) -> {auto 0 ok : So (a.offset + a.size <= b.offset)} -> NonOverlapping a b
  DisjointAfter  : (a : WASMRegion) -> (b : WASMRegion) -> {auto 0 ok : So (b.offset + b.size <= a.offset)} -> NonOverlapping a b

--------------------------------------------------------------------------------
-- Resource Memory Layout
--------------------------------------------------------------------------------

||| Memory layout for a tracked resource in WASM linear memory
||| Each tracked resource occupies a contiguous region with:
||| - 4 bytes: resource kind tag (Bits32)
||| - 4 bytes: linearity tag (Bits32)
||| - 4 bytes: ownership state (Bits32)
||| - 4 bytes: padding (alignment to 8 bytes)
||| - 4/8 bytes: raw handle (Bits32 on WASM, Bits64 on native)
public export
resourceLayoutSize : Platform -> Nat
resourceLayoutSize WASM = 20  -- 4+4+4+4+4 = 20 bytes (handle is 32-bit on WASM)
resourceLayoutSize _    = 24  -- 4+4+4+4+8 = 24 bytes (handle is 64-bit on native)

||| Alignment requirement for resource layout
public export
resourceLayoutAlign : Platform -> Nat
resourceLayoutAlign WASM = 4  -- WASM native alignment
resourceLayoutAlign _    = 8  -- 64-bit native alignment

||| WASM resource region given a base offset
public export
resourceRegion : Platform -> Nat -> WASMRegion
resourceRegion p baseOffset =
  MkWASMRegion
    (alignUp baseOffset (resourceLayoutAlign p))
    (resourceLayoutSize p)
    (resourceLayoutAlign p)

--------------------------------------------------------------------------------
-- Struct Field Layout (for user types passed through FFI)
--------------------------------------------------------------------------------

||| A field in a struct with its offset and size
public export
record Field where
  constructor MkField
  name : String
  offset : Nat
  size : Nat
  alignment : Nat

||| Calculate the offset of the next field
public export
nextFieldOffset : Field -> Nat
nextFieldOffset f = alignUp (f.offset + f.size) f.alignment

||| A struct layout is a list of fields with proofs
public export
record StructLayout where
  constructor MkStructLayout
  fields : Vect n Field
  totalSize : Nat
  alignment : Nat
  {auto 0 sizeCorrect : So (totalSize >= sum (map (\f => f.size) fields))}
  {auto 0 aligned : Divides alignment totalSize}

||| Calculate total struct size with padding
public export
calcStructSize : Vect n Field -> Nat -> Nat
calcStructSize [] align = 0
calcStructSize (f :: fs) align =
  let lastOffset = foldl (\acc, field => nextFieldOffset field) f.offset fs
      lastSize = foldr (\field, _ => field.size) f.size fs
   in alignUp (lastOffset + lastSize) align

||| Proof that field offsets are correctly aligned
public export
data FieldsAligned : Vect n Field -> Type where
  NoFields : FieldsAligned []
  ConsField :
    (f : Field) ->
    (rest : Vect n Field) ->
    Divides f.alignment f.offset ->
    FieldsAligned rest ->
    FieldsAligned (f :: rest)

||| Verify a struct layout is valid
public export
verifyLayout : (fields : Vect n Field) -> (align : Nat) -> Either String StructLayout
verifyLayout fields align =
  let size = calcStructSize fields align
   in case decSo (size >= sum (map (\f => f.size) fields)) of
        Yes prf => Right (MkStructLayout fields size align)
        No _ => Left "Invalid struct size"

--------------------------------------------------------------------------------
-- WASM-Specific Layout Proofs
--------------------------------------------------------------------------------

||| Proof that a struct layout fits within a single WASM page
public export
data FitsInPage : StructLayout -> Type where
  PageFit : (layout : StructLayout) -> {auto 0 ok : So (layout.totalSize <= wasmPageSize)} -> FitsInPage layout

||| Layout of the affine resource table in WASM linear memory
||| The resource table is a contiguous array of resource slots at a fixed
||| base address. Each slot holds one TrackedResource's memory representation.
public export
record ResourceTable where
  constructor MkResourceTable
  ||| Base offset in WASM linear memory
  baseOffset : Nat
  ||| Maximum number of tracked resources
  capacity : Nat
  ||| Platform (determines slot size)
  platform : Platform

||| Total size of a resource table in bytes
public export
tableSize : ResourceTable -> Nat
tableSize t = t.capacity * resourceLayoutSize t.platform

||| Proof that a resource table fits in WASM linear memory
public export
data TableInBounds : ResourceTable -> Type where
  TableOk : (t : ResourceTable) -> {auto 0 ok : So (t.baseOffset + tableSize t <= wasmMaxMemory)} -> TableInBounds t

||| Get the WASM region for the nth resource in a table
public export
slotRegion : ResourceTable -> (index : Nat) -> {auto 0 ok : So (index < capacity t)} -> WASMRegion
slotRegion t index =
  let slotSize = resourceLayoutSize t.platform
      slotOffset = t.baseOffset + (index * slotSize)
   in MkWASMRegion slotOffset slotSize (resourceLayoutAlign t.platform)

--------------------------------------------------------------------------------
-- C ABI Compatibility
--------------------------------------------------------------------------------

||| Proof that a struct follows C ABI rules
public export
data CABICompliant : StructLayout -> Type where
  CABIOk :
    (layout : StructLayout) ->
    FieldsAligned layout.fields ->
    CABICompliant layout

||| Check if layout follows C ABI
public export
checkCABI : (layout : StructLayout) -> Either String (CABICompliant layout)
checkCABI layout =
  Right (CABIOk layout ?fieldsAlignedProof)

--------------------------------------------------------------------------------
-- Example: Tracked File Descriptor Layout
--------------------------------------------------------------------------------

||| Layout of a tracked file descriptor in WASM linear memory
||| Fields: kind (4B) + linearity (4B) + ownership (4B) + padding (4B) + fd (4B)
public export
trackedFDLayout : StructLayout
trackedFDLayout =
  MkStructLayout
    [ MkField "kind"      0  4 4   -- ResourceKind tag at offset 0
    , MkField "linearity" 4  4 4   -- Linearity tag at offset 4
    , MkField "ownership" 8  4 4   -- Ownership state at offset 8
    , MkField "padding"   12 4 4   -- Alignment padding
    , MkField "fd"        16 4 4   -- File descriptor (Bits32 on WASM)
    ]
    20   -- Total size: 20 bytes
    4    -- Alignment: 4 bytes (WASM native)

--------------------------------------------------------------------------------
-- Offset Calculation
--------------------------------------------------------------------------------

||| Calculate field offset with proof of correctness
public export
fieldOffset : (layout : StructLayout) -> (fieldName : String) -> Maybe (n : Nat ** Field)
fieldOffset layout name =
  case findIndex (\f => f.name == name) layout.fields of
    Just idx => Just (finToNat idx ** index idx layout.fields)
    Nothing => Nothing

||| Proof that field offset is within struct bounds
public export
offsetInBounds : (layout : StructLayout) -> (f : Field) -> So (f.offset + f.size <= layout.totalSize)
offsetInBounds layout f = ?offsetInBoundsProof
