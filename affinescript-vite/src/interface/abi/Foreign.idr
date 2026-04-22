-- SPDX-License-Identifier: PMPL-1.0-or-later
||| Foreign Function Interface Bridge
|||
||| This module defines the raw FFI calls and their safe wrappers,
||| implemented in the Zig FFI layer.

module Abi.Foreign

import Abi.Types
import Abi.Layout

%default total

--------------------------------------------------------------------------------
-- Library Lifecycle
--------------------------------------------------------------------------------

||| Raw FFI call to initialize the library
%foreign "C:rsr_init,librsr"
prim__init : PrimIO Bits64

||| Raw FFI call to free library resources
%foreign "C:rsr_free,librsr"
prim__free : Bits64 -> PrimIO ()

||| Safe wrapper for initialization
export
init : IO (Maybe Handle)
init = do
  ptr <- primIO prim__init
  pure (createHandle ptr)

||| Safe wrapper for cleanup
export
free : Handle -> IO ()
free h = primIO (prim__free h.ptr)

--------------------------------------------------------------------------------
-- Core Operations
--------------------------------------------------------------------------------

||| Raw FFI call for main processing
%foreign "C:rsr_process,librsr"
prim__process : Bits64 -> Bits32 -> PrimIO Bits32

||| Safe wrapper with error handling
export
process : Handle -> Bits32 -> IO (Either Result Bits32)
process h input = do
  result <- primIO (prim__process h.ptr input)
  if result == 0
    then pure (Left Error)
    else pure (Right result)

--------------------------------------------------------------------------------
-- Status and Metrics
--------------------------------------------------------------------------------

||| Get the current error description from the library
%foreign "C:rsr_get_error,librsr"
prim__getError : Bits64 -> PrimIO (Ptr String)

||| Detailed error string helper
export
errorDescription : Result -> String
errorDescription Ok = "Success"
errorDescription Error = "Generic error"
errorDescription InvalidParam = "Invalid parameter"
errorDescription Busy = "Library is busy"

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

||| Summary of ABI safety properties:
||| 1. All functions are total (total keyword enforced).
||| 2. Pointers are verified non-null before being wrapped in Handle.
||| 3. Memory layouts are proven C-ABI compliant in Abi.Layout.
||| 4. FFI boundary uses explicitly tagged types from Abi.Types.
public export
abiSafetyGuarantees : String
abiSafetyGuarantees = "RSR-Template ABI: 4 proven safety properties for FFI integration"
