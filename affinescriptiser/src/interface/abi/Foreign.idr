-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| Foreign Function Interface Declarations for Affinescriptiser
|||
||| This module declares the C-compatible FFI for the affinescriptiser WASM
||| compilation engine. Functions here manage the lifecycle of the compilation
||| context, submit source files for analysis, configure affine tracking, and
||| trigger WASM compilation. All functions are implemented in the Zig FFI
||| layer (src/interface/ffi/).
|||
||| The key domain-specific operations are:
||| - Resource registration: declare which handles need affine tracking
||| - Affine analysis: verify that tracked resources satisfy linearity constraints
||| - WASM compilation: compile analysed + wrapped code to .wasm output

module Affinescriptiser.ABI.Foreign

import Affinescriptiser.ABI.Types
import Affinescriptiser.ABI.Layout

%default total

--------------------------------------------------------------------------------
-- Library Lifecycle
--------------------------------------------------------------------------------

||| Initialize the affinescriptiser compilation context
||| Returns a handle to the context, or Nothing on failure
export
%foreign "C:affinescriptiser_init, libaffinescriptiser"
prim__init : PrimIO Bits64

||| Safe wrapper for context initialization
export
init : IO (Maybe Handle)
init = do
  ptr <- primIO prim__init
  pure (createHandle ptr)

||| Clean up the compilation context and all tracked resources
export
%foreign "C:affinescriptiser_free, libaffinescriptiser"
prim__free : Bits64 -> PrimIO ()

||| Safe wrapper for cleanup
export
free : Handle -> IO ()
free h = primIO (prim__free (handlePtr h))

--------------------------------------------------------------------------------
-- Source File Registration
--------------------------------------------------------------------------------

||| Register a source file for analysis
||| The file path is a null-terminated C string
export
%foreign "C:affinescriptiser_register_source, libaffinescriptiser"
prim__registerSource : Bits64 -> String -> Bits32 -> PrimIO Bits32

||| Source language identifier
||| 0 = Rust, 1 = C, 2 = Zig
public export
data SourceLang = Rust | CLang | Zig

||| Convert source language to C integer tag
public export
sourceLangToInt : SourceLang -> Bits32
sourceLangToInt Rust  = 0
sourceLangToInt CLang = 1
sourceLangToInt Zig   = 2

||| Register a source file for affine analysis
export
registerSource : Handle -> String -> SourceLang -> IO (Either Result ())
registerSource h path lang = do
  result <- primIO (prim__registerSource (handlePtr h) path (sourceLangToInt lang))
  pure $ case result of
    0 => Right ()
    _ => Left Error

--------------------------------------------------------------------------------
-- Resource Tracking Configuration
--------------------------------------------------------------------------------

||| Register a resource kind for affine tracking
||| Takes the context handle, a resource name, and a resource kind tag
export
%foreign "C:affinescriptiser_track_resource, libaffinescriptiser"
prim__trackResource : Bits64 -> String -> Bits32 -> Bits32 -> PrimIO Bits32

||| Convert ResourceKind to C integer tag
public export
resourceKindToInt : ResourceKind -> Bits32
resourceKindToInt FileDescriptor   = 0
resourceKindToInt HeapAllocation   = 1
resourceKindToInt NetworkSocket    = 2
resourceKindToInt MutexLock        = 3
resourceKindToInt GPUBuffer        = 4
resourceKindToInt MemoryMap        = 5
resourceKindToInt DatabaseHandle   = 6
resourceKindToInt (OpaqueResource _) = 7

||| Convert Linearity to C integer tag
public export
linearityToInt : Linearity -> Bits32
linearityToInt Linear       = 0
linearityToInt Affine       = 1
linearityToInt Unrestricted = 2

||| Register a resource for affine tracking
export
trackResource : Handle -> String -> ResourceKind -> Linearity -> IO (Either Result ())
trackResource h name kind lin = do
  result <- primIO (prim__trackResource (handlePtr h) name (resourceKindToInt kind) (linearityToInt lin))
  pure $ case result of
    0 => Right ()
    _ => Left Error

--------------------------------------------------------------------------------
-- Affine Analysis
--------------------------------------------------------------------------------

||| Run affine analysis on all registered source files
||| Returns 0 on success (all resources satisfy linearity), nonzero on violation
export
%foreign "C:affinescriptiser_analyse, libaffinescriptiser"
prim__analyse : Bits64 -> PrimIO Bits32

||| Run affine analysis and report result
export
analyse : Handle -> IO (Either Result ())
analyse h = do
  result <- primIO (prim__analyse (handlePtr h))
  pure $ case result of
    0 => Right ()
    5 => Left AffineViolation
    6 => Left ResourceLeak
    _ => Left Error

||| Get the number of affine violations found
export
%foreign "C:affinescriptiser_violation_count, libaffinescriptiser"
prim__violationCount : Bits64 -> PrimIO Bits32

||| Get violation count after analysis
export
violationCount : Handle -> IO Bits32
violationCount h = primIO (prim__violationCount (handlePtr h))

||| Get a human-readable violation report
export
%foreign "C:affinescriptiser_violation_report, libaffinescriptiser"
prim__violationReport : Bits64 -> PrimIO Bits64

||| Get violation report as string
export
violationReport : Handle -> IO (Maybe String)
violationReport h = do
  ptr <- primIO (prim__violationReport (handlePtr h))
  if ptr == 0
    then pure Nothing
    else pure (Just (prim__getString ptr))
  where
    %foreign "support:idris2_getString, libidris2_support"
    prim__getString : Bits64 -> String

--------------------------------------------------------------------------------
-- WASM Compilation
--------------------------------------------------------------------------------

||| Compile analysed code to WASM
||| Must be called after successful analyse()
||| output_path is the path for the .wasm file
export
%foreign "C:affinescriptiser_compile_wasm, libaffinescriptiser"
prim__compileWasm : Bits64 -> String -> Bits32 -> PrimIO Bits32

||| WASM optimisation level
public export
data WasmOptLevel = OptNone | OptSize | OptSpeed

||| Convert optimisation level to C integer
public export
optLevelToInt : WasmOptLevel -> Bits32
optLevelToInt OptNone  = 0
optLevelToInt OptSize  = 1
optLevelToInt OptSpeed = 2

||| Compile to WASM with the given optimisation level
export
compileWasm : Handle -> String -> WasmOptLevel -> IO (Either Result ())
compileWasm h outputPath opt = do
  result <- primIO (prim__compileWasm (handlePtr h) outputPath (optLevelToInt opt))
  pure $ case result of
    0 => Right ()
    _ => Left Error

||| Get the size of the compiled WASM binary in bytes
export
%foreign "C:affinescriptiser_wasm_size, libaffinescriptiser"
prim__wasmSize : Bits64 -> PrimIO Bits32

||| Get compiled WASM binary size
export
wasmSize : Handle -> IO Bits32
wasmSize h = primIO (prim__wasmSize (handlePtr h))

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

||| Get last error message
export
%foreign "C:affinescriptiser_last_error, libaffinescriptiser"
prim__lastError : PrimIO Bits64

||| Retrieve last error as string
export
lastError : IO (Maybe String)
lastError = do
  ptr <- primIO prim__lastError
  if ptr == 0
    then pure Nothing
    else pure (Just (prim__getString ptr))
  where
    %foreign "support:idris2_getString, libidris2_support"
    prim__getString : Bits64 -> String

||| Get error description for result code
export
errorDescription : Result -> String
errorDescription Ok              = "Success"
errorDescription Error           = "Generic error"
errorDescription InvalidParam    = "Invalid parameter"
errorDescription OutOfMemory     = "Out of memory"
errorDescription NullPointer     = "Null pointer"
errorDescription AffineViolation = "Affine type violation: resource used more than once or used after transfer/release"
errorDescription ResourceLeak    = "Resource leak: affine resource not released before scope exit"

--------------------------------------------------------------------------------
-- String Operations
--------------------------------------------------------------------------------

||| Free a string allocated by the library
export
%foreign "C:affinescriptiser_free_string, libaffinescriptiser"
prim__freeString : Bits64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Version Information
--------------------------------------------------------------------------------

||| Get library version
export
%foreign "C:affinescriptiser_version, libaffinescriptiser"
prim__version : PrimIO Bits64

||| Get version as string
export
version : IO String
version = do
  ptr <- primIO prim__version
  pure (prim__getString ptr)
  where
    %foreign "support:idris2_getString, libidris2_support"
    prim__getString : Bits64 -> String

||| Get library build info
export
%foreign "C:affinescriptiser_build_info, libaffinescriptiser"
prim__buildInfo : PrimIO Bits64

||| Get build information
export
buildInfo : IO String
buildInfo = do
  ptr <- primIO prim__buildInfo
  pure (prim__getString ptr)
  where
    %foreign "support:idris2_getString, libidris2_support"
    prim__getString : Bits64 -> String

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Check if context is initialized
export
%foreign "C:affinescriptiser_is_initialized, libaffinescriptiser"
prim__isInitialized : Bits64 -> PrimIO Bits32

||| Check initialization status
export
isInitialized : Handle -> IO Bool
isInitialized h = do
  result <- primIO (prim__isInitialized (handlePtr h))
  pure (result /= 0)

||| Get the number of registered source files
export
%foreign "C:affinescriptiser_source_count, libaffinescriptiser"
prim__sourceCount : Bits64 -> PrimIO Bits32

||| Get registered source file count
export
sourceCount : Handle -> IO Bits32
sourceCount h = primIO (prim__sourceCount (handlePtr h))

||| Get the number of tracked resource kinds
export
%foreign "C:affinescriptiser_tracked_count, libaffinescriptiser"
prim__trackedCount : Bits64 -> PrimIO Bits32

||| Get tracked resource count
export
trackedCount : Handle -> IO Bits32
trackedCount h = primIO (prim__trackedCount (handlePtr h))
