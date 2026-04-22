||| AffineScript PixiJS Connector ABI
||| (c) 2026 hyperpolymath
||| SPDX-License-Identifier: AGPL-3.0-or-later

module AffineScript.Pixi.ABI

import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Opaque Handles
--------------------------------------------------------------------------------

||| Opaque handle to a PixiJS object (Application, Sprite, etc.)
public export
record Handle where
  constructor MkHandle
  ptr : Bits64

--------------------------------------------------------------------------------
-- Foreign Function Declarations (Zig FFI Layer)
--------------------------------------------------------------------------------

||| Initialize PixiJS Application
export
%foreign "C:as_pixi_init, libaspixi"
prim__init : Bits32 -> Bits32 -> Bits32 -> PrimIO Bits64

||| Create a sprite from a texture
export
%foreign "C:as_pixi_create_sprite, libaspixi"
prim__createSprite : Bits64 -> PrimIO Bits64

||| Get the stage from the application
export
%foreign "C:as_pixi_get_stage, libaspixi"
prim__getStage : Bits64 -> PrimIO Bits64

||| Add a child to a container
export
%foreign "C:as_pixi_add_child, libaspixi"
prim__addChild : Bits64 -> Bits64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Safe High-Level Wrappers
--------------------------------------------------------------------------------

||| Initialize application safely
export
init : (width : Bits32) -> (height : Bits32) -> (bgColor : Bits32) -> IO Handle
init w h bg = do
  ptr <- primIO (prim__init w h bg)
  pure (MkHandle ptr)

||| Get the stage handle
export
getStage : Handle -> IO Handle
getStage (MkHandle app) = do
  ptr <- primIO (prim__getStage app)
  pure (MkHandle ptr)
