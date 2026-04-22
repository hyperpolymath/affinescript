||| AffineScript DOM Connector ABI
||| (c) 2026 hyperpolymath
||| SPDX-License-Identifier: AGPL-3.0-or-later

module AffineScript.DOM.ABI

import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Opaque Handles
--------------------------------------------------------------------------------

||| Opaque handle to a DOM object (Element, Node, etc.)
public export
record Handle where
  constructor MkHandle
  ptr : Bits64

--------------------------------------------------------------------------------
-- Foreign Function Declarations (Zig FFI Layer)
--------------------------------------------------------------------------------

||| Query the DOM for an element by selector
export
%foreign "C:as_dom_query_selector, libasdom"
prim__querySelector : String -> PrimIO Bits64

||| Create a new element
export
%foreign "C:as_dom_create_element, libasdom"
prim__createElement : String -> PrimIO Bits64

||| Create a text node
export
%foreign "C:as_dom_create_text_node, libasdom"
prim__createTextNode : String -> PrimIO Bits64

||| Append a child to an element
export
%foreign "C:as_dom_append_child, libasdom"
prim__appendChild : Bits64 -> Bits64 -> PrimIO ()

||| Set an attribute on an element
export
%foreign "C:as_dom_set_attribute, libasdom"
prim__setAttribute : Bits64 -> String -> String -> PrimIO ()

||| Register an event listener
export
%foreign "C:as_dom_add_event_listener, libasdom"
prim__addEventListener : Bits64 -> String -> AnyPtr -> PrimIO ()

--------------------------------------------------------------------------------
-- Safe High-Level Wrappers
--------------------------------------------------------------------------------

||| Safe wrapper for querySelector
export
querySelector : String -> IO (Maybe Handle)
querySelector sel = do
  ptr <- primIO (prim__querySelector sel)
  if ptr == 0
    then pure Nothing
    else pure (Just (MkHandle ptr))

||| Safe wrapper for createElement
export
createElement : String -> IO Handle
createElement tag = do
  ptr <- primIO (prim__createElement tag)
  pure (MkHandle ptr)

||| Safe wrapper for appendChild
export
appendChild : Handle -> Handle -> IO ()
appendChild (MkHandle parent) (MkHandle child) = 
  primIO (prim__appendChild parent child)
