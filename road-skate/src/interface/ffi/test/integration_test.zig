// RSR Template FFI Integration Tests
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI.
// This is a TEMPLATE FILE — when instantiating a new project:
// 1. Replace "template" with your project name in lowercase
// 2. Link against your actual FFI implementation library
// 3. Uncomment the test functions below
//
// For now, this file contains documentation of what tests should exist.

const std = @import("std");

// NOTE: When instantiated, declare the actual FFI functions here:
// extern fn mylib_init() ?*Handle;
// extern fn mylib_free(?*Handle) void;
// ... etc

// And define Handle appropriately:
// const Handle = opaque {};

test "placeholder test - implementation required" {
    // This test ensures the file compiles
    // Actual tests depend on FFI implementation
    try std.testing.expect(true);
}

// ==============================================================================
// Example tests (uncomment when instantiated with real FFI):
// ==============================================================================
//
// test "lifecycle: create and destroy handle" {
//     const handle = mylib_init() orelse return error.InitFailed;
//     defer mylib_free(handle);
// }
//
// test "operations: process with valid handle" {
//     const handle = mylib_init() orelse return error.InitFailed;
//     defer mylib_free(handle);
//
//     const result = mylib_process(handle, 42);
//     try std.testing.expectEqual(@as(c_int, 0), result);
// }
//
// test "memory safety: double free is safe" {
//     const handle = mylib_init() orelse return error.InitFailed;
//     mylib_free(handle);
//     mylib_free(handle); // Should not crash
// }
//
// test "strings: get string result from handle" {
//     const handle = mylib_init() orelse return error.InitFailed;
//     defer mylib_free(handle);
//
//     const str = mylib_get_string(handle);
//     defer if (str) |s| mylib_free_string(s);
//
//     try std.testing.expect(str != null);
// }
//
// test "version: returns non-empty version string" {
//     const ver = mylib_version();
//     const ver_str = std.mem.span(ver);
//     try std.testing.expect(ver_str.len > 0);
// }
