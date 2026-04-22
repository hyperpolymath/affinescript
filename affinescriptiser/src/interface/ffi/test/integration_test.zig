// Affinescriptiser Integration Tests
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// These tests verify that the Zig FFI correctly implements the Idris2 ABI
// defined in src/interface/abi/{Types,Layout,Foreign}.idr

const std = @import("std");
const testing = std.testing;

// Import FFI functions (C ABI, matching Foreign.idr declarations)
extern fn affinescriptiser_init() ?*opaque {};
extern fn affinescriptiser_free(?*opaque {}) void;
extern fn affinescriptiser_register_source(?*opaque {}, ?[*:0]const u8, u32) c_int;
extern fn affinescriptiser_track_resource(?*opaque {}, ?[*:0]const u8, u32, u32) c_int;
extern fn affinescriptiser_analyse(?*opaque {}) c_int;
extern fn affinescriptiser_violation_count(?*opaque {}) u32;
extern fn affinescriptiser_compile_wasm(?*opaque {}, ?[*:0]const u8, u32) c_int;
extern fn affinescriptiser_wasm_size(?*opaque {}) u32;
extern fn affinescriptiser_last_error() ?[*:0]const u8;
extern fn affinescriptiser_free_string(?[*:0]const u8) void;
extern fn affinescriptiser_version() [*:0]const u8;
extern fn affinescriptiser_build_info() [*:0]const u8;
extern fn affinescriptiser_is_initialized(?*opaque {}) u32;
extern fn affinescriptiser_source_count(?*opaque {}) u32;
extern fn affinescriptiser_tracked_count(?*opaque {}) u32;

//==============================================================================
// Lifecycle Tests
//==============================================================================

test "create and destroy context" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    try testing.expect(handle != null);
}

test "context is initialized" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const initialized = affinescriptiser_is_initialized(handle);
    try testing.expectEqual(@as(u32, 1), initialized);
}

test "null handle is not initialized" {
    const initialized = affinescriptiser_is_initialized(null);
    try testing.expectEqual(@as(u32, 0), initialized);
}

//==============================================================================
// Source Registration Tests
//==============================================================================

test "register Rust source file" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const result = affinescriptiser_register_source(handle, "src/lib.rs", 0);
    try testing.expectEqual(@as(c_int, 0), result); // 0 = ok

    try testing.expectEqual(@as(u32, 1), affinescriptiser_source_count(handle));
}

test "register C source file" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const result = affinescriptiser_register_source(handle, "src/main.c", 1);
    try testing.expectEqual(@as(c_int, 0), result);
}

test "register Zig source file" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const result = affinescriptiser_register_source(handle, "src/main.zig", 2);
    try testing.expectEqual(@as(c_int, 0), result);
}

test "register source with null handle returns error" {
    const result = affinescriptiser_register_source(null, "test.rs", 0);
    try testing.expectEqual(@as(c_int, 4), result); // 4 = null_pointer
}

test "register multiple source files" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    _ = affinescriptiser_register_source(handle, "a.rs", 0);
    _ = affinescriptiser_register_source(handle, "b.c", 1);
    _ = affinescriptiser_register_source(handle, "c.zig", 2);

    try testing.expectEqual(@as(u32, 3), affinescriptiser_source_count(handle));
}

//==============================================================================
// Resource Tracking Tests
//==============================================================================

test "track file descriptor as affine" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    // kind=0 (FileDescriptor), linearity=1 (Affine)
    const result = affinescriptiser_track_resource(handle, "fd", 0, 1);
    try testing.expectEqual(@as(c_int, 0), result);

    try testing.expectEqual(@as(u32, 1), affinescriptiser_tracked_count(handle));
}

test "track mutex lock as linear" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    // kind=3 (MutexLock), linearity=0 (Linear)
    const result = affinescriptiser_track_resource(handle, "lock", 3, 0);
    try testing.expectEqual(@as(c_int, 0), result);
}

test "track GPU buffer as affine" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    // kind=4 (GPUBuffer), linearity=1 (Affine)
    const result = affinescriptiser_track_resource(handle, "gpu_buf", 4, 1);
    try testing.expectEqual(@as(c_int, 0), result);
}

test "track resource with null handle" {
    const result = affinescriptiser_track_resource(null, "fd", 0, 1);
    try testing.expectEqual(@as(c_int, 4), result); // null_pointer
}

//==============================================================================
// Analysis Tests
//==============================================================================

test "analyse with registered sources" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    _ = affinescriptiser_register_source(handle, "test.rs", 0);
    _ = affinescriptiser_track_resource(handle, "fd", 0, 1);

    const result = affinescriptiser_analyse(handle);
    try testing.expectEqual(@as(c_int, 0), result);

    try testing.expectEqual(@as(u32, 0), affinescriptiser_violation_count(handle));
}

test "analyse with null handle" {
    const result = affinescriptiser_analyse(null);
    try testing.expectEqual(@as(c_int, 4), result); // null_pointer
}

test "analyse with no sources returns invalid_param" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const result = affinescriptiser_analyse(handle);
    try testing.expectEqual(@as(c_int, 2), result); // invalid_param
}

//==============================================================================
// WASM Compilation Tests
//==============================================================================

test "compile wasm with null handle" {
    const result = affinescriptiser_compile_wasm(null, "out.wasm", 0);
    try testing.expectEqual(@as(c_int, 4), result); // null_pointer
}

test "compile wasm with null output path" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    const result = affinescriptiser_compile_wasm(handle, null, 0);
    try testing.expectEqual(@as(c_int, 4), result); // null_pointer
}

//==============================================================================
// Error Handling Tests
//==============================================================================

test "last error after null handle operation" {
    _ = affinescriptiser_analyse(null);

    const err = affinescriptiser_last_error();
    try testing.expect(err != null);

    if (err) |e| {
        const err_str = std.mem.span(e);
        try testing.expect(err_str.len > 0);
        affinescriptiser_free_string(e);
    }
}

test "no error after successful operation" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    _ = affinescriptiser_register_source(handle, "test.rs", 0);

    // Error should be cleared after successful operation
    const err = affinescriptiser_last_error();
    try testing.expect(err == null);
}

//==============================================================================
// Version Tests
//==============================================================================

test "version string is not empty" {
    const ver = affinescriptiser_version();
    const ver_str = std.mem.span(ver);

    try testing.expect(ver_str.len > 0);
}

test "version string is semantic version format" {
    const ver = affinescriptiser_version();
    const ver_str = std.mem.span(ver);

    // Should be in format X.Y.Z
    try testing.expect(std.mem.count(u8, ver_str, ".") >= 1);
}

test "build info contains affinescriptiser" {
    const info = affinescriptiser_build_info();
    const info_str = std.mem.span(info);

    try testing.expect(std.mem.indexOf(u8, info_str, "affinescriptiser") != null);
}

//==============================================================================
// Memory Safety Tests
//==============================================================================

test "multiple contexts are independent" {
    const h1 = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(h1);

    const h2 = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(h2);

    try testing.expect(h1 != h2);

    // Registering sources on h1 should not affect h2
    _ = affinescriptiser_register_source(h1, "a.rs", 0);
    try testing.expectEqual(@as(u32, 1), affinescriptiser_source_count(h1));
    try testing.expectEqual(@as(u32, 0), affinescriptiser_source_count(h2));
}

test "free null is safe" {
    affinescriptiser_free(null); // Should not crash
}
