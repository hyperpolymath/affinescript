// Affinescriptiser FFI Implementation
//
// This module implements the C-compatible FFI declared in src/interface/abi/Foreign.idr.
// It provides the WASM compilation engine's core operations: source registration,
// resource tracking, affine analysis, and WASM compilation.
//
// All types and layouts must match the Idris2 ABI definitions in Types.idr and Layout.idr.
//
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

const std = @import("std");

// Version information (keep in sync with Cargo.toml)
const VERSION = "0.1.0";
const BUILD_INFO = "affinescriptiser built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/interface/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type in Types.idr)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
    affine_violation = 5,
    resource_leak = 6,
};

/// Source language tag (must match Foreign.idr SourceLang)
pub const SourceLang = enum(u32) {
    rust = 0,
    c_lang = 1,
    zig = 2,
};

/// Resource kind tag (must match Types.idr ResourceKind)
pub const ResourceKind = enum(u32) {
    file_descriptor = 0,
    heap_allocation = 1,
    network_socket = 2,
    mutex_lock = 3,
    gpu_buffer = 4,
    memory_map = 5,
    database_handle = 6,
    opaque_resource = 7,
};

/// Linearity tag (must match Types.idr Linearity)
pub const Linearity = enum(u32) {
    linear = 0,
    affine = 1,
    unrestricted = 2,
};

/// WASM optimisation level (must match Foreign.idr WasmOptLevel)
pub const WasmOptLevel = enum(u32) {
    opt_none = 0,
    opt_size = 1,
    opt_speed = 2,
};

/// A tracked resource registration
const TrackedResourceEntry = struct {
    name: []const u8,
    kind: ResourceKind,
    linearity: Linearity,
};

/// A registered source file
const SourceEntry = struct {
    path: []const u8,
    lang: SourceLang,
};

/// Affinescriptiser compilation context (opaque to C callers)
pub const Handle = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
    /// Registered source files for analysis
    sources: std.ArrayList(SourceEntry),
    /// Tracked resource declarations
    tracked_resources: std.ArrayList(TrackedResourceEntry),
    /// Number of affine violations found by last analysis
    violation_count: u32,
    /// Compiled WASM binary size (0 if not yet compiled)
    wasm_size: u32,
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the affinescriptiser compilation context
/// Returns a handle, or null on failure
export fn affinescriptiser_init() ?*Handle {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(Handle) catch {
        setError("Failed to allocate affinescriptiser context");
        return null;
    };

    handle.* = .{
        .allocator = allocator,
        .initialized = true,
        .sources = std.ArrayList(SourceEntry).init(allocator),
        .tracked_resources = std.ArrayList(TrackedResourceEntry).init(allocator),
        .violation_count = 0,
        .wasm_size = 0,
    };

    clearError();
    return handle;
}

/// Free the compilation context and all associated data
export fn affinescriptiser_free(handle: ?*Handle) void {
    const h = handle orelse return;
    const allocator = h.allocator;

    h.sources.deinit();
    h.tracked_resources.deinit();
    h.initialized = false;

    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Source File Registration
//==============================================================================

/// Register a source file for affine analysis
export fn affinescriptiser_register_source(
    handle: ?*Handle,
    path: ?[*:0]const u8,
    lang: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Context not initialized");
        return .@"error";
    }

    const p = path orelse {
        setError("Null path");
        return .null_pointer;
    };

    const source_lang: SourceLang = @enumFromInt(lang);

    h.sources.append(.{
        .path = std.mem.span(p),
        .lang = source_lang,
    }) catch {
        setError("Failed to register source file");
        return .out_of_memory;
    };

    clearError();
    return .ok;
}

//==============================================================================
// Resource Tracking Configuration
//==============================================================================

/// Register a resource kind for affine tracking
export fn affinescriptiser_track_resource(
    handle: ?*Handle,
    name: ?[*:0]const u8,
    kind: u32,
    linearity: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Context not initialized");
        return .@"error";
    }

    const n = name orelse {
        setError("Null resource name");
        return .null_pointer;
    };

    const resource_kind: ResourceKind = @enumFromInt(kind);
    const lin: Linearity = @enumFromInt(linearity);

    h.tracked_resources.append(.{
        .name = std.mem.span(n),
        .kind = resource_kind,
        .linearity = lin,
    }) catch {
        setError("Failed to register tracked resource");
        return .out_of_memory;
    };

    clearError();
    return .ok;
}

//==============================================================================
// Affine Analysis
//==============================================================================

/// Run affine analysis on all registered source files
/// Returns ok if all resources satisfy their linearity constraints
export fn affinescriptiser_analyse(handle: ?*Handle) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Context not initialized");
        return .@"error";
    }

    if (h.sources.items.len == 0) {
        setError("No source files registered");
        return .invalid_param;
    }

    // Stub: analysis not yet implemented
    // When implemented, this will:
    // 1. Parse each source file according to its language
    // 2. Identify resource handle usage patterns
    // 3. Verify that tracked resources satisfy linearity constraints
    // 4. Report violations

    h.violation_count = 0;
    clearError();
    return .ok;
}

/// Get the number of affine violations found by the last analysis
export fn affinescriptiser_violation_count(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return h.violation_count;
}

/// Get a human-readable violation report
export fn affinescriptiser_violation_report(handle: ?*Handle) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (h.violation_count == 0) {
        return null; // No violations — no report needed
    }

    // Stub: report generation not yet implemented
    const report = h.allocator.dupeZ(u8, "No violations found") catch {
        setError("Failed to allocate violation report");
        return null;
    };

    return report.ptr;
}

//==============================================================================
// WASM Compilation
//==============================================================================

/// Compile analysed code to WASM
export fn affinescriptiser_compile_wasm(
    handle: ?*Handle,
    output_path: ?[*:0]const u8,
    opt_level: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Context not initialized");
        return .@"error";
    }

    _ = output_path orelse {
        setError("Null output path");
        return .null_pointer;
    };

    _ = @as(WasmOptLevel, @enumFromInt(opt_level));

    // Stub: WASM compilation not yet implemented
    // When implemented, this will:
    // 1. Generate AffineScript wrapper code from analysis results
    // 2. Lower AffineScript AST to WASM instructions
    // 3. Apply optimisation passes based on opt_level
    // 4. Write .wasm binary to output_path

    h.wasm_size = 0;
    clearError();
    return .ok;
}

/// Get the size of the last compiled WASM binary in bytes
export fn affinescriptiser_wasm_size(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return h.wasm_size;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn affinescriptiser_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// String Operations
//==============================================================================

/// Free a string allocated by the library
export fn affinescriptiser_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;

    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn affinescriptiser_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn affinescriptiser_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if context is initialized
export fn affinescriptiser_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

/// Get the number of registered source files
export fn affinescriptiser_source_count(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return @intCast(h.sources.items.len);
}

/// Get the number of tracked resource kinds
export fn affinescriptiser_tracked_count(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return @intCast(h.tracked_resources.items.len);
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    try std.testing.expect(affinescriptiser_is_initialized(handle) == 1);
}

test "error handling" {
    const result = affinescriptiser_analyse(null);
    try std.testing.expectEqual(Result.null_pointer, result);

    const err = affinescriptiser_last_error();
    try std.testing.expect(err != null);
}

test "version" {
    const ver = affinescriptiser_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}

test "source registration" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    try std.testing.expectEqual(@as(u32, 0), affinescriptiser_source_count(handle));

    const result = affinescriptiser_register_source(handle, "test.rs", 0);
    try std.testing.expectEqual(Result.ok, result);
    try std.testing.expectEqual(@as(u32, 1), affinescriptiser_source_count(handle));
}

test "resource tracking" {
    const handle = affinescriptiser_init() orelse return error.InitFailed;
    defer affinescriptiser_free(handle);

    try std.testing.expectEqual(@as(u32, 0), affinescriptiser_tracked_count(handle));

    // Track a file descriptor as affine
    const result = affinescriptiser_track_resource(handle, "fd", 0, 1);
    try std.testing.expectEqual(Result.ok, result);
    try std.testing.expectEqual(@as(u32, 1), affinescriptiser_tracked_count(handle));
}
