// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Types shared between the wasm_export_call dispatcher and any host that
// embeds it. The layout deliberately matches the Deno-ESM lowering's
// `{ kind: "i32"|"i64"|"f32"|"f64", v }` semantics so the AS-side
// `WasmValue` round-trips cleanly across either host.
//
// ABI stability: every type here is `extern struct` / `extern union` /
// `enum(u8)` — the layouts are part of the public C ABI and must not
// change without a major version bump of the crate.

const std = @import("std");

/// `WasmValue` discriminator. Numeric values are deliberately stable so
/// downstream consumers can hard-code them in `.h` constants. `err` is
/// reserved for the "no such export" failure path; it cannot appear as
/// the kind of a value the host successfully marshals in.
pub const WasmKind = enum(u8) {
    i32 = 0,
    i64 = 1,
    f32 = 2,
    f64 = 3,
    /// Sentinel returned by `wasm_export_call` when the registry lookup
    /// fails. The payload is zeroed; callers should branch on
    /// `result.kind == .err` rather than inspect `payload`.
    err = 0xff,
};

/// Tagged scalar matching the Deno-ESM `{ kind, v }` representation.
///
/// `i64` is preserved at full 64-bit precision; the JS-side carries it
/// as `BigInt` to avoid the 2^53 safe-integer cliff. Choice of payload
/// is selected by `kind`; reading the wrong variant is undefined at the
/// ABI level (callers can recover via the AS-side `wv_kind`).
pub const WasmValueRepr = extern struct {
    kind: WasmKind,
    /// Padding so the payload is naturally aligned on 8-byte
    /// boundaries. The seven trailing bytes are not part of the
    /// observable value; the dispatcher zeroes them on every return.
    _pad: [7]u8 = [_]u8{0} ** 7,
    payload: extern union {
        i32_v: i32,
        i64_v: i64,
        f32_v: f32,
        f64_v: f64,
    },
};

/// Type of a per-export callback the host registers. Each callback
/// receives the raw `WasmValueRepr[]` slice the AS-side passed in, and
/// returns a single `WasmValueRepr` — the wasm scalar return.
///
/// The callback is the host's single chance to invoke the actual wasm
/// export (typically via `wasmtime` / `wasmer` / `wasm3` / whatever
/// runtime the embedder ships); this crate does not embed a wasm
/// runtime of its own.
pub const WasmExportCallback = *const fn (
    args_ptr: [*]const WasmValueRepr,
    args_len: usize,
) callconv(.C) WasmValueRepr;

/// One entry in a `WasmExportRegistry`. `name_ptr` + `name_len` is a
/// byte slice; it does not need to be NUL-terminated. The dispatcher
/// compares against the AS-side `String` (also a ptr+len) verbatim.
pub const WasmExportEntry = extern struct {
    name_ptr: [*]const u8,
    name_len: usize,
    callback: WasmExportCallback,
};

/// The "exports handle" the AS-side `WasmExports` opaque resolves to on
/// this Zig host. The host constructs this once per wasm instance and
/// passes a pointer to it as `exports_handle` whenever it invokes the
/// dispatcher (typically by binding the dispatcher under `env` in the
/// wasm instance's import object and threading the registry through a
/// thread-local or a host-state pointer the runtime supports).
pub const WasmExportRegistry = extern struct {
    entries: [*]const WasmExportEntry,
    count: usize,
};

/// Construct an error `WasmValueRepr` with kind `.err` and a zeroed
/// payload. Used by the dispatcher's failure path; exposed publicly so
/// host-side callbacks can return the same sentinel when they detect
/// an internal invocation error (wasm trap, type mismatch, etc.).
pub fn errValue() WasmValueRepr {
    return WasmValueRepr{
        .kind = .err,
        ._pad = [_]u8{0} ** 7,
        .payload = .{ .i64_v = 0 },
    };
}

/// Constructor for an i32 result.
pub fn fromI32(v: i32) WasmValueRepr {
    return WasmValueRepr{
        .kind = .i32,
        ._pad = [_]u8{0} ** 7,
        .payload = .{ .i32_v = v },
    };
}

/// Constructor for an i64 result.
pub fn fromI64(v: i64) WasmValueRepr {
    return WasmValueRepr{
        .kind = .i64,
        ._pad = [_]u8{0} ** 7,
        .payload = .{ .i64_v = v },
    };
}

/// Constructor for an f32 result.
pub fn fromF32(v: f32) WasmValueRepr {
    return WasmValueRepr{
        .kind = .f32,
        ._pad = [_]u8{0} ** 7,
        .payload = .{ .f32_v = v },
    };
}

/// Constructor for an f64 result.
pub fn fromF64(v: f64) WasmValueRepr {
    return WasmValueRepr{
        .kind = .f64,
        ._pad = [_]u8{0} ** 7,
        .payload = .{ .f64_v = v },
    };
}

test "WasmValueRepr is 16 bytes on common targets" {
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(WasmValueRepr));
}

test "errValue has kind .err and zero payload" {
    const v = errValue();
    try std.testing.expectEqual(WasmKind.err, v.kind);
    try std.testing.expectEqual(@as(i64, 0), v.payload.i64_v);
}

test "round-trip constructors preserve their kind tag" {
    try std.testing.expectEqual(WasmKind.i32, fromI32(42).kind);
    try std.testing.expectEqual(WasmKind.i64, fromI64(42).kind);
    try std.testing.expectEqual(WasmKind.f32, fromF32(1.5).kind);
    try std.testing.expectEqual(WasmKind.f64, fromF64(1.5).kind);
}
