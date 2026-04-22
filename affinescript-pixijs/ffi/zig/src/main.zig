/// AffineScript PixiJS Connector FFI (Zig Implementation)
/// (c) 2026 hyperpolymath
/// SPDX-License-Identifier: AGPL-3.0-or-later

const std = @import("std");

/// External JS functions imported into the WASM environment
extern fn js_pixi_init(w: u32, h: u32, bg: u32) u64;
extern fn js_pixi_create_sprite(texture: u64) u64;
extern fn js_pixi_get_stage(app: u64) u64;
extern fn js_pixi_add_child(parent: u64, child: u64) void;

export fn as_pixi_init(w: u32, h: u32, bg: u32) u64 {
    return js_pixi_init(w, h, bg);
}

export fn as_pixi_create_sprite(texture: u64) u64 {
    return js_pixi_create_sprite(texture);
}

export fn as_pixi_get_stage(app: u64) u64 {
    return js_pixi_get_stage(app);
}

export fn as_pixi_add_child(parent: u64, child: u64) void {
    js_pixi_add_child(parent, child);
}
