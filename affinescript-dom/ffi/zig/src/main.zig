/// AffineScript DOM Connector FFI (Zig Implementation)
/// (c) 2026 hyperpolymath
/// SPDX-License-Identifier: AGPL-3.0-or-later

const std = @import("std");

/// External JS functions imported into the WASM environment
extern fn js_query_selector(ptr: [*]const u8, len: usize) u64;
extern fn js_create_element(ptr: [*]const u8, len: usize) u64;
extern fn js_create_text_node(ptr: [*]const u8, len: usize) u64;
extern fn js_append_child(parent: u64, child: u64) void;
extern fn js_set_attribute(el: u64, name_ptr: [*]const u8, name_len: usize, val_ptr: [*]const u8, val_len: usize) void;

export fn as_dom_query_selector(selector_ptr: [*]const u8, selector_len: usize) u64 {
    return js_query_selector(selector_ptr, selector_len);
}

export fn as_dom_create_element(tag_ptr: [*]const u8, tag_len: usize) u64 {
    return js_create_element(tag_ptr, tag_len);
}

export fn as_dom_create_text_node(text_ptr: [*]const u8, text_len: usize) u64 {
    return js_create_text_node(text_ptr, text_len);
}

export fn as_dom_append_child(parent: u64, child: u64) void {
    js_append_child(parent, child);
}

export fn as_dom_set_attribute(el: u64, name_ptr: [*]const u8, name_len: usize, val_ptr: [*]const u8, val_len: usize) void {
    js_set_attribute(el, name_ptr, name_len, val_ptr, val_len);
}
