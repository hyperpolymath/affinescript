// SPDX-License-Identifier: MPL-2.0
// campaign #239 STEP 4-A — Node-ESM harness for binary I/O bindings.
//
// Exercises the bytes_new / bytes_fill / LE setters + getters round-trip
// against a raze-tui-shaped RazeEvent record.

import assert from "node:assert/strict";

const {
  build_event,
  read_kind,
  read_key_code,
  read_modifiers,
  read_mouse_x,
  read_mouse_y,
  fill_byte,
  fill_first,
} = await import("./bytes_binary_io.deno.js");

// Round-trip: build a Key event and read every field back.
{
  const buf = build_event(1, 0x61, 0x03, 0, 0);
  assert.equal(buf.byteLength, 16, "RazeEvent buffer is 16 bytes");
  assert.equal(read_kind(buf), 1, "kind field LE round-trip");
  assert.equal(read_key_code(buf), 0x61, "key_code field LE round-trip");
  assert.equal(read_modifiers(buf), 0x03, "modifiers field round-trip");
  assert.equal(read_mouse_x(buf), 0, "mouse_x field LE round-trip");
  assert.equal(read_mouse_y(buf), 0, "mouse_y field LE round-trip");
}

// Round-trip: Mouse event with non-zero coordinates.
{
  const buf = build_event(2, 0, 0, 80, 24);
  assert.equal(read_kind(buf), 2, "Mouse kind");
  assert.equal(read_mouse_x(buf), 80, "mouse_x = 80");
  assert.equal(read_mouse_y(buf), 24, "mouse_y = 24");
}

// Boundary: u32 max value through the field.
{
  const buf = build_event(0, 0xFFFFFFFF, 0, 0, 0);
  assert.equal(read_key_code(buf), 0xFFFFFFFF, "u32 max round-trips");
}

// Boundary: negative i32 through the kind field.
{
  const buf = build_event(-1, 0, 0, 0, 0);
  assert.equal(read_kind(buf), -1, "i32 -1 round-trips with sign");
}

// LE byte order check — write 0x12345678 as u32 at offset 4; the first
// byte at offset 4 must be 0x78 (LE low byte) and the last 0x12.
{
  const buf = build_event(0, 0x12345678, 0, 0, 0);
  // Reading bytes 4..7 individually via bytes_get_u8 would be ideal, but
  // this harness can call build_event + DataView read directly. Instead,
  // confirm via the read helper that the value is preserved.
  assert.equal(read_key_code(buf), 0x12345678, "0x12345678 LE round-trip");
  // And via byte-level read — JS DataView side-effect-free confirmation:
  const view = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
  assert.equal(view.getUint8(4), 0x78, "LE low byte at offset 4");
  assert.equal(view.getUint8(7), 0x12, "LE high byte at offset 7");
}

// bytes_fill: 8-byte all-0xAA buffer.
{
  const filled = fill_byte(8, 0xAA);
  assert.equal(filled.byteLength, 8, "bytes_fill respects size");
  for (let i = 0; i < 8; i++) {
    assert.equal(filled[i], 0xAA, `bytes_fill byte ${i} = 0xAA`);
  }
}

// bytes_fill: byte arg is masked to 0xFF (256 → 0).
{
  assert.equal(fill_first(4, 256), 0, "bytes_fill masks 256 → 0");
  assert.equal(fill_first(4, 0xFF), 0xFF, "bytes_fill 0xFF passes through");
  assert.equal(fill_first(4, -1), 0xFF, "bytes_fill -1 masks to 0xFF");
}

// bytes_new: zero-initialised.
{
  const empty = build_event(0, 0, 0, 0, 0);
  for (let i = 0; i < 16; i++) {
    if (i !== 0) { // kind field was set to 0 by build_event explicitly
      // Every offset zero by default
    }
    assert.equal(empty[i], 0, `bytes_new byte ${i} zero-initialised`);
  }
}

console.log("bytes_binary_io.harness.mjs OK");
