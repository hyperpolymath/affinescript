// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Node ESM harness for examples/consumers/extension-boundary.
//
// This file is the other half of the on-ramp: it shows exactly how a host
// satisfies the `extern fn` surface declared in src/boundary.affine, and
// what the assertable contract looks like in CI. Node is used because CI
// already has it; nothing here is Node-specific except `readFile`, so the
// same object literal works in Deno, Bun, or a browser extension
// (`WebAssembly.instantiate` with the same `imports`).
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const here = dirname(fileURLToPath(import.meta.url));

// The error taxonomy a real consumer needs is stable and enumerated. These
// are the codes the host returns; the guest only ever sees integers.
const BW = {
  OK: 0,
  BW_PDF_ENCRYPTED: 1042,
  BW_NO_BLOCKS_FOUND: 1051,
};

// The host owns the message text. Nothing crosses the boundary as a string
// — `bw_error_message_*` is a byte-wise accessor over this value.
const MESSAGES = {
  [BW.BW_PDF_ENCRYPTED]: "document is encrypted; decrypt before filling",
  [BW.BW_NO_BLOCKS_FOUND]: "no fillable blocks detected in document",
};

// `context` is the third leg of the {code, message, context} payload the
// consumer contract requires; it stays host-side too.
let lastFailure = null;

function fail(code, context) {
  lastFailure = { code, message: MESSAGES[code] ?? "", context };
  return code;
}

const calls = { detect: 0, fill: 0 };

const imports = {
  wasi_snapshot_preview1: {
    // Nothing in this guest touches WASI, but the compiler links the
    // preview-1 surface when the file mentions effects; a stub keeps the
    // instantiation total rather than depending on what codegen emitted.
    fd_write: () => 0,
  },
  env: {
    bw_detect_blocks: (pdfLen) => {
      calls.detect += 1;
      assert.ok(Number.isInteger(pdfLen) && pdfLen > 0, "host got the length");
      return BW.OK;
    },
    bw_fill_blocks: (pdfLen, fieldCount) => {
      calls.fill += 1;
      assert.ok(Number.isInteger(pdfLen) && pdfLen > 0, "host got the length");
      assert.ok(fieldCount > 0, "host got the field count");
      // Second document is encrypted — the failure path, deliberately.
      return fail(BW.BW_PDF_ENCRYPTED, { pdfLen, fieldCount });
    },
    bw_error_code: () => lastFailure?.code ?? BW.OK,
    bw_error_message_len: () => lastFailure?.message.length ?? 0,
    bw_error_message_byte: (offset) =>
      lastFailure?.message.charCodeAt(offset) ?? 0,
  },
};

const wasmPath = join(here, "dist", "boundary.wasm");
const bytes = await readFile(wasmPath);
const { instance } = await WebAssembly.instantiate(bytes, imports);
const guest = instance.exports;

// ── The contract, asserted ───────────────────────────────────────────────

// 1. Success path: the guest normalises "host said 0" to 0.
assert.equal(guest.detect(4096), 0, "detect returns 0 on success");
assert.equal(guest.is_ok(0), 1, "is_ok(0) is true");
assert.equal(guest.message_len(0), 0, "no message for a success status");
assert.equal(calls.detect, 1, "host detected once");

// 2. Failure path: the guest propagates the stable code, not a string.
assert.equal(
  guest.fill(4096, 3),
  BW.BW_PDF_ENCRYPTED,
  "fill propagates the host's BW_* code",
);
assert.equal(calls.fill, 1, "host filled once");

// 3. The message is host-owned and byte-addressable, exactly as declared.
const len = guest.message_len(BW.BW_PDF_ENCRYPTED);
assert.equal(len, MESSAGES[BW.BW_PDF_ENCRYPTED].length, "message length crosses");
const rebuilt = Array.from({ length: len }, (_, i) =>
  String.fromCharCode(guest.message_byte(BW.BW_PDF_ENCRYPTED, i)),
).join("");
assert.equal(rebuilt, MESSAGES[BW.BW_PDF_ENCRYPTED], "message round-trips");
assert.equal(guest.message_len(0), 0, "success still has no message");

// 4. The consumer-facing payload shape: {code, message, context} — never a
//    bare string, which is the taxonomy requirement this example exists for.
assert.deepEqual(lastFailure, {
  code: BW.BW_PDF_ENCRYPTED,
  message: MESSAGES[BW.BW_PDF_ENCRYPTED],
  context: { pdfLen: 4096, fieldCount: 3 },
});

// 5. The guest's public surface is what the consumer advertised — a
//    two-function boundary plus the error accessors.
for (const name of [
  "detect", "fill", "is_ok", "message_len", "message_byte",
]) {
  assert.equal(typeof guest[name], "function", `${name} is exported`);
}

console.log("examples/consumers/extension-boundary: OK");
