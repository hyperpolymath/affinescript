// SPDX-License-Identifier: MPL-2.0
// crypto-stdlib-2026-05 — Node-ESM harness for crypto_primitives.affine.
//
// Strategy: stub `globalThis.crypto.subtle.{importKey,verify,sign}` so the
// HMAC + RS256 lowerings have a deterministic surface, then assert that
// the AffineScript-side calls thread arguments through correctly and
// observe both the happy path and the fail-closed return.

import assert from "node:assert/strict";

// ── crypto.subtle stub ──────────────────────────────────────────────
let importedKeys = [];
let lastVerify = null;
let lastSign = null;

// Node 20 exposes `globalThis.crypto` as a read-only getter, so we
// install our stub via `defineProperty` rather than direct assignment.
Object.defineProperty(globalThis, "crypto", {
  value: {
    subtle: {
      importKey: async (format, keyData, algorithm, extractable, usages) => {
        const handle = {
          _id: importedKeys.length,
          format,
          keyData,
          algorithm,
          extractable,
          usages,
        };
        importedKeys.push(handle);
        return handle;
      },
      // HMAC verify — the AffineScript-level `hmac_sha256_verify` shim
      // converts hex -> Uint8Array before calling us, so what arrives
      // here is bytes. Decide truth by a sentinel byte at the head.
      verify: async (algorithm, key, signature, data) => {
        lastVerify = {
          algorithm,
          key,
          signatureBytes: Array.from(signature),
          dataText: new TextDecoder().decode(data),
        };
        // Sentinel: a signature whose first byte is 0xAA verifies true.
        return signature[0] === 0xaa;
      },
      // RS256 sign — return a deterministic 256-byte buffer so we can
      // assert on the base64url encoding length.
      sign: async (algorithm, key, data) => {
        lastSign = {
          algorithm,
          key,
          dataText: new TextDecoder().decode(data),
        };
        const out = new Uint8Array(256);
        out[0] = 0x42;
        out[255] = 0xbe;
        return out.buffer;
      },
    },
  },
  writable: true,
  configurable: true,
});

// ── Load the compiled module ────────────────────────────────────────
const {
  verify_ok,
  sign_b64u,
  b64u_encode,
  b64u_decode_roundtrip,
} = await import("./crypto_primitives.deno.js");

// ── HMAC-SHA256 verify ──────────────────────────────────────────────
// Happy path: signature hex starting with `aa` -> stub returns true.
assert.equal(
  await verify_ok("topsecret", "the body", "aa".repeat(32)),
  true,
  "hmac verify happy path returns true",
);
assert.equal(lastVerify.algorithm, "HMAC", "verify algorithm = HMAC");
assert.equal(lastVerify.dataText, "the body", "verify body threaded through");
assert.equal(
  lastVerify.signatureBytes.length,
  32,
  "signature decoded to 32 bytes (64 hex chars)",
);
assert.equal(lastVerify.signatureBytes[0], 0xaa, "first byte is 0xAA");

// Wrong-key path: stub returns false because first byte is 0xBB.
assert.equal(
  await verify_ok("topsecret", "the body", "bb".repeat(32)),
  false,
  "hmac verify wrong-key path returns false",
);

// Fail-closed on malformed hex (odd length).
assert.equal(
  await verify_ok("topsecret", "the body", "abc"),
  false,
  "hmac verify rejects malformed hex without calling subtle",
);

// Fail-closed on non-hex characters.
assert.equal(
  await verify_ok("topsecret", "the body", "g".repeat(64)),
  false,
  "hmac verify rejects non-hex characters",
);

// ── RS256 sign ──────────────────────────────────────────────────────
// Build a minimal PKCS#8 PEM. The shim only checks the header marker
// and base64-decodes the body; it does NOT validate the DER, so a
// trivial 1-byte body is sufficient for the test.
const fakePem = [
  "-----BEGIN PRIVATE KEY-----",
  "AA==",
  "-----END PRIVATE KEY-----",
].join("\n");

const sigB64u = await sign_b64u(fakePem, "header.payload");
assert.equal(
  typeof sigB64u,
  "string",
  "rs256_sign returns a string via base64url_encode_bytes",
);
// 256 bytes -> 342 chars base64url (no padding).
assert.equal(sigB64u.length, 342, "256-byte signature encodes to 342 b64u chars");
assert.equal(sigB64u.startsWith("Qg"), true, "first byte 0x42 -> b64u 'Qg'");
assert.equal(
  lastSign.algorithm,
  "RSASSA-PKCS1-v1_5",
  "rs256_sign algorithm threaded",
);
assert.equal(lastSign.dataText, "header.payload", "rs256_sign payload threaded");

// Loud failure on PKCS#1 (rejected at the boundary).
await assert.rejects(
  () =>
    sign_b64u(
      ["-----BEGIN RSA PRIVATE KEY-----", "AA==", "-----END RSA PRIVATE KEY-----"]
        .join("\n"),
      "x",
    ),
  /PKCS#8/,
  "rs256_sign rejects PKCS#1 with a clear message",
);

// ── base64url ───────────────────────────────────────────────────────
// JWT header example — known fixture, easy to verify by eye.
assert.equal(
  b64u_encode(`{"alg":"RS256","typ":"JWT"}`),
  "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9",
  "b64u_encode matches the canonical JWT header encoding",
);

// `+` / `/` => `-` / `_`. The string ">>>>" encodes to "Pj4+Pg==" in
// stock base64 — verify the b64u rewrites and strip padding.
assert.equal(b64u_encode(">>>>"), "Pj4-Pg", "b64u uses '-'/'_' and strips padding");

// Round-trip a non-aligned-length input.
assert.equal(
  await b64u_decode_roundtrip("SGVsbG8"),
  "SGVsbG8",
  "b64u decode/encode round-trips a 7-char string",
);

console.log("crypto_primitives.harness.mjs OK");
