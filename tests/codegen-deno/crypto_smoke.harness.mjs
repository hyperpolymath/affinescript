// SPDX-License-Identifier: MPL-2.0
// #746: Node-ESM harness for the Crypto externs.

import assert from "node:assert/strict";
import { generateKeyPairSync } from "node:crypto";

const { digest, mac, b64e, b64d, sign, verify } = await import("./crypto_smoke.bun.js");

const bytes = (s) => Array.from(new TextEncoder().encode(s));
const hex = (a) => a.map((x) => x.toString(16).padStart(2, "0")).join("");

// FIPS 180-2 vectors (from the issue).
assert.equal(hex(digest(bytes("abc"))), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
assert.equal(hex(digest([])), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");

// RFC 4231 test case 2.
assert.equal(hex(mac(bytes("Jefe"), bytes("what do ya want for nothing?"))),
  "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");

// base64url: RFC 4648 alphabet, no padding, round trip.
assert.equal(b64e([0xfb, 0xff, 0xfe]), "-__-");
assert.equal(b64e(bytes("{\"alg\":\"RS256\",\"typ\":\"JWT\"}")), "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9");
const all = Array.from({ length: 256 }, (_, i) => i);
assert.deepEqual(b64d(b64e(all)), all);
assert.throws(() => b64d("ab=c"), /invalid base64url/);
assert.throws(() => b64d("abcde"), /invalid base64url/);

// Bytes outside 0..255 are rejected, not wrapped.
assert.throws(() => digest([256]), RangeError);
assert.throws(() => digest([-1]), RangeError);

// RS256: sign with PKCS#8 and PKCS#1 keys, verify, reject tampering and wrong key.
const kp = () => generateKeyPairSync("rsa", { modulusLength: 2048 });
const { privateKey, publicKey } = kp();
const pkcs8 = privateKey.export({ type: "pkcs8", format: "pem" });
const pkcs1 = privateKey.export({ type: "pkcs1", format: "pem" });
const pub = publicKey.export({ type: "spki", format: "pem" });
const msg = bytes("eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiIxIn0");
const sig = sign(pkcs8, msg);
assert.equal(sig.length, 256);
assert.deepEqual(sign(pkcs1, msg), sig); // PKCS#1 v1.5 is deterministic
assert.equal(verify(pub, msg, sig), true);
assert.equal(verify(pub, [...msg, 0x41], sig), false);
assert.equal(verify(pub, msg, [...sig.slice(0, 255), sig[255] ^ 1]), false);
assert.equal(verify(kp().publicKey.export({ type: "spki", format: "pem" }), msg, sig), false);

// Non-RSA keys are refused.
const ec = generateKeyPairSync("ec", { namedCurve: "P-256" }).privateKey.export({ type: "pkcs8", format: "pem" });
assert.throws(() => sign(ec, msg), /expected an RSA key/);

console.log("crypto_smoke: ok");
