// SPDX-License-Identifier: PMPL-1.0-or-later
// issue #160 — Node-ESM harness for portable Http.fetch (Deno-ESM backend).
//
// Stubs `globalThis.fetch` so nothing touches the network: the stub
// echoes the request so we can assert the lowering passed url/method/
// headers/body through correctly, and round-trips status/headers/body.
import assert from "node:assert/strict";

let lastCall = null;
globalThis.fetch = async (url, init) => {
  lastCall = { url, init };
  const h = new Map([
    ["content-type", "text/plain"],
    ["x-echo-method", init.method],
  ]);
  return {
    status: url.includes("/missing") ? 404 : 200,
    headers: { forEach: (cb) => h.forEach((v, k) => cb(v, k)) },
    text: async () => `body-for:${init.method}:${init.body ?? ""}`,
  };
};

const {
  get_status,
  get_body,
  post_status,
  req_get_status,
  req_post_body,
  ok_get,
} = await import("./http_fetch.deno.js");

// GET — status round-trips, no body sent
assert.equal(await get_status("https://example.test/ok"), 200, "GET 200");
assert.equal(lastCall.init.method, "GET", "method lowered = GET");
assert.equal(lastCall.init.body, undefined, "GET has no body");

// GET — body text round-trips
assert.equal(
  await get_body("https://example.test/ok"),
  "body-for:GET:",
  "GET body round-trip",
);

// GET — 404 path
assert.equal(
  await get_status("https://example.test/missing"),
  404,
  "GET 404 status",
);

// POST — method + body passed through
assert.equal(
  await post_status("https://example.test/p", "hello"),
  200,
  "POST 200",
);
assert.equal(lastCall.init.method, "POST", "method lowered = POST");
assert.equal(lastCall.init.body, "hello", "POST body passed through");

// request() builder + fetch() — header assoc list -> request headers
assert.equal(
  await req_get_status("https://example.test/h"),
  200,
  "fetch(request(...)) status",
);
assert.equal(
  lastCall.init.headers["x-test"],
  "1",
  "assoc-list header [(\"x-test\",\"1\")] lowered to header object",
);

// request() POST with Some(body)
assert.equal(
  await req_post_body("https://example.test/p", "payload"),
  "body-for:POST:payload",
  "Some(body) -> request body",
);

// is_ok — 2xx classification
assert.equal(await ok_get("https://example.test/ok"), true, "is_ok 200");
assert.equal(await ok_get("https://example.test/missing"), false, "is_ok 404");

console.log("http_fetch.harness.mjs OK");
