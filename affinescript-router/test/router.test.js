// SPDX-License-Identifier: MPL-2.0
// Unit tests for Router.affine's pure functions, compiled with the Bun-ESM
// backend (the host's encode/decode primitives are installed as globals).

import { beforeAll, expect, test } from "bun:test";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const repo = new URL("../../", import.meta.url).pathname;
let R;

beforeAll(async () => {
  globalThis.router_decode = (s) => {
    try {
      return decodeURIComponent(s);
    } catch {
      return s;
    }
  };
  globalThis.router_encode = (s) => encodeURIComponent(s);
  const out = join(mkdtempSync(join(tmpdir(), "router-")), "router.bun.js");
  const r = Bun.spawnSync({
    cmd: [join(repo, "_build/default/bin/main.exe"), "compile", "--bun-esm", "Router.affine", "-o", out],
    cwd: join(repo, "affinescript-router/src"),
    env: { ...process.env, AFFINESCRIPT_STDLIB: join(repo, "stdlib"), AFFINESCRIPT_PATH: join(repo, "affinescript-tea/src") },
  });
  if (r.exitCode !== 0) throw new Error(`compile failed:\n${r.stdout}${r.stderr}`);
  R = await import(out);
});

/** Unwrap an AffineScript Option into value-or-null. */
const opt = (o) => (o && o.tag === "Some" ? o.value : null);

test("parse_url splits path, query and hash", () => {
  expect({ ...R.parse_url("/note/42?tab=links&x=1#top") }).toEqual({ path: "/note/42", query: "tab=links&x=1", hash: "top" });
  expect({ ...R.parse_url("") }).toEqual({ path: "/", query: "", hash: "" });
  expect({ ...R.parse_url("search?q=a?b#x#y") }).toEqual({ path: "/search", query: "q=a?b", hash: "x#y" });
});

test("segments drop empty parts", () => {
  expect(R.segments("/note//42/")).toEqual(["note", "42"]);
  expect(R.segments("/")).toEqual([]);
});

test("match_route captures decoded params, literals must match, * takes the rest", () => {
  expect(opt(R.match_route("/note/:id", "/note/a%20b"))).toEqual(["a b"]);
  expect(opt(R.match_route("/note/:id", "/note"))).toBeNull();
  expect(opt(R.match_route("/note/:id", "/note/1/extra"))).toBeNull();
  expect(opt(R.match_route("/note/:id", "/notes/1"))).toBeNull();
  expect(opt(R.match_route("/", "/"))).toEqual([]);
  expect(opt(R.match_route("/files/*", "/files/a/b%2Fc"))).toEqual(["a/b/c"]);
  expect(opt(R.match_route("/u/:a/p/:b", "/u/x/p/y"))).toEqual(["x", "y"]);
});

test("query_param decodes values and form-encoded spaces", () => {
  expect(opt(R.query_param("q=hello+world&n=1", "q"))).toBe("hello world");
  expect(opt(R.query_param("a=1&flag&b=x%3Dy", "flag"))).toBe("");
  expect(opt(R.query_param("a=1&b=x%3Dy", "b"))).toBe("x=y");
  expect(opt(R.query_param("a=1", "z"))).toBeNull();
});

test("href encodes each segment; matching round-trips", () => {
  const h = R.href(["item", "a b/c"]);
  expect(h).toBe("/item/a%20b%2Fc");
  expect(opt(R.match_route("/item/:id", h))).toEqual(["a b/c"]);
  expect(R.href([])).toBe("/");
});
