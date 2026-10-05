// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// Browser tests for affinescript-router, driven through the Nav example
// (../examples/nav/Nav.affine) in headless Chromium.
//
// Run: cd affinescript-router/e2e && bun install --frozen-lockfile && bun test

import { afterAll, beforeAll, expect, test } from "bun:test";
import { copyFileSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { chromium } from "playwright";

const repo = new URL("../../", import.meta.url).pathname;
const site = mkdtempSync(join(tmpdir(), "router-e2e-"));
let server;
let browser;
let page;
let base;
const errors = [];

beforeAll(async () => {
  const r = Bun.spawnSync({
    cmd: [join(repo, "_build/default/bin/main.exe"), "compile", "--bun-esm", "Nav.affine", "-o", join(site, "nav.bun.js")],
    cwd: join(repo, "affinescript-router/examples/nav"),
    env: {
      ...process.env,
      AFFINESCRIPT_STDLIB: join(repo, "stdlib"),
      AFFINESCRIPT_PATH: `${join(repo, "affinescript-tea/src")}:${join(repo, "affinescript-router/src")}`,
    },
  });
  if (r.exitCode !== 0) throw new Error(`compile failed:\n${r.stdout}${r.stderr}`);
  copyFileSync(join(repo, "affinescript-tea/src/tea_host.js"), join(site, "tea_host.js"));
  copyFileSync(join(repo, "affinescript-router/src/router_host.js"), join(site, "router_host.js"));
  writeFileSync(
    join(site, "index.html"),
    `<!doctype html><meta charset="utf-8"><div id="root"></div>
<script type="module">import "./tea_host.js"; import "./router_host.js"; import "./nav.bun.js";</script>`,
  );
  server = Bun.serve({
    port: 0,
    fetch(req) {
      const path = new URL(req.url).pathname;
      return new Response(Bun.file(join(site, path === "/" ? "index.html" : path.slice(1))));
    },
  });
  base = `http://localhost:${server.port}/`;
  browser = await chromium.launch();
  page = await browser.newPage();
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("console", (m) => m.type() === "error" && errors.push(m.text()));
  await page.goto(base);
  await page.waitForSelector("#app");
});

afterAll(async () => {
  await browser?.close();
  server?.stop(true);
});

/** Wait until the rendered page label equals `label`. */
const pageIs = (label) => page.waitForFunction((l) => document.querySelector("#page")?.textContent === l, label);

test("initial route is read from the URL", async () => {
  expect(await page.textContent("#page")).toBe("home");
});

test("navigate pushes a history entry and delivers the decoded route", async () => {
  await page.click("#to-item");
  await pageIs("item a b/c");
  expect(new URL(page.url()).hash).toBe("#/item/a%20b%2Fc");
  await page.click("#to-search");
  await pageIs("search hello world");
});

test("replace swaps the entry without adding one; back returns", async () => {
  await page.click("#swap");
  await pageIs("item 7");
  await page.click("#back");
  await pageIs("item a b/c");
  await page.goBack();
  await pageIs("home");
  await page.goForward();
  await pageIs("item a b/c");
});

test("editing the hash (address bar) is picked up", async () => {
  await page.evaluate(() => {
    location.hash = "#/nowhere";
  });
  await pageIs("missing /nowhere");
});

test("a deep link loads directly", async () => {
  await page.goto(`${base}#/search?q=deep+link`);
  await pageIs("search deep link");
});

test("no runtime errors", () => {
  expect(errors).toEqual([]);
});
