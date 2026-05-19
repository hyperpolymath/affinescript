// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// @hyperpolymath/affinescript shim tests (ADR-019 / #260 S3).
// Network-free: a fake `fetchImpl` serves bytes; AFFINESCRIPT_CACHE is
// redirected to a temp dir. The "binary" is a tiny shell script so the
// exec/argv path is exercised for real without a built compiler.
//
// Run: deno test --allow-read --allow-write --allow-env --allow-run mod_test.js

import { assertEquals, assertRejects } from "jsr:@std/assert@1";
import { hostTarget, resolveCompiler, run, sha256Hex } from "./mod.js";

Deno.test("hostTarget maps the three supported triples", () => {
  assertEquals(hostTarget("linux", "x86_64"), "linux-x64");
  assertEquals(hostTarget("darwin", "x86_64"), "macos-x64");
  assertEquals(hostTarget("darwin", "aarch64"), "macos-arm64");
});

Deno.test("hostTarget rejects unsupported hosts (incl. windows)", () => {
  for (const [os, arch] of [["windows", "x86_64"], ["linux", "aarch64"]]) {
    let threw = false;
    try {
      hostTarget(os, arch);
    } catch (e) {
      threw = e.message.includes("unsupported host");
    }
    assertEquals(threw, true, `${os}/${arch} should be rejected`);
  }
});

// A fake "compiler": a POSIX script echoing argv and exiting 7.
const FAKE = new TextEncoder().encode(
  '#!/bin/sh\necho "args:$*"\nexit 7\n',
);

async function pinsFor() {
  return {
    version: "vtest",
    targets: {
      [hostTarget()]: {
        url: "https://example.invalid/affinescript",
        sha256: await sha256Hex(FAKE),
      },
    },
  };
}

const okFetch = () => ({
  ok: true,
  arrayBuffer: async () => FAKE.buffer.slice(0),
});

function withTempCache(fn) {
  return async () => {
    const dir = await Deno.makeTempDir();
    const prev = Deno.env.get("AFFINESCRIPT_CACHE");
    Deno.env.set("AFFINESCRIPT_CACHE", dir);
    try {
      await fn(dir);
    } finally {
      if (prev === undefined) Deno.env.delete("AFFINESCRIPT_CACHE");
      else Deno.env.set("AFFINESCRIPT_CACHE", prev);
      await Deno.remove(dir, { recursive: true });
    }
  };
}

Deno.test(
  "resolveCompiler downloads, checksum-verifies, caches (executable)",
  withTempCache(async () => {
    const pins = await pinsFor();
    const p = await resolveCompiler({ pins, fetchImpl: okFetch });
    const stat = await Deno.stat(p);
    assertEquals(stat.isFile, true);
    // mode is masked on some FSs; just assert the owner-exec bit is set.
    assertEquals((stat.mode & 0o100) !== 0, true);
    assertEquals(await sha256Hex(await Deno.readFile(p)), pins.targets[hostTarget()].sha256);
  }),
);

Deno.test(
  "resolveCompiler refuses on checksum mismatch and does not cache",
  withTempCache(async (dir) => {
    const pins = await pinsFor();
    pins.targets[hostTarget()].sha256 = "0".repeat(64); // wrong pin
    await assertRejects(
      () => resolveCompiler({ pins, fetchImpl: okFetch }),
      Error,
      "checksum mismatch",
    );
    // Nothing written under the cache root.
    let entries = 0;
    for await (const _ of Deno.readDir(dir)) entries++;
    assertEquals(entries, 0);
  }),
);

Deno.test(
  "resolveCompiler cache-hit skips fetch; corrupt cache re-downloads",
  withTempCache(async () => {
    const pins = await pinsFor();
    await resolveCompiler({ pins, fetchImpl: okFetch });
    // Cache hit: a throwing fetch must NOT be called.
    const p = await resolveCompiler({
      pins,
      fetchImpl: () => {
        throw new Error("fetch must not run on a valid cache hit");
      },
    });
    // Corrupt the cache → must re-fetch (okFetch) and restore.
    await Deno.writeFile(p, new TextEncoder().encode("tampered"));
    const p2 = await resolveCompiler({ pins, fetchImpl: okFetch });
    assertEquals(p2, p);
    assertEquals(
      await sha256Hex(await Deno.readFile(p2)),
      pins.targets[hostTarget()].sha256,
    );
  }),
);

Deno.test(
  "run execs the resolved binary with argv passthrough + exit code",
  withTempCache(async () => {
    const pins = await pinsFor();
    const code = await run(["compile", "x.affine"], {
      pins,
      fetchImpl: okFetch,
    });
    assertEquals(code, 7); // the fake binary always exits 7
  }),
);
