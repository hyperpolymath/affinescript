// SPDX-License-Identifier: MPL-2.0
/**
 * Vite plugin scaffold for AffineScript (#56).
 *
 * Transforms `*.affine` sources by shelling out to `affinescript compile
 * --bun-esm`. This is a compile-pass wiring, not a production bundler
 * integration: the compiler must be on PATH, and the plugin does not
 * ship a JS-hosted AffineScript frontend.
 */
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

export default function affinescriptVite(options = {}) {
  const compiler = options.compiler || process.env.AFFINESCRIPT || "affinescript";
  return {
    name: "affinescript",
    enforce: "pre",
    async transform(_code, id) {
      const filename = id.split("?")[0];
      if (!filename.endsWith(".affine")) return null;
      const dir = mkdtempSync(join(tmpdir(), "affinescript-vite-"));
      const src = join(dir, "input.affine");
      const out = join(dir, "out.bun.js");
      try {
        writeFileSync(src, _code);
        const r = spawnSync(compiler, ["compile", src, "-o", out, "--bun-esm"], {
          encoding: "utf8",
        });
        if (r.status !== 0) {
          const msg = (r.stderr || r.stdout || "affinescript compile failed").trim();
          this.error(msg);
          return null;
        }
        return { code: readFileSync(out, "utf8"), map: null };
      } finally {
        rmSync(dir, { recursive: true, force: true });
      }
    },
  };
}
