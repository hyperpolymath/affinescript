// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// @hyperpolymath/affinescript — the thin compiler shim (ADR-019 / #260 S3).
//
// The AffineScript compiler is a native OCaml binary, not a JS package.
// Per ADR-019 the GitHub Release (cut by .github/workflows/release.yml,
// #260 S2) is the CANONICAL artifact: per-platform `affinescript-<target>`
// binaries + a `SHA256SUMS` manifest. This package is the ergonomic
// front door: it downloads the binary for the host triple from the
// Release pinned by THIS package version, verifies it against the
// checksum embedded here (no floating fetch — one version+checksum per
// shim release, the ADR-019 supply-chain rule), caches it, and execs it
// with the caller's argv.
//
// Runtime support — Deno is canonical; Bun and Node.js are first-class
// targets because the shim's consumers (LSP installers, IDE extensions,
// build scripts) often run in those environments.  All three branches
// are guarded by runtime detection at module load; nothing dynamically
// imports a foreign runtime's globals.  Browsers and Cloudflare Workers
// are NOT supported and never will be: the shim's purpose is "fetch,
// save to disk, exec a native binary" — steps 2 and 3 are not possible
// in a sandboxed JS runtime.
//
// JavaScript, not ReScript, because this is entirely host-API surface
// (spawn / fetch / crypto.subtle / fs) — the documented "JS only where
// ReScript cannot" carve-out, same as packages/affine-js.

/// <reference types="./mod.d.ts" />

import { PINS } from "./pins.js";

// ─── runtime detection ──────────────────────────────────────────────
const isDeno = typeof Deno !== "undefined";
const isBun = !isDeno && typeof Bun !== "undefined";
const isNode = !isDeno && !isBun &&
  typeof process !== "undefined" && !!process.versions?.node;

if (!isDeno && !isBun && !isNode) {
  throw new Error(
    "@hyperpolymath/affinescript: unsupported runtime. " +
      "This package targets Deno (canonical), Bun, and Node.js. " +
      "Browsers and Workers cannot exec native binaries.",
  );
}

// ─── host helpers ───────────────────────────────────────────────────
// Each helper picks one branch at runtime; the others are dead code in
// that process and never load. `await import("node:…")` in the Node
// branch works under Deno too (Deno node-compat), so a Bun path that
// shells out to node: APIs is still valid — but we keep Bun's native
// APIs where they exist for fewer surprises and better diagnostics.

function hostOs() {
  // Deno: "linux"|"darwin"|"windows"|...
  if (isDeno) return Deno.build.os;
  // Bun and Node both populate process.platform identically.
  return process.platform;
}

function hostArch() {
  // Deno reports "x86_64"|"aarch64"; Node/Bun report "x64"|"arm64".
  // Normalise to Deno's spelling — that's what hostTarget() matches.
  if (isDeno) return Deno.build.arch;
  if (process.arch === "x64") return "x86_64";
  if (process.arch === "arm64") return "aarch64";
  return process.arch;
}

function envGet(name) {
  if (isDeno) return Deno.env.get(name);
  return process.env[name];
}

async function readBytes(path) {
  if (isDeno) return await Deno.readFile(path);
  if (isBun) return new Uint8Array(await Bun.file(path).arrayBuffer());
  const { readFile } = await import("node:fs/promises");
  return new Uint8Array(await readFile(path));
}

async function writeBytes(path, bytes, { mode } = {}) {
  if (isDeno) {
    return await Deno.writeFile(path, bytes, mode != null ? { mode } : {});
  }
  if (isBun) {
    await Bun.write(path, bytes);
    if (mode != null) {
      const { chmod } = await import("node:fs/promises");
      await chmod(path, mode).catch(() => {});
    }
    return;
  }
  const { writeFile, chmod } = await import("node:fs/promises");
  await writeFile(path, bytes);
  if (mode != null) await chmod(path, mode).catch(() => {});
}

async function mkdirRecursive(path) {
  if (isDeno) return await Deno.mkdir(path, { recursive: true });
  const { mkdir } = await import("node:fs/promises");
  await mkdir(path, { recursive: true });
}

async function chmodExec(path) {
  if (isDeno) return await Deno.chmod(path, 0o755).catch(() => {});
  const { chmod } = await import("node:fs/promises");
  await chmod(path, 0o755).catch(() => {});
}

async function spawnInherit(bin, args) {
  if (isDeno) {
    const { code } = await new Deno.Command(bin, {
      args,
      stdin: "inherit",
      stdout: "inherit",
      stderr: "inherit",
    }).output();
    return code;
  }
  if (isBun) {
    // Bun.spawn returns a process whose `.exited` resolves to the code.
    const proc = Bun.spawn([bin, ...args], {
      stdin: "inherit",
      stdout: "inherit",
      stderr: "inherit",
    });
    return await proc.exited;
  }
  const { spawn } = await import("node:child_process");
  return await new Promise((resolve, reject) => {
    const child = spawn(bin, args, { stdio: "inherit" });
    child.on("close", (code) => resolve(code ?? 0));
    child.on("error", reject);
  });
}

/** Does this process look like it was invoked as `<runtime> <thisfile>`? */
function thisIsMain() {
  // Deno: import.meta.main; Bun also honours it.  Node: compare URLs.
  if (isDeno || isBun) return import.meta.main === true;
  if (isNode) {
    const arg1 = process.argv[1];
    if (!arg1) return false;
    try {
      const here = new URL(import.meta.url).pathname;
      return here === arg1 || here.endsWith(arg1);
    } catch {
      return false;
    }
  }
  return false;
}

// ─── public API ─────────────────────────────────────────────────────

/** Map the host to an ADR-019 release target triple. */
export function hostTarget(os = hostOs(), arch = hostArch()) {
  if (os === "linux" && arch === "x86_64") return "linux-x64";
  if (os === "darwin" && arch === "x86_64") return "macos-x64";
  if (os === "darwin" && arch === "aarch64") return "macos-arm64";
  throw new Error(
    `@hyperpolymath/affinescript: unsupported host ${os}/${arch}. ` +
      `Supported: linux-x64, macos-x64, macos-arm64 (windows-x64 is a ` +
      `tracked follow-up). Build from source: hyperpolymath/affinescript.`,
  );
}

/** Lower-case hex of the SHA-256 of `bytes`. */
export async function sha256Hex(bytes) {
  const digest = await crypto.subtle.digest("SHA-256", bytes);
  return Array.from(new Uint8Array(digest))
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
}

/** Cache path for a pinned binary (XDG, then HOME, then a temp dir). */
export function cachePath(version, target) {
  const base = envGet("AFFINESCRIPT_CACHE") ??
    envGet("XDG_CACHE_HOME") ??
    (envGet("HOME") ? `${envGet("HOME")}/.cache` : null) ??
    envGet("TMPDIR") ?? "/tmp";
  return `${base}/affinescript/${version}/affinescript-${target}`;
}

/**
 * Resolve a runnable compiler binary path for the host, downloading +
 * checksum-verifying from the pinned Release on a cache miss.
 *
 * @param {{ pins?: object, fetchImpl?: typeof fetch }} [opts]
 *   `pins`/`fetchImpl` are test seams; production uses the embedded
 *   PINS and global fetch.
 * @returns {Promise<string>} absolute path to the verified binary
 */
export async function resolveCompiler(opts = {}) {
  const pins = opts.pins ?? PINS;
  const doFetch = opts.fetchImpl ?? fetch;
  const target = hostTarget();
  const entry = pins.targets?.[target];
  if (!entry || !entry.sha256 || !entry.url) {
    throw new Error(
      `@hyperpolymath/affinescript: no pinned binary for ${target} at ` +
        `version ${pins.version} (pins.js not finalised for this release).`,
    );
  }
  const path = cachePath(pins.version, target);

  // Cache hit only counts if the cached bytes still match the pin
  // (defends against a corrupted/tampered cache).
  try {
    const cached = await readBytes(path);
    if ((await sha256Hex(cached)) === entry.sha256) return path;
  } catch { /* miss — fall through to download */ }

  const res = await doFetch(entry.url);
  if (!res.ok) {
    throw new Error(
      `@hyperpolymath/affinescript: download failed for ${target} ` +
        `(${entry.url}): HTTP ${res.status}`,
    );
  }
  const bytes = new Uint8Array(await res.arrayBuffer());
  const got = await sha256Hex(bytes);
  if (got !== entry.sha256) {
    throw new Error(
      `@hyperpolymath/affinescript: checksum mismatch for ${target} — ` +
        `expected ${entry.sha256}, got ${got}. Refusing to run (the ` +
        `Release artifact does not match this shim version's pin).`,
    );
  }
  await mkdirRecursive(path.slice(0, path.lastIndexOf("/")));
  await writeBytes(path, bytes, { mode: 0o755 });
  // writeBytes' mode is pre-umask on some FSes; ensure the executable
  // bit is set so the spawn doesn't fail with EACCES.
  await chmodExec(path);
  return path;
}

/**
 * Resolve then exec the compiler with `args`, inheriting stdio.
 * Returns the child's exit code (caller decides whether to exit).
 */
export async function run(args = [], opts = {}) {
  const bin = await resolveCompiler(opts);
  return await spawnInherit(bin, args);
}

if (thisIsMain()) {
  const argv = isDeno ? Deno.args : process.argv.slice(2);
  const code = await run(argv);
  if (isDeno) Deno.exit(code);
  else process.exit(code);
}
