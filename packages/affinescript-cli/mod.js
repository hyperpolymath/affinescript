// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// @hyperpolymath/affinescript — the thin compiler shim (ADR-019 / #260 S3).
//
// The AffineScript compiler is a native OCaml binary, not a JS package.
// Per ADR-019 the GitHub Release (cut by .github/workflows/release.yml,
// #260 S2) is the CANONICAL artifact: per-platform `affinescript-<target>`
// binaries + a `SHA256SUMS` manifest. This package is the ergonomic Deno
// front door: it downloads the binary for the host triple from the
// Release pinned by THIS package version, verifies it against the
// checksum embedded here (no floating fetch — one version+checksum per
// shim release, the ADR-019 supply-chain rule), caches it, and execs it
// with the caller's argv.
//
// Deno-first (CLAUDE.md). JavaScript, not ReScript, because this is
// entirely Deno host APIs (Deno.Command / fetch / crypto.subtle / fs) —
// the documented "JS only where ReScript cannot" carve-out, same as
// packages/affine-js.

import { PINS } from "./pins.js";

/** Map the host to an ADR-019 release target triple. */
export function hostTarget(os = Deno.build.os, arch = Deno.build.arch) {
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
  const base = Deno.env.get("AFFINESCRIPT_CACHE") ??
    Deno.env.get("XDG_CACHE_HOME") ??
    (Deno.env.get("HOME") ? `${Deno.env.get("HOME")}/.cache` : null) ??
    Deno.env.get("TMPDIR") ?? "/tmp";
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
    const cached = await Deno.readFile(path);
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
  await Deno.mkdir(path.slice(0, path.lastIndexOf("/")), { recursive: true });
  await Deno.writeFile(path, bytes, { mode: 0o755 });
  // writeFile mode is pre-umask; ensure it is executable.
  await Deno.chmod(path, 0o755).catch(() => {});
  return path;
}

/**
 * Resolve then exec the compiler with `args`, inheriting stdio.
 * Returns the child's exit code (caller decides whether to exit).
 */
export async function run(args = Deno.args, opts = {}) {
  const bin = await resolveCompiler(opts);
  const cmd = new Deno.Command(bin, {
    args,
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
  });
  const { code } = await cmd.output();
  return code;
}

if (import.meta.main) {
  Deno.exit(await run());
}
