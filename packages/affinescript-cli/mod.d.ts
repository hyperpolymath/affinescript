// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Type declarations for mod.js — the @hyperpolymath/affinescript shim.
// Lets JSR's fast-check publish without the "JavaScript entrypoint without
// type declarations" warning, and gives consumers proper editor types.
//
// Per CLAUDE.md, .d.ts is an approved TypeScript carve-out (file format
// for declaration files only); the implementation in mod.js remains JS
// because every effect is a Deno.* host API, the documented "JS only
// where ReScript cannot" exception.

/** ADR-019 release target triples this shim knows about. */
export type Target = "linux-x64" | "macos-x64" | "macos-arm64";

/** A single per-target entry in the {@link Pins} table. */
export interface PinEntry {
  /** Canonical Release asset URL — `affinescript-<target>` raw executable. */
  url: string;
  /** Lower-case hex SHA-256.  Empty string ⇒ fail-closed for that target. */
  sha256: string;
}

/**
 * The full pin table — ONE compiler version + ONE sha256 per target, per
 * shim release.  Filled in `pins.js` when a `v*` tag is cut.
 */
export interface Pins {
  /** The pinned compiler tag (e.g. `"v0.1.1"`). */
  version: string;
  /** Per-target download + checksum entries.  Targets without a sha256
   *  refuse to resolve (fail-closed). */
  targets: Partial<Record<Target, PinEntry>>;
}

/** Options accepted by {@link resolveCompiler} and {@link run}. */
export interface ResolveOptions {
  /** Override the embedded pin table (test seam). */
  pins?: Pins;
  /** Override the global `fetch` (test seam). */
  fetchImpl?: typeof fetch;
}

/**
 * Map a host OS/arch to one of the supported ADR-019 release targets.
 * Throws if the host isn't covered (e.g. windows-x64 is a tracked follow-up).
 */
export function hostTarget(os?: string, arch?: string): Target;

/** Lower-case hex of the SHA-256 of `bytes`. */
export function sha256Hex(bytes: ArrayBuffer | Uint8Array): Promise<string>;

/**
 * Absolute path the shim caches a pinned binary at.  Resolves
 * `AFFINESCRIPT_CACHE` → `XDG_CACHE_HOME` → `$HOME/.cache` → `TMPDIR` → `/tmp`.
 */
export function cachePath(version: string, target: Target): string;

/**
 * Resolve a runnable compiler binary path for the host.  On a cache
 * miss, downloads the pinned Release asset, verifies its SHA-256 against
 * the embedded pin, writes it to {@link cachePath} with the executable
 * bit set, and returns the path.  Throws on checksum mismatch (refuses
 * to cache or run the tampered bytes).
 */
export function resolveCompiler(opts?: ResolveOptions): Promise<string>;

/**
 * Resolve via {@link resolveCompiler}, then `Deno.Command`-spawn the
 * binary with `args`, inheriting stdio.  Returns the child's exit code
 * (caller decides whether to `Deno.exit()`).
 */
export function run(args?: string[], opts?: ResolveOptions): Promise<number>;
