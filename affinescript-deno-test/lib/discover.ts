// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: discover.ts
//
// Glob-based discovery of AffineScript test files.
// Default convention: any file matching `*_test.affine` or `*.test.affine`.

import { walk } from "jsr:@std/fs@1/walk";

/** Default regex for matching AffineScript test files by filename. */
export const DEFAULT_TEST_PATTERN = /(?:_test|\.test)\.(?:affine|afs|rattle|pyaff|jsaff)$/;

/**
 * Recursively walk `root` and return absolute paths of files matching
 * `pattern` (default: `*_test.affine` or `*.test.affine`).
 */
export async function discoverTestFiles(
  root: string,
  pattern: RegExp = DEFAULT_TEST_PATTERN,
): Promise<string[]> {
  const absoluteRoot = root.startsWith("/") ? root : `${Deno.cwd()}/${root}`;
  const matches: string[] = [];

  for await (const entry of walk(absoluteRoot, { includeDirs: false, match: [pattern] })) {
    matches.push(entry.path);
  }

  matches.sort();
  return matches;
}
