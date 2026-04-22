// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: mod.ts (public API)
//
// High-level entry point. Compiles matching AffineScript test files under a
// given root, then registers every `test_*` WASM export as a Deno.test() case.
//
// Usage (library mode, from a harness script):
//
//   import { runAll } from "@hyperpolymath/affinescript-deno-test";
//   await runAll("./tests");
//
// Usage (CLI mode):
//
//   deno run --allow-read --allow-run --allow-env \
//     jsr:@hyperpolymath/affinescript-deno-test/cli ./tests

import { compileToWasm } from "./lib/compile.ts";
import { discoverTestFiles } from "./lib/discover.ts";
import { registerTestsFromWasm } from "./lib/runner.ts";

export { compileToWasm, resolveCompilerPath } from "./lib/compile.ts";
export { DEFAULT_TEST_PATTERN, discoverTestFiles } from "./lib/discover.ts";
export { TEST_PREFIX, registerTestsFromWasm } from "./lib/runner.ts";

/**
 * Discover, compile, and register every AffineScript test file under `root`.
 *
 * Returns the total number of test cases registered across all files.
 * Deno's test framework reports pass/fail per case when `deno test` runs.
 */
export async function runAll(root: string): Promise<number> {
  const sources = await discoverTestFiles(root);
  if (sources.length === 0) {
    throw new Error(
      `affinescript-deno-test: no test files found under ${root} ` +
        `(expected *_test.affine or *.test.affine)`,
    );
  }

  let total = 0;
  for (const source of sources) {
    const wasm = await compileToWasm(source);
    total += await registerTestsFromWasm(wasm);
  }
  return total;
}
