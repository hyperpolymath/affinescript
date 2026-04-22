// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: CLI entry
//
// Usage:
//   deno run --allow-read --allow-run --allow-env cli.ts <root>
//   deno run --allow-read --allow-run --allow-env cli.ts ./tests
//
// The CLI is a thin wrapper — the actual test registration happens via
// runAll() which calls Deno.test(). To see reporting, invoke via `deno test`
// on a driver script that imports runAll, rather than running this module
// standalone.
//
// This file is primarily here so `deno run cli.ts <root>` works as a
// smoke-test of discovery + compile (it prints the source list + compile
// results without actually running assertions — that requires deno test).

import { compileToWasm } from "./lib/compile.ts";
import { discoverTestFiles } from "./lib/discover.ts";

function usage(): never {
  console.error(
    "Usage: deno run --allow-read --allow-run --allow-env cli.ts <root>\n" +
      "\n" +
      "Discovers *_test.affine files under <root>, compiles each to .wasm,\n" +
      "and prints the compile results. To actually run the tests, invoke\n" +
      "`deno test` on a driver script that imports runAll() from mod.ts.",
  );
  Deno.exit(2);
}

if (import.meta.main) {
  const root = Deno.args[0];
  if (!root) usage();

  const sources = await discoverTestFiles(root);
  if (sources.length === 0) {
    console.error(`No *_test.affine files found under ${root}`);
    Deno.exit(1);
  }

  console.log(`Discovered ${sources.length} test file(s):`);
  for (const source of sources) {
    try {
      const wasm = await compileToWasm(source);
      console.log(`  ✓ ${source} → ${wasm}`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.error(`  ✗ ${source}\n    ${message}`);
      Deno.exit(1);
    }
  }
}
