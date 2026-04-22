// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: smoke test driver
//
// Run with:
//   deno test --allow-read --allow-run --allow-env example/smoke_driver.ts
//
// Expected output: three green tests (test_addition, test_identity,
// test_inequality) from example/hello_test.affine.

import { runAll } from "../mod.ts";

await runAll(new URL("./", import.meta.url).pathname);
