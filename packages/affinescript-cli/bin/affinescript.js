#!/usr/bin/env node
// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Executable entry point for the `affinescript` command.
//
// WHY THIS FILE EXISTS. `mod.js` already self-executes when it is the entry
// module, but it carries no hashbang, so it cannot be the target of a
// package.json `bin` field on Unix. This wrapper adds the hashbang and nothing
// else — all behaviour stays in mod.js.
import { run } from "../mod.js";

const argv = typeof Deno !== "undefined" ? Deno.args : process.argv.slice(2);
const code = await run(argv);
if (typeof Deno !== "undefined") Deno.exit(code);
else process.exit(code);
