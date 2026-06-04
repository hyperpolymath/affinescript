// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity -- a generic differential-parity harness runner for AffineScript
// wasm. Roadmap asks #2/#3 of the nextgen-languages-evangelist.
//
// Given a config, it:
//   1. compiles the `.affine` source host-native (via the prebuilt compiler),
//   2. instantiates the resulting wasm (scalar i32 ABI; wasi stubbed),
//   3. calls each exported `pub fn` over a sweep of inputs,
//   4. compares every result against an independent JS oracle,
//   5. reports "N/N pass" and exits 1 on any mismatch.
//
// It is the config-driven generalisation of the one-off harnesses written by
// hand this session: instead of bespoke `deno eval` per module, you hand it a
// config describing which exports to sweep, over which input ranges, against
// which oracle.
//
//## ABI SCOPE (read this)
// This runner handles the **scalar i32 ABI** only: every argument is an i32 and
// every result is an i32. That is exactly the boundary the DESIGN-VISION
// prescribes ("they pass primitives across the wasm boundary") and it covers
// pure-integer co-processors such as SecurityRank. Array/string parameters use
// a different convention -- `[len:i32 LE][utf8 bytes]` written into linear
// memory plus an exported `__affine_alloc` -- and are a deliberate follow-on,
// NOT implemented here. Exports taking [Int]/String params therefore can only be
// invoked here in their nullary form (e.g. Kernel_IO's `main`, whose array
// arguments are inlined inside the .affine source).
//
//## CONFIG SHAPE
// A config is a `.mjs` module with a default export:
//
//   export default {
//     affine: "SecurityRank.affine",          // path, resolved relative to cfg
//     // optional: out: "SecurityRank.wasm",   // wasm output path
//     // optional: compile: true,              // default true; false = reuse wasm
//     cases: [
//       {
//         export: "rank_security_level",       // wasm export to call
//         args: [ [-5, 8] ],                    // one [lo,hi] inclusive range / arg
//         oracle: (level) => (...),             // independent expected-value fn
//         // optional: name: "..."              // label for the report
//       },
//       ...
//     ],
//   };
//
// `args` is a list, one entry per function parameter. Each entry is either:
//   * an inclusive integer range `[lo, hi]`  -> swept lo..hi, OR
//   * an explicit array of values `{ values: [..] }` -> swept verbatim, OR
//   * a single number -> a fixed scalar (1-element sweep).
// The runner takes the Cartesian product across all parameters. A nullary export
// (no params) uses `args: []` and is called once.
//
// Usage:
//   deno run --allow-read --allow-run parity.mjs <config.mjs>
//   deno run --allow-read --allow-run parity.mjs <config.mjs> --verbose
//
// Exit codes: 0 = all cases pass, 1 = a mismatch / compile / load error,
//             2 = bad invocation.

const COMPILER = "/home/user/affinescript/_build/default/bin/main.exe";

// A WASI shim: every import returns 0 and does nothing. The scalar-i32 pub fns
// never actually drive I/O at runtime, but the module imports fd_write (and the
// `_start`/wasi surface may reference more), so a permissive stub keeps
// instantiation total regardless of which wasi names a given module pulls in.
function makeWasiImports() {
  const stub = new Proxy(
    {},
    { get: () => () => 0 },
  );
  return { wasi_snapshot_preview1: stub };
}

// Resolve a path that may be relative to the config file's directory.
function resolveFrom(baseDir, p) {
  if (p.startsWith("/")) return p;
  return `${baseDir}/${p}`;
}

// Compile <affine> -> <out> by calling the prebuilt compiler binary directly
// (no dune, to dodge the build lock). Returns the path to the produced wasm.
async function compileAffine(affinePath, outPath) {
  const cmd = new Deno.Command(COMPILER, {
    args: ["compile", affinePath, "-o", outPath],
    stdout: "piped",
    stderr: "piped",
  });
  const { code, stdout, stderr } = await cmd.output();
  const out = new TextDecoder().decode(stdout).trim();
  const err = new TextDecoder().decode(stderr).trim();
  if (code !== 0) {
    throw new Error(
      `compile failed (exit ${code})\n  cmd: ${COMPILER} compile ${affinePath} -o ${outPath}\n  stdout: ${out}\n  stderr: ${err}`,
    );
  }
  return { outPath, log: out || err };
}

// Instantiate a wasm file under the WASI stub; return the exports object.
async function instantiate(wasmPath) {
  const bytes = await Deno.readFile(wasmPath);
  const { instance } = await WebAssembly.instantiate(bytes, makeWasiImports());
  return instance.exports;
}

// Expand one parameter spec into the concrete list of values it sweeps.
function expandArgSpec(spec, paramIndex) {
  if (typeof spec === "number") return [spec];
  if (Array.isArray(spec)) {
    if (spec.length !== 2) {
      throw new Error(
        `arg spec #${paramIndex} as a range must be [lo, hi]; got [${spec}]`,
      );
    }
    const [lo, hi] = spec;
    if (!Number.isInteger(lo) || !Number.isInteger(hi)) {
      throw new Error(`arg spec #${paramIndex} range bounds must be integers`);
    }
    if (hi < lo) {
      throw new Error(`arg spec #${paramIndex} range hi(${hi}) < lo(${lo})`);
    }
    const out = [];
    for (let v = lo; v <= hi; v++) out.push(v);
    return out;
  }
  if (spec && Array.isArray(spec.values)) return spec.values.slice();
  throw new Error(
    `arg spec #${paramIndex} must be a number, a [lo,hi] range, or { values: [...] }`,
  );
}

// Cartesian product of a list of value-lists. [] -> [[]] (the single empty tuple).
function cartesian(lists) {
  return lists.reduce(
    (acc, list) => acc.flatMap((tuple) => list.map((v) => [...tuple, v])),
    [[]],
  );
}

// Run a single case (one export + its arg sweep + its oracle). Returns
// { name, total, pass, failures: [{args, got, want}] }.
function runCase(exports, kase, idx) {
  const name = kase.name || kase.export || `case#${idx}`;
  const fn = exports[kase.export];
  if (typeof fn !== "function") {
    throw new Error(
      `export "${kase.export}" not found or not a function (case "${name}")`,
    );
  }
  if (typeof kase.oracle !== "function") {
    throw new Error(`case "${name}" has no oracle function`);
  }
  const argSpecs = kase.args || [];
  const perParam = argSpecs.map((s, i) => expandArgSpec(s, i));
  const tuples = cartesian(perParam);

  let pass = 0;
  const failures = [];
  for (const tuple of tuples) {
    const got = fn(...tuple) | 0; // normalise to i32
    const want = kase.oracle(...tuple) | 0;
    if (got === want) {
      pass++;
    } else {
      failures.push({ args: tuple, got, want });
    }
  }
  return { name, total: tuples.length, pass, failures };
}

async function loadConfig(configPath) {
  const abs = configPath.startsWith("/")
    ? configPath
    : `${Deno.cwd()}/${configPath}`;
  const mod = await import(`file://${abs}`);
  const cfg = mod.default;
  if (!cfg || typeof cfg !== "object") {
    throw new Error(`config "${configPath}" must have a default-export object`);
  }
  if (!cfg.affine) throw new Error(`config "${configPath}" missing "affine"`);
  if (!Array.isArray(cfg.cases) || cfg.cases.length === 0) {
    throw new Error(`config "${configPath}" needs a non-empty "cases" array`);
  }
  // baseDir = directory of the config file, for resolving relative paths.
  const baseDir = abs.slice(0, abs.lastIndexOf("/")) || ".";
  return { cfg, baseDir, configPath };
}

export async function runParity(configPath, { verbose = false } = {}) {
  const { cfg, baseDir } = await loadConfig(configPath);

  const affinePath = resolveFrom(baseDir, cfg.affine);
  const outPath = resolveFrom(
    baseDir,
    cfg.out || cfg.affine.replace(/\.affine$/, ".wasm"),
  );

  const doCompile = cfg.compile !== false;
  if (doCompile) {
    const { log } = await compileAffine(affinePath, outPath);
    if (verbose && log) console.log(`[compile] ${log}`);
  } else if (verbose) {
    console.log(`[compile] skipped; reusing ${outPath}`);
  }

  const exports = await instantiate(outPath);

  let grandTotal = 0;
  let grandPass = 0;
  const results = [];
  cfg.cases.forEach((kase, i) => {
    const r = runCase(exports, kase, i);
    results.push(r);
    grandTotal += r.total;
    grandPass += r.pass;
  });

  // Report.
  for (const r of results) {
    const ok = r.pass === r.total;
    const tag = ok ? "PASS" : "FAIL";
    console.log(`  [${tag}] ${r.name}: ${r.pass}/${r.total}`);
    if (!ok) {
      const show = r.failures.slice(0, 10);
      for (const f of show) {
        console.log(
          `        args=(${f.args.join(", ")})  got=${f.got}  want=${f.want}`,
        );
      }
      if (r.failures.length > show.length) {
        console.log(`        ... and ${r.failures.length - show.length} more`);
      }
    } else if (verbose) {
      for (let i = 0; i < Math.min(r.total, 0); i++) { /* no-op */ }
    }
  }

  const allPass = grandPass === grandTotal;
  console.log("");
  console.log(`${grandPass}/${grandTotal} pass${allPass ? "" : "  <-- MISMATCH"}`);
  return { grandPass, grandTotal, allPass, results };
}

if (import.meta.main) {
  const args = Deno.args.slice();
  const verbose = args.includes("--verbose");
  const positional = args.filter((a) => !a.startsWith("--"));
  if (positional.length !== 1) {
    console.error(
      "usage: deno run --allow-read --allow-run parity.mjs <config.mjs> [--verbose]",
    );
    Deno.exit(2);
  }
  try {
    const { allPass } = await runParity(positional[0], { verbose });
    Deno.exit(allPass ? 0 : 1);
  } catch (e) {
    console.error(`affine-parity error: ${e.message}`);
    Deno.exit(1);
  }
}
