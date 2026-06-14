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
// This runner handles i32 results over i32 / f64 / String arguments:
//   * i32 args   -- swept as integer ranges/values (the original scalar ABI),
//   * f64 args   -- any non-integer values in a `{ values: [...] }` spec are
//                   passed through verbatim as wasm f64 (args are not coerced),
//   * String args -- written into linear memory as `[len:i32 LE][utf8 bytes]`
//                   and passed BY POINTER (an i32). The string layout matches
//                   the compiler's (codegen `gen_literal`). The oracle still
//                   receives the original JS string, not the pointer.
// Every result is read back as an i32 (`| 0`).
//
// String scratch memory: after instantiation the runner grows the module's
// linear memory by one page (64 KiB) and writes string args into that fresh
// page (reset before each call). Because the page sits above all of the
// module's data and bump-heap base, a read-only string consumer (the common
// case -- a classifier that reads its String arg and returns an Int) never
// collides with it. A module that *allocates heavily during the call* before
// reading its String arg could in principle reach the scratch page; such cases
// should size their sweeps modestly or write a bespoke driver.
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
  if (spec && Array.isArray(spec.strings)) return spec.strings.slice();
  throw new Error(
    `arg spec #${paramIndex} must be a number, a [lo,hi] range, { values: [...] }, or { strings: [...] }`,
  );
}

// Build a writer for String args. Grows the module's linear memory by one page
// and bump-allocates `[len:i32 LE][utf8]` strings into it, 4-byte aligned, so
// each is a valid AffineScript string the wasm can read by pointer. Returns
// null when the module exports no memory (then String args are a usage error).
function makeStringWriter(memory) {
  if (!(memory instanceof WebAssembly.Memory)) return null;
  const oldPages = memory.grow(1); // fresh page above all module data + heap
  const base = oldPages * 65536;
  const enc = new TextEncoder();
  let bump = base;
  return {
    reset() {
      bump = base;
    },
    write(s) {
      const u = enc.encode(s);
      const need = 4 + u.length;
      // Grow if a long string would overrun the scratch page.
      if (bump + need > memory.buffer.byteLength) {
        memory.grow(Math.ceil(need / 65536) + 1);
      }
      const ptr = bump;
      const dv = new DataView(memory.buffer); // re-read: grow detaches the buffer
      dv.setInt32(ptr, u.length, true);
      new Uint8Array(memory.buffer, ptr + 4, u.length).set(u);
      bump = ptr + need;
      bump += (4 - (bump & 3)) & 3; // 4-byte align the next string
      return ptr;
    },
  };
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
function runCase(exports, kase, idx, stringWriter) {
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
    // String args are written into scratch linear memory as [len][utf8] and
    // passed by pointer; the oracle still sees the original JS string. i32 /
    // f64 args pass through unchanged.
    let wasmArgs = tuple;
    if (tuple.some((v) => typeof v === "string")) {
      if (!stringWriter) {
        throw new Error(
          `case "${name}" passes a String arg but the module exports no "memory"`,
        );
      }
      stringWriter.reset();
      wasmArgs = tuple.map((v) =>
        typeof v === "string" ? stringWriter.write(v) : v
      );
    }
    const got = fn(...wasmArgs) | 0; // normalise result to i32
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
  const stringWriter = makeStringWriter(exports.memory);

  let grandTotal = 0;
  let grandPass = 0;
  const results = [];
  cfg.cases.forEach((kase, i) => {
    const r = runCase(exports, kase, i, stringWriter);
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
