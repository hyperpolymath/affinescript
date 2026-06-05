// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-migratability — triage whether a ReScript `.res` kernel can be
// migrated to AffineScript-wasm today, or is blocked on a specific compiler
// wall. Roadmap ask #4 of the nextgen-languages-evangelist.
//
// Two known AffineScript compiler walls are detected:
//   STRING-GATED  — the variable-string backend gap (string introspection
//                   ops that the current codegen cannot lower).
//   EFFECT-GATED  — the effect-codegen wall (ambient effects + module-level
//                   mutable state that has no effect-tracked lowering yet).
//
// Plus re-decomposition signals: patterns that are migratable, but not 1:1.
// They do NOT block migration — they are reported as "re-decompose" notes.
//
// Verdict precedence: STRING-GATED > EFFECT-GATED > MIGRATABLE NOW.
//
// Usage:
//   deno run --allow-read migratability.mjs <file.res> [<file.res> ...]
//   deno run --allow-read migratability.mjs --json <file.res>
//
// Exit code: 0 if all inputs are MIGRATABLE NOW, 1 if any are walled,
// 2 on usage / IO error. (Re-decompose notes alone do NOT fail.)

// ---------------------------------------------------------------------------
// Detection rules
// ---------------------------------------------------------------------------
//
// Each rule is { wall, label, test } where `test(line)` returns either null
// (no match) or the matched substring. A line may match several rules.

// --- STRING-GATED: variable-string backend gap -----------------------------
// Plain `++` concatenation and string *literals* are FINE and intentionally
// not listed here.
const STRING_RULES = [
  ["String.fromCharCode", /String\.fromCharCode\b/],
  ["String.length", /String\.length\b/],
  ["String.startsWith", /String\.startsWith\b/],
  ["String.endsWith", /String\.endsWith\b/],
  ["String.slice", /String\.slice\b/],
  ["String.get", /String\.get\b/],
  ["String.charCodeAt", /String\.charCodeAt\b/],
  ["String.indexOf", /String\.indexOf\b/],
  // String subscripting, e.g. `s[i]` / `path[0]`. Heuristic: an identifier
  // immediately followed by `[ <index-ish> ]`. We bias toward names that look
  // string-ish OR an index that is plainly numeric/identifier, while skipping
  // obvious array-of-int literals like `[1, 2, 3]`.
  ["string subscript", /\b[A-Za-z_][A-Za-z0-9_']*\[[A-Za-z0-9_'. ]+\]/],
];

// --- EFFECT-GATED: effect-codegen wall -------------------------------------
// Ambient-effect calls. Module-level mutable state is handled separately
// (it needs column/scope awareness, not just a per-line regex).
const EFFECT_RULES = [
  ["Date.now", /\bDate\.now\b/],
  ["Js.Date", /\bJs\.Date\b/],
  ["Console.log", /\bConsole\.log\b/],
  ["Math.random", /\bMath\.random\b/],
];

// --- re-decomposition signals (non-blocking) -------------------------------
const REDECOMPOSE_RULES = [
  ["promise type (incidental async)", /\bpromise</],
  ["Promise.resolve (incidental async)", /\bPromise\.resolve\b/],
  ["service-locator: getDeviceState", /\bgetDeviceState\b/],
  ["service-locator: getState(", /\bgetState\(/],
];

// ---------------------------------------------------------------------------
// Module-level mutable-state detection
// ---------------------------------------------------------------------------
//
// A `let` binding whose RHS allocates mutable state (`Dict.make()` or
// `ref(...)`) is an effect wall ONLY when it is module-level — i.e. not
// nested inside a function body. In ReScript with in-source formatting,
// module-level `let`s start at column 0; function-local `let`s are always
// indented inside their enclosing `let f = (...) => {`.
//
// So: a line that begins with `let ` (no leading whitespace) and whose RHS
// (text after the first `=`) contains a mutable allocator.

const MUT_ALLOCATORS = [
  ["Dict.make()", /Dict\.make\(\)/],
  ["ref(...)", /\bref\(/],
];

function detectModuleLevelMutable(line) {
  // Must be a top-level `let` (column 0, exactly one `let ` at the start).
  if (!/^let\s/.test(line)) return null;
  const eq = line.indexOf("=");
  if (eq === -1) return null;
  const rhs = line.slice(eq + 1);
  for (const [label, re] of MUT_ALLOCATORS) {
    const m = rhs.match(re);
    if (m) return { label, snippet: m[0] };
  }
  return null;
}

// ---------------------------------------------------------------------------
// Comment stripping
// ---------------------------------------------------------------------------
//
// We do not want a pattern that only appears in a comment (e.g. an `@val
// external` doc, or prose mentioning `Date.now`) to count as a real wall.
// We strip line comments (`//...`) before scanning. Block comments (`/* */`)
// are rare in ReScript and handled with a simple stateful pass. String
// literals are deliberately NOT stripped: an `@val external` BINDING whose
// payload string is `"Date.now"` is a genuine effect site only when called,
// and the call (`dateNow()`) is what carries the effect — but to stay
// conservative and avoid false-negatives we keep literal text out of the
// scan for the literal-only `@val external ... = "Date.now"` shape, since
// that line is a binding, not a use. See `isExternalBinding`.

function isExternalBinding(line) {
  // `@val external foo: ... = "..."` — a binding line, not a use site.
  return /@(val|scope|send|get|set|new|module)\b/.test(line) &&
    /\bexternal\b/.test(line);
}

// Strip `//` line comments outside of string literals. Good enough for
// ReScript: we walk the line tracking double-quote string state.
function stripLineComment(line) {
  let inStr = false;
  for (let i = 0; i < line.length - 1; i++) {
    const c = line[i];
    if (c === '"' && line[i - 1] !== "\\") inStr = !inStr;
    if (!inStr && c === "/" && line[i + 1] === "/") {
      return line.slice(0, i);
    }
  }
  return line;
}

// ---------------------------------------------------------------------------
// Scanner
// ---------------------------------------------------------------------------

function scan(source, file) {
  const rawLines = source.split(/\r?\n/);

  const walls = []; // { kind, label, line, snippet }
  const notes = []; // { label, line, snippet }

  let inBlockComment = false;

  rawLines.forEach((raw, idx) => {
    const lineNo = idx + 1;

    // --- block comment handling ---
    let line = raw;
    if (inBlockComment) {
      const end = line.indexOf("*/");
      if (end === -1) return; // whole line is inside a block comment
      line = line.slice(end + 2);
      inBlockComment = false;
    }
    const open = line.indexOf("/*");
    if (open !== -1) {
      const close = line.indexOf("*/", open + 2);
      if (close === -1) {
        line = line.slice(0, open);
        inBlockComment = true;
      } else {
        line = line.slice(0, open) + " " + line.slice(close + 2);
      }
    }

    // --- line comment handling ---
    line = stripLineComment(line);
    if (line.trim() === "") return;

    const externalBinding = isExternalBinding(line);

    // --- STRING-GATED ---
    for (const [label, re] of STRING_RULES) {
      const m = line.match(re);
      if (m) {
        walls.push({ kind: "STRING-GATED", label, line: lineNo, snippet: m[0] });
      }
    }

    // --- EFFECT-GATED: ambient calls ---
    // Skip pure `@val external ... = "..."` binding lines: they declare a
    // name, the effect is carried by the *use* of that name elsewhere.
    if (!externalBinding) {
      for (const [label, re] of EFFECT_RULES) {
        const m = line.match(re);
        if (m) {
          walls.push({ kind: "EFFECT-GATED", label, line: lineNo, snippet: m[0] });
        }
      }
    }

    // --- EFFECT-GATED: module-level mutable state ---
    const mut = detectModuleLevelMutable(line);
    if (mut) {
      walls.push({
        kind: "EFFECT-GATED",
        label: `module-level mutable state (${mut.label})`,
        line: lineNo,
        snippet: line.trim(),
      });
    }

    // --- re-decomposition notes (non-blocking) ---
    for (const [label, re] of REDECOMPOSE_RULES) {
      const m = line.match(re);
      if (m) {
        notes.push({ label, line: lineNo, snippet: m[0] });
      }
    }
  });

  // --- verdict precedence ---
  const stringWalls = walls.filter((w) => w.kind === "STRING-GATED");
  const effectWalls = walls.filter((w) => w.kind === "EFFECT-GATED");

  let verdict;
  if (stringWalls.length > 0) verdict = "STRING-GATED";
  else if (effectWalls.length > 0) verdict = "EFFECT-GATED";
  else verdict = "MIGRATABLE NOW";

  return { file, verdict, walls, notes, stringWalls, effectWalls };
}

// ---------------------------------------------------------------------------
// Reporting
// ---------------------------------------------------------------------------

function basename(p) {
  const parts = p.split("/");
  return parts[parts.length - 1];
}

function printHuman(result) {
  const { file, verdict, walls, notes } = result;
  const tag = verdict === "MIGRATABLE NOW" ? "[OK]" : "[WALL]";
  console.log(`${tag} ${basename(file)} -> ${verdict}`);
  console.log(`     ${file}`);

  if (walls.length > 0) {
    // The deciding walls first, grouped by kind.
    const deciding = verdict === "STRING-GATED" ? "STRING-GATED" : "EFFECT-GATED";
    const decidingWalls = walls.filter((w) => w.kind === deciding);
    console.log(`  walls (${deciding}):`);
    for (const w of decidingWalls) {
      console.log(`    ${file}:${w.line}  ${w.label}  ->  ${w.snippet}`);
    }
    // Any other-kind walls that exist but did not decide the verdict.
    const other = walls.filter((w) => w.kind !== deciding);
    if (other.length > 0) {
      console.log(`  also flagged (${other[0].kind}):`);
      for (const w of other) {
        console.log(`    ${file}:${w.line}  ${w.label}  ->  ${w.snippet}`);
      }
    }
  }

  if (notes.length > 0) {
    console.log("  re-decompose notes (non-blocking):");
    for (const n of notes) {
      console.log(`    ${file}:${n.line}  ${n.label}  ->  ${n.snippet}`);
    }
  }
}

function summaryJSON(result) {
  return {
    file: result.file,
    verdict: result.verdict,
    walls: result.walls.map((w) => ({
      kind: w.kind,
      label: w.label,
      line: w.line,
      snippet: w.snippet,
    })),
    notes: result.notes.map((n) => ({
      label: n.label,
      line: n.line,
      snippet: n.snippet,
    })),
  };
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

function usage() {
  console.error(
    "usage: deno run --allow-read migratability.mjs [--json] <file.res> [<file.res> ...]",
  );
}

async function main() {
  const args = [...Deno.args];
  let jsonOnly = false;
  const files = [];
  for (const a of args) {
    if (a === "--json") jsonOnly = true;
    else if (a === "-h" || a === "--help") {
      usage();
      Deno.exit(0);
    } else files.push(a);
  }

  if (files.length === 0) {
    usage();
    Deno.exit(2);
  }

  const results = [];
  for (const file of files) {
    let source;
    try {
      source = await Deno.readTextFile(file);
    } catch (e) {
      console.error(`error: cannot read ${file}: ${e.message}`);
      Deno.exit(2);
    }
    results.push(scan(source, file));
  }

  if (jsonOnly) {
    const out = results.length === 1
      ? summaryJSON(results[0])
      : results.map(summaryJSON);
    console.log(JSON.stringify(out, null, 2));
  } else {
    results.forEach((r, i) => {
      if (i > 0) console.log("");
      printHuman(r);
      console.log("  summary: " + JSON.stringify(summaryJSON(r)));
    });
  }

  const anyWalled = results.some((r) => r.verdict !== "MIGRATABLE NOW");
  Deno.exit(anyWalled ? 1 : 0);
}

// Exported for testing / orchestrator embedding.
export { scan, summaryJSON };

if (import.meta.main) {
  await main();
}
