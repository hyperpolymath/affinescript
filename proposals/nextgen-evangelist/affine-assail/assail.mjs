// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// assail.mjs -- a weak-point scanner for AffineScript (.affine) source.
//
// This is the *working reference implementation* for the panic-attack
// AffineScript gap (panic-attack scans 49 languages but not .affine).
// nextgen-languages-evangelist roadmap ask #5. It demonstrates, against real
// migrated kernels, the three .affine-specific weak points the production
// Rust analyzer should learn:
//
//   PA-AFF-001  undeclared clamp-sentinel  (high)    -- the key rule
//   PA-AFF-002  unguarded boundary decoder (low/med) -- heuristic
//   PA-AFF-003  unbounded scan             (info)    -- perf/informational
//
// It is deliberately a line-oriented heuristic scanner (no full AffineScript
// parse): a tool in this family must run standalone, including on partial /
// non-compiling sources, exactly like panic-attack's other 49 analyzers.
//
// Usage:
//   deno run --allow-read assail.mjs <path-to.affine> [--json]
//
// Output: human-readable findings, then a JSON summary
//   { file, findings: [ { rule, severity, line, snippet } ] }
// (with --json, ONLY the JSON object is printed, for pipeline integration.)

// ---------------------------------------------------------------------------
// Rule catalogue
// ---------------------------------------------------------------------------

const RULES = {
  CLAMP_SENTINEL: {
    id: "PA-AFF-001",
    name: "undeclared-clamp-sentinel",
    severity: "high",
  },
  BOUNDARY_DECODER: {
    id: "PA-AFF-002",
    name: "unguarded-boundary-decoder",
    severity: "medium", // lowered to "low" when guards are detected
  },
  UNBOUNDED_SCAN: {
    id: "PA-AFF-003",
    name: "unbounded-scan",
    severity: "info",
  },
};

const ECHO_BOUNDARY_HINT =
  "out-of-band collapses to in-band code N; declare a clamp or prove " +
  "injective via echo-boundary (proposals/nextgen-evangelist/echo-boundary).";

// ---------------------------------------------------------------------------
// Lightweight source helpers
// ---------------------------------------------------------------------------

// Strip a trailing line comment (// ...) so comment text never triggers a
// rule. We do NOT strip block comments (AffineScript canonical face has none);
// `//` inside a string literal is rare in these integer kernels, accepted as
// a known heuristic limitation.
function stripLineComment(line) {
  const idx = line.indexOf("//");
  return idx === -1 ? line : line.slice(0, idx);
}

function trimSnippet(line) {
  return line.trim();
}

// Does a string contain a bare integer literal as a "result" token? Matches a
// standalone integer (optionally negative) that is the value of an arm/branch,
// e.g. `=> 0`, `{ 0 }`, `else { 3 }`, `return 3;`. Returns the literal text or
// null.
function extractIntResult(text) {
  // Common result positions: after `=>`, inside `{ N }`, after `return`,
  // after `else`/`then`. Capture the first standalone integer.
  const patterns = [
    /=>\s*(-?\d+)\b/, // match arm: Invalid(v) => 0
    /\breturn\s+(-?\d+)\b/, // return 0
    /\{\s*(-?\d+)\s*\}/, // braces: { 0 }
    /\belse\s+(-?\d+)\b/, // else 0 (brace-less)
    /\bthen\s+(-?\d+)\b/, // then 0 (pseudocode-ish)
  ];
  for (const re of patterns) {
    const m = text.match(re);
    if (m) return m[1];
  }
  return null;
}

// Catch-all / out-of-band arm heads we care about. `_` is the wildcard;
// `Invalid`, `Unknown`, `Other`, `Err`, `None` are the conventional
// out-of-band / sentinel constructor names seen across the migrated kernels.
const OUT_OF_BAND_HEAD =
  /(^|[\s|(])(_|Invalid|Unknown|Other|OutOfBand|Err|None|Default)\b/;

// An array-typed parameter list mention: `[Int]`, `[Float]`, `[Byte]` etc.
const ARRAY_PARAM = /\[\s*(Int|Float|Byte|Bool|Char|String)\s*\]/;
// A scalar Int/Float param.
const SCALAR_NUM_PARAM = /\b(a|b|x|y|z|i|j|k|n|m|level|threshold|idx|index)\s*:\s*(Int|Float)\b/;

// Indexing / scanning operations in a body.
const INDEX_OP = /\bnth\s*\(|\bget\s*\(|\bindex\s*\(|\[\s*[A-Za-z_][A-Za-z0-9_]*\s*\]|\.\s*at\s*\(/;

// A bounds/length guard somewhere in a function body.
const GUARD =
  /\b(len|length|array_len|path_len|count|size)\b|\bif\b[^/\n]*\b(<|<=|>|>=|==)\b/;

// ---------------------------------------------------------------------------
// Rule: PA-AFF-001  undeclared clamp-sentinel
// ---------------------------------------------------------------------------
// Flags a `match` arm OR an `if/else` that maps a catch-all / out-of-band /
// Invalid case to a *literal integer* in-band code. These collapse out-of-band
// input onto an in-band code without a declared clamp or an injectivity proof.
//
// Two shapes, both on a single line (the migrated kernels write flat arms):
//   (a) match arm:  Invalid(v) => { if v < 0 { 0 } else { 3 } }
//                   _ => 0
//   (b) brace-less: Invalid(v) => 3
function scanClampSentinel(lines) {
  const findings = [];
  for (let i = 0; i < lines.length; i++) {
    const raw = lines[i];
    const code = stripLineComment(raw);
    if (!code.trim()) continue;

    // Must look like a match arm or a guarded mapping: needs `=>` (arm) and an
    // out-of-band head before it.
    const armIdx = code.indexOf("=>");
    if (armIdx === -1) continue;

    const head = code.slice(0, armIdx);
    if (!OUT_OF_BAND_HEAD.test(head)) continue;

    const body = code.slice(armIdx);
    const lit = extractIntResult(body);
    if (lit === null) continue;

    findings.push({
      rule: RULES.CLAMP_SENTINEL.id,
      severity: RULES.CLAMP_SENTINEL.severity,
      line: i + 1,
      snippet: trimSnippet(raw),
      detail: ECHO_BOUNDARY_HINT.replace("N", lit),
    });
  }
  return findings;
}

// ---------------------------------------------------------------------------
// Rule: PA-AFF-002  unguarded boundary decoder
// ---------------------------------------------------------------------------
// A `pub fn` whose params include `[Int]`/`Int` and whose body indexes/scans
// (nth(, subscript, get(...)) — a boundary decoder. Severity medium; lowered to
// low when a length/bounds guard is detected in the same body. (Heuristic.)
function scanBoundaryDecoder(lines) {
  const findings = [];
  for (let i = 0; i < lines.length; i++) {
    const code = stripLineComment(lines[i]);
    const m = code.match(/\bpub\s+fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)/);
    if (!m) continue;

    const params = m[2];
    const takesNumericInput =
      ARRAY_PARAM.test(params) || SCALAR_NUM_PARAM.test(params) || /:\s*Int\b/.test(params);
    if (!takesNumericInput) continue;

    // Gather the function body: from this line to the matching end of the
    // brace block (best-effort brace counting starting at the `{`).
    const { bodyText, endLine } = collectBody(lines, i);
    if (!INDEX_OP.test(bodyText)) continue;

    const hasGuard = GUARD.test(bodyText);
    findings.push({
      rule: RULES.BOUNDARY_DECODER.id,
      severity: hasGuard ? "low" : "medium",
      line: i + 1,
      snippet: trimSnippet(lines[i]),
      detail:
        `pub fn '${m[1]}' decodes numeric boundary input and indexes/scans` +
        (hasGuard
          ? " (a length/bounds guard was detected; severity lowered to low — confirm it covers every index)."
          : " with NO obvious length/bounds guard; a malformed [Int]/Int can read out of range."),
    });
    // Skip ahead past the body to avoid re-matching nested `pub fn` (there are
    // none nested in practice, but keep the scan O(n)).
    i = Math.max(i, endLine - 1);
  }
  return findings;
}

// Collect a brace-delimited body starting at the line containing the fn's
// opening `{`. Returns the concatenated body text and the (1-based-exclusive)
// end line index. Falls back to a 30-line window if braces don't balance.
function collectBody(lines, startLine) {
  let depth = 0;
  let started = false;
  const parts = [];
  let end = startLine;
  for (let j = startLine; j < lines.length && j < startLine + 200; j++) {
    const code = stripLineComment(lines[j]);
    parts.push(code);
    for (const ch of code) {
      if (ch === "{") {
        depth += 1;
        started = true;
      } else if (ch === "}") {
        depth -= 1;
      }
    }
    end = j + 1;
    if (started && depth <= 0) break;
  }
  return { bodyText: parts.join("\n"), endLine: end };
}

// ---------------------------------------------------------------------------
// Rule: PA-AFF-003  unbounded scan
// ---------------------------------------------------------------------------
// `for <v> in <coll>` loops over an array-typed parameter. Informational/perf:
// in pure-integer kernels these are O(n) passes; flagged so a reviewer can
// confirm n is bounded (e.g. fixed-length protocol payloads) rather than
// attacker-controlled unboundedly.
function scanUnboundedScan(lines) {
  const findings = [];

  // First, collect array-typed parameter names from all fn signatures so we can
  // tell "loops over a param" from "loops over a local".
  const arrayParams = new Set();
  for (const line of lines) {
    const code = stripLineComment(line);
    const sig = code.match(/\bfn\s+[A-Za-z_][A-Za-z0-9_]*\s*\(([^)]*)\)/);
    if (!sig) continue;
    // Each param is `name: Type`; keep those with an array type.
    for (const part of sig[1].split(",")) {
      const pm = part.match(/([A-Za-z_][A-Za-z0-9_]*)\s*:\s*(\[[^\]]*\])/);
      if (pm) arrayParams.add(pm[1]);
    }
  }

  for (let i = 0; i < lines.length; i++) {
    const code = stripLineComment(lines[i]);
    const fm = code.match(/\bfor\s+[A-Za-z_][A-Za-z0-9_]*\s+in\s+([A-Za-z_][A-Za-z0-9_]*)\b/);
    if (!fm) continue;
    const coll = fm[1];
    const overParam = arrayParams.has(coll);
    findings.push({
      rule: RULES.UNBOUNDED_SCAN.id,
      severity: RULES.UNBOUNDED_SCAN.severity,
      line: i + 1,
      snippet: trimSnippet(lines[i]),
      detail: overParam
        ? `scans array parameter '${coll}' — O(n) over caller-supplied length; confirm n is bounded.`
        : `scans '${coll}' — O(n) loop; informational.`,
    });
  }
  return findings;
}

// ---------------------------------------------------------------------------
// Driver
// ---------------------------------------------------------------------------

function analyze(source) {
  const lines = source.split("\n");
  const findings = [
    ...scanClampSentinel(lines),
    ...scanBoundaryDecoder(lines),
    ...scanUnboundedScan(lines),
  ];
  // Stable order: by line, then by rule id.
  findings.sort((a, b) => a.line - b.line || a.rule.localeCompare(b.rule));
  return findings;
}

const SEVERITY_RANK = { high: 0, medium: 1, low: 2, info: 3 };

function printHuman(file, findings) {
  console.log(`affine-assail: scanning ${file}`);
  console.log("");
  if (findings.length === 0) {
    console.log("  no weak points found.");
  } else {
    for (const f of findings) {
      console.log(`  [${f.severity.toUpperCase()}] ${f.rule}  ${file}:${f.line}`);
      console.log(`      ${f.snippet}`);
      console.log(`      -> ${f.detail}`);
      console.log("");
    }
  }
  // Tally by severity.
  const tally = {};
  for (const f of findings) tally[f.severity] = (tally[f.severity] || 0) + 1;
  const order = Object.keys(tally).sort(
    (a, b) => SEVERITY_RANK[a] - SEVERITY_RANK[b],
  );
  const summary = order.map((s) => `${tally[s]} ${s}`).join(", ");
  console.log(`  ${findings.length} finding(s)${summary ? ": " + summary : ""}.`);
  console.log("");
}

function main() {
  const args = Deno.args.slice();
  const jsonOnly = args.includes("--json");
  const positional = args.filter((a) => !a.startsWith("--"));

  if (positional.length !== 1) {
    console.error("usage: deno run --allow-read assail.mjs <path-to.affine> [--json]");
    Deno.exit(2);
  }

  const file = positional[0];
  let source;
  try {
    source = Deno.readTextFileSync(file);
  } catch (e) {
    console.error(`affine-assail: cannot read ${file}: ${e.message}`);
    Deno.exit(2);
  }

  // The JSON summary: drop the human-only `detail` field's verbosity? No —
  // keep `detail` too; it is useful for pipeline consumers. The spec's minimal
  // shape is {rule, severity, line, snippet}; we include those plus detail.
  const findings = analyze(source);
  const jsonSummary = {
    file,
    findings: findings.map((f) => ({
      rule: f.rule,
      severity: f.severity,
      line: f.line,
      snippet: f.snippet,
      detail: f.detail,
    })),
  };

  if (jsonOnly) {
    console.log(JSON.stringify(jsonSummary, null, 2));
  } else {
    printHuman(file, findings);
    console.log("JSON summary:");
    console.log(JSON.stringify(jsonSummary));
  }

  // Exit code: 1 if any high-severity finding, else 0 (CI-gate friendly).
  const anyHigh = findings.some((f) => f.severity === "high");
  Deno.exit(anyHigh ? 1 : 0);
}

main();
