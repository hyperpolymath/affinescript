#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# TEMPORARY diagnostics bridge (PR-only; deleted before merge).
#
# Why this exists: this repo's Actions job logs are served from
# productionresultssa1.blob.core.windows.net, which is unreachable from some
# sandboxes. GitHub *annotations* are reachable through api.github.com, so
# this republishes the interesting parts of a failing `dune runtest` — plus
# two probes — as annotations, readable without the Actions log UI.
set -uo pipefail

python3 - <<'PY'
import os, pathlib, subprocess, urllib.parse

def annotate(title, text, limit=60000):
    msg = urllib.parse.quote(text[:limit], safe="")
    print(f"::error title={title}::{msg}", flush=True)

def summary(title, text):
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not path:
        return
    with open(path, "a") as fh:
        fh.write(f"\n### {title}\n\n```\n{text}\n```\n")

def run(argv, timeout=300):
    try:
        r = subprocess.run(argv, capture_output=True, text=True, timeout=timeout)
        return r.returncode, (r.stdout + r.stderr).strip()
    except subprocess.TimeoutExpired:
        return 124, "TIMEOUT"

# ── 1. the dune runtest failure ────────────────────────────────────────────
log = pathlib.Path("runtest.log")
if log.exists():
    lines = log.read_text(errors="replace").splitlines()
    keep = [l for l in lines
            if ("[FAIL]" in l or "FAIL " in l or "Error" in l or "error:" in l
                or "Assert" in l or "expected" in l)]
    body = ("== lines matching FAIL/Error/Assert/expected ==\n"
            + "\n".join(keep[:80])
            + "\n\n== tail (120 lines) ==\n"
            + "\n".join(lines[-120:]))
    annotate("diag-runtest", body)
else:
    annotate("diag-runtest", "runtest.log was not produced")

# ── 2. parser probe: which construct does the #644 test need? ─────────────
VARIANTS = {
    "v1-exact-test-source": """module EmptyArm;
enum Opt { SomeV(Int), NoneV }
pub fn f(o: Opt) -> Int {
  match o {
    SomeV(v) => { return v; }
    NoneV => {}
  }
  return 0;
}
""",
    "v2-plus-semicolon-after-match": """module EmptyArm;
enum Opt { SomeV(Int), NoneV }
pub fn f(o: Opt) -> Int {
  match o {
    SomeV(v) => { return v; }
    NoneV => {}
  };
  return 0;
}
""",
    "v3-match-as-final-expr": """module EmptyArm;
enum Opt { SomeV(Int), NoneV }
pub fn f(o: Opt) -> Int {
  match o {
    SomeV(v) => { return v; }
    NoneV => {}
  }
}
""",
    "v4-empty-block-arm-only-final": """module EmptyArm;
enum Opt { Som(Int), Non }
pub fn f(o: Opt) -> Int {
  match o {
    Non => {}
  }
}
""",
    "v5-nonempty-block-arm-only-final": """module EmptyArm;
enum Opt { Som(Int), Non }
pub fn f(o: Opt) -> Int {
  match o {
    Som(v) => { return v; }
  }
}
""",
    "v6-empty-block-empty-body": """module EmptyArm;
pub fn f() -> Int {
  {}
}
""",
    "v7-two-empty-block-arms-final": """module EmptyArm;
enum Opt { Som(Int), Non }
pub fn f(o: Opt) -> Int {
  match o {
    Som(v) => {}
    Non => {}
  }
}
""",
}

out = ["parser probe: `affinescript parse` on variants of the #644 test source"]
probe_dir = pathlib.Path("/tmp/parse-probe")
probe_dir.mkdir(parents=True, exist_ok=True)
for name, src in VARIANTS.items():
    p = probe_dir / f"{name}.affine"
    p.write_text(src)
    for label, extra in (("canonical", []), ("face-js", ["--face", "js"])):
        rc, text = run(["opam", "exec", "--", "dune", "exec", "affinescript",
                        "--", "parse"] + extra + [str(p)])
        first = " | ".join(text.splitlines()[:3]) if text else "(silent)"
        out.append(f"{name} [{label}] rc={rc}: {first}")

body = "\n".join(out)
annotate("diag-parser", body)
summary("diag: parser probe", body)

# ── 3. on-ramp example consumer (compile + run) ───────────────────────────
example = pathlib.Path("examples/consumers/extension-boundary")
if example.exists():
    rc, text = run(["bash", str(example / "build.sh")])
    body = f"build.sh rc={rc}\n---\n{text[-6000:]}"
else:
    body = "examples/consumers/extension-boundary missing"
annotate("diag-example", body)
summary("diag: on-ramp example", body)

# ── 4. downstream probe: blocky-writer's sources (issue #771) ─────────────
probe = pathlib.Path("/tmp/probe")
probe.mkdir(parents=True, exist_ok=True)
clone = subprocess.run(
    ["git", "clone", "--depth", "1", "--quiet",
     "https://github.com/hyperpolymath/blocky-writer", "/tmp/probe/bw"],
    capture_output=True, text=True)
out = []
if clone.returncode != 0:
    out.append("clone failed: " + clone.stderr[-400:])
else:
    src = pathlib.Path("/tmp/probe/bw/src")
    files = sorted(str(p) for p in src.rglob("*.affine"))
    out.append(f"downstream .affine files: {len(files)}")
    for f in files:
        rc, text = run(["opam", "exec", "--", "dune", "exec", "affinescript",
                        "--", "check", f])
        first = " | ".join(text.splitlines()[:2]) if text else "(silent)"
        out.append(f"{f} rc={rc}: {first}")
body = "\n".join(out)
annotate("diag-downstream", body)
summary("diag: downstream probe", body)
print("[diag] done")
PY
