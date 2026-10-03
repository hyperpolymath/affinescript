#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# TEMPORARY diagnostics bridge (PR-only; deleted before merge).
#
# Why this exists: this repo's Actions job logs are served from
# productionresultssa1.blob.core.windows.net, which is unreachable from some
# sandboxes. GitHub *annotations* and the job *step summary* are, however,
# reachable through api.github.com. This script therefore republishes the
# interesting parts of a failing `dune runtest` — and the result of compiling a
# real downstream consumer's sources — as annotations, so the failure can be
# read without the Actions log UI.
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

# ── 1. the dune runtest failure ────────────────────────────────────────────
log = pathlib.Path("runtest.log")
if log.exists():
    lines = log.read_text(errors="replace").splitlines()
    print(f"[diag] runtest.log has {len(lines)} lines")
    keep = [l for l in lines
            if ("FAIL" in l or "Error" in l or "error:" in l
                or "Assert" in l or "expected" in l or "Fatal" in l)]
    body = ("== lines matching FAIL/Error/Assert/expected ==\n"
            + "\n".join(keep[:80])
            + "\n\n== tail (200 lines) ==\n"
            + "\n".join(lines[-200:]))
    annotate("diag-runtest", body)
    summary("diag: dune runtest", "\n".join(lines[-250:]))
else:
    annotate("diag-runtest", "runtest.log was not produced")

# ── 2. downstream probe: blocky-writer's sources (issue #771) ──────────────
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
    # control: a file this repo's own suite already compiles
    control = ["examples/hello.affine"]
    for f in control + files[:12]:
        out.append(f"\n──── {f} ────")
        for label, argv in [
            ("check(canonical)", ["check", f]),
            ("check(--face js)", ["check", "--face", "js", f]),
        ]:
            r = subprocess.run(["opam", "exec", "--", "dune", "exec",
                                "affinescript", "--"] + argv,
                               capture_output=True, text=True, timeout=300)
            tail = (r.stdout + r.stderr).strip().splitlines()
            out.append(f"[{label}] exit={r.returncode}")
            out.append("\n".join(tail[:12]) if tail else "(no output)")
        r = subprocess.run(["opam", "exec", "--", "dune", "exec", "affinescript",
                            "--", "compile", f, "-o", "/tmp/probe/out.wasm"],
                           capture_output=True, text=True, timeout=300)
        tail = (r.stdout + r.stderr).strip().splitlines()
        out.append(f"[compile] exit={r.returncode}")
        out.append("\n".join(tail[:12]) if tail else "(no output)")

body = "\n".join(out)
annotate("diag-downstream", body)
summary("diag: downstream probe", body)
print("[diag] done")
PY
