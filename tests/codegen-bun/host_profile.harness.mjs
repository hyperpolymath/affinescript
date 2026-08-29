// SPDX-License-Identifier: MPL-2.0
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import * as subject from "./host_profile.bun.js";

const root = mkdtempSync(join(tmpdir(), "affinescript-bun-esm-"));
const nested = join(root, "nested");
const file = join(nested, "probe.txt");

try {
  subject.make_directory(nested);
  if (!subject.is_directory(nested)) throw new Error("mkdir/stat directory failed");
  subject.write_text(file, "Bun host profile");
  if (!subject.is_file(file)) throw new Error("write/stat file failed");
  if (subject.read_text(file) !== "Bun host profile") throw new Error("text round trip failed");
  if (subject.file_size(file) !== 16) throw new Error("file size failed");
  if (subject.first_byte(file) !== 66) throw new Error("byte read failed");
  if (subject.argument_count() !== 2) throw new Error("argv lowering failed");
  if (subject.environment_value("AFFINESCRIPT_BUN_PROBE") !== "estate") {
    throw new Error("environment lowering failed");
  }
  if (subject.environment_value("AFFINESCRIPT_BUN_MISSING") !== "") {
    throw new Error("missing environment value failed");
  }
  if (subject.run_successful_child() !== 0) throw new Error("subprocess lowering failed");
  subject.remove_path(file);
  let missingPathThrew = false;
  try {
    subject.is_file(file);
  } catch (error) {
    if (error?.code !== "ENOENT") throw error;
    missingPathThrew = true;
  }
  if (!missingPathThrew) throw new Error("remove did not make path absent");
} finally {
  rmSync(root, { recursive: true, force: true });
}

const moduleUrl = new URL("./host_profile.bun.js", import.meta.url).href;
const stdinProbe = Bun.spawnSync({
  cmd: ["bun", "-e", `const m = await import(${JSON.stringify(moduleUrl)}); process.stdout.write(m.read_standard_input());`],
  stdin: new TextEncoder().encode("stdin-probe"),
  stdout: "pipe",
  stderr: "pipe",
});
if (stdinProbe.exitCode !== 0 || stdinProbe.stdout.toString() !== "stdin-probe") {
  throw new Error(`stdin lowering failed: ${stdinProbe.stderr.toString()}`);
}

const exitProbe = Bun.spawnSync({
  cmd: ["bun", "-e", `const m = await import(${JSON.stringify(moduleUrl)}); m.exit_with(23);`],
  stdout: "pipe",
  stderr: "pipe",
});
if (exitProbe.exitCode !== 23) throw new Error("exit-status lowering failed");

console.log("Bun-ESM host profile: ok");
