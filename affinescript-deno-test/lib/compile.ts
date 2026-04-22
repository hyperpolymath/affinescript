// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: compile.ts
//
// Wraps the `affinescript compile` CLI. Given a `.affine` source file,
// produces a sibling `.wasm` file and returns its absolute path.
//
// The `AFFINESCRIPT_BIN` env var overrides the default path to the compiler.
// Default is the local dev-build at developer-ecosystem/nextgen-languages/
// affinescript/_build/install/default/bin/affinescript (useful while the
// compiler is not on $PATH).

const DEFAULT_BIN =
  "/var/mnt/eclipse/repos/developer-ecosystem/nextgen-languages/affinescript/_build/install/default/bin/affinescript";

/** Absolute path to the `affinescript` compiler binary. */
export function resolveCompilerPath(): string {
  return Deno.env.get("AFFINESCRIPT_BIN") ?? DEFAULT_BIN;
}

/**
 * Compile an AffineScript source file to WebAssembly. Returns the absolute
 * path to the emitted `.wasm`. Throws with compiler stderr if compilation
 * fails.
 */
export async function compileToWasm(sourcePath: string): Promise<string> {
  const absolute = sourcePath.startsWith("/")
    ? sourcePath
    : `${Deno.cwd()}/${sourcePath}`;

  const wasmPath = absolute.replace(/\.(affine|afs|rattle|pyaff|jsaff)$/, ".wasm");
  if (wasmPath === absolute) {
    throw new Error(
      `compileToWasm: source file must end in .affine / .afs / .rattle / .pyaff / .jsaff — got ${sourcePath}`,
    );
  }

  const bin = resolveCompilerPath();
  const cmd = new Deno.Command(bin, {
    args: ["compile", absolute, "-o", wasmPath],
    stdout: "piped",
    stderr: "piped",
  });

  const { code, stdout, stderr } = await cmd.output();
  if (code !== 0) {
    const out = new TextDecoder().decode(stdout);
    const err = new TextDecoder().decode(stderr);
    throw new Error(
      `affinescript compile failed (exit ${code}) for ${sourcePath}\n` +
        `STDOUT:\n${out}\nSTDERR:\n${err}`,
    );
  }

  return wasmPath;
}
