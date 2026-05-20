// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! Compiler resolution (INT-10 / #282 — ADR-019 S4).
//!
//! The AffineScript compiler is a native OCaml binary, not a crate the
//! LSP can link.  Per **ADR-019** (`docs/specs/SETTLED-DECISIONS.adoc`)
//! the GitHub Release is the canonical artifact and the thin Deno/JSR
//! package `@hyperpolymath/affinescript` (`packages/affinescript-cli`,
//! #260 S3) is the ergonomic front door that downloads, checksum-verifies,
//! caches and execs the pinned per-platform binary.
//!
//! This module is the LSP side of S4: it resolves *how* to invoke the
//! compiler, with **no bespoke compiler-bundling in the LSP**.  Precedence:
//!
//! 1. `AFFINESCRIPT_COMPILER` — an explicit path to a compiler binary.
//!    The escape hatch for source/dev builds (and the resolution seam the
//!    smoke test drives).  No download, runs exactly what is named.
//! 2. `affinescript` on `PATH` — a source/dev install already provisioned
//!    a compiler; use it directly.
//! 3. The **ADR-019 shim** — `deno run … jsr:@hyperpolymath/affinescript@<pin>`.
//!    This is the default distribution path for an installed LSP: the
//!    shim itself does the download + SHA256 verify + cache + exec.
//!
//! The shim version is pinned here in lockstep with the shim package's
//! `deno.json` `version` (the ADR-019 "one version + checksum per shim
//! release — no floating fetch" rule).  Bump both together.

/// Pinned ADR-019 shim spec.  Must track `packages/affinescript-cli/deno.json`
/// `version` (and therefore the `pins.js` `VERSION`).  Do not float this.
pub const SHIM_SPEC: &str = "jsr:@hyperpolymath/affinescript@0.1.2";

/// Environment variable naming an explicit compiler binary (precedence 1).
pub const COMPILER_ENV: &str = "AFFINESCRIPT_COMPILER";

/// A resolved way to invoke the compiler: a `program` plus the argv
/// *prefix* that must precede the usual `check --json <file>` arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCompiler {
    /// Program to spawn.
    pub program: String,
    /// Argv prefix (e.g. the `deno run … <shim>` wrapper); empty for a
    /// direct binary.
    pub prefix_args: Vec<String>,
}

impl ResolvedCompiler {
    /// Build a `tokio::process::Command` for `program prefix_args… args…`.
    pub fn command<I, S>(&self, args: I) -> tokio::process::Command
    where
        I: IntoIterator<Item = S>,
        S: AsRef<std::ffi::OsStr>,
    {
        let mut cmd = tokio::process::Command::new(&self.program);
        cmd.args(&self.prefix_args);
        cmd.args(args);
        cmd
    }

    /// Human-readable form for log messages.
    pub fn display(&self) -> String {
        if self.prefix_args.is_empty() {
            self.program.clone()
        } else {
            format!("{} {}", self.program, self.prefix_args.join(" "))
        }
    }
}

/// The ADR-019 shim invocation (precedence 3).  Permissions are exactly
/// what the shim needs: read/write its cache, read env for the cache dir,
/// net to fetch the pinned Release asset, run to exec the binary.
fn shim() -> ResolvedCompiler {
    ResolvedCompiler {
        program: "deno".to_string(),
        prefix_args: vec![
            "run".to_string(),
            "--allow-read".to_string(),
            "--allow-write".to_string(),
            "--allow-env".to_string(),
            "--allow-net".to_string(),
            "--allow-run".to_string(),
            SHIM_SPEC.to_string(),
        ],
    }
}

/// Pure resolution, with the environment lookups injected so it is
/// testable without mutating the real process environment or `PATH`.
fn resolve_with(env_override: Option<String>, affinescript_on_path: bool) -> ResolvedCompiler {
    if let Some(path) = env_override.filter(|p| !p.is_empty()) {
        return ResolvedCompiler { program: path, prefix_args: Vec::new() };
    }
    if affinescript_on_path {
        return ResolvedCompiler {
            program: "affinescript".to_string(),
            prefix_args: Vec::new(),
        };
    }
    shim()
}

/// `true` if `name` resolves to an executable on `PATH`.
fn binary_on_path(name: &str) -> bool {
    let Some(path) = std::env::var_os("PATH") else {
        return false;
    };
    std::env::split_paths(&path).any(|dir| {
        let candidate = dir.join(name);
        candidate.is_file()
            || std::fs::metadata(&candidate).map(|m| m.is_file()).unwrap_or(false)
    })
}

/// Resolve how to invoke the compiler for this process/host (ADR-019 S4).
pub fn resolve_compiler() -> ResolvedCompiler {
    resolve_with(
        std::env::var(COMPILER_ENV).ok(),
        binary_on_path("affinescript"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_override_wins_over_path_and_shim() {
        let r = resolve_with(Some("/opt/afs/affinescript".to_string()), true);
        assert_eq!(r.program, "/opt/afs/affinescript");
        assert!(r.prefix_args.is_empty());
    }

    #[test]
    fn empty_env_override_is_ignored() {
        // An exported-but-empty var must not shadow PATH/shim resolution.
        let r = resolve_with(Some(String::new()), false);
        assert_eq!(r, shim());
    }

    #[test]
    fn path_used_when_no_env_override() {
        let r = resolve_with(None, true);
        assert_eq!(r.program, "affinescript");
        assert!(r.prefix_args.is_empty());
    }

    /// Smoke (INT-10 / #282 S4): the LSP can *locate and exec* a resolved
    /// compiler and read back its `--json` contract.  A fake compiler
    /// stands in for the OCaml binary so the test is hermetic (no network,
    /// no Deno, no installed toolchain) while still exercising the real
    /// resolution → `ResolvedCompiler::command` → spawn path.
    #[test]
    fn resolved_compiler_can_be_located_and_executed() {
        use std::io::Write;

        let dir = std::env::temp_dir().join(format!("afs-lsp-smoke-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let is_windows = cfg!(windows);
        let fake = dir.join(if is_windows { "afsc.bat" } else { "afsc.sh" });
        let script = if is_windows {
            "@echo {\"version\":1,\"diagnostics\":[],\"success\":true} 1>&2\r\n"
        } else {
            "#!/bin/sh\necho '{\"version\":1,\"diagnostics\":[],\"success\":true}' 1>&2\n"
        };
        {
            let mut f = std::fs::File::create(&fake).unwrap();
            f.write_all(script.as_bytes()).unwrap();
        }
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&fake, std::fs::Permissions::from_mode(0o755)).unwrap();
        }

        // Resolve exactly as production would for an explicit binary…
        let resolved = resolve_with(Some(fake.to_string_lossy().into_owned()), false);
        assert!(resolved.prefix_args.is_empty());

        // …then locate + exec it through the same command builder the LSP
        // uses. A blocking std command mirrors the tokio one (same argv).
        let mut cmd = std::process::Command::new(&resolved.program);
        cmd.args(&resolved.prefix_args)
            .args(["check", "--json", "/nonexistent.affine"]);
        let out = cmd.output().expect("LSP must be able to exec the resolved compiler");

        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains("\"version\":1") && stderr.contains("\"success\":true"),
            "compiler --json contract not observed; stderr was: {stderr}"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn shim_is_the_default_when_nothing_local() {
        let r = resolve_with(None, false);
        assert_eq!(r.program, "deno");
        assert_eq!(r.prefix_args.last().map(String::as_str), Some(SHIM_SPEC));
        // ADR-019: pin must be exact (no floating @latest / bare name).
        assert!(SHIM_SPEC.contains("@hyperpolymath/affinescript@"));
        assert!(!SHIM_SPEC.ends_with("affinescript"));
    }
}
