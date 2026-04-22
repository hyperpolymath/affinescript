// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! build.rs — detect or require the `affinescript` binary path.
//!
//! Checks, in order:
//! 1. `AFFINESCRIPT` env var (explicit override)
//! 2. `affinescript` on PATH (standard install)
//! 3. `affinescript/_build/default/bin/main.exe` (standalone repo, submodule built)
//! 4. `../../_build/default/bin/main.exe` (monorepo dev, distributions/rattlescript/)
//!
//! The resolved path is baked in as the `AFFINESCRIPT_BIN` compile-time env.
//! Run `just setup` to build affinescript from the submodule before `cargo build`.

use std::env;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-env-changed=AFFINESCRIPT");

    let manifest = env::var("CARGO_MANIFEST_DIR").unwrap();

    let bin = if let Ok(p) = env::var("AFFINESCRIPT") {
        p
    } else if which_affinescript() {
        "affinescript".to_string()
    } else {
        // Standalone repo: submodule built with `just setup`
        let standalone_path = Path::new(&manifest)
            .join("affinescript/_build/default/bin/main.exe")
            .canonicalize()
            .ok()
            .map(|p| p.to_string_lossy().into_owned());

        if let Some(p) = standalone_path {
            p
        } else {
            // Monorepo dev: distributions/rattlescript/ → ../../_build/...
            let monorepo_path = Path::new(&manifest)
                .join("../../_build/default/bin/main.exe")
                .canonicalize()
                .ok()
                .map(|p| p.to_string_lossy().into_owned());

            monorepo_path.unwrap_or_else(|| {
                // Fall back to the name — will fail at runtime with a clear message.
                // Run `just setup` to build affinescript first.
                "affinescript".to_string()
            })
        }
    };

    println!("cargo:rustc-env=AFFINESCRIPT_BIN={bin}");
}

fn which_affinescript() -> bool {
    std::process::Command::new("which")
        .arg("affinescript")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}
