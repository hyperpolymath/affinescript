// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath)

//! RattleScript CLI — thin distribution wrapper around AffineScript with
//! Python-face pre-selected.
//!
//! All real work is delegated to the `affinescript` binary.  RattleScript
//! adds:
//!
//! - `.rattle` file extension auto-detection (maps to `--face python`)
//! - Branded banner and help text
//! - `rattle` as the CLI entry-point name

use std::env;
use std::process::{self, Command};

const AFFINESCRIPT_BIN: &str = env!("AFFINESCRIPT_BIN");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() || args[0] == "--help" || args[0] == "-h" {
        print_help();
        return;
    }

    if args[0] == "--version" || args[0] == "-V" {
        println!("rattle {VERSION} (powered by AffineScript)");
        return;
    }

    // Build affinescript command, injecting --face python before any path arg.
    // If the user already passes --face, honour their choice.
    let has_face = args.iter().any(|a| a == "--face");
    let mut cmd_args: Vec<String> = Vec::with_capacity(args.len() + 2);

    // Subcommand goes first.
    let mut rest = args.iter().peekable();
    if let Some(subcommand) = rest.next() {
        cmd_args.push(subcommand.clone());
    }

    // Inject --face python after the subcommand (unless user overrides).
    if !has_face {
        cmd_args.push("--face".to_string());
        cmd_args.push("python".to_string());
    }

    for a in rest {
        cmd_args.push(a.clone());
    }

    let status = Command::new(AFFINESCRIPT_BIN)
        .args(&cmd_args)
        .status()
        .unwrap_or_else(|e| {
            eprintln!("rattle: could not launch affinescript ({AFFINESCRIPT_BIN}): {e}");
            eprintln!("Check that affinescript is installed and on PATH.");
            process::exit(127);
        });

    process::exit(status.code().unwrap_or(1));
}

fn print_help() {
    println!(
        r#"rattle {VERSION} — Python-syntax AffineScript

USAGE:
    rattle <subcommand> [OPTIONS] <file.rattle>

SUBCOMMANDS:
    check      Type-check a .rattle file
    eval       Evaluate a .rattle file
    compile    Compile a .rattle file to WASM
    fmt        Format a .rattle file
    lint       Lint a .rattle file
    preview-python-transform  Show the canonical AffineScript produced by the
                              Python-face preprocessor (useful for learning)

FILE EXTENSIONS:
    .rattle    RattleScript (Python-face AffineScript) — preferred
    .pyaff     Python-face AffineScript — also accepted
    .affine    Canonical AffineScript — accepted (bypasses Python-face)

The Python face maps:
    def f(x: T) -> R:  →  fn f(x: T) -> R {{ ... }}
    True / False        →  true / false
    None                →  ()
    and / or / not      →  && / || / !
    import a.b          →  use a::b;
    ...and more.  Run `rattle preview-python-transform <file>` to see the
    full transform for any file.

EXAMPLES:
    rattle check hello.rattle
    rattle eval  hello.rattle
    rattle compile --target wasm-gc hello.rattle
    rattle preview-python-transform hello.rattle

Powered by AffineScript <https://github.com/hyperpolymath/affinescript>
"#
    );
}
