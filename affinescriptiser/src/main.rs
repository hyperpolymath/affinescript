#![allow(
    dead_code,
    clippy::too_many_arguments,
    clippy::manual_strip,
    clippy::if_same_then_else,
    clippy::vec_init_then_push
)]
#![forbid(unsafe_code)]
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affinescriptiser CLI — Wrap existing code in affine + dependent types targeting WASM
// via AffineScript. Analyses source code (Rust, C, Zig) to identify resource handles,
// generates AffineScript type annotations enforcing at-most-once or exactly-once usage,
// compiles wrapped code to WebAssembly, and reports affine type violations.

use anyhow::Result;
use clap::{Parser, Subcommand};

mod abi;
mod codegen;
mod manifest;

/// affinescriptiser — Wrap existing code in affine + dependent types targeting WASM via AffineScript
#[derive(Parser)]
#[command(name = "affinescriptiser", version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

/// Available CLI subcommands for affinescriptiser.
#[derive(Subcommand)]
enum Commands {
    /// Initialise a new affinescriptiser.toml manifest with example resource declarations.
    Init {
        /// Directory in which to create the manifest file.
        #[arg(short, long, default_value = ".")]
        path: String,
    },
    /// Validate an affinescriptiser.toml manifest for structural correctness.
    Validate {
        /// Path to the manifest file.
        #[arg(short, long, default_value = "affinescriptiser.toml")]
        manifest: String,
    },
    /// Generate AffineScript type wrappers, WASM build config, and entry point.
    Generate {
        /// Path to the manifest file.
        #[arg(short, long, default_value = "affinescriptiser.toml")]
        manifest: String,
        /// Output directory for generated artefacts.
        #[arg(short, long, default_value = "generated/affinescriptiser")]
        output: String,
    },
    /// Build the generated WASM artefacts.
    Build {
        /// Path to the manifest file.
        #[arg(short, long, default_value = "affinescriptiser.toml")]
        manifest: String,
        /// Build in release mode (optimised).
        #[arg(long)]
        release: bool,
    },
    /// Run the generated WASM module.
    Run {
        /// Path to the manifest file.
        #[arg(short, long, default_value = "affinescriptiser.toml")]
        manifest: String,
        /// Arguments to pass to the WASM runtime.
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
    /// Show manifest information (project, sources, resources, WASM config).
    Info {
        /// Path to the manifest file.
        #[arg(short, long, default_value = "affinescriptiser.toml")]
        manifest: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Init { path } => {
            manifest::init_manifest(&path)?;
        }
        Commands::Validate { manifest } => {
            let m = manifest::load_manifest(&manifest)?;
            manifest::validate(&m)?;
            println!("Valid: {}", m.project.name);
        }
        Commands::Generate { manifest, output } => {
            let m = manifest::load_manifest(&manifest)?;
            manifest::validate(&m)?;
            codegen::generate_all(&m, &output)?;
        }
        Commands::Build { manifest, release } => {
            let m = manifest::load_manifest(&manifest)?;
            codegen::build(&m, release)?;
        }
        Commands::Run { manifest, args } => {
            let m = manifest::load_manifest(&manifest)?;
            codegen::run(&m, &args)?;
        }
        Commands::Info { manifest } => {
            let m = manifest::load_manifest(&manifest)?;
            manifest::print_info(&m);
        }
    }
    Ok(())
}
